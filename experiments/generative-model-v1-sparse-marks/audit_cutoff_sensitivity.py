#!/usr/bin/env python3
"""Training-prefix-only sensitivity audit for decomposition occurrence counts."""
import argparse
import csv
import hashlib
import io
import json
from pathlib import Path

import numpy as np

TRAINING_PREFIX_SHA256 = "017e38220cc7590653c630455732c90c2c4c80e148a105e1dabe02e133ebe032"
TRAINING_SAMPLES = 2048
CUTOFF_FRACTIONS = (0.001, 0.002, 0.004, 0.008, 0.016, 0.032)


def read_prefix(path):
    lines = []
    with Path(path).open("rb", buffering=0) as stream:
        lines.append(stream.readline())
        for _ in range(TRAINING_SAMPLES):
            row = stream.readline()
            if not row:
                raise ValueError("training prefix ended before 2,048 rows")
            lines.append(row)
    raw = b"".join(lines)
    if hashlib.sha256(raw).hexdigest() != TRAINING_PREFIX_SHA256:
        raise ValueError("training-prefix SHA-256 does not match verified artifact")
    reader = csv.DictReader(io.StringIO(raw.decode("utf-8")))
    if reader.fieldnames != ["sample_index", "value"]:
        raise ValueError("expected sample_index,value columns")
    values = []
    for expected, row in enumerate(reader):
        if int(row["sample_index"]) != expected:
            raise ValueError("missing or misaligned training-prefix sample")
        value = float(row["value"])
        if not np.isfinite(value) or value < 0:
            raise ValueError("training values must be finite and non-negative")
        values.append(value)
    if len(values) != TRAINING_SAMPLES:
        raise ValueError("expected exactly 2,048 training-prefix rows")
    return np.asarray(values, dtype=float)


def optimize_intervals(values, fraction):
    x = np.asarray(values, dtype=float).copy()
    n = len(x)
    if n < 3:
        return x
    cutoff = n * fraction
    if cutoff > 1:
        binary = (x > 0).astype(int)
        runs = np.zeros(n + 1, dtype=int)
        for i in range(n - 1, -1, -1):
            runs[i] = binary[i] * (runs[i + 1] + binary[i])
        runs = runs[:n]
        for i in range(1, n):
            runs[i] = 0 if runs[i] == 0 else max(runs[i], runs[i - 1])
        x = (runs > cutoff) * x
    for i in range(1, n - 1):
        low, high = min(x[i - 1], x[i + 1]), max(x[i - 1], x[i + 1])
        if x[i] < low:
            x[i] = low
        elif x[i] > low:
            x[i] = high
    return x


def extract_event_counts(signal, fraction):
    n = len(signal)
    max_scale = int(round(np.log2(n)))
    coefficients = np.zeros((max_scale + 1, n))
    remainder = signal.copy()
    for scale in range(max_scale, 0, -1):
        width = 2**scale
        candidates = []
        for start in range(0, n, width):
            values, counts = np.unique(remainder[start:start + width], return_counts=True)
            singletons = values[counts == 1]
            candidates.append(float(singletons.max()) if len(singletons) else 0.0)
        candidates = optimize_intervals(candidates, fraction)
        for index, mark in enumerate(candidates):
            start = index * width
            if mark <= 0:
                continue
            hits = np.flatnonzero(remainder[start:start + width] == mark)
            if len(hits):
                remainder[start + hits[0]] = 0.0
            tick = start + width // 2 - 1
            remainder[tick] = max(0.0, remainder[tick] - mark)
            coefficients[scale, start:start + width] = mark
    coefficients[0] = optimize_intervals(remainder, fraction)
    counts = [int(np.count_nonzero(coefficients[0] > 0))]
    for scale in range(1, max_scale + 1):
        width = 2**scale
        ticks = range(width // 2 - 1, n, width)
        counts.append(int(sum(coefficients[scale, tick] > 0 for tick in ticks)))
    return counts


def run(path):
    signal = read_prefix(path)
    rows = []
    for fraction in CUTOFF_FRACTIONS:
        positives = extract_event_counts(signal, fraction)
        per_scale = []
        for scale, positive in enumerate(positives):
            opportunities = len(signal) // (2**scale)
            per_scale.append({
                "scale": scale,
                "opportunities": opportunities,
                "positive_events": int(positive),
                "zero_events": int(opportunities - positive),
                "occupancy": positive / opportunities,
                "variation": ("all_zero" if positive == 0 else
                              "all_one" if positive == opportunities else "variable"),
            })
        rows.append({"cutoff_fraction": fraction, "scales": per_scale})
    baseline = next(row for row in rows if row["cutoff_fraction"] == 0.004)
    expected = [0, 193, 287, 224, 128, 64, 32, 16, 8, 4, 2, 1]
    observed = [item["positive_events"] for item in baseline["scales"]]
    if observed != expected:
        raise ValueError(f"default cutoff conformance failed: {observed}")
    return {
        "experiment": "westermo-training-prefix-decomposition-cutoff-sensitivity",
        "status": "descriptive_training_only_not_model_selection",
        "training_prefix": [0, TRAINING_SAMPLES],
        "training_prefix_sha256": TRAINING_PREFIX_SHA256,
        "validation_test_rows_read": False,
        "synthetic_or_artificial_events": False,
        "cutoff_fractions": list(CUTOFF_FRACTIONS),
        "results": rows,
        "limitations": [
            "The cutoff grid is descriptive, not a selected cutoff.",
            "Only the interval cutoff fraction varies; extraction semantics are unchanged.",
            "Results are conditional on this single training prefix and dataset.",
            "No occurrence probability, pooling, policy status, regime, or mark family is fitted or selected.",
            "Validation/test rows are not read; no synthetic events are added.",
        ],
    }


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("trace_csv", type=Path)
    parser.add_argument("--output", type=Path)
    args = parser.parse_args()
    report = run(args.trace_csv)
    rendered = json.dumps(report, indent=2, sort_keys=True) + "\n"
    if args.output:
        args.output.write_text(rendered, encoding="utf-8")
    print(rendered)
