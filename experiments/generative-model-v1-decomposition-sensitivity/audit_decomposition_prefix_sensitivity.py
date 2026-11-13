#!/usr/bin/env python3
"""Training-prefix-only replay sensitivity audit for deterministic MSD events."""
import argparse
import csv
import hashlib
import io
import json
import math
import sys
from pathlib import Path

import numpy as np

TRAINING_PREFIX_SHA256 = "017e38220cc7590653c630455732c90c2c4c80e148a105e1dabe02e133ebe032"
TRAINING_SAMPLES = 2048
FOLD_ORIGINS = (1024, 1280, 1536, 1792)
CUTOFF_FRACTION = 0.004
PINNED_COUNTS = (0, 193, 287, 224, 128, 64, 32, 16, 8, 4, 2, 1)
MAX_REFERENCE_SCALE = 11
PINNED_REFERENCE_EVENT_MAP_SHA256 = "29a500dbcb42163c37260fafa3f67f421995ad72d3eeee5346d1afeab06d0ea6"


def read_training_prefix(path):
    """Read and hash only the CSV header and first 2,048 data rows."""
    lines = []
    with Path(path).open("rb", buffering=0) as stream:
        lines.append(stream.readline())
        for _ in range(TRAINING_SAMPLES):
            row = stream.readline()
            if not row:
                raise ValueError("training prefix ended before 2,048 rows")
            lines.append(row)
    raw = b"".join(lines)
    digest = hashlib.sha256(raw).hexdigest()
    if digest != TRAINING_PREFIX_SHA256:
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


def optimize_intervals(values, fraction=CUTOFF_FRACTION):
    """Audited interval optimization, applied only to supplied observations."""
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
        low = min(x[i - 1], x[i + 1])
        high = max(x[i - 1], x[i + 1])
        if x[i] < low:
            x[i] = low
        elif x[i] > low:
            x[i] = high
    return x


def extract_events(signal, fraction=CUTOFF_FRACTION):
    """Decompose a prefix using only globally aligned, complete higher windows.

    A trailing incomplete window is not optimized or consumed at that scale;
    its samples remain in the residual for lower scales. No padding is used.
    """
    signal = np.asarray(signal, dtype=float)
    n = len(signal)
    max_scale = int(math.floor(math.log2(n))) if n else 0
    coefficients = np.zeros((max_scale + 1, n), dtype=float)
    remainder = signal.copy()

    for scale in range(max_scale, 0, -1):
        width = 2**scale
        starts = range(0, n - width + 1, width)
        candidates = []
        for start in starts:
            values, counts = np.unique(
                remainder[start:start + width], return_counts=True
            )
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
    events = [
        (0, tick, float(mark))
        for tick, mark in enumerate(coefficients[0])
        if mark > 0
    ]
    for scale in range(1, max_scale + 1):
        width = 2**scale
        for tick in range(width // 2 - 1, n, width):
            mark = coefficients[scale, tick]
            if mark > 0:
                events.append((scale, tick, float(mark)))
    return events


def eligible_event_map(events, end):
    """Return positive events whose aligned observation window is fully seen."""
    result = {}
    for scale, tick, mark in events:
        width = 2**scale
        if scale == 0:
            eligible = 0 <= tick < end
        else:
            start = tick - width // 2 + 1
            eligible = start >= 0 and start % width == 0 and start + width <= end
        if eligible:
            key = (int(scale), int(tick))
            if key in result:
                raise ValueError(f"duplicate event key: {key}")
            result[key] = float(mark)
    return result


def canonical_event_bytes(event_map):
    rows = [
        {"scale": scale, "tick": tick, "mark": event_map[(scale, tick)]}
        for scale, tick in sorted(event_map)
    ]
    return (json.dumps(rows, sort_keys=True, separators=(",", ":"),
                       allow_nan=False) + "\n").encode("utf-8")


def sha256_bytes(value):
    return hashlib.sha256(value).hexdigest()


def summarize_values(values):
    values = np.asarray(values, dtype=float)
    if not len(values):
        return {"count": 0}
    return {
        "count": int(len(values)),
        "median": float(np.median(values)),
        "mean": float(np.mean(values)),
        "p05": float(np.quantile(values, 0.05)),
        "p95": float(np.quantile(values, 0.95)),
        "max_absolute": float(np.max(np.abs(values))),
    }


def compare_fold(reference_events, local_events, end, fold):
    reference = eligible_event_map(reference_events, end)
    local = eligible_event_map(local_events, end)
    reference_keys = set(reference)
    local_keys = set(local)
    common = sorted(reference_keys & local_keys)
    union = reference_keys | local_keys

    per_scale = []
    all_absolute, all_relative, all_log_ratio = [], [], []
    for scale in range(MAX_REFERENCE_SCALE + 1):
        width = 2**scale
        opportunities = end if scale == 0 else end // width
        ref_scale = {key: value for key, value in reference.items() if key[0] == scale}
        local_scale = {key: value for key, value in local.items() if key[0] == scale}
        common_scale = sorted(set(ref_scale) & set(local_scale))
        absolute = [abs(local_scale[key] - ref_scale[key]) for key in common_scale]
        relative = [abs(local_scale[key] - ref_scale[key]) / ref_scale[key]
                    for key in common_scale]
        log_ratio = [math.log(local_scale[key] / ref_scale[key]) for key in common_scale]
        all_absolute.extend(absolute)
        all_relative.extend(relative)
        all_log_ratio.extend(log_ratio)
        ref_sum = float(sum(ref_scale.values()))
        local_sum = float(sum(local_scale.values()))
        scale_union = set(ref_scale) | set(local_scale)
        per_scale.append({
            "scale": scale,
            "window_width": width,
            "eligible_opportunities": int(opportunities),
            "reference_positive_events": len(ref_scale),
            "prefix_positive_events": len(local_scale),
            "event_key_agreement_count": len(common_scale),
            "event_key_union_count": len(scale_union),
            "event_key_jaccard": (
                None if opportunities == 0 else
                len(common_scale) / len(scale_union) if scale_union else 1.0
            ),
            "additions_prefix_only": len(set(local_scale) - set(ref_scale)),
            "removals_reference_only": len(set(ref_scale) - set(local_scale)),
            "reference_positive_mark_sum": ref_sum,
            "prefix_positive_mark_sum": local_sum,
            "positive_mark_sum_difference": local_sum - ref_sum,
            "matched_mark_absolute_difference": summarize_values(absolute),
            "matched_mark_relative_absolute_difference": summarize_values(relative),
            "matched_mark_log_ratio": summarize_values(log_ratio),
        })

    reference_bytes = canonical_event_bytes(reference)
    local_bytes = canonical_event_bytes(local)
    return {
        "fold": fold,
        "prefix_end": end,
        "observable_range": [0, end],
        "reference_event_count": len(reference),
        "prefix_event_count": len(local),
        "event_key_agreement_count": len(common),
        "event_key_union_count": len(union),
        "event_key_jaccard": len(common) / len(union) if union else 1.0,
        "additions_prefix_only": len(local_keys - reference_keys),
        "removals_reference_only": len(reference_keys - local_keys),
        "reference_event_map_sha256": sha256_bytes(reference_bytes),
        "prefix_event_map_sha256": sha256_bytes(local_bytes),
        "matched_mark_absolute_difference": summarize_values(all_absolute),
        "matched_mark_relative_absolute_difference": summarize_values(all_relative),
        "matched_mark_log_ratio": summarize_values(all_log_ratio),
        "per_scale": per_scale,
    }


def run(path):
    signal = read_training_prefix(path)

    # Recompute the frozen reference twice and compare canonical bytes before
    # producing any fold comparison. The 2048-sample reference has only
    # complete globally aligned windows at every supported scale.
    reference_first = extract_events(signal)
    reference_second = extract_events(signal.copy())
    reference_first_map = eligible_event_map(reference_first, TRAINING_SAMPLES)
    reference_second_map = eligible_event_map(reference_second, TRAINING_SAMPLES)
    reference_bytes = canonical_event_bytes(reference_first_map)
    if reference_bytes != canonical_event_bytes(reference_second_map):
        raise ValueError("independent reference event-map determinism check failed")

    counts = [
        sum(scale == event_scale for scale, _, _ in reference_first)
        for event_scale in range(MAX_REFERENCE_SCALE + 1)
    ]
    if tuple(counts) != PINNED_COUNTS:
        raise ValueError(f"pinned reference event-count conformance failed: {counts}")
    reference_sha256 = sha256_bytes(reference_bytes)
    if reference_sha256 != PINNED_REFERENCE_EVENT_MAP_SHA256:
        raise ValueError(
            "pinned reference event-map SHA-256 mismatch: " + reference_sha256
        )

    folds = []
    for number, end in enumerate(FOLD_ORIGINS, 1):
        local_events = extract_events(signal[:end])
        folds.append(compare_fold(reference_first, local_events, end, number))

    configuration = {
        "training_prefix": [0, TRAINING_SAMPLES],
        "fold_origins": list(FOLD_ORIGINS),
        "cutoff_fraction": CUTOFF_FRACTION,
        "pinned_positive_events_by_scale": list(PINNED_COUNTS),
        "window_rule": "globally_aligned_complete_windows_only",
        "emission_offset": "2^(scale-1)-1",
        "validation_test_rows_read": False,
        "synthetic_or_artificial_events": False,
    }
    script_bytes = Path(__file__).read_bytes()
    return {
        "experiment": "westermo-training-prefix-decomposition-prefix-sensitivity",
        "status": "descriptive_training_only_not_model_selection",
        "training_prefix": [0, TRAINING_SAMPLES],
        "training_prefix_sha256": TRAINING_PREFIX_SHA256,
        "validation_test_rows_read": False,
        "synthetic_or_artificial_events": False,
        "cutoff_fraction": CUTOFF_FRACTION,
        "fold_origins": list(FOLD_ORIGINS),
        "window_rule": "globally_aligned_complete_windows_only",
        "emission_offset": "2^(scale-1)-1",
        "reference_positive_events_by_scale_0_to_11": counts,
        "reference_event_map_sha256": reference_sha256,
        "runtime_versions": {"python": sys.version.split()[0], "numpy": np.__version__},
        "script_sha256": sha256_bytes(script_bytes),
        "configuration_sha256": sha256_bytes(
            json.dumps(configuration, sort_keys=True, separators=(",", ":")).encode()
        ),
        "folds": folds,
        "limitations": [
            "Prefix-local interval optimization uses all context within the available prefix; this is not strict per-tick causality.",
            "Center-left event emission can precede completion of its aligned observation window.",
            "This audit measures decomposition stability only; it does not fit an HMM, occurrence process, or mark family.",
            "No score, representation, regime count, cutoff, pooling strategy, or model family is selected.",
            "Only the checksum-verified training prefix is read; validation/test rows are not consumed.",
        ],
    }


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("trace_csv", type=Path)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    report = run(args.trace_csv)
    rendered = json.dumps(report, indent=2, sort_keys=True, allow_nan=False) + "\n"
    args.output.write_text(rendered, encoding="utf-8")
    report_sha = sha256_bytes(rendered.encode("utf-8"))
    args.output.with_suffix(args.output.suffix + ".sha256").write_text(
        report_sha + "\n", encoding="utf-8"
    )
    print(json.dumps({"report_sha256": report_sha,
                      "reference_event_map_sha256": report["reference_event_map_sha256"],
                      "folds": len(report["folds"])}, sort_keys=True))
