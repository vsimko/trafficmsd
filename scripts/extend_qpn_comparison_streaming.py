#!/usr/bin/env python3
"""Extend the measured/offline QPN report with a test-only streaming condition."""

from __future__ import annotations

import argparse
import hashlib
import json
import math
import statistics
from pathlib import Path

T95 = {
    1: 12.706205, 2: 4.302653, 3: 3.182446, 4: 2.776445, 5: 2.570582,
    6: 2.446912, 7: 2.364624, 8: 2.306004, 9: 2.262157, 10: 2.228139,
    11: 2.200985, 12: 2.178813, 13: 2.160369, 14: 2.144787, 15: 2.131450,
    16: 2.119905, 17: 2.109816, 18: 2.100922, 19: 2.093024, 20: 2.085963,
    21: 2.079614, 22: 2.073873, 23: 2.068658, 24: 2.063899, 25: 2.059539,
    26: 2.055529, 27: 2.051831, 28: 2.048407, 29: 2.045230,
}
METRICS = {"throughput": "jobs/s", "queue_length": "jobs", "utilization": "ratio", "residence_time": "s"}


def load(path: Path):
    raw = path.read_bytes()
    value = json.loads(raw, parse_constant=lambda token: (_ for _ in ()).throw(ValueError(token)))
    if not isinstance(value, dict):
        raise ValueError(f"{path} must contain a JSON object")
    return value, raw


def values(result, metric):
    found = []
    for replication in result["replications"]:
        matches = [item["value"] for item in replication["raw_observations"] if item["metric"] == metric]
        if len(matches) != 1 or not math.isfinite(float(matches[0])):
            raise ValueError(f"replication {replication['index']} must have one finite {metric}")
        found.append(float(matches[0]))
    if len(found) < 2:
        raise ValueError(f"{metric} requires at least two replications")
    return found


def interval(data):
    n = len(data)
    df = n - 1
    if df not in T95:
        raise ValueError("95% Student-t intervals support 2 to 30 replications")
    mean = statistics.mean(data)
    sd = statistics.stdev(data)
    half = T95[df] * sd / math.sqrt(n)
    return mean, [mean - half, mean + half], sd


def welch(left, right):
    n1, n2 = len(left), len(right)
    v1, v2 = statistics.variance(left), statistics.variance(right)
    a, b = v1 / n1, v2 / n2
    se = math.sqrt(a + b)
    den = a * a / (n1 - 1) + b * b / (n2 - 1)
    df = (a + b) ** 2 / den if den else n1 + n2 - 2
    critical = T95[max(1, min(29, math.floor(df)))]
    diff = statistics.mean(right) - statistics.mean(left)
    half = critical * se
    return {"estimate": diff, "confidence_interval_95": [diff - half, diff + half], "welch_degrees_freedom": df}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--manifest", type=Path, required=True)
    parser.add_argument("--measured-result", type=Path, required=True)
    parser.add_argument("--replay-result", type=Path, required=True)
    parser.add_argument("--streaming-result", type=Path, required=True)
    parser.add_argument("--report-dir", type=Path, required=True)
    args = parser.parse_args()

    manifest, _ = load(args.manifest)
    measured, _ = load(args.measured_result)
    replay, _ = load(args.replay_result)
    streaming, streaming_raw = load(args.streaming_result)
    report_path = args.report_dir / "comparison.json"
    comparison, _ = load(report_path)

    results = [measured, replay, streaming]
    if any(result.get("validation") != "passed" for result in results):
        raise ValueError("all three LINE conditions must pass validation")
    if any(result["backend"] != measured["backend"] for result in results):
        raise ValueError("all conditions must use the same LINE backend and version")
    if any(result["checksums"]["model_sha256"] != manifest["model"]["sha256"] for result in results):
        raise ValueError("all conditions must use the exact resolved QPN model")
    if any(result["runtime"]["termination_policy"] != measured["runtime"]["termination_policy"] for result in results):
        raise ValueError("all conditions must use the same termination policy")
    if any(result["runtime"]["warmup_policy"] != measured["runtime"]["warmup_policy"] for result in results):
        raise ValueError("all conditions must use the same warm-up policy")

    streaming_meta = manifest["conditions"]["streaming"]
    if streaming_meta["schedule_sha256"] != streaming["checksums"]["schedule_sha256"]:
        raise ValueError("streaming schedule checksum does not match the manifest")
    seed_sets = [{item["seed"] for item in result["replications"]} for result in results]
    if any(seed_sets[i] & seed_sets[j] for i in range(3) for j in range(i + 1, 3)):
        raise ValueError("all three conditions must use disjoint replication seeds")

    for metric, unit in METRICS.items():
        m = values(measured, metric)
        r = values(replay, metric)
        s = values(streaming, metric)
        mean, ci, sd = interval(s)
        comparison["metrics"][metric]["streaming_fixed_duration"] = {
            "estimate": mean, "confidence_interval_95": ci,
            "standard_deviation": sd, "replication_count": len(s), "unit": unit,
        }
        comparison["metrics"][metric]["streaming_minus_measured"] = welch(m, s)
        comparison["metrics"][metric]["streaming_minus_replay"] = welch(r, s)

    comparison["comparison_id"] = "westermo-measured-offline-streaming-qpn-v1"
    comparison["conditions"]["streaming_fixed_duration"] = streaming_meta
    comparison["runtime_seconds"]["streaming_fixed_duration"] = streaming["runtime"]["elapsed_seconds_total"]
    comparison["input_checksums"]["streaming_result_sha256"] = hashlib.sha256(streaming_raw).hexdigest()
    comparison["streaming_policy"] = manifest.get("streaming_policy")
    comparison["interpretation_note"] = (
        "Streaming is the test-only fixed-duration multiscale prototype at T_min=32s; "
        "it is not a production optimizer or stochastic generator."
    )
    report_path.write_text(json.dumps(comparison, indent=2, sort_keys=True, allow_nan=False) + "\n", encoding="utf-8")

    lines = [
        "",
        "## Test-only streaming fixed-duration condition (T_min=32 s)",
        "",
        "The same resolved QPN model, byte-to-job mapping, warm-up, completion budget and LINE backend were used. "
        "Streaming uses a third disjoint seed set.",
        "",
        "| Metric | Streaming estimate (95% CI) | Streaming − measured (95% CI) | Streaming − offline (95% CI) |",
        "|---|---:|---:|---:|",
    ]
    for metric in METRICS:
        item = comparison["metrics"][metric]
        stream = item["streaming_fixed_duration"]
        sm = item["streaming_minus_measured"]
        sr = item["streaming_minus_replay"]
        lines.append(
            f"| {metric} ({METRICS[metric]}) | {stream['estimate']:.6g} "
            f"[{stream['confidence_interval_95'][0]:.6g}, {stream['confidence_interval_95'][1]:.6g}] | "
            f"{sm['estimate']:.6g} [{sm['confidence_interval_95'][0]:.6g}, {sm['confidence_interval_95'][1]:.6g}] | "
            f"{sr['estimate']:.6g} [{sr['confidence_interval_95'][0]:.6g}, {sr['confidence_interval_95'][1]:.6g}] |"
        )
    with (args.report_dir / "report.md").open("a", encoding="utf-8") as handle:
        handle.write("\n".join(lines) + "\n")
    print("\n".join(lines))


if __name__ == "__main__":
    main()
