#!/usr/bin/env python3
"""Summarize measured/replay QPN outcomes with independent-replication intervals."""

from __future__ import annotations

import argparse
import json
import math
import statistics
from pathlib import Path
from typing import Any

T_CRITICAL_95 = {
    1: 12.706205, 2: 4.302653, 3: 3.182446, 4: 2.776445, 5: 2.570582,
    6: 2.446912, 7: 2.364624, 8: 2.306004, 9: 2.262157, 10: 2.228139,
    11: 2.200985, 12: 2.178813, 13: 2.160369, 14: 2.144787, 15: 2.131450,
    16: 2.119905, 17: 2.109816, 18: 2.100922, 19: 2.093024, 20: 2.085963,
    21: 2.079614, 22: 2.073873, 23: 2.068658, 24: 2.063899, 25: 2.059539,
    26: 2.055529, 27: 2.051831, 28: 2.048407, 29: 2.045230,
}


def load_json(path: Path) -> tuple[dict[str, Any], bytes]:
    raw = path.read_bytes()
    value = json.loads(raw, parse_constant=lambda token: (_ for _ in ()).throw(
        ValueError(f"Non-finite JSON constant is forbidden: {token}")
    ))
    if not isinstance(value, dict):
        raise ValueError(f"{path} must contain a JSON object")
    return value, raw


def observations(result: dict[str, Any], metric: str) -> list[float]:
    values = []
    for replication in result["replications"]:
        matches = [
            item["value"] for item in replication["raw_observations"]
            if item["metric"] == metric
        ]
        if len(matches) != 1:
            raise ValueError(f"replication {replication['index']} must contain one {metric} observation")
        value = float(matches[0])
        if not math.isfinite(value):
            raise ValueError(f"{metric} observation must be finite")
        values.append(value)
    if len(values) < 2:
        raise ValueError(f"{metric} requires at least two replications per condition")
    return values


def mean_interval(values: list[float]) -> tuple[float, float, float]:
    df = len(values) - 1
    if df not in T_CRITICAL_95:
        raise ValueError("95% Student-t intervals support 2 to 30 replications")
    mean = statistics.mean(values)
    standard_deviation = statistics.stdev(values)
    half_width = T_CRITICAL_95[df] * standard_deviation / math.sqrt(len(values))
    return mean, mean - half_width, mean + half_width


def welch_difference(
    measured: list[float],
    replay: list[float],
) -> tuple[float, float, float, float]:
    """Replay minus measured; conservative Student-t critical value rounded down in df."""
    n1, n2 = len(measured), len(replay)
    mean1, mean2 = statistics.mean(measured), statistics.mean(replay)
    var1, var2 = statistics.variance(measured), statistics.variance(replay)
    component1, component2 = var1 / n1, var2 / n2
    standard_error = math.sqrt(component1 + component2)
    denominator = (component1 * component1) / (n1 - 1) + (component2 * component2) / (n2 - 1)
    degrees_freedom = ((component1 + component2) ** 2 / denominator) if denominator else float(n1 + n2 - 2)
    df_floor = max(1, min(29, math.floor(degrees_freedom)))
    critical = T_CRITICAL_95[df_floor]
    difference = mean2 - mean1
    half_width = critical * standard_error
    return difference, difference - half_width, difference + half_width, degrees_freedom


def summarize(
    manifest_path: Path,
    measured_path: Path,
    replay_path: Path,
    control_path: Path,
    control_config_path: Path,
    output_dir: Path,
) -> dict[str, Any]:
    manifest, manifest_raw = load_json(manifest_path)
    measured, measured_raw = load_json(measured_path)
    replay, replay_raw = load_json(replay_path)
    control, control_raw = load_json(control_path)
    control_config, control_config_raw = load_json(control_config_path)

    if measured.get("validation") != "passed" or replay.get("validation") != "passed":
        raise ValueError("both measured and replay LINE results must have validation=passed")
    if control.get("validation") != "passed":
        raise ValueError("the analytical M/M/1 control must pass")
    if measured["backend"] != replay["backend"] or measured["backend"]["id"] != "line-solver":
        raise ValueError("measured and replay results must use the same LINE backend/version")
    if measured["checksums"]["model_sha256"] != replay["checksums"]["model_sha256"]:
        raise ValueError("measured and replay results must use the exact same QPN model")
    if measured["runtime"]["termination_policy"] != replay["runtime"]["termination_policy"]:
        raise ValueError("measured and replay termination policies must match")
    if measured["runtime"]["warmup_policy"] != replay["runtime"]["warmup_policy"]:
        raise ValueError("measured and replay warm-up policies must match")

    measured_seeds = {rep["seed"] for rep in measured["replications"]}
    replay_seeds = {rep["seed"] for rep in replay["replications"]}
    if measured_seeds & replay_seeds:
        raise ValueError("conditions must use disjoint seeds for independent-replication inference")

    measured_meta = manifest["conditions"]["measured"]
    replay_meta = manifest["conditions"]["replay"]
    if measured_meta["schedule_sha256"] != measured["checksums"]["schedule_sha256"]:
        raise ValueError("measured result schedule checksum does not match the manifest")
    if replay_meta["schedule_sha256"] != replay["checksums"]["schedule_sha256"]:
        raise ValueError("replay result schedule checksum does not match the manifest")
    if manifest["model"]["sha256"] != measured["checksums"]["model_sha256"]:
        raise ValueError("QPN model checksum does not match the manifest")

    expected_control_rate = measured_meta["total_jobs"] / measured_meta["horizon"]
    if not math.isclose(
        float(control["arrival_rate_per_s"]), expected_control_rate,
        rel_tol=0.0, abs_tol=1e-12,
    ):
        raise ValueError("analytical control arrival rate must equal measured offered job rate")
    if control["model_sha256"] != manifest["model"]["sha256"]:
        raise ValueError("analytical control must use the same authored QPN model spec")

    metric_units = {
        "throughput": "jobs/s",
        "queue_length": "jobs",
        "utilization": "ratio",
        "residence_time": "s",
    }
    metric_summary = {}
    for metric, unit in metric_units.items():
        measured_values = observations(measured, metric)
        replay_values = observations(replay, metric)
        measured_mean, measured_low, measured_high = mean_interval(measured_values)
        replay_mean, replay_low, replay_high = mean_interval(replay_values)
        difference, difference_low, difference_high, degrees_freedom = welch_difference(
            measured_values, replay_values
        )
        metric_summary[metric] = {
            "unit": unit,
            "measured": {
                "estimate": measured_mean,
                "confidence_interval_95": [measured_low, measured_high],
                "standard_deviation": statistics.stdev(measured_values),
                "replication_count": len(measured_values),
            },
            "deterministic_replay": {
                "estimate": replay_mean,
                "confidence_interval_95": [replay_low, replay_high],
                "standard_deviation": statistics.stdev(replay_values),
                "replication_count": len(replay_values),
            },
            "replay_minus_measured": {
                "estimate": difference,
                "confidence_interval_95": [difference_low, difference_high],
                "welch_degrees_freedom": degrees_freedom,
                "relative_change": None if measured_mean == 0 else difference / measured_mean,
            },
        }

    comparison = {
        "schema_version": "1.0",
        "comparison_id": "westermo-measured-vs-deterministic-replay-qpn-v1",
        "status": "completed",
        "dataset_id": manifest.get("source_dataset_id"),
        "backend": measured["backend"],
        "model_sha256": manifest["model"]["sha256"],
        "service_rate_per_s": manifest["model"]["service_rate_per_s"],
        "service_rate_policy": manifest["model"]["service_rate_policy"],
        "mean_offered_load": {
            "measured": measured_meta["mean_offered_job_rate_per_s"] / manifest["model"]["service_rate_per_s"],
            "deterministic_replay": replay_meta["mean_offered_job_rate_per_s"] / manifest["model"]["service_rate_per_s"],
        },
        "source_manifest_sha256": manifest["source_manifest_sha256"],
        "builder_config_sha256": manifest["config_sha256"],
        "input_checksums": {
            "manifest_sha256": __import__("hashlib").sha256(manifest_raw).hexdigest(),
            "measured_result_sha256": __import__("hashlib").sha256(measured_raw).hexdigest(),
            "replay_result_sha256": __import__("hashlib").sha256(replay_raw).hexdigest(),
            "analytical_control_sha256": __import__("hashlib").sha256(control_raw).hexdigest(),
            "analytical_control_config_sha256": __import__("hashlib").sha256(control_config_raw).hexdigest(),
        },
        "conditions": {
            "measured": measured_meta,
            "deterministic_replay": replay_meta,
        },
        "runtime_seconds": {
            "measured": measured["runtime"]["elapsed_seconds_total"],
            "deterministic_replay": replay["runtime"]["elapsed_seconds_total"],
            "analytical_control": control["runtime_seconds_total"],
        },
        "predeclared_metrics": {
            "primary": ["throughput", "queue_length"],
            "secondary": ["utilization", "residence_time"],
        },
        "metrics": metric_summary,
        "analytical_control": {
            "control_id": control["control_id"],
            "validation": control["validation"],
            "arrival_rate_per_s": control["arrival_rate_per_s"],
            "metrics": control["metrics"],
        },
        "limitations": [
            "The replay condition is deterministic reconstruction, not stochastic generation.",
            "One QPN job represents the configured abstract byte quantity, not a packet, flow, request, or application transaction.",
            "The authored system is a single FCFS queue with exponential service; results are specific to this QPN model.",
            "Westermo is industrial-network traffic and is not evidence of general Internet representativeness.",
            "No packet-level TCP/IP, routing, or congestion-control fidelity is claimed.",
        ],
        "generated_condition": {
            "status": "blocked",
            "reason": "No stochastic traffic generator and fitting procedure are implemented in this experiment version.",
        },
    }

    output_dir.mkdir(parents=True, exist_ok=True)
    comparison_path = output_dir / "comparison.json"
    comparison_path.write_text(
        json.dumps(comparison, indent=2, sort_keys=True, allow_nan=False) + "\n",
        encoding="utf-8",
    )

    lines = [
        "# Westermo QPN workload evaluation v1",
        "",
        f"- Dataset: `{comparison['dataset_id']}`",
        f"- LINE: `{measured['backend']['version']}` / `{measured['backend']['engine']}`",
        f"- QPN model SHA-256: `{comparison['model_sha256']}`",
        f"- Service rate: {comparison['service_rate_per_s']:.6g} jobs/s ({comparison['service_rate_policy']})",
        f"- Mean offered load (measured/replay): {comparison['mean_offered_load']['measured']:.6g} / {comparison['mean_offered_load']['deterministic_replay']:.6g}",
        f"- Measured schedule: `{measured_meta['schedule_sha256']}`",
        f"- Replay schedule: `{replay_meta['schedule_sha256']}`",
        f"- Measured bins/jobs/bytes: {measured_meta['bin_count']} / {measured_meta['total_jobs']} / {measured_meta['total_bytes']:.0f}",
        f"- Replay bins/jobs/bytes: {replay_meta['bin_count']} / {replay_meta['total_jobs']} / {replay_meta['total_bytes']:.0f}",
        f"- Final carried remainder (measured/replay): {measured_meta['final_remainder_bytes']:.6g} / {replay_meta['final_remainder_bytes']:.6g} bytes",
        f"- Simulation runtime (measured/replay/control): {measured['runtime']['elapsed_seconds_total']:.3f} / {replay['runtime']['elapsed_seconds_total']:.3f} / {control['runtime_seconds_total']:.3f} seconds",
        "",
        "## Results",
        "",
        "| Metric | Measured estimate (95% CI) | Deterministic replay estimate (95% CI) | Replay − measured (95% CI) |",
        "|---|---:|---:|---:|",
    ]
    for metric, item in metric_summary.items():
        m = item["measured"]
        r = item["deterministic_replay"]
        d = item["replay_minus_measured"]
        lines.append(
            f"| {metric} ({item['unit']}) | {m['estimate']:.6g} "
            f"[{m['confidence_interval_95'][0]:.6g}, {m['confidence_interval_95'][1]:.6g}] | "
            f"{r['estimate']:.6g} [{r['confidence_interval_95'][0]:.6g}, "
            f"{r['confidence_interval_95'][1]:.6g}] | {d['estimate']:.6g} "
            f"[{d['confidence_interval_95'][0]:.6g}, {d['confidence_interval_95'][1]:.6g}] |"
        )
    lines.extend([
        "",
        "Primary metrics: throughput and queue population. Secondary metrics: utilization and residence time.",
        "The replay-minus-measured intervals use independent replications with disjoint seed sets and a Welch two-sample standard error.",
        "",
        "## Analytical control",
        "",
        f"The Poisson-arrival M/M/1 control used the measured offered job rate "
        f"({control['arrival_rate_per_s']:.6g} jobs/s) and passed its analytical confidence checks.",
        "",
        "## Interpretation limits",
        "",
        "- The replay condition is deterministic reconstruction, not stochastic generation.",
        "- The generated condition remains blocked until a stochastic generator and fitting procedure exist.",
        "- Results apply only to the authored single-queue QPN and explicit byte-to-job mapping.",
        "- Westermo is industrial-network traffic; no general Internet representativeness is claimed.",
        "- No packet-level network fidelity is claimed.",
        "",
        f"Machine-readable results and provenance: `comparison.json`.",
    ])
    (output_dir / "report.md").write_text("\n".join(lines) + "\n", encoding="utf-8")
    print("\n".join(lines))
    return comparison


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--manifest", type=Path, required=True)
    parser.add_argument("--measured-result", type=Path, required=True)
    parser.add_argument("--replay-result", type=Path, required=True)
    parser.add_argument("--control-result", type=Path, required=True)
    parser.add_argument("--control-config", type=Path, required=True)
    parser.add_argument("--output-dir", type=Path, required=True)
    args = parser.parse_args()
    summarize(
        args.manifest,
        args.measured_result,
        args.replay_result,
        args.control_result,
        args.control_config,
        args.output_dir,
    )


if __name__ == "__main__":
    main()
