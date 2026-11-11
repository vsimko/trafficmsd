#!/usr/bin/env python3
"""Build checksummed measured and deterministic-replay QPN arrival schedules."""

from __future__ import annotations

import argparse
import csv
import copy
import hashlib
import json
import math
from decimal import Decimal, InvalidOperation
from pathlib import Path
from typing import Any

ROOT = Path(__file__).resolve().parents[1]
EXPERIMENT_DIR = ROOT / "experiments" / "qpn-workload-evaluation-v1"
SCHEMA_DIR = ROOT / "schemas" / "simulator" / "v1"
BUILDER_VERSION = "1.0"


def sha256_bytes(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def canonical_json(value: Any) -> bytes:
    return (
        json.dumps(value, sort_keys=True, indent=2, ensure_ascii=False, allow_nan=False)
        + "\n"
    ).encode("utf-8")


def load_json(path: Path) -> tuple[dict[str, Any], bytes]:
    raw = path.read_bytes()
    value = json.loads(raw, parse_constant=lambda token: (_ for _ in ()).throw(
        ValueError(f"Non-finite JSON constant is forbidden: {token}")
    ))
    if not isinstance(value, dict):
        raise ValueError(f"{path} must contain a JSON object")
    return value, raw


def read_signal_csv(path: Path) -> tuple[list[Decimal], bytes]:
    raw = path.read_bytes()
    try:
        text = raw.decode("utf-8")
    except UnicodeDecodeError as error:
        raise ValueError(f"{path} must be UTF-8") from error
    rows = csv.DictReader(text.splitlines())
    if rows.fieldnames != ["sample_index", "value"]:
        raise ValueError(f"{path} must have exactly sample_index,value columns")
    values: list[Decimal] = []
    for expected_index, row in enumerate(rows):
        if row.get("sample_index") != str(expected_index):
            raise ValueError(
                f"{path}: sample_index must be contiguous from zero; "
                f"expected {expected_index}, got {row.get('sample_index')!r}"
            )
        try:
            value = Decimal(row["value"])
        except (InvalidOperation, TypeError) as error:
            raise ValueError(f"{path}: invalid numeric value at bin {expected_index}") from error
        if not value.is_finite() or value < 0:
            raise ValueError(f"{path}: bin {expected_index} must be finite and non-negative")
        values.append(value)
    if len(values) < 1:
        raise ValueError(f"{path} contains no samples")
    return values, raw


def build_schedule(
    values: list[Decimal],
    *,
    schedule_id: str,
    time_unit: str,
    bin_width: Decimal,
    bytes_per_job: Decimal,
) -> tuple[dict[str, Any], dict[str, Any]]:
    if not values:
        raise ValueError("signal must contain at least one bin")
    if not bin_width.is_finite() or bin_width <= 0:
        raise ValueError("bin_width must be finite and positive")
    if not bytes_per_job.is_finite() or bytes_per_job <= 0:
        raise ValueError("bytes_per_job must be finite and positive")

    cumulative_bytes = Decimal(0)
    previous_jobs = 0
    batches = []
    for source_bin, byte_count in enumerate(values):
        cumulative_bytes += byte_count
        jobs_through_bin = int(cumulative_bytes // bytes_per_job)
        job_count = jobs_through_bin - previous_jobs
        batches.append({
            "source_bin": source_bin,
            "time": float(Decimal(source_bin) * bin_width),
            "job_count": job_count,
        })
        previous_jobs = jobs_through_bin

    remainder = cumulative_bytes - Decimal(previous_jobs) * bytes_per_job
    schedule = {
        "schema_version": "1.0",
        "schedule_id": schedule_id,
        "time_unit": time_unit,
        "bin_width": float(bin_width),
        "arrival_placement": "bin_start",
        "workload_mapping": {
            "quantity_unit": "bytes",
            "bytes_per_job": float(bytes_per_job),
            "remainder_policy": "carry_forward",
            "final_remainder_bytes": float(remainder),
        },
        "batches": batches,
        "generation": {
            "mode": "deterministic",
            "generator": "trafficmsd-qpn-workload-schedule-builder",
            "generator_version": BUILDER_VERSION,
            "seed": None,
        },
    }
    nonempty_bins = [row["source_bin"] for row in batches if row["job_count"] > 0]
    first_nonempty_bin = min(nonempty_bins) if nonempty_bins else -1
    last_nonempty_bin = max(nonempty_bins) if nonempty_bins else -1
    active_horizon_bins = max(last_nonempty_bin - first_nonempty_bin, 0)
    full_horizon = float(Decimal(len(values)) * bin_width)
    active_horizon = float(Decimal(active_horizon_bins) * bin_width)
    # LINE's native Replayer cycles. Preserve the source signal's full period
    # by representing the gap from the final arrival to the next cycle's first
    # arrival, including both trailing and leading idle bins.
    periodic_replay_gap = max(full_horizon - active_horizon, 0.0)
    summary = {
        "bin_count": len(values),
        "horizon": full_horizon,
        "first_arrival_bin": first_nonempty_bin,
        "last_arrival_bin": last_nonempty_bin,
        "active_horizon": active_horizon,
        "periodic_replay_gap": periodic_replay_gap,
        "periodic_replay_period": full_horizon,
        "trailing_idle_bins": len(values) - last_nonempty_bin - 1,
        "total_bytes": float(cumulative_bytes),
        "total_jobs": previous_jobs,
        "final_remainder_bytes": float(remainder),
    }
    return schedule, summary


def schedule_interarrivals(schedule: dict[str, Any]) -> list[float]:
    """Expand bins into cyclic interarrivals whose sum preserves the full signal period."""
    events: list[float] = []
    previous_bin = -1
    width = float(schedule["bin_width"])
    for batch in schedule["batches"]:
        source_bin = int(batch["source_bin"])
        if source_bin != previous_bin + 1:
            raise ValueError("schedule source_bin values must be contiguous from zero")
        previous_bin = source_bin
        expected_time = source_bin * width
        if not math.isclose(float(batch["time"]), expected_time, rel_tol=0.0, abs_tol=1e-12):
            raise ValueError("schedule bin-start time does not match source_bin * bin_width")
        count = int(batch["job_count"])
        if count < 0:
            raise ValueError("schedule job_count must be non-negative")
        events.extend([expected_time] * count)
    if not events:
        raise ValueError("schedule contains no jobs")
    active_span = events[-1] - events[0]
    period = len(schedule["batches"]) * width
    cycle_gap = period - active_span
    if not math.isfinite(cycle_gap) or cycle_gap < 0:
        raise ValueError("periodic replay gap must be finite and non-negative")
    # Replayer cycles the vector. Its first interval is the gap from the
    # previous cycle's last arrival to this cycle's first arrival.
    intervals = [cycle_gap]
    intervals.extend(events[index] - events[index - 1] for index in range(1, len(events)))
    if any(not math.isfinite(value) or value < 0 for value in intervals):
        raise ValueError("periodic interarrival intervals must be finite and non-negative")
    if not math.isclose(sum(intervals), period, rel_tol=1e-12, abs_tol=1e-9):
        raise AssertionError("periodic interarrival vector must sum to the full source-signal period")
    if len(intervals) != len(events):
        raise AssertionError("one interarrival interval is required per offered job")
    return intervals


def validate_document(name: str, value: dict[str, Any]) -> None:
    try:
        from jsonschema import Draft202012Validator
    except ImportError as error:
        raise RuntimeError("Install jsonschema>=4.20,<5 to validate schedule outputs") from error
    schema_path = SCHEMA_DIR / {
        "schedule": "arrival-schedule.schema.json",
        "experiment": "experiment.schema.json",
        "result": "result.schema.json",
    }[name]
    schema = json.loads(schema_path.read_text(encoding="utf-8"))
    errors = sorted(
        Draft202012Validator(schema).iter_errors(value),
        key=lambda item: list(item.path),
    )
    if errors:
        detail = "; ".join(
            f"{'.'.join(map(str, error.path)) or '<root>'}: {error.message}"
            for error in errors
        )
        raise ValueError(f"{name} schema validation failed: {detail}")


def build_experiment(
    *,
    experiment_id: str,
    condition: str,
    model: dict[str, Any],
    model_raw: bytes,
    schedule: dict[str, Any],
    schedule_raw: bytes,
    config: dict[str, Any],
    horizon: float,
    total_jobs: int,
    source_csv_sha256: str,
    source_manifest_sha256: str,
) -> dict[str, Any]:
    seed_key = "measured_replication_seeds" if condition == "measured_trace_reference" else "replay_replication_seeds"
    seeds = config["simulation"][seed_key]
    metrics = config["metrics"]["primary"] + config["metrics"]["secondary"]
    constructs = [
        "open_class",
        "fcfs_queueing_place",
        "exponential_service",
        "trace_driven_arrivals",
        "immediate_completion_transition",
    ]
    return {
        "schema_version": "1.0",
        "experiment_id": experiment_id,
        "model": {
            "model_id": model["model_id"],
            "sha256": sha256_bytes(model_raw),
        },
        "schedule": {
            "schedule_id": schedule["schedule_id"],
            "sha256": sha256_bytes(schedule_raw),
        },
        "time_unit": config["time_unit"],
        "warmup": {"policy": config["simulation"]["warmup_policy"]},
        "termination": {
            "kind": config["simulation"]["termination_kind"],
            "value": max(200000, total_jobs * 20),
        },
        "replications": {"count": len(seeds), "seeds": seeds},
        "required_metrics": metrics,
        "capabilities": {"constructs": constructs, "metrics": metrics},
        "metadata": {
            "condition": condition,
            "confidence_level": config["simulation"]["confidence_level"],
            "primary_metrics": config["metrics"]["primary"],
            "secondary_metrics": config["metrics"]["secondary"],
            "source_signal_sha256": source_csv_sha256,
            "source_manifest_sha256": source_manifest_sha256,
            "model_id": model["model_id"],
            "service_rate_per_s": model["queueing_place"]["service_distribution"]["rate_per_s"],
            "service_rate_policy": model["service_rate_policy"],
        },
    }


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--source-csv", type=Path, required=True)
    parser.add_argument("--replay-csv", type=Path, required=True)
    parser.add_argument("--source-manifest", type=Path, required=True)
    parser.add_argument(
        "--config", type=Path,
        default=EXPERIMENT_DIR / "config.json",
    )
    parser.add_argument(
        "--model-spec", type=Path,
        default=EXPERIMENT_DIR / "model-spec.json",
    )
    parser.add_argument("--output-dir", type=Path, required=True)
    args = parser.parse_args()

    config, config_raw = load_json(args.config)
    base_model, base_model_raw = load_json(args.model_spec)
    source_manifest, source_manifest_raw = load_json(args.source_manifest)
    source_values, source_raw = read_signal_csv(args.source_csv)
    replay_values, replay_raw = read_signal_csv(args.replay_csv)
    if len(source_values) != len(replay_values):
        raise ValueError("measured and replay signals must have the same number of bins")
    if config["time_unit"] != base_model["time_unit"]:
        raise ValueError("config and QPN model time units must match")
    if Decimal(str(source_manifest["sampling_interval"])) != Decimal(str(config["bin_width"])):
        raise ValueError("QPN bin_width must match the source manifest sampling_interval")
    if int(source_manifest["sample_count"]) != len(source_values):
        raise ValueError("source signal length must match the source manifest sample_count")
    if config["qpn_model"]["service_rate_policy"] != base_model["service_rate_policy"]:
        raise ValueError("config and model service-rate policies must match")

    bin_width = Decimal(str(config["bin_width"]))
    bytes_per_job = Decimal(str(config["workload_mapping"]["bytes_per_job"]))
    if config["workload_mapping"]["remainder_policy"] != "carry_forward":
        raise ValueError("only carry_forward remainder policy is supported")
    if config["workload_mapping"]["arrival_placement"] != "bin_start":
        raise ValueError("only bin_start arrival placement is supported")
    if config["simulation"]["warmup_policy"] != "none":
        raise ValueError("periodic replay comparisons must use warmup=none")
    if config["simulation"]["termination_kind"] != "completions":
        raise ValueError("periodic replay comparisons require a service-completion event budget")
    measured_seeds = config["simulation"]["measured_replication_seeds"]
    replay_seeds = config["simulation"]["replay_replication_seeds"]
    if len(measured_seeds) < 2 or len(replay_seeds) < 2:
        raise ValueError("at least two explicit replication seeds per condition are required")
    if set(measured_seeds) & set(replay_seeds):
        raise ValueError("measured and replay conditions must use disjoint replication seeds")

    args.output_dir.mkdir(parents=True, exist_ok=True)
    conditions = [
        ("measured", source_values, source_raw, "measured_trace_reference"),
        ("replay", replay_values, replay_raw, "deterministic_replay_control"),
    ]
    prepared = []
    for suffix, values, raw_signal, condition_name in conditions:
        schedule, summary = build_schedule(
            values,
            schedule_id=f"westermo-{suffix}-qpn-schedule-v1",
            time_unit=config["time_unit"],
            bin_width=bin_width,
            bytes_per_job=bytes_per_job,
        )
        if summary["total_jobs"] < 1:
            raise ValueError(f"{condition_name} schedule contains no jobs")
        schedule_raw = canonical_json(schedule)
        validate_document("schedule", schedule)
        prepared.append((suffix, raw_signal, condition_name, schedule, summary, schedule_raw))

    measured_summary = prepared[0][4]
    if measured_summary["horizon"] <= 0:
        raise ValueError("measured schedule must define a positive periodic replay period")
    measured_arrival_rate = measured_summary["total_jobs"] / measured_summary["horizon"]
    if not math.isfinite(measured_arrival_rate) or measured_arrival_rate <= 0:
        raise ValueError("measured offered job rate must be finite and positive")
    service_rate = 2.0 * measured_arrival_rate
    resolved_model = copy.deepcopy(base_model)
    resolved_model["queueing_place"]["service_distribution"]["rate_per_s"] = service_rate
    resolved_model["resolved_service_rate_policy"] = "twice_measured_offered_rate"
    resolved_model_raw = canonical_json(resolved_model)
    resolved_model_path = args.output_dir / "resolved-model-spec.json"
    resolved_model_path.write_bytes(resolved_model_raw)

    manifest: dict[str, Any] = {
        "schema_version": "1.0",
        "experiment_family": config["experiment_family"],
        "source_dataset_id": source_manifest.get("dataset_id"),
        "source_manifest_sha256": sha256_bytes(source_manifest_raw),
        "config_sha256": sha256_bytes(config_raw),
        "base_model_sha256": sha256_bytes(base_model_raw),
        "model": {
            "model_id": resolved_model["model_id"],
            "sha256": sha256_bytes(resolved_model_raw),
            "service_rate_per_s": service_rate,
            "service_rate_policy": resolved_model["resolved_service_rate_policy"],
            "file": resolved_model_path.name,
        },
        "conditions": {},
    }

    for suffix, raw_signal, condition_name, schedule, summary, schedule_raw in prepared:
        if summary["horizon"] <= 0:
            raise ValueError(f"{condition_name} must define a positive periodic replay period")
        arrival_rate = summary["total_jobs"] / summary["horizon"]
        if not math.isfinite(service_rate) or service_rate <= arrival_rate:
            raise ValueError(
                f"QPN must be stable for {condition_name}: "
                f"arrival rate {arrival_rate:.6g}/s must be below service rate {service_rate:.6g}/s"
            )
        schedule_path = args.output_dir / f"{suffix}-arrival-schedule.json"
        schedule_path.write_bytes(schedule_raw)
        experiment = build_experiment(
            experiment_id=f"westermo-{suffix}-qpn-v1",
            condition=condition_name,
            model=resolved_model,
            model_raw=resolved_model_raw,
            schedule=schedule,
            schedule_raw=schedule_raw,
            config=config,
            horizon=summary["horizon"],
            total_jobs=summary["total_jobs"],
            source_csv_sha256=sha256_bytes(raw_signal),
            source_manifest_sha256=sha256_bytes(source_manifest_raw),
        )
        validate_document("experiment", experiment)
        experiment_raw = canonical_json(experiment)
        experiment_path = args.output_dir / f"{suffix}-experiment.json"
        experiment_path.write_bytes(experiment_raw)
        manifest["conditions"][suffix] = {
            **summary,
            "condition": condition_name,
            "mean_offered_job_rate_per_s": arrival_rate,
            "source_signal_sha256": sha256_bytes(raw_signal),
            "schedule_sha256": sha256_bytes(schedule_raw),
            "experiment_sha256": sha256_bytes(experiment_raw),
            "schedule_file": schedule_path.name,
            "experiment_file": experiment_path.name,
        }

    manifest["comparison"] = {
        "same_model_sha256": manifest["model"]["sha256"],
        "same_time_unit": config["time_unit"],
        "same_bin_width": float(bin_width),
        "same_bytes_per_job": float(bytes_per_job),
        "replication_strategy": "10 independent replications per condition with disjoint seed sets",
        "measured_replication_seeds": config["simulation"]["measured_replication_seeds"],
        "replay_replication_seeds": config["simulation"]["replay_replication_seeds"],
        "same_warmup_policy": config["simulation"]["warmup_policy"],
        "same_termination_policy": "service_completion_event_budget_on_periodic_replay",
        "primary_metrics": config["metrics"]["primary"],
        "secondary_metrics": config["metrics"]["secondary"],
        "confidence_level": config["simulation"]["confidence_level"],
    }
    manifest_raw = canonical_json(manifest)
    (args.output_dir / "manifest.json").write_bytes(manifest_raw)
    print(json.dumps(manifest, indent=2, sort_keys=True))


if __name__ == "__main__":
    main()
