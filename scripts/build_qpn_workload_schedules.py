#!/usr/bin/env python3
"""Build checksummed measured and deterministic-replay QPN arrival schedules."""

from __future__ import annotations

import argparse
import csv
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
    summary = {
        "bin_count": len(values),
        "horizon": float(Decimal(len(values)) * bin_width),
        "total_bytes": float(cumulative_bytes),
        "total_jobs": previous_jobs,
        "final_remainder_bytes": float(remainder),
    }
    return schedule, summary


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
    seeds = config["simulation"]["replication_seeds"]
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
            "value": total_jobs,
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
    model, model_raw = load_json(args.model_spec)
    source_manifest, source_manifest_raw = load_json(args.source_manifest)
    source_values, source_raw = read_signal_csv(args.source_csv)
    replay_values, replay_raw = read_signal_csv(args.replay_csv)
    if len(source_values) != len(replay_values):
        raise ValueError("measured and replay signals must have the same number of bins")
    if config["time_unit"] != model["time_unit"]:
        raise ValueError("config and QPN model time units must match")

    bin_width = Decimal(str(config["bin_width"]))
    bytes_per_job = Decimal(str(config["workload_mapping"]["bytes_per_job"]))
    if config["workload_mapping"]["remainder_policy"] != "carry_forward":
        raise ValueError("only carry_forward remainder policy is supported")
    if config["workload_mapping"]["arrival_placement"] != "bin_start":
        raise ValueError("only bin_start arrival placement is supported")
    if config["simulation"]["warmup_policy"] != "none":
        raise ValueError("finite trace comparisons must retain the full trace (warmup=none)")
    if config["simulation"]["termination_kind"] != "completions":
        raise ValueError("finite trace comparisons require drain-all-completions termination")
    if len(config["simulation"]["replication_seeds"]) < 2:
        raise ValueError("at least two explicit replication seeds are required")

    args.output_dir.mkdir(parents=True, exist_ok=True)
    manifest: dict[str, Any] = {
        "schema_version": "1.0",
        "experiment_family": config["experiment_family"],
        "source_dataset_id": source_manifest.get("dataset_id"),
        "source_manifest_sha256": sha256_bytes(source_manifest_raw),
        "config_sha256": sha256_bytes(config_raw),
        "model": {
            "model_id": model["model_id"],
            "sha256": sha256_bytes(model_raw),
        },
        "conditions": {},
    }

    conditions = [
        ("measured", source_values, source_raw, "measured_trace_reference"),
        ("replay", replay_values, replay_raw, "deterministic_replay_control"),
    ]
    for suffix, values, raw_signal, condition_name in conditions:
        schedule, summary = build_schedule(
            values,
            schedule_id=f"westermo-{suffix}-qpn-schedule-v1",
            time_unit=config["time_unit"],
            bin_width=bin_width,
            bytes_per_job=bytes_per_job,
        )
        schedule_raw = canonical_json(schedule)
        validate_document("schedule", schedule)
        schedule_path = args.output_dir / f"{suffix}-arrival-schedule.json"
        schedule_path.write_bytes(schedule_raw)
        if summary["total_jobs"] < 1:
            raise ValueError(f"{condition_name} schedule contains no jobs")
        arrival_rate = summary["total_jobs"] / summary["horizon"]
        service_rate = float(config["qpn_model"]["service_rate_per_s"])
        if not math.isfinite(service_rate) or service_rate <= arrival_rate:
            raise ValueError(
                f"QPN must be stable for {condition_name}: "
                f"arrival rate {arrival_rate:.6g}/s must be below service rate {service_rate:.6g}/s"
            )
        experiment = build_experiment(
            experiment_id=f"westermo-{suffix}-qpn-v1",
            condition=condition_name,
            model=model,
            model_raw=model_raw,
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
        (args.output_dir / f"{suffix}-experiment.json").write_bytes(experiment_raw)
        manifest["conditions"][suffix] = {
            **summary,
            "condition": condition_name,
            "source_signal_sha256": sha256_bytes(raw_signal),
            "schedule_sha256": sha256_bytes(schedule_raw),
            "experiment_sha256": sha256_bytes(experiment_raw),
            "schedule_file": schedule_path.name,
            "experiment_file": f"{suffix}-experiment.json",
        }

    manifest["comparison"] = {
        "same_model_sha256": manifest["model"]["sha256"],
        "same_time_unit": config["time_unit"],
        "same_bin_width": float(bin_width),
        "same_bytes_per_job": float(bytes_per_job),
        "same_replication_seeds": config["simulation"]["replication_seeds"],
        "same_warmup_policy": config["simulation"]["warmup_policy"],
        "same_termination_policy": "drain_all_offered_jobs",
        "primary_metrics": config["metrics"]["primary"],
        "secondary_metrics": config["metrics"]["secondary"],
        "confidence_level": config["simulation"]["confidence_level"],
    }
    manifest_raw = canonical_json(manifest)
    (args.output_dir / "manifest.json").write_bytes(manifest_raw)
    print(json.dumps(manifest, indent=2, sort_keys=True))


if __name__ == "__main__":
    main()
