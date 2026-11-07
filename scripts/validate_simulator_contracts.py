#!/usr/bin/env python3
"""Validate backend-neutral simulator v1 contracts and deterministic fixtures."""

from __future__ import annotations

import hashlib
import json
import math
from pathlib import Path
from typing import Any

from jsonschema import Draft202012Validator

ROOT = Path(__file__).resolve().parents[1]
SCHEMA_DIR = ROOT / "schemas" / "simulator" / "v1"
FIXTURE_DIR = ROOT / "experiments" / "simulator-contract-v1"
SCHEMAS = {
    "schedule": "arrival-schedule.schema.json",
    "experiment": "experiment.schema.json",
    "capabilities": "capabilities.schema.json",
    "result": "result.schema.json",
}


def _reject_constant(value: str) -> None:
    raise ValueError(f"Non-finite JSON number is forbidden: {value}")


def _parse_finite_float(value: str) -> float:
    parsed = float(value)
    if not math.isfinite(parsed):
        raise ValueError(f"JSON number overflows finite range: {value}")
    return parsed


def load_json(path: Path) -> dict[str, Any]:
    value = json.loads(
        path.read_text(encoding="utf-8"),
        parse_constant=_reject_constant,
        parse_float=_parse_finite_float,
    )
    if not isinstance(value, dict):
        raise ValueError(f"{path} must contain a JSON object")
    return value


def canonical_json(value: Any) -> bytes:
    """Stable UTF-8 representation used for checksums; NaN/Infinity are rejected."""
    return (
        json.dumps(value, sort_keys=True, separators=(",", ":"), ensure_ascii=False, allow_nan=False)
        + "\n"
    ).encode("utf-8")


def sha256_json(value: Any) -> str:
    return hashlib.sha256(canonical_json(value)).hexdigest()


def validate_schema(name: str, value: dict[str, Any]) -> None:
    schema = load_json(SCHEMA_DIR / SCHEMAS[name])
    Draft202012Validator.check_schema(schema)
    errors = sorted(Draft202012Validator(schema).iter_errors(value), key=lambda e: list(e.path))
    if errors:
        details = "; ".join(
            f"{'.'.join(map(str, error.path)) or '<root>'}: {error.message}"
            for error in errors
        )
        raise ValueError(f"{name} schema validation failed: {details}")


def validate_schedule_semantics(schedule: dict[str, Any]) -> None:
    batches = schedule["batches"]
    width = schedule["bin_width"]
    mapping = schedule["workload_mapping"]
    remainder = mapping["final_remainder_bytes"]
    if remainder >= mapping["bytes_per_job"]:
        raise ValueError("final remainder must be smaller than bytes_per_job")
    process = schedule.get("arrival_process")
    if process is not None:
        if batches:
            raise ValueError("periodic arrival_process cannot be combined with binned batches")
        if process["first_arrival"] < 0 or process["interarrival"] <= 0:
            raise ValueError("periodic arrival times must be non-negative with positive interarrival")
        if not math.isclose(process["first_arrival"] / width,
                            round(process["first_arrival"] / width), rel_tol=0.0, abs_tol=1e-12):
            raise ValueError("periodic first_arrival must align to bin_width")
        if not math.isclose(process["interarrival"] / width,
                            round(process["interarrival"] / width), rel_tol=0.0, abs_tol=1e-12):
            raise ValueError("periodic interarrival must align to bin_width")
    else:
        for index, batch in enumerate(batches):
            if batch["source_bin"] != index:
                raise ValueError("source_bin values must be contiguous from zero; retain empty bins")
            expected_time = index * width
            if not math.isfinite(expected_time):
                raise ValueError("bin time calculation overflowed finite range")
            if not math.isclose(batch["time"], expected_time, rel_tol=0.0, abs_tol=1e-12):
                raise ValueError("bin_start times must equal source_bin * bin_width")
    generation = schedule["generation"]
    if generation["mode"] == "deterministic" and generation["seed"] is not None:
        raise ValueError("deterministic generation must use seed=null")
    if generation["mode"] == "stochastic" and generation["seed"] is None:
        raise ValueError("stochastic generation requires an explicit seed")


def validate_experiment_semantics(experiment: dict[str, Any], capabilities: dict[str, Any]) -> None:
    replications = experiment["replications"]
    if replications["count"] != len(replications["seeds"]):
        raise ValueError("replication count must equal the number of explicit seeds")
    if experiment["warmup"]["policy"] == "fixed_time":
        if "duration" not in experiment["warmup"] or experiment["warmup"]["duration"] <= 0:
            raise ValueError("fixed_time warm-up requires a positive duration")
    elif "duration" in experiment["warmup"]:
        raise ValueError("warm-up duration is only valid with fixed_time policy")
    missing_constructs = sorted(
        set(experiment["capabilities"]["constructs"]) - set(capabilities["supported_constructs"])
    )
    missing_metrics = sorted(
        set(experiment["capabilities"]["metrics"]) - set(capabilities["supported_metrics"])
    )
    missing_warmup = experiment["warmup"]["policy"] not in capabilities["supported_warmup_policies"]
    missing_termination = experiment["termination"]["kind"] not in capabilities["supported_termination_kinds"]
    if missing_constructs or missing_metrics or missing_warmup or missing_termination:
        raise ValueError(
            "backend capability mismatch: "
            f"constructs={missing_constructs}, metrics={missing_metrics}, "
            f"warmup={missing_warmup}, termination={missing_termination}"
        )
    if not set(experiment["required_metrics"]).issubset(experiment["capabilities"]["metrics"]):
        raise ValueError("all required metrics must be declared in experiment capabilities")


def validate_result_semantics(experiment: dict[str, Any], result: dict[str, Any]) -> None:
    required = set(experiment["required_metrics"])
    returned = set(result["metrics"])
    if not required.issubset(returned):
        raise ValueError(f"result omits required metrics: {sorted(required - returned)}")
    unavailable = sorted(
        metric for metric in required if result["metrics"][metric]["status"] != "available"
    )
    if unavailable and result["validation"] != "failed":
        raise ValueError("a result missing required metrics must have validation=failed")
    for metric, record in result["metrics"].items():
        if record["status"] == "available" and record.get("estimate") is None:
            raise ValueError(f"available metric {metric} requires an estimate")
        if record["status"] != "available" and not record.get("reason"):
            raise ValueError(f"unavailable metric {metric} requires an explicit reason")
    checksums = result["checksums"]
    if checksums["model_sha256"] != experiment["model"]["sha256"]:
        raise ValueError("result model checksum does not match experiment")
    if checksums["schedule_sha256"] != experiment["schedule"]["sha256"]:
        raise ValueError("result schedule checksum does not match experiment")


def validate_line_adapter_inputs() -> None:
    """Check that the LINE bridge inputs are valid neutral v1 contract documents."""
    line_dir = ROOT / "experiments" / "line-qpn-v1"
    model_path = line_dir / "model-spec.json"
    schedule_path = line_dir / "arrival-schedule-neutral.json"
    experiment_path = line_dir / "experiment.json"
    model = load_json(model_path)
    schedule = load_json(schedule_path)
    experiment = load_json(experiment_path)
    capabilities = load_json(FIXTURE_DIR / "capabilities.json")
    validate_schema("schedule", schedule)
    validate_schema("experiment", experiment)
    validate_schedule_semantics(schedule)
    validate_experiment_semantics(experiment, capabilities)
    if experiment["model"]["sha256"] != hashlib.sha256(model_path.read_bytes()).hexdigest():
        raise ValueError("LINE experiment model checksum does not match model-spec.json")
    if experiment["schedule"]["sha256"] != hashlib.sha256(schedule_path.read_bytes()).hexdigest():
        raise ValueError("LINE experiment schedule checksum does not match neutral arrival schedule")
    if experiment["schedule"]["schedule_id"] != schedule["schedule_id"]:
        raise ValueError("LINE experiment schedule_id does not match neutral schedule")
    if experiment["time_unit"] != schedule["time_unit"] or experiment["time_unit"] != model["time_unit"]:
        raise ValueError("LINE model, schedule, and experiment time units must match")
    if experiment["replications"]["count"] != 1 or len(experiment["replications"]["seeds"]) != 1:
        raise ValueError("LINE smoke currently requires exactly one explicit replication")
    if experiment["termination"]["kind"] != "completions":
        raise ValueError("LINE smoke currently requires completion-count termination")


def validate_fixtures() -> None:
    schedule = load_json(FIXTURE_DIR / "arrival-schedule.json")
    experiment = load_json(FIXTURE_DIR / "experiment.json")
    capabilities = load_json(FIXTURE_DIR / "capabilities.json")
    result = load_json(FIXTURE_DIR / "result.json")
    for name, value in (
        ("schedule", schedule),
        ("experiment", experiment),
        ("capabilities", capabilities),
        ("result", result),
    ):
        validate_schema(name, value)
    validate_schedule_semantics(schedule)
    validate_experiment_semantics(experiment, capabilities)
    validate_result_semantics(experiment, result)
    validate_line_adapter_inputs()


if __name__ == "__main__":
    validate_fixtures()
    print("Simulator-neutral contract v1 fixtures: valid")
