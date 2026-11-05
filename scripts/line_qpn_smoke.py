#!/usr/bin/env python3
"""Run and verify the minimal headless LINE source -> QPN queue -> sink model."""

from __future__ import annotations

import hashlib
import json
import os
from importlib.metadata import version
from pathlib import Path

from line_solver import (
    Det, GlobalConstants, Network, OpenClass, Place, SchedStrategy, Sink,
    SolverLDES, Source, TimingStrategy, Transition, model_to_dict,
)

ROOT = Path(__file__).resolve().parents[1]
EXPERIMENT_DIR = ROOT / "experiments" / "line-qpn-v1"
ARTIFACT_DIR = ROOT / "artifacts" / "line-qpn-v1"
PINNED_LINE_VERSION = "3.0.8.0"
SEED = 23000
SAMPLES = 10_000
TOLERANCE = 0.03

EXPECTED = {
    "QLen": 0.5,
    "Util": 0.5,
    "RespT": 1.0,
    "ResidT": 1.0,
    "ArvR": 0.5,
    "Tput": 0.5,
}


def sha256(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def read_json(path: Path) -> tuple[dict, bytes]:
    raw = path.read_bytes()
    return json.loads(raw), raw


def build_model(model_spec: dict, schedule: dict) -> Network:
    """Build a real LINE QPN: Source -> queueing Place -> immediate Transition -> Sink."""
    if schedule["kind"] != "deterministic-periodic":
        raise ValueError("Only the declared deterministic-periodic schedule is supported")
    if schedule["first_arrival_s"] != schedule["interarrival_s"]:
        raise ValueError("The first arrival must occur one interarrival interval after t=0")

    model = Network(model_spec["model_id"])
    source = Source(model, model_spec["source"]["node"])
    queue_spec = model_spec["queueing_place"]
    queue = Place(model, queue_spec["node"], SchedStrategy.FCFS)
    completion = Transition(model, model_spec["completion"]["transition"])
    sink = Sink(model, model_spec["completion"]["destination"])
    traffic = OpenClass(model, model_spec["job_class"], 0)

    source.set_arrival(traffic, Det(float(schedule["interarrival_s"])))
    queue.set_service(
        traffic,
        Det(float(queue_spec["service_distribution"]["duration_s"])),
    )
    queue.set_state(0)

    mode = completion.add_mode("complete")
    completion.set_number_of_servers(mode, GlobalConstants.MaxInt)
    completion.set_timing_strategy(mode, TimingStrategy.IMMEDIATE)
    completion.set_enabling_conditions(mode, traffic, queue, 1)
    completion.set_firing_outcome(mode, traffic, sink, 1)

    routing = model.init_routing_matrix()
    routing.set(traffic, traffic, source, queue, 1.0)
    routing.set(traffic, traffic, queue, completion, 1.0)
    routing.set(traffic, traffic, completion, sink, 1.0)
    model.link(routing)
    return model


def main() -> None:
    installed_version = version("line-solver")
    if installed_version != PINNED_LINE_VERSION:
        raise RuntimeError(
            f"Expected line-solver {PINNED_LINE_VERSION}, found {installed_version}"
        )

    os.chdir(ROOT)
    ARTIFACT_DIR.mkdir(parents=True, exist_ok=True)
    model_spec, model_spec_raw = read_json(EXPERIMENT_DIR / "model-spec.json")
    schedule, schedule_raw = read_json(EXPERIMENT_DIR / "arrival-schedule.json")
    if schedule["first_arrival_s"] != schedule["interarrival_s"]:
        raise ValueError("The first arrival must occur one interarrival interval after t=0")
    (ARTIFACT_DIR / "model-spec.json").write_bytes(model_spec_raw)
    (ARTIFACT_DIR / "arrival-schedule.json").write_bytes(schedule_raw)
    model = build_model(model_spec, schedule)

    # Hash the actual LINE interchange document, not just the human-authored spec.
    model_document = model_to_dict(model)

    def normalize_paths(value):
        if isinstance(value, dict):
            return {key: normalize_paths(item) for key, item in value.items()}
        if isinstance(value, list):
            return [normalize_paths(item) for item in value]
        if isinstance(value, str) and value.startswith(str(ROOT)):
            return Path(value).relative_to(ROOT).as_posix()
        return value

    normalized_model = normalize_paths(model_document)
    model_canonical = json.dumps(
        normalized_model, sort_keys=True, separators=(",", ":"), allow_nan=False
    ).encode("utf-8")

    # Keep the artifact relocatable: trace paths are repository-relative.
    model_path = ARTIFACT_DIR / "line-model.json"
    model_path.write_text(
        json.dumps(normalized_model, indent=2, sort_keys=True) + "\n",
        encoding="utf-8",
    )

    solver = SolverLDES(
        model, seed=SEED, samples=SAMPLES, tranfilter="none",
        cimethod="none", verbose="silent",
    )
    table = solver.avg_table()
    rows = table.reset_index().to_dict(orient="records")
    queue_row = next(
        (
            row for row in rows
            if str(row.get("Station", "")) == model_spec["queueing_place"]["node"]
            and str(row.get("JobClass", "")) == model_spec["job_class"]
        ),
        None,
    )
    if queue_row is None:
        raise AssertionError(f"LINE result has no queueing-place row: {rows!r}")

    observed = {metric: float(queue_row[metric]) for metric in EXPECTED}
    errors = {metric: abs(observed[metric] - expected) for metric, expected in EXPECTED.items()}
    failures = {
        metric: {"expected": EXPECTED[metric], "observed": observed[metric], "error": errors[metric]}
        for metric in EXPECTED if errors[metric] > TOLERANCE
    }
    if failures:
        raise AssertionError(f"Deterministic QPN result outside tolerance: {failures}")

    result = {
        "schema_version": "1.0",
        "backend": "LINE Solver",
        "backend_version": installed_version,
        "engine": "LDES",
        "headless": True,
        "seed": SEED,
        "service_completion_events": SAMPLES,
        "model_sha256": sha256(model_canonical),
        "model_spec_sha256": sha256(model_spec_raw),
        "arrival_schedule_sha256": sha256(schedule_raw),
        "metrics": observed,
        "expected_metrics": EXPECTED,
        "absolute_tolerance": TOLERANCE,
        "validation": "passed",
    }
    (ARTIFACT_DIR / "result.json").write_text(
        json.dumps(result, indent=2, sort_keys=True) + "\n", encoding="utf-8"
    )
    print(json.dumps(result, indent=2, sort_keys=True))


if __name__ == "__main__":
    main()
