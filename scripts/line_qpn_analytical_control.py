#!/usr/bin/env python3
"""Validate the LINE QPN bridge against an analytical M/M/1 control."""

from __future__ import annotations

import argparse
import json
import math
import time
from importlib.metadata import version
from pathlib import Path
from typing import Any

from line_solver import (
    Exp,
    GlobalConstants,
    Network,
    OpenClass,
    Place,
    SchedStrategy,
    Sink,
    SolverLDES,
    Source,
    TimingStrategy,
    Transition,
)

from line_qpn_trace_evaluation import confidence_interval
from build_qpn_workload_schedules import load_json, sha256_bytes

ROOT = Path(__file__).resolve().parents[1]
PINNED_LINE_VERSION = "3.0.8.0"


def build_poisson_control_model(model_spec: dict[str, Any], arrival_rate: float) -> Network:
    model = Network(model_spec["model_id"] + "-analytical-control")
    source = Source(model, model_spec["source"]["node"])
    queue_spec = model_spec["queueing_place"]
    queue = Place(model, queue_spec["node"], SchedStrategy.FCFS)
    queue.set_number_of_servers(int(queue_spec["servers"]))
    completion = Transition(model, model_spec["completion"]["transition"])
    sink = Sink(model, model_spec["completion"]["destination"])
    traffic = OpenClass(model, model_spec["job_class"], 0)

    service_rate = float(queue_spec["service_distribution"]["rate_per_s"])
    source.set_arrival(traffic, Exp(arrival_rate))
    queue.set_service(traffic, Exp(service_rate))
    queue.set_state(int(queue_spec["initial_marking"]))

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


def run_control(
    model_path: Path,
    arrival_rate: float,
    sample_count: int,
    seeds: list[int],
    confidence_level: float,
    output_path: Path,
) -> dict[str, Any]:
    installed_version = version("line-solver")
    if installed_version != PINNED_LINE_VERSION:
        raise RuntimeError(
            f"Expected line-solver {PINNED_LINE_VERSION}, found {installed_version}"
        )
    model_spec, model_raw = load_json(model_path)
    service_rate = float(model_spec["queueing_place"]["service_distribution"]["rate_per_s"])
    if not math.isfinite(arrival_rate) or arrival_rate <= 0 or arrival_rate >= service_rate:
        raise ValueError("analytical M/M/1 control requires 0 < arrival_rate < service_rate")
    if sample_count < 1000 or len(seeds) < 2:
        raise ValueError("control requires at least 1000 completions and two replications")

    expected = {
        "throughput": arrival_rate,
        "queue_length": arrival_rate / (service_rate - arrival_rate),
        "utilization": arrival_rate / service_rate,
        "residence_time": 1.0 / (service_rate - arrival_rate),
    }
    columns = {
        "throughput": ("Tput", "jobs/s"),
        "queue_length": ("QLen", "jobs"),
        "utilization": ("Util", "ratio"),
        "residence_time": ("ResidT", "s"),
    }
    replications = []
    elapsed = 0.0
    for index, seed in enumerate(seeds):
        model = build_poisson_control_model(model_spec, arrival_rate)
        started = time.perf_counter()
        solver = SolverLDES(
            model,
            seed=int(seed),
            samples=sample_count,
            tranfilter="mser5",
            cimethod="none",
            verbose="silent",
        )
        table = solver.avg_table()
        elapsed += time.perf_counter() - started
        rows = table.reset_index().to_dict(orient="records")
        row = next(
            (
                item for item in rows
                if str(item.get("Station", "")) == model_spec["queueing_place"]["node"]
                and str(item.get("JobClass", "")) == model_spec["job_class"]
            ),
            None,
        )
        if row is None:
            raise AssertionError(f"LINE analytical control has no queueing-place row: {rows!r}")
        observations = []
        for metric, (source_column, unit) in columns.items():
            value = float(row[source_column])
            if not math.isfinite(value):
                raise ValueError(f"LINE returned non-finite {metric} for seed {seed}")
            observations.append({"metric": metric, "value": value, "unit": unit})
        replications.append({
            "index": index,
            "seed": int(seed),
            "termination_reason": "completed",
            "raw_observations": observations,
        })

    metrics = {}
    failures = {}
    for metric, (_, unit) in columns.items():
        values = [
            next(item["value"] for item in replication["raw_observations"] if item["metric"] == metric)
            for replication in replications
        ]
        mean, standard_deviation, lower, upper = confidence_interval(values, confidence_level)
        metrics[metric] = {
            "unit": unit,
            "analytical_estimate": expected[metric],
            "estimate": mean,
            "standard_deviation": standard_deviation,
            "confidence_interval": [lower, upper],
            "contains_analytical_estimate": lower <= expected[metric] <= upper,
        }
        if not metrics[metric]["contains_analytical_estimate"]:
            failures[metric] = {
                "analytical_estimate": expected[metric],
                "confidence_interval": [lower, upper],
            }

    result = {
        "schema_version": "1.0",
        "control_id": "qpn-mm1-poisson-analytical-control-v1",
        "backend": {"id": "line-solver", "version": installed_version, "engine": "LDES"},
        "model_sha256": sha256_bytes(model_raw),
        "arrival_rate_per_s": arrival_rate,
        "service_rate_per_s": service_rate,
        "samples_per_replication": sample_count,
        "warmup_policy": "mser5",
        "replication_seeds": seeds,
        "confidence_level": confidence_level,
        "analytical_metrics": expected,
        "metrics": metrics,
        "replications": replications,
        "runtime_seconds_total": elapsed,
        "validation": "failed" if failures else "passed",
        "failures": failures,
    }
    output_path.parent.mkdir(parents=True, exist_ok=True)
    output_path.write_text(
        json.dumps(result, indent=2, sort_keys=True, allow_nan=False) + "\n",
        encoding="utf-8",
    )
    print(json.dumps(result, indent=2, sort_keys=True))
    if failures:
        raise AssertionError(f"LINE M/M/1 control failed analytical confidence checks: {failures}")
    return result


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--arrival-rate", type=float, required=True)
    parser.add_argument(
        "--model-spec", type=Path,
        default=ROOT / "artifacts/qpn-workload-evaluation-v1/resolved-model-spec.json",
    )
    parser.add_argument(
        "--config", type=Path,
        default=ROOT / "experiments/qpn-workload-evaluation-v1/analytical-control.json",
    )
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()

    config, _ = load_json(args.config)
    run_control(
        args.model_spec,
        args.arrival_rate,
        int(config["samples_per_replication"]),
        [int(seed) for seed in config["replication_seeds"]],
        float(config["confidence_level"]),
        args.output,
    )


if __name__ == "__main__":
    main()
