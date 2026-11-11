#!/usr/bin/env python3
"""Run one fixed-horizon, trace-driven QPN workload experiment with LINE Solver."""

from __future__ import annotations

import argparse
import json
import math
import statistics
import time
from pathlib import Path
from typing import Any

from line_solver import (
    Exp,
    GlobalConstants,
    Network,
    OpenClass,
    Place,
    Replayer,
    SchedStrategy,
    Sink,
    SolverLDES,
    Source,
    TimingStrategy,
    Transition,
)

from build_qpn_workload_schedules import (
    canonical_json,
    load_json,
    sha256_bytes,
    validate_document,
)

ROOT = Path(__file__).resolve().parents[1]
PINNED_LINE_VERSION = "3.0.8.0"
T_CRITICAL_95 = {
    1: 12.706205, 2: 4.302653, 3: 3.182446, 4: 2.776445, 5: 2.570582,
    6: 2.446912, 7: 2.364624, 8: 2.306004, 9: 2.262157, 10: 2.228139,
    11: 2.200985, 12: 2.178813, 13: 2.160369, 14: 2.144787, 15: 2.131450,
    16: 2.119905, 17: 2.109816, 18: 2.100922, 19: 2.093024, 20: 2.085963,
    21: 2.079614, 22: 2.073873, 23: 2.068658, 24: 2.063899, 25: 2.059539,
    26: 2.055529, 27: 2.051831, 28: 2.048407, 29: 2.045230,
}
METRICS = {
    "arrival_rate": ("ArvR", "jobs/s"),
    "throughput": ("Tput", "jobs/s"),
    "queue_length": ("QLen", "jobs"),
    "utilization": ("Util", "ratio"),
    "residence_time": ("ResidT", "s"),
}


def schedule_interarrivals(schedule: dict[str, Any]) -> list[float]:
    """Expand bin-start batches into exact empirical interarrival intervals."""
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
    # Replayer cycles the vector. The first interval is therefore the gap
    # from the previous cycle's final arrival to this cycle's first arrival,
    # not a one-off delay from simulation time zero.
    intervals = [cycle_gap]
    intervals.extend(events[index] - events[index - 1] for index in range(1, len(events)))
    if any(not math.isfinite(value) or value < 0 for value in intervals):
        raise ValueError("expanded periodic interarrival intervals must be finite and non-negative")
    if not math.isclose(sum(intervals), period, rel_tol=1e-12, abs_tol=1e-9):
        raise AssertionError("periodic interarrival vector must sum to the full source-signal period")
    if len(intervals) != len(events):
        raise AssertionError("one interarrival interval is required per offered job")
    return intervals


def build_model(model_spec: dict[str, Any], trace_path: Path) -> Network:
    if model_spec["time_unit"] != "s":
        raise ValueError("this LINE experiment runner currently requires seconds")
    service = model_spec["queueing_place"]["service_distribution"]
    if service["kind"] != "exponential":
        raise ValueError("the trace evaluation runner requires exponential service")
    model = Network(model_spec["model_id"])
    source = Source(model, model_spec["source"]["node"])
    queue_spec = model_spec["queueing_place"]
    queue = Place(model, queue_spec["node"], SchedStrategy.FCFS)
    queue.set_number_of_servers(int(queue_spec["servers"]))
    completion = Transition(model, model_spec["completion"]["transition"])
    sink = Sink(model, model_spec["completion"]["destination"])
    traffic = OpenClass(model, model_spec["job_class"], 0)

    # The native LDES serializer drops Replayer.loop; the finite timespan below\n    # prevents the cyclic trace from reaching its next iteration.\n    source.set_arrival(traffic, Replayer(str(trace_path)))
    queue.set_service(traffic, Exp(float(service["rate_per_s"])))
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


def confidence_interval(values: list[float], confidence_level: float) -> tuple[float, float, float, float]:
    if len(values) < 2:
        raise ValueError("at least two replications are required for uncertainty estimates")
    if confidence_level != 0.95:
        raise ValueError("this runner currently implements the predeclared 95% Student-t interval")
    degrees_freedom = len(values) - 1
    if degrees_freedom not in T_CRITICAL_95:
        raise ValueError("the 95% interval table supports 2 to 30 replications")
    mean = statistics.mean(values)
    standard_deviation = statistics.stdev(values)
    half_width = T_CRITICAL_95[degrees_freedom] * standard_deviation / math.sqrt(len(values))
    return mean, standard_deviation, mean - half_width, mean + half_width


def run_experiment(
    experiment_path: Path,
    schedule_path: Path,
    model_path: Path,
    output_dir: Path,
) -> dict[str, Any]:
    from importlib.metadata import version

    installed_version = version("line-solver")
    if installed_version != PINNED_LINE_VERSION:
        raise RuntimeError(
            f"Expected line-solver {PINNED_LINE_VERSION}, found {installed_version}"
        )
    experiment, experiment_raw = load_json(experiment_path)
    schedule, schedule_raw = load_json(schedule_path)
    model_spec, model_raw = load_json(model_path)
    validate_document("schedule", schedule)
    validate_document("experiment", experiment)

    if experiment["model"]["sha256"] != sha256_bytes(model_raw):
        raise ValueError("experiment model checksum does not match model-spec.json")
    if experiment["schedule"]["sha256"] != sha256_bytes(schedule_raw):
        raise ValueError("experiment schedule checksum does not match schedule file")
    if experiment["schedule"]["schedule_id"] != schedule["schedule_id"]:
        raise ValueError("experiment schedule_id does not match schedule")
    if experiment["time_unit"] != schedule["time_unit"] or experiment["time_unit"] != model_spec["time_unit"]:
        raise ValueError("model, schedule, and experiment time units must match")
    if experiment["warmup"]["policy"] != "none":
        raise ValueError("finite trace runs must use warmup=none")
    if experiment["termination"]["kind"] != "completions":
        raise ValueError("periodic replay runs require a service-completion event budget")
    if len(experiment["replications"]["seeds"]) != experiment["replications"]["count"]:
        raise ValueError("replication count must match the explicit seed list")

    interarrivals = schedule_interarrivals(schedule)
    offered_jobs = len(interarrivals)
    service_rate = float(model_spec["queueing_place"]["service_distribution"]["rate_per_s"])
    full_horizon = len(schedule["batches"]) * float(schedule["bin_width"])
    completion_budget = int(experiment["termination"]["value"])
    expected_completion_budget = max(200000, offered_jobs * 100)
    if completion_budget != expected_completion_budget:
        raise ValueError(
            "completion event budget must match the predeclared max(200000, offered_jobs * 100) policy"
        )
    arrival_bins = [
        int(batch["source_bin"])
        for batch in schedule["batches"]
        if int(batch["job_count"]) > 0
    ]
    first_arrival_bin = min(arrival_bins)
    last_arrival_bin = max(arrival_bins)
    first_arrival_epoch = first_arrival_bin * float(schedule["bin_width"])
    last_arrival_epoch = last_arrival_bin * float(schedule["bin_width"])
    active_horizon = last_arrival_epoch - first_arrival_epoch
    periodic_replay_gap = full_horizon - active_horizon
    horizon = full_horizon
    if not math.isfinite(horizon) or horizon <= 0:
        raise ValueError("periodic replay must define a positive source-signal period")
    mean_offered_rate = offered_jobs / horizon
    trace_mean_interarrival_rate = offered_jobs / sum(interarrivals)
    if not math.isfinite(trace_mean_interarrival_rate) or trace_mean_interarrival_rate <= 0:
        raise ValueError("trace mean interarrival rate must be finite and positive")
    if not math.isfinite(mean_offered_rate) or mean_offered_rate >= service_rate:
        raise ValueError("the configured QPN is unstable over the finite simulation horizon")

    output_dir.mkdir(parents=True, exist_ok=True)
    trace_path = (output_dir / f"{schedule['schedule_id']}-interarrivals.trace").resolve()
    trace_raw = ("\n".join(format(value, ".17g") for value in interarrivals) + "\n").encode("utf-8")
    trace_path.write_bytes(trace_raw)
    replications = []
    elapsed_seconds = 0.0
    for index, seed in enumerate(experiment["replications"]["seeds"]):
        model = build_model(model_spec, trace_path)
        started = time.perf_counter()
        solver = SolverLDES(
            model,
            seed=int(seed),
            samples=completion_budget,
            tranfilter="none",
            cimethod="none",
            verbose="silent",
        )
        table = solver.avg_table()
        elapsed_seconds += time.perf_counter() - started
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
        observed_arrival_rate = float(queue_row["ArvR"])
        observed_throughput = float(queue_row["Tput"])
        if not math.isfinite(observed_arrival_rate) or not math.isfinite(observed_throughput):
            raise ValueError(f"LINE returned non-finite arrival rate or throughput for seed {seed}")
        if not math.isclose(
            observed_arrival_rate, trace_mean_interarrival_rate, rel_tol=0.02, abs_tol=1e-6
        ):
            raise ValueError(
                f"LINE replay arrival rate {observed_arrival_rate:.9g} jobs/s does not "
                f"match the reciprocal mean of the supplied interarrival trace "
                f"{trace_mean_interarrival_rate:.9g} jobs/s"
            )
        if observed_throughput > mean_offered_rate * 1.05:
            raise ValueError(
                f"LINE throughput {observed_throughput:.9g} jobs/s exceeds the "
                f"finite schedule's offered rate {mean_offered_rate:.9g} jobs/s"
            )
        raw_observations = []
        for metric, (source_name, unit) in METRICS.items():
            value = float(queue_row[source_name])
            if not math.isfinite(value):
                raise ValueError(f"LINE returned non-finite {metric} for seed {seed}")
            raw_observations.append({"metric": metric, "value": value, "unit": unit})
        replications.append({
            "index": index,
            "seed": int(seed),
            "termination_reason": "completed",
            "raw_observations": raw_observations,
        })

    confidence_level = float(experiment["metadata"]["confidence_level"])
    normalized_metrics = {}
    for metric, (_, unit) in METRICS.items():
        values = [
            next(item["value"] for item in replication["raw_observations"] if item["metric"] == metric)
            for replication in replications
        ]
        mean, standard_deviation, lower, upper = confidence_interval(values, confidence_level)
        normalized_metrics[metric] = {
            "status": "available",
            "unit": unit,
            "estimate": mean,
            "standard_deviation": standard_deviation,
            "confidence_interval": [lower, upper],
        }

    result = {
        "schema_version": "1.0",
        "experiment_id": experiment["experiment_id"],
        "backend": {"id": "line-solver", "version": installed_version, "engine": "LDES"},
        "checksums": {
            "model_sha256": experiment["model"]["sha256"],
            "schedule_sha256": experiment["schedule"]["sha256"],
            "experiment_sha256": sha256_bytes(experiment_raw),
        },
        "validation": "passed",
        "replications": replications,
        "metrics": normalized_metrics,
        "runtime": {
            "elapsed_seconds_total": elapsed_seconds,
            "offered_jobs": offered_jobs,
            "schedule_horizon_s": horizon,
            "full_schedule_horizon_s": full_horizon,
            "first_arrival_epoch_s": first_arrival_epoch,
            "last_arrival_epoch_s": last_arrival_epoch,
            "active_arrival_span_s": active_horizon,
            "periodic_replay_gap_s": periodic_replay_gap,
            "leading_idle_bins": first_arrival_bin,
            "trailing_idle_bins": len(schedule["batches"]) - last_arrival_bin - 1,
            "mean_offered_job_rate_per_s": mean_offered_rate,
            "trace_mean_interarrival_rate_per_s": trace_mean_interarrival_rate,
            "termination_policy": "service_completion_event_budget_on_periodic_replay",
            "warmup_policy": "none",
            "service_rate_per_s": service_rate,
            "confidence_level": confidence_level,
            "interarrival_trace_sha256": sha256_bytes(trace_raw),
            "interarrival_trace_file": trace_path.name,
        },
        "artifacts": [experiment_path.name, schedule_path.name, model_path.name, trace_path.name],
    }
    result_raw = canonical_json(result)
    output_path = output_dir / "result.json"
    output_path.write_bytes(result_raw)
    validate_document("result", result)
    print(json.dumps(result, indent=2, sort_keys=True))
    return result


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--experiment", type=Path, required=True)
    parser.add_argument("--schedule", type=Path, required=True)
    parser.add_argument(
        "--model-spec", type=Path,
        default=ROOT / "artifacts/qpn-workload-evaluation-v1/resolved-model-spec.json",
    )
    parser.add_argument("--output-dir", type=Path, required=True)
    args = parser.parse_args()
    run_experiment(args.experiment, args.schedule, args.model_spec, args.output_dir)


if __name__ == "__main__":
    main()
