# Westermo QPN workload evaluation v1

This experiment is designed to compare two inputs against one frozen, authored QPN system model:

1. **Measured-trace reference** — one job schedule derived from the observed captured-frame-byte bins.
2. **Deterministic replay control** — the same byte-to-job mapping applied to the deterministic reconstruction exported by the model-extraction experiment.

The second condition is a deterministic reconstruction control. It is not a stochastic TrafficMSD generator and must never be labelled as one.

## Frozen workload and QPN interpretation

- Input trace: Westermo right observation-point archive, processed by `experiments/model-extraction-v1/run-westermo.R`.
- Source values: captured frame bytes per one-second bin, including zero-valued empty bins.
- Workload mapping: 1,000 captured-frame bytes represent one abstract QPN job.
- Quantization: cumulative bytes are divided by bytes-per-job; the per-bin job count is the difference between consecutive cumulative floors.
- Fractional bytes are carried into the next bin and the final remainder is recorded.
- All jobs in a bin are placed at the bin start. No within-bin arrival times are inferred.
- QPN model: one open class, one FCFS queueing place, one exponential service process, and an immediate completion transition. The service rate is resolved by the predeclared rule `2 × measured offered job rate over the finite observation window`. The observation window starts at time zero and ends halfway through the first interarrival interval of the next cyclic replay, after the final scheduled arrival. This keeps leading idle time in the same denominator used by the simulator and prevents a second replay cycle. This is a normalized queueing scenario, not an inferred capacity of the observed network.
- Rate semantics are deliberately explicit: LINE's `ArvR` is checked against the reciprocal mean of the supplied interarrival vector, including the initial delay from time zero to the first arrival. The native sampler floors zero intervals to `1e-9` when scheduling events; this affects the effective event epochs and the cycle guard, not the trace's supplied mean. The throughput bound uses `N / simulation_horizon`, where the simulation horizon includes the short guard interval after the final effective arrival but ends before the next cyclic replay arrival.
- LINE Solver 3.0.8.0's native LDES Replayer cycles its trace and replaces non-positive interarrival values with `1e-9`; its Python `loop=False` setting is not serialized into the LDES model. The run is bounded by the LDES transient `timespan=[0, T]`, with `T` chosen strictly before the first arrival of the next replay cycle. `samples` remains only a generous service-completion safety budget and is not interpreted as the offered-job count. This is a bounded observation of one schedule, not a claim that the Replayer exhausts its source.
- Both conditions use the exact same resolved model file and service rate, queue discipline, mapping, time unit, warm-up policy, termination policy, replication count, and metric definitions. Each condition uses its own finite observation horizon derived from its schedule. They use disjoint seed sets so the between-condition interval is an independent two-sample comparison.

The one-kilobyte job is an explicit workload abstraction, not a claim that one job is a packet, flow, request, or application transaction. The service-rate calibration rule is user-authored and predeclared; it normalizes the reference workload to a 50% Poisson M/M/1 control utilization. It is not an estimate of the observed network's actual service capacity.

## Reproduction

First run the Westermo extraction workflow or follow [the model-extraction experiment](../model-extraction-v1/README.md) to produce:

- `artifacts/westermo-model-extraction-v1/model/source-signal.csv`
- `artifacts/westermo-model-extraction-v1/model/reconstructed-signal.csv`
- `artifacts/westermo-model-extraction-v1/manifest.json`

Then build the two neutral schedules and checksummed experiment definitions:

```sh
python scripts/build_qpn_workload_schedules.py \
  --source-csv artifacts/westermo-model-extraction-v1/model/source-signal.csv \
  --replay-csv artifacts/westermo-model-extraction-v1/model/reconstructed-signal.csv \
  --source-manifest artifacts/westermo-model-extraction-v1/manifest.json \
  --output-dir artifacts/qpn-workload-evaluation-v1
```

The builder validates the input series, preserves empty bins, applies identical cumulative quantization, and writes a manifest with source checksums, model/schedule checksums, workload totals, and the final remainder for each condition. The builder writes a checksummed `resolved-model-spec.json`: its exponential service rate is twice the measured reference's mean offered job rate. This predeclared calibration yields a 50% utilization target in the Poisson M/M/1 control; the same resolved model is used unchanged for measured and replay conditions. The experiment JSON records the same replication count and metric set for both conditions, using disjoint explicit seed sets for independent replications. Each run uses a finite simulated-time horizon ending after the final scheduled arrival and before the next cyclic replay arrival. The experiment records this horizon explicitly as `termination.kind=simulated_time`; the LDES completion budget is only a safety cap. The schedule preserves leading, internal, and trailing empty bins. The finite-window offered rate includes the initial idle delay from time zero and is computed using the exact same time origin as the transient simulation.

The LINE trace-driven simulation runner is exercised on a tiny synthetic trace in CI. The Westermo workflow also runs the full measured and deterministic-replay schedules, the Poisson-arrival M/M/1 analytical control, and the machine-readable comparison report. The control checks throughput, mean queue population, utilization, and residence time against their analytical values using 95% Student-t intervals across independent replications.

## Predeclared outcomes

Primary outcomes:
- mean completed-job throughput;
- mean queue population.

Secondary outcomes:
- server utilization;
- mean job residence time. Per-job residence-time samples are not yet exposed by the pinned Python LDES wrapper; distributional comparisons remain an explicit limitation.

Report per-replication observations, sample standard deviations, 95% confidence intervals, simulation runtime, scheduled offered jobs, finite observation horizon, and input checksums. Queueing and throughput metrics are transient time-window estimates; jobs still in service or queued at the horizon are not drained or counted as completed by an artificial completion budget. Preserve measured and deterministic replay results separately.

## Interpretation limits

- The experiment evaluates one authored single-queue QPN system, not a general network.
- The QPN job mapping is an abstraction of captured bytes.
- The deterministic reconstruction is not stochastic generation and does not establish generator fidelity.
- Westermo is industrial-network traffic and is not representative evidence for general Internet traffic.
- No packet-level TCP/IP, routing, congestion-control, or application semantics are modeled.
