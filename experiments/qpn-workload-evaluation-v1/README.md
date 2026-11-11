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
- QPN model: one open class, one FCFS queueing place, one exponential service process, and an immediate completion transition. The service rate is resolved by the predeclared rule `2 × measured mean offered job rate over the active arrival span`. The active span is the time from the first non-empty bin's arrival epoch to the last non-empty bin's arrival epoch; leading and trailing idle bins are reported separately. This gives the Poisson M/M/1 analytical control a 50% target utilization relative to that active-span rate. This is a normalized queueing scenario, not an inferred capacity of the observed network.
- Rate semantics are deliberately separated: LINE's `ArvR` is checked against the reciprocal mean of the complete supplied interarrival vector, which includes the delay from time zero to the first arrival. The finite-window throughput bound uses `N / (last_arrival_epoch - first_arrival_epoch)`, so both compared quantities refer to the same first-to-last arrival observation window. The initial idle delay is not silently included in one denominator and excluded from the other.
- LINE Solver 3.0.8.0's native LDES Replayer cycles its trace and replaces non-positive interarrival values with `1e-9`; its Python `loop=False` setting is not serialized into the LDES model. The run is therefore bounded by the configured completion count (one schedule's offered-job count), not by source-trace exhaustion. Do not describe this as proof that the Replayer itself is one-shot.
- Both conditions use the exact same resolved model file and service rate, queue discipline, mapping, time unit, warm-up policy, termination policy, replication count, and metric definitions. They use disjoint seed sets so the between-condition interval is an independent two-sample comparison.

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

The builder validates the input series, preserves empty bins, applies identical cumulative quantization, and writes a manifest with source checksums, model/schedule checksums, workload totals, and the final remainder for each condition. The builder writes a checksummed `resolved-model-spec.json`: its exponential service rate is twice the measured reference's mean offered job rate. This predeclared calibration yields a 50% utilization target in the Poisson M/M/1 control; the same resolved model is used unchanged for measured and replay conditions. The experiment JSON records the same replication count and metric set for both conditions, using disjoint explicit seed sets for independent replications. Each run uses a completion budget equal to the number of jobs in its own schedule and records that budget explicitly. The schedule preserves leading, internal, and trailing empty bins. Its active arrival span runs from the first to the last non-empty bin; leading and trailing idle bins remain visible in the manifest but are not mixed into the active-span throughput denominator. Because the native Replayer is cyclic, the completion budget—not `loop=False`—bounds the run.

The LINE trace-driven simulation runner is exercised on a tiny synthetic trace in CI. The Westermo workflow also runs the full measured and deterministic-replay schedules, the Poisson-arrival M/M/1 analytical control, and the machine-readable comparison report. The control checks throughput, mean queue population, utilization, and residence time against their analytical values using 95% Student-t intervals across independent replications.

## Predeclared outcomes

Primary outcomes:
- mean completed-job throughput;
- mean queue population.

Secondary outcomes:
- server utilization;
- mean job residence time. Per-job residence-time samples are not yet exposed by the pinned Python LDES wrapper; distributional comparisons remain an explicit limitation.

Report per-replication observations, sample standard deviations, 95% confidence intervals, simulation runtime, offered jobs, completed jobs, and input checksums. Preserve measured and deterministic replay results separately.

## Interpretation limits

- The experiment evaluates one authored single-queue QPN system, not a general network.
- The QPN job mapping is an abstraction of captured bytes.
- The deterministic reconstruction is not stochastic generation and does not establish generator fidelity.
- Westermo is industrial-network traffic and is not representative evidence for general Internet traffic.
- No packet-level TCP/IP, routing, congestion-control, or application semantics are modeled.
