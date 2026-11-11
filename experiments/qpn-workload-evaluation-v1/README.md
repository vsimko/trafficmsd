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
- QPN model: one open class, one FCFS queueing place, one exponential service process, and an immediate completion transition. The service rate is resolved by the predeclared rule `2 × measured offered job rate per full source-signal period`. The native Replayer is cyclic, so the interarrival vector includes the cycle-boundary gap from the final arrival to the next cycle's first arrival. That gap preserves both trailing and leading idle bins; the sum of one cycle's interarrival intervals equals the complete source-signal period. This is a periodic extension of the observed segment, not a one-shot finite trace. This is a normalized queueing scenario, not an inferred capacity of the observed network.
- Rate semantics are deliberately explicit: LINE's `ArvR` is checked against the reciprocal mean of the periodic interarrival vector, which is `N / full_source_signal_period`. The native sampler floors zero intervals to `1e-9` when scheduling events; this creates only a negligible positive period extension for simultaneous jobs. The throughput bound uses the same periodic offered rate.
- LINE Solver 3.0.8.0's native LDES Replayer cycles its trace and replaces non-positive interarrival values with `1e-9`; its Python `loop=False` setting is not serialized into the LDES model. The input is therefore explicitly periodic. The service-completion event budget controls run length; it is not interpreted as the offered-job count and does not imply source exhaustion.
- Both conditions use the exact same resolved model file and service rate, queue discipline, mapping, time unit, warm-up policy, completion-event budget policy, replication count, and metric definitions. Their source-signal periods are the same; each periodic trace retains its own arrival pattern and cycle-boundary gap. They use disjoint seed sets so the between-condition interval is an independent two-sample comparison.

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

The builder validates the input series, preserves empty bins, applies identical cumulative quantization, and writes a manifest with source checksums, model/schedule checksums, workload totals, and the final remainder for each condition. The builder writes a checksummed `resolved-model-spec.json`: its exponential service rate is twice the measured reference's mean offered job rate over the full source-signal period. This predeclared calibration yields a 50% utilization target in the Poisson M/M/1 control; the same resolved model is used unchanged for measured and replay conditions. The experiment JSON records the same replication count and metric set for both conditions, using disjoint explicit seed sets for independent replications. Each run uses a service-completion event budget of `max(200000, offered_jobs × 20)`. The schedule preserves leading, internal, and trailing empty bins, and the periodic Replayer interarrival vector includes the full cycle-boundary gap so its total period matches the source signal.

The LINE trace-driven simulation runner is exercised on a tiny synthetic trace in CI. The Westermo workflow also runs the full measured and deterministic-replay schedules, the Poisson-arrival M/M/1 analytical control, and the machine-readable comparison report. The control checks throughput, mean queue population, utilization, and residence time against their analytical values using 95% Student-t intervals across independent replications.

## Predeclared outcomes

Primary outcomes:
- mean completed-job throughput;
- mean queue population.

Secondary outcomes:
- server utilization;
- mean job residence time. Per-job residence-time samples are not yet exposed by the pinned Python LDES wrapper; distributional comparisons remain an explicit limitation.

Report per-replication observations, sample standard deviations, 95% confidence intervals, simulation runtime, scheduled offered jobs per period, periodic replay period, and input checksums. Queueing and throughput metrics are steady-state estimates for the explicitly periodic workload; they are not one-shot finite-trace drain metrics. Preserve measured and deterministic replay results separately.

## Interpretation limits

- The experiment evaluates one authored single-queue QPN system, not a general network.
- The QPN job mapping is an abstraction of captured bytes.
- The deterministic reconstruction is not stochastic generation and does not establish generator fidelity.
- Westermo is industrial-network traffic and is not representative evidence for general Internet traffic.
- No packet-level TCP/IP, routing, congestion-control, or application semantics are modeled.
