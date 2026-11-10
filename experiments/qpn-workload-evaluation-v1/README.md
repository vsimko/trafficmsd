# Westermo QPN workload evaluation v1

This experiment is designed to compare two inputs against one frozen, authored QPN system model:

1. **Measured-trace reference** — one job schedule derived from the observed captured-frame-byte bins.
2. **Deterministic replay control** — the same byte-to-job mapping applied to the deterministic reconstruction exported by the model-extraction experiment.

The second condition is a deterministic reconstruction control. It is not a stochastic TrafficMSD generator and must never be labelled as one.

## Frozen workload and QPN interpretation

- Input trace: Westermo right observation-point archive, processed by `experiments/model-extraction-v1/run-westermo.R`.
- Source values: captured frame bytes per one-second bin, including zero-valued empty bins.
- Workload mapping: 1,000,000 captured-frame bytes represent one abstract QPN job.
- Quantization: cumulative bytes are divided by bytes-per-job; the per-bin job count is the difference between consecutive cumulative floors.
- Fractional bytes are carried into the next bin and the final remainder is recorded.
- All jobs in a bin are placed at the bin start. No within-bin arrival times are inferred.
- QPN model: one open class, one FCFS queueing place, one exponential service process, and an immediate completion transition. The service rate is resolved by the predeclared rule `2 × measured mean offered job rate`, giving the Poisson M/M/1 analytical control a 50% target utilization. This is a normalized queueing scenario, not an inferred capacity of the observed network.
- Both conditions use the exact same model file, service rate, queue discipline, mapping, time unit, warm-up policy, termination policy, replication seeds, and metric definitions.

The one-megabyte job is an explicit workload abstraction, not a claim that one job is a packet, flow, request, or application transaction. The service-rate calibration rule is user-authored and predeclared; it normalizes the reference workload to a 50% Poisson M/M/1 control utilization. It is not an estimate of the observed network's actual service capacity.

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

The builder validates the input series, preserves empty bins, applies identical cumulative quantization, and writes a manifest with source checksums, model/schedule checksums, workload totals, and the final remainder for each condition. The builder writes a checksummed `resolved-model-spec.json`: its exponential service rate is twice the measured reference's mean offered job rate. This predeclared calibration yields a 50% utilization target in the Poisson M/M/1 control; the same resolved model is used unchanged for measured and replay conditions. The experiment JSON records the same replication count and metric set for both conditions, using disjoint explicit seed sets for independent replications. Each finite-trace run terminates after all jobs offered by its own schedule have completed; the per-condition completion budget is recorded, while the termination policy remains identical (drain all offered jobs). The builder rejects a schedule whose mean offered job rate is not below the fixed service rate.

The LINE trace-driven simulation runner is exercised on a tiny synthetic trace in CI. A separate Poisson-arrival M/M/1 control uses the same QPN topology and exponential service, and checks throughput, mean queue population, utilization, and residence time against their analytical values using 95% Student-t intervals across independent replications. The full Westermo run/report is still required before interpreting measured-versus-replay differences.

## Predeclared outcomes

Primary outcomes:
- mean completed-job throughput;
- mean queue population.

Secondary outcomes:
- server utilization;
- job residence-time distribution/mean.

Report per-replication observations, sample standard deviations, 95% confidence intervals, simulation runtime, offered jobs, completed jobs, and input checksums. Preserve measured and deterministic replay results separately.

## Interpretation limits

- The experiment evaluates one authored single-queue QPN system, not a general network.
- The QPN job mapping is an abstraction of captured bytes.
- The deterministic reconstruction is not stochastic generation and does not establish generator fidelity.
- Westermo is industrial-network traffic and is not representative evidence for general Internet traffic.
- No packet-level TCP/IP, routing, congestion-control, or application semantics are modeled.
