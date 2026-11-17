# Westermo QPN workload evaluation v1

This dependent research branch extends the experiment to compare three inputs against one frozen, authored QPN system model:

1. **Measured-trace reference** — one job schedule derived from the observed captured-frame-byte bins.
2. **Offline deterministic replay control** — the same byte-to-job mapping applied to the deterministic reconstruction exported by the model-extraction experiment.
3. **Streaming fixed-duration reconstruction** — a test-only multiscale reconstruction at exploratory `T_min=32 s`, exported from the PR #111 prototype; this is not a production optimizer.

The second condition is a deterministic reconstruction control. It is not a stochastic TrafficMSD generator and must never be labelled as one.

## Frozen workload and QPN interpretation

- Input trace: Westermo right observation-point archive, processed by `experiments/model-extraction-v1/run-westermo.R`.
- Source values: captured frame bytes per one-second bin, including zero-valued empty bins.
- Workload mapping: 1,000 captured-frame bytes represent one abstract QPN job.
- Quantization: cumulative bytes are divided by bytes-per-job; the per-bin job count is the difference between consecutive cumulative floors.
- Fractional bytes are carried into the next bin and the final remainder is recorded.
- All jobs in a bin are placed at the bin start. No within-bin arrival times are inferred.
- QPN model: one open class, one FCFS queueing place, one exponential service process, and an immediate completion transition. The service rate is resolved by the predeclared rule `2 × measured offered job rate per full source-signal period`. The native Replayer is cyclic, so the interarrival vector includes the cycle-boundary gap from the final arrival to the next cycle's first arrival. That gap preserves both trailing and leading idle bins; the sum of one cycle's interarrival intervals equals the complete source-signal period. This is a periodic extension of the observed segment, not a one-shot finite trace. This is a normalized queueing scenario, not an inferred capacity of the observed network.
- Rate semantics are deliberately explicit: LINE's `ArvR` is checked against the reciprocal mean of the periodic interarrival vector, which is `N / full_source_signal_period`. The native sampler floors zero intervals to `1e-9` when scheduling events; this creates only a negligible positive period extension for simultaneous jobs. LINE 3.0.8.0's non-MSER filter modes still apply the default `warmupfrac=0.2`; the runner therefore uses `tranfilter=fixed, warmupfrac=0` to ensure that `warmup=none` really includes the entire run. The throughput bound uses the same periodic offered rate.
- LINE Solver 3.0.8.0's native LDES Replayer cycles its trace and replaces non-positive interarrival values with `1e-9`; its Python `loop=False` setting is not serialized into the LDES model. The input is therefore explicitly periodic. The service-completion event budget controls run length; it is not interpreted as the offered-job count and does not imply source exhaustion.
- All three conditions use the exact same resolved model file and service rate, queue discipline, mapping, time unit, warm-up policy, completion-event budget policy, replication count, and metric definitions. Their source-signal periods are the same; each periodic trace retains its own arrival pattern and cycle-boundary gap. They use disjoint seed sets so the between-condition interval is an independent two-sample comparison.

The one-kilobyte job is an explicit workload abstraction, not a claim that one job is a packet, flow, request, or application transaction. The service-rate calibration rule is user-authored and predeclared; it normalizes the reference workload to a 50% Poisson M/M/1 control utilization. It is not an estimate of the observed network's actual service capacity.

## Reproduction

First run the Westermo extraction workflow or follow [the model-extraction experiment](../model-extraction-v1/README.md) to produce:

- `artifacts/westermo-model-extraction-v1/model/source-signal.csv`
- `artifacts/westermo-model-extraction-v1/model/reconstructed-signal.csv`
- `artifacts/westermo-model-extraction-v1/manifest.json`

Run the test-only Rust multiscale benchmark on the same right-point trace with `TRAFFICMSD_STREAMING_QPN_OUTPUT` set, then build all three checksummed schedules and experiment definitions:

```sh
python scripts/build_qpn_workload_schedules.py \
  --source-csv artifacts/westermo-model-extraction-v1/model/source-signal.csv \
  --replay-csv artifacts/westermo-model-extraction-v1/model/reconstructed-signal.csv \
  --streaming-csv artifacts/qpn-workload-evaluation-v1/streaming-signal.csv \
  --streaming-metadata artifacts/qpn-workload-evaluation-v1/streaming-signal.json \
  --source-manifest artifacts/westermo-model-extraction-v1/manifest.json \
  --output-dir artifacts/qpn-workload-evaluation-v1
```

The builder validates the input series, preserves empty bins, applies identical cumulative quantization, and writes a manifest with source checksums, model/schedule checksums, workload totals, and the final remainder for each condition. The builder writes a checksummed `resolved-model-spec.json`: its exponential service rate is twice the measured reference's mean offered job rate over the full source-signal period. This predeclared calibration yields a 50% utilization target in the Poisson M/M/1 control; the same resolved model is used unchanged for all three conditions. The experiment JSON records the same replication count and metric set for all three conditions, using disjoint explicit seed sets for independent replications. Each run uses a service-completion event budget of `max(200000, offered_jobs × 20)`. The schedule preserves leading, internal, and trailing empty bins, and the periodic Replayer interarrival vector includes the full cycle-boundary gap so its total period matches the source signal.

The LINE trace-driven simulation runner is exercised on a tiny synthetic trace in CI. The Westermo workflow also runs the full measured, deterministic-replay, and streaming schedules, the Poisson-arrival M/M/1 analytical control, and the machine-readable comparison report. The control checks throughput, mean queue population, utilization, and residence time against their analytical values using 95% Student-t intervals across independent replications.

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

## Latest test-only streaming comparison (2026-11-17)

The latest successful end-to-end run is [workflow 44105306711](https://github.com/vsimko/trafficmsd/actions/runs/44105306711), artifact `westermo-model-extraction-and-qpn-v1`, on merged `master` at HEAD `9d0b8cdeb8edd7d81137aa4fbd88f005ba3aad7a`. All 26 experiment steps passed, including streaming export, schedule generation, all three LINE conditions, report extension, contract validation, and artifact upload. All 15 workflow runs on the merged PR head passed. The separate three-observation-point streaming benchmark passed in [workflow 44105306493](https://github.com/vsimko/trafficmsd/actions/runs/44105306493), artifact `streaming-fixed-threshold-westermo`.

The QPN artifact contains the aligned `streaming-signal.csv`, `streaming-signal.json` metadata, `streaming-multiscale-benchmark.json`, streaming schedule/experiment/result, and extended `report/comparison.json` and `report/report.md`. The metadata records 5,445 observed samples, 8,192 padded samples, the right observation point, `T_min=32 s`, and the measured trace SHA-256. The workflow sets all streaming output environment paths under `${{ github.workspace }}`, because Cargo executes package tests from the crate directory.

| Condition | Jobs / bytes | Offered rate (jobs/s) | Offered load | Replication seeds |
|---|---:|---:|---:|---:|
| Measured | 2,824 / 2,824,350 | 0.518641 | 0.5000 | 23000–23009 |
| Offline replay | 2,926 / 2,926,979 | 0.537374 | 0.5181 | 33000–33009 |
| Streaming T_min=32 s | 2,951 / 2,951,779 | 0.541965 | 0.5225 | 43000–43009 |

All three conditions passed LINE 3.0.8.0 validation and use the same resolved model SHA-256 `3929f567a6ad5f8035db6b1f63ba5333d7b224a0045c903e9d57bb1191c5d2e2` (service rate 1.03728191 jobs/s), 1-second bins, 1,000 bytes/job, `warmup=none`, and the same service-completion event budget. Each condition has 10 replications with disjoint seeds.

Streaming minus measured (95% Welch CI) is positive for all four metrics: throughput +0.0238123 jobs/s [0.0238067, 0.0238178], queue population +2.84145 jobs [2.59635, 3.08655], utilization +0.0240629 [0.0232247, 0.0249011], and residence time +0.791135 s [0.336507, 1.24576].

Streaming minus offline replay is smaller: throughput +0.00529516 jobs/s [0.00527913, 0.00531120] and utilization +0.00584459 [0.00512317, 0.00656601]. The queue-population difference (+0.269740 jobs, 95% CI [-0.0469074, 0.586387]) and residence-time difference (-0.505014 s, 95% CI [-1.09146, 0.0814304]) include zero. Thus, under this particular authored QPN, streaming and offline reconstruction are close on queue population and residence time, but not identical on offered workload, throughput, or utilization.

The three conditions use the exact same resolved model, workload mapping, backend, warm-up, completion budget, and metric definitions; streaming does not trigger model recalibration. This is one observation point from one industrial capture and one exploratory threshold; the observation points are not independent datasets. The result is not held-out validation, not a production optimizer result, and not evidence of general Internet traffic or network capacity. It does show that downstream QPN effects can be measured with a frozen model and explicit workload mapping rather than inferred from reconstruction MAE/RMSE alone.
