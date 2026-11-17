# Westermo streaming workload descriptors v1

**Status:** research result; test-only streaming prototype, not a production contract  
**Issue:** [#108](https://github.com/vsimko/trafficmsd/issues/108)  
**Experiment:** GitHub Actions run [44123007093](https://github.com/vsimko/trafficmsd/actions/runs/44123007093), artifact `westermo-model-extraction-and-qpn-v1`  
**Artifact SHA-256:** `a514eca96be665e2a8db40f5eaf132822c44b46ec12fce21a4b9f86e759ec531`

## Scope and reproducibility

This report adds signal-distribution, temporal-dependence, burst, schedule, and scale-wise retained-candidate descriptors to the existing measured/offline/streaming comparison. All signal comparisons use the same 5,445 observed one-second bins from the Westermo v1.0 right observation point; padded samples are excluded.

The three conditions are:

- **Measured:** captured-frame bytes per one-second bin.
- **Offline:** the model-v1 reconstruction, using the existing offline cutoff fraction 0.004.
- **Streaming:** test-only fixed-duration multiscale reconstruction with (T_{min}=32\,s).

The artifact contains the exact input CSVs, schedules, model, benchmark JSON, manifests, and checksums. Input SHA-256 values:

| Input | SHA-256 |
|---|---|
| Measured signal CSV | `00a7410eb998cc46f5b284cb5c9ed69ff491ea11161809509f56f5fcf2438f60` |
| Offline reconstruction CSV | `a4298df58b3d7619564cbbd17ccaf40353629edd049f8a0acfe4600973bd64ea` |
| Streaming reconstruction CSV | `8046026b581ba66776515c9bf49b6706b686d16ab673e5941d071d52246b698f` |
| Offline model-v1 JSON | `233cd8f0e76e2ca9dcb275a494ca63a802ea724a382c137e3b10be71c4d39ce5` |
| Streaming multiscale benchmark JSON | `bfdca79c6ffb409985f68efd92569e0c63b67f3baaaac375a74aea9656c5946b` |
| Measured / offline / streaming schedule JSON | `fa7bc6db23434050b30999c8541d9dfb765e67139401d22393941946ad29bb1f` / `bd6e8ed63a1ba3368f973f1d80c453b04b46ddaf67cef6af119ff0c86ef212b7` / `3a99dabce092c9489464fa936d6c4d947ed9eb9610dc22c5f552cdb60cfaaea3`

Metrics below are computed on the unpadded signal. Standard deviation is population SD; percentiles use linear interpolation; lag-(k) autocorrelation is the Pearson correlation between (x_t) and (x_{t-k}). Burst runs are contiguous bins strictly above the measured signal's 95th-percentile threshold, 2,102.6 bytes/s, reused unchanged for all three signals. Schedule interarrivals expand each batch into one event per abstract job and include the periodic cycle-boundary interval, as required by the LINE Replayer contract.

## Signal descriptors

| Descriptor | Measured | Offline | Streaming (T_{min}=32s) |
|---|---:|---:|---:|
| Total bytes | 2,824,350 | 2,926,979 | 2,951,779 |
| Mean bytes/bin | 518.705 | 537.554 | 542.108 |
| Population SD | 1,602.174 | 2,043.239 | 2,040.358 |
| Coefficient of variation | 3.089 | 3.801 | 3.764 |
| Zero-bin fraction | 0.000% | 75.702% | 73.682% |
| p50 bytes/bin | 180 | 0 | 0 |
| p90 bytes/bin | 744 | 744 | 744 |
| p95 bytes/bin | 2,102.6 | 2,772 | 2,664 |
| p99 bytes/bin | 8,998.12 | 11,578 | 11,578 |
| Lag-1 autocorrelation | 0.6992 | -0.0692 | -0.0706 |
| Lag-2 autocorrelation | 0.6081 | 0.4256 | 0.4222 |
| Lag-5 autocorrelation | 0.5816 | -0.0693 | -0.0707 |
| Lag-10 autocorrelation | 0.4798 | 0.3912 | 0.3869 |
| Runs above 2,102.6 bytes/s | 80 | 276 | 275 |
| Longest such run (bins) | 26 | 1 | 1 |

The two reconstructions are close to one another at (T_{min}=32s), but both have a substantially different distribution and temporal structure from the measured trace. In particular, the measured signal has positive lag-1 dependence (0.699), while both reconstructions are slightly negative (about -0.07). Both reconstructions also fragment the 80 measured high-volume runs into roughly 275 one-bin runs. Similar aggregate volume is therefore not evidence of burst-structure fidelity.

## Downstream job-schedule descriptors

The same cumulative 1,000-byte-to-one-job mapping is used for all conditions. Jobs are placed at the start of their source bin; the mapping carries fractional bytes between bins.

| Descriptor | Measured | Offline | Streaming |
|---|---:|---:|---:|
| Total jobs per 5,445-second period | 2,824 | 2,926 | 2,951 |
| Non-empty bins | 1,272 | 788 | 813 |
| Non-empty-bin occupancy | 23.36% | 14.47% | 14.93% |
| Mean jobs per non-empty bin | 2.220 | 3.713 | 3.630 |
| Maximum jobs in one bin | 22 | 22 | 21 |
| Contiguous non-empty-bin runs | 926 | 788 | 813 |
| Longest non-empty-bin run | 62 | 1 | 1 |
| Lag-1 autocorrelation of per-bin job counts | 0.6584 | -0.0683 | -0.0696 |
| Mean interarrival (s) | 1.928 | 1.861 | 1.845 |
| Zero interarrival fraction (simultaneous jobs) | 54.96% | 73.07% | 72.45% |
| Interarrival p95 / p99 (s) | 8 / 9 | 8 / 16 | 8 / 16 |
| Maximum interarrival (s) | 11 | 32 | 30 |

The offline and streaming schedules create fewer, larger, more isolated batches than the measured schedule. Their longest run of consecutive non-empty bins is one, versus 62 for measured traffic. The schedules also contain more simultaneous jobs and longer upper-tail interarrival gaps. These are material workload differences even though the three QPN runs share a frozen model and configuration.

## Scale-wise retained-candidate counts

The offline counts below are positive-amplitude components in `model-v1.json`; streaming counts are `retained_positive_candidates` from the (T_{min}=32s) multiscale benchmark. The offline cutoff is 0.004. Counts are comparable as a retained-candidate diagnostic, not as coefficient-amplitude equality.

| Scale | Window (s) | Offline positive components | Streaming retained candidates | Difference |
|---:|---:|---:|---:|---:|
| 13 | 8,192 | 1 | 1 | 0 |
| 12 | 4,096 | 2 | 2 | 0 |
| 11 | 2,048 | 3 | 3 | 0 |
| 10 | 1,024 | 6 | 6 | 0 |
| 9 | 512 | 11 | 11 | 0 |
| 8 | 256 | 22 | 22 | 0 |
| 7 | 128 | 43 | 43 | 0 |
| 6 | 64 | 86 | 86 | 0 |
| 5 | 32 | 171 | 171 | 0 |
| 4 | 16 | 341 | 341 | 0 |
| 3 | 8 | 322 | 380 | +58 |
| 2 | 4 | 321 | 373 | +52 |
| 1 | 2 | 0 | 0 | 0 |
| 0 | 1 | 0 | 0 | 0 |
| **Total** | | **1,329** | **1,439** | **+110 (+8.28%)** |

The count difference is concentrated at the 4- and 8-second scales. The current streaming artifact does not serialize the full per-scale coefficient amplitude matrix, so per-scale amplitude error, sign/value alignment, and scale-wise reconstruction contribution are not yet available. Those must not be inferred from candidate counts.

## Reconstruction and QPN context

For the (T_{min}=32s) streaming policy:

- Offline reconstruction vs measured trace: MAE 710.607 bytes/s, RMSE 2,149.185, maximum absolute error 21,510, total-volume error +3.634%.
- Streaming reconstruction vs measured trace: MAE 710.805 bytes/s, RMSE 2,149.987, maximum absolute error 21,510, total-volume error +4.512%.
- Streaming vs offline reconstruction: MAE 14.892 bytes/s, RMSE 118.317, maximum absolute error 5,099, total-volume difference +0.847%.

The frozen-QPN experiment used 2,824 measured, 2,926 offline, and 2,951 streaming jobs, 10 disjoint replications per condition, and the same resolved model SHA-256 `3929f567a6ad5f8035db6b1f63ba5333d7b224a0045c903e9d57bb1191c5d2e2`. This remains an authored single-FCFS-queue abstraction, not a packet-level network model.

## Finality and provisional records

The fixed-duration prototype returns records only when they are final. It does not publish provisional records, so there is no provisional-revision rate to estimate for this implementation; the relevant consumer contract is to wait for final output. The benchmark's finality is source-time availability latency, not wall-clock consumer delay. For (T_{min}=32s), the aggregate finality p50/p95/p99/max is 3/26/96/4,096 seconds on the right observation point. The coarsest-scale EOF-sensitive tail is not a bounded-latency guarantee.

## Direct per-scale coefficient-amplitude comparison

The test-only workflow now exports the full padded streaming matrix and runs `scripts/compare_streaming_coefficients.py`. The generated report is included in the same workflow artifact under `qpn-workload-evaluation-v1/coefficient-comparison/`.

**Provenance:** workflow [44123007093](https://github.com/vsimko/trafficmsd/actions/runs/44123007093), artifact SHA-256 `a514eca96be665e2a8db40f5eaf132822c44b46ec12fce21a4b9f86e759ec531`; offline model SHA-256 `233cd8f0e76e2ca9dcb275a494ca63a802ea724a382c137e3b10be71c4d39ce5`; streaming coefficient matrix SHA-256 `bd17491b79dafa24ccd8ce9d6d21afc6d126c9bb6f05e3e0cf470b9a06a45ad2`.

The comparison extracts each scale's coefficient at the center-left emission position used by reconstruction. It reports both the complete 8,192-sample padded domain and the 5,445 observed bins. Both representations independently reconstruct their corresponding signal CSVs with maximum absolute round-trip error no greater than (10^{-8}).

| Scale | Window (s) | Offline positive | Streaming positive | Full-domain MAE | Full-domain RMSE | Observed MAE | Observed RMSE | Max abs. error |
|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 0 | 1 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| 1 | 2 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| 2 | 4 | 321 | 373 | 23.8823 | 123.592 | 35.9375 | 151.610 | 2,416 |
| 3 | 8 | 322 | 380 | 31.4229 | 209.495 | 47.2496 | 256.891 | 5,099 |
| 4 | 16 | 341 | 341 | 0 | 0 | 0 | 0 | 0 |
| 5 | 32 | 171 | 171 | 0 | 0 | 0 | 0 | 0 |
| 6 | 64 | 86 | 86 | 0 | 0 | 0 | 0 | 0 |
| 7 | 128 | 43 | 43 | 0 | 0 | 0 | 0 | 0 |
| 8 | 256 | 22 | 22 | 0 | 0 | 0 | 0 | 0 |
| 9 | 512 | 11 | 11 | 0 | 0 | 0 | 0 | 0 |
| 10 | 1,024 | 6 | 6 | 0 | 0 | 0 | 0 | 0 |
| 11 | 2,048 | 3 | 3 | 0 | 0 | 0 | 0 | 0 |
| 12 | 4,096 | 2 | 2 | 0 | 0 | 0 | 0 | 0 |
| 13 | 8,192 | 1 | 1 | 0 | 0 | 0 | 0 | 0 |
| **Total** | | **1,329** | **1,439** | | | | | |

All observed coefficient-amplitude differences are concentrated at the 4-second and 8-second scales. Scales 0, 1, and 4–13 match exactly at the reconstruction emission positions. This localizes the observed discrepancy to the shorter multiscale candidates affected by the fixed-duration policy. It does not demonstrate equivalence of the complete models or generalize beyond this capture.

### Threshold-policy diagnosis

The two paths do not currently apply the same positive-run retention boundary. Offline `optimize_intervals()` removes a positive run when its length is less than or equal to `0.004 × N`, where N is the number of candidates at that scale. The streaming prototype instead retains a run as soon as its length reaches `ceil(T_min / window_seconds)`. For the exploratory `T_min=32 s` policy, this gives:

| Scale | Window | Candidate count | Offline cutoff | Offline minimum retained run | Streaming minimum retained run | Runs retained only by streaming |
|---:|---:|---:|---:|---:|---:|---:|
| 3 | 8 s | 1,024 | 4.096 | 5 | 4 | 13 |
| 2 | 4 s | 2,048 | 8.192 | 9 | 8 | 13 |

The boundary-run counts were reproduced from the same padded trace and the candidate-generation/residual-update order, and the resulting per-scale rows match the exported offline model and streaming matrix exactly. At scale 3, 13 raw positive runs of length 4 are retained by streaming but removed offline. At scale 2, 13 raw positive runs of length 8 are retained by streaming but removed offline. The higher-scale difference also changes the residual passed into the 4-second scale, so the 4-second effect is not independent; interpolation can further spread the coefficient differences beyond the number of boundary runs.

This explains the observed discrepancy as a policy-definition mismatch, not a serialization or reconstruction error. The streaming experiment is a fixed-duration policy, whereas offline v1 uses a scale-relative fraction of the candidate-vector length. The current comparison therefore measures two different retention policies. It must not be presented as a like-for-like optimizer equivalence test. A follow-up equivalence experiment should apply the exact offline threshold rule in streaming form (or explicitly compare the two policies as separate candidates) before drawing conclusions about implementation compatibility.

## Interpretation and remaining work

This report strengthens the workload comparison but does not establish compatibility:

- Only one Westermo observation point and one exploratory threshold are evaluated. The three Westermo observation points are not independent datasets.
- Per-scale coefficient amplitudes are now compared directly for this one capture; independent-capture validation and held-out threshold selection remain open.
- No independent capture has yet been processed.
- Threshold selection has not been validated on held-out captures.
- No production optimizer, model schema, or API change is proposed.

The next experiment should export the streaming coefficient matrix with provenance, compare it directly with offline coefficients on identical observed bins, and repeat the frozen-QPN analysis on an independent raw capture selected from the dataset registry. Large captures must use the documented on-demand/local acquisition policy.
