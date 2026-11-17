# Fixed-horizon streaming interval optimizer — research prototype

**Status:** test-only prototype; not a production API or selected model contract.  
**Issue:** #107  
**Reference:** `decomposition::optimize_intervals()` remains unchanged and is the offline oracle.

> **Finality correction (2026-11-17):** The multiscale benchmark reports below used an incorrect conversion from candidate-index delay to source-sample time. For a positive scale with window `w`, candidate `k` represents `event_time = k*w + w/2 - 1`, while candidate `j` is available at source watermark `(j+1)*w - 1`. The correct delay is `(j-k)*w + w/2`. The earlier per-scale and aggregate p50/p95/p99/max finality values (including the previously reported 2,048-second maximum) are **superseded and must not be used**. The corrected implementation and regression test are in PR #111; corrected three-point measurements are pending CI. Runtime and reconstruction metrics are unaffected by this timestamp-only correction.

## Question

Can the existing interval optimization be evaluated incrementally without changing its output, if the complete candidate-vector horizon is declared before the first value arrives?

The answer for the local optimizer is **yes**, with delayed finality. This does not establish that the complete multiscale decomposition can be scheduled with useful latency.

## Fixed-horizon contract

The caller declares candidate-vector length `N` and cutoff fraction `f` at construction. The optimizer freezes `c = N * f` for the entire run. Changing `N` creates a new run; it cannot extend a run whose outputs have already been finalized.

Input values arrive in order through `push(value)`. Exactly `N` values are required before `finish()`. Overrun and premature finish are errors.

Each output carries:
- `event_time`: original candidate index;
- `available_at`: input index at which its value became final;
- `value`: the immutable optimized value.

These are distinct clocks. A candidate may represent an earlier event but become available later.

## Incremental state and update order

The test-only implementation in `crates/trafficmsd-core/src/streaming_interval_tests.rs` uses:

1. **Run filter:** when `c > 1`, retain a positive run provisionally until a zero closes it or the declared horizon ends. Keep it iff its length is strictly greater than `c`, matching the batch implementation. When `c <= 1`, this filter is skipped exactly as in batch.
2. **Interpolation:** feed filtered values into a two-value lookahead queue. Commit the middle value only after its right filtered neighbor is known. Keep the previous *optimized* value as the left neighbor, preserving the batch pass's in-place left-to-right behavior.
3. **Boundaries:** emit the first value when it is known; flush the last value at `finish()`. EOF closes a positive run without appending a synthetic zero.

The pending state is the open positive run plus at most two interpolation lookahead values. Thus the local optimizer uses `O(R)` pending candidate state where `R` is the longest unresolved positive run; the worst case is `O(N)`. The prototype records the peak count of these pending candidates.

## Evidence in tests

The test-only suite:
- exhaustively enumerates every vector over `{0,1,2}` for horizons 1 through 8 and seven cutoff fractions;
- checks each emitted value against the batch result for the *complete* declared horizon at the exact push/finish that emitted it;
- checks that each index is emitted exactly once;
- verifies sequential in-place interpolation with a case where the previous value was itself interpolated;
- checks run closure by a zero and by EOF;
- checks the 512/1,024 horizon cutoff counterexample;
- rejects input beyond the declared horizon and premature finish.

The exhaustive test exercises every possible suffix for each prefix at these small horizons: a value emitted while consuming a prefix must agree with the batch result for every enumerated completion sharing that prefix.

## Latency and limitations

- With `c <= 1`, the run filter is inactive and interpolation requires one right-hand candidate of lookahead (except boundaries).
- With `c > 1`, an output inside a positive run cannot be finalized until that run closes. If the run reaches EOF, its outputs become available only at `N - 1`.
- Worst-case local-optimizer finalization latency is therefore the full declared horizon. A fixed horizon preserves legacy semantics but does not guarantee useful low-latency output.
- This prototype has not yet been benchmarked for throughput or RSS.
- It does not integrate candidate generation, residual updates, or cross-scale readiness. The complete multiscale dependency schedule and its latency remain unproven by this local test.
- The current work uses synthetic/adversarial inputs, not provenance-backed packet traces.
- Unknown-horizon operation is not supported. The horizon-relative legacy cutoff is incompatible with arbitrary horizon extension and immutable prefix output.

## Next decision

Use this as the exact fixed-horizon baseline. Compare it with explicitly different causal policies (for example fixed absolute or scale-local run thresholds) using retained interval counts, amplitude differences, reconstruction error, finality latency, and pending state. Do not select a replacement policy or modify the production optimizer/model schema until those results are reviewed.
## Candidate causal policies to compare

| Policy | Run-retention rule | Horizon dependence | Expected run-decision delay |
|---|---|---|---|
| Fixed-horizon reference | Keep iff run length > N * f | Declared N | Up to the remaining horizon |
| Fixed candidate count | Keep iff run length >= L | None | At most L candidates from run start |
| Fixed physical duration | Keep iff run duration >= T_min | None; uses the scale's time width | At most ceil(T_min / (2^s * Δt)) candidates |

For a scale s, one candidate represents 2^s * Δt seconds when the source sampling interval is Δt. A physical-duration policy therefore uses L_s = max(1, ceil(T_min / (2^s * Δt))). The contract must state whether a run exactly at the threshold is retained; the proposed convention is length >= L_s. This intentionally differs from the legacy strict length > N * f rule.

A bounded-delay implementation can hold a positive run provisionally until it reaches its threshold. If it reaches the threshold, the run is known to be retained; if a zero arrives first, the shorter run is known to be removed. The interpolation pass still needs its right-hand neighbor and must preserve the legacy left-to-right update order. Do not emit a value as final until both the run decision and interpolation dependency are resolved. With a fixed threshold, pending run state is bounded by L_s (plus interpolation lookahead), rather than by the complete horizon.

These are candidate policies, not selected semantics. A threshold expressed in physical time is preferable to a single candidate count when comparing multiple scales, but its value must be calibrated against downstream simulation requirements and the packet-trace sampling interval. A threshold of one candidate disables meaningful short-run filtering and should be included as a diagnostic baseline.

## Comparison protocol and metrics

Run all policies on the same deterministic fixtures and provenance-backed traces, across multiple total horizons and scale levels. For every policy and trace, record:

- **Retention:** number and fraction of positive candidates retained; run-length distribution before and after filtering.
- **Amplitude:** absolute and relative difference of retained candidate values against the offline reference.
- **Reconstruction:** MAE, RMSE, and maximum absolute error of reconstructed traffic, plus total traffic-volume error.
- **Events:** number of emitted intervals and their duration/amplitude distributions; do not equate candidate count with packet count.
- **Finality:** available_at - event_time in candidate units and converted to seconds; report median, p95, p99, and maximum.
- **Memory:** peak unresolved run and interpolation state; separately report process RSS.
- **Runtime:** throughput and per-candidate processing latency, including warm and cold runs.

Report results by scale as well as in aggregate. Include quiet tails, long uninterrupted positive runs, threshold-boundary lengths (L_s - 1, L_s, L_s + 1), one-sample gaps, alternating values, repeated maxima, and prefix extensions. For the causal policies, replay every prefix and assert that previously finalized records remain bit-identical. For the fixed-horizon reference, explicitly report its known horizon sensitivity rather than treating prefix replay as an online implementation.

The evaluation should use a trace-level train/validation split if T_min or any other threshold is tuned. Select thresholds on validation data only, then report held-out results. Keep the threshold, time unit, sampling interval, scale, and tie-breaking rule in the experiment metadata so runs are reproducible.

## Test-only fixed-count prototype

`crates/trafficmsd-core/src/streaming_interval_tests.rs` now also contains a separate fixed-count prototype. It retains a positive run once its length reaches L; a shorter run is discarded when a zero closes it or at EOF. It uses the same two-candidate interpolation lookahead and left-to-right update order, but its run-retention rule is intentionally different from the offline reference.

The initial tests cover threshold values L = 1, 2, 3, and 4, exact-threshold retention, shorter-run removal, and equality of already-emitted records with the completed fixed-threshold stream. A follow-up exhaustive suite now enumerates all vectors over {0,1,2} for horizons 1–8 and L = 1..4, checking every emitted record against the completed fixed-threshold result. It also enumerates all prefixes of lengths 1–6 and checks that emissions during the prefix are identical across every larger declared horizon through 8. This is still synthetic correctness evidence, not a performance/fidelity evaluation. In particular, no physical-duration adapter, trace-level calibration, multiscale integration, RSS measurement, or downstream simulation comparison is included yet.

## Westermo trace benchmark protocol

A reproducible, ignored Rust benchmark now exercises the fixed-count prototype on the checksum-verified Westermo v1.0 right-observation-point trace. The input is the 5,445-sample, one-second series of captured frame bytes per bin (trace SHA-256 `6429e3d6a84efdc13edc14e44b2de2e6cbd31452e1958116c271745069b3fbb7`). It is a real packet-derived trace, but applying the aggregate series directly as one candidate vector is a **single-scale proxy**, not the actual multiscale candidate-generation/residual-update pipeline.

The benchmark compares fixed run thresholds L = 1, 2, 4, 8, 16, 22, 32, and 64 with the unchanged offline `optimize_intervals()` at cutoff fraction 0.004. L=22 is included because the fixed-horizon offline cutoff on 5,445 samples is 21.78 and the fixed-count policy retains at integer threshold L when a run reaches that length. This is a comparison point, not a selected threshold. Once a positive run reaches L, the prototype now keeps the remainder of that same run until zero or EOF; a regression test covers runs longer than 2L so the threshold transition cannot accidentally split them.

For each policy it records:
- streaming and offline elapsed time and candidate throughput;
- observed peak pending run plus interpolation candidates (not process RSS);
- retained positive candidates and positive interval count;
- finality delay in candidate units (p50/p95/p99/max);
- MAE, RMSE, maximum absolute error, and total-volume error against the measured series;
- the same reconstruction differences against the offline optimizer.

Reproduction uses the immutable artifact from Westermo extraction workflow run 42001977735. The dedicated GitHub Actions workflow downloads it, verifies the trace SHA-256 and exact sample count, then runs:

```sh
TRAFFICMSD_STREAMING_TRACE=artifacts/westermo-model-extraction-v1/trace.csv \
TRAFFICMSD_STREAMING_BENCHMARK_OUTPUT=artifacts/streaming-fixed-threshold-westermo/benchmark.json \
cargo test -p trafficmsd-core fixed_threshold_westermo_packet_trace_benchmark -- --ignored --nocapture
```

The workflow passes absolute workspace paths because Cargo runs package tests from the crate directory, then uploads `benchmark.json` as the `streaming-fixed-threshold-westermo` artifact. Timings are CI-environment measurements and should be interpreted as a baseline, not a general performance claim. The threshold grid is exploratory and is not calibrated on held-out data. This benchmark does not measure raw PCAP parsing, scale-specific candidate generation, residual updates, cross-scale finality, downstream simulation effects, or process RSS. Westermo is industrial-network traffic from one observation point, so it does not establish general Internet representativeness.

## Multiscale Westermo benchmark

The same workflow now runs a second experiment that applies a fixed **physical-duration** threshold throughout the real multiscale decomposition. It pads the 5,445 observed samples to 8,192 as the model-extraction pipeline does, uses the production unique-maximum candidate generation and residual-update order at every scale, and replaces only the interval optimizer with the test-only fixed-count stream. For a duration threshold T_min and scale s, it uses L_s = max(1, ceil(T_min / (2^s * Δt))); here Δt = 1 second and T_min is swept over 1, 2, 4, 8, 16, 32, and 64 seconds. Scale zero uses L_0 = T_min.

The multiscale report records per-scale candidate counts, thresholds, retained intervals, finality delays in seconds, and interval-optimizer runtime; aggregate interval-only and full-pipeline throughput, pending-state proxy; and reconstruction MAE/RMSE/max/volume error against both the measured trace and the unchanged offline decomposition. Full-pipeline timing includes candidate generation, interval optimization, residual updates, and reconstruction; offline timing measures the corresponding complete production decomposition plus reconstruction. This is materially closer to the production decomposition path than the single-scale proxy above. It still does not measure process RSS, PCAP parsing, downstream simulator outcomes, or calibration on a held-out dataset. No production implementation or API/schema change is made.

The benchmark workflow runs both ignored tests whose names match `fixed_threshold_westermo` and uploads both JSON reports under the `streaming-fixed-threshold-westermo` artifact.

## Preliminary Westermo multiscale result

The first completed multiscale run is GitHub Actions workflow run [44044797672](https://github.com/vsimko/trafficmsd/actions/runs/44044797672), using the immutable trace checksum above. It evaluates all 16,383 candidate positions across scales for each duration policy. The report artifact is `streaming-fixed-threshold-westermo`.

The offline decomposition plus reconstruction took 19.17 ms in this CI invocation. The fixed-duration full pipeline (candidate generation, interval optimization, residual updates, and reconstruction) took 15.22–16.66 ms across the tested thresholds. These are single-run CI measurements without a repeated warmup/median protocol; **do not interpret them as a performance advantage**. Interval-optimizer-only time was 2.66–3.16 ms.

| T_min | Full pipeline | Finality p50 / p95 / p99 / max | MAE | RMSE | Volume error |
|---:|---:|---:|---:|---:|---:|
| 1 s | 16.66 ms | 1 / 16 / 64 / 2,048 s | 601.98 | 1,988.60 | +46.24% |
| 16 s | 15.41 ms | 2 / 16 / 64 / 2,048 s | 679.74 | 2,079.10 | +21.35% |
| 32 s | 15.34 ms | 2 / 24 / 64 / 2,048 s | 710.80 | 2,149.99 | +4.51% |
| 64 s | 15.22 ms | 4 / 32 / 64 / 2,048 s | 721.36 | 2,183.31 | −10.46% |

The offline reference on the observed 5,445 samples has MAE 710.61, RMSE 2,149.18, and volume error +3.63%. T_min = 32 s is the closest tested reconstruction to the offline reference (MAE difference 14.89; RMSE difference 118.32), but this is only similarity to the legacy decomposition—not a claim of better measured-trace fidelity. T_min = 16 s improves MAE/RMSE modestly while materially worsening volume preservation. No threshold is selected.

The maximum 2,048-second finality delay is caused by one-candidate interpolation lookahead at a coarse scale whose candidate represents 2,048 seconds. This shows that bounding the run threshold alone does not bound wall-clock finality to a small value across scales. Scale-dependent event duration and interpolation dependencies must be included in any latency contract.

The benchmark remains exploratory: one industrial trace, one observation point, one CI run, no held-out threshold calibration, no process RSS, and no downstream simulator comparison. The full JSON report contains per-scale candidate counts, retained intervals, thresholds, and delay distributions. No production optimizer or schema was changed.

## Independent observation-point sweep

The benchmark workflow now extracts and evaluates the left and bottom archives independently in addition to the immutable right-point trace. It never combines observation points, because the source dataset warns that packets may be duplicated across locations. The two additional archives are verified against the release `SHA256SUMS`; each derived trace is SHA-256 checked against its own manifest. The three-point run is [workflow 44045596539](https://github.com/vsimko/trafficmsd/actions/runs/44045596539), artifact `streaming-fixed-threshold-westermo`.

For T_min = 32 seconds (the closest tested policy to offline on each point), the per-point comparison is:

| Observation point | Samples | Offline MAE / RMSE | Fixed-duration MAE / RMSE | Offline volume error | Fixed-duration volume error | Finality p50 / p95 / p99 / max |
|---|---:|---:|---:|---:|---:|---:|
| right | 5,445 | 710.61 / 2,149.18 | 710.80 / 2,149.99 | +3.63% | +4.51% | 2 / 24 / 64 / 2,048 s |
| left | 5,441 | 5,788.47 / 11,821.54 | 5,787.78 / 11,821.52 | +19.20% | +19.22% | 2 / 20 / 64 / 2,048 s |
| bottom | 5,443 | 11,242.35 / 14,945.84 | 11,123.98 / 14,864.53 | −43.75% | −43.18% | 2 / 16 / 64 / 2,048 s |

T_min = 32 s is the closest of the tested grid to the offline reconstruction at all three points, but this is only agreement with the legacy offline method. The volume errors show that offline reconstruction itself has substantial differences from the measured series at left and bottom. This is not evidence that the policy is a faithful traffic generator or a universally good threshold. The observation points belong to one dataset and may share duplicate packet observations; they are not independent datasets or an external validation set.

The full pipeline timing remains a single CI invocation per point, with no repeated warmups or medians. It is retained as a reproducibility baseline only. All three points show the same coarse-scale maximum delay of 2,048 seconds, reinforcing that scale-window interpolation—not just the run-retention threshold—dominates the worst-case wall-clock finality.


## Three-point rerun and process-level memory snapshot

A subsequent successful workflow run [44046240297](https://github.com/vsimko/trafficmsd/actions/runs/44046240297) repeated both the single-scale candidate-vector proxy and the full multiscale experiment for all three checksum-verified observation points. Its artifact contains six JSON reports. The multiscale process reports peak RSS (process-level, including runtime and test harness), so this is not an isolated allocation measurement for the optimizer.

For the physical-duration policy T_min = 32 seconds:

| Observation point | Offline full pipeline | Fixed-duration full pipeline | Interval optimizer only | Process peak RSS | Peak pending candidates |
|---|---:|---:|---:|---:|---:|
| right | 8.83 ms | 9.87 ms | 1.63 ms | 6,236 KiB | 15 |
| left | 11.54 ms | 12.66 ms | 1.66 ms | 6,392 KiB | 16 |
| bottom | 13.10 ms | 14.24 ms | 1.62 ms | 6,288 KiB | 16 |

These are single CI invocations, not repeated medians or a performance comparison. The fixed-duration full pipeline was slower in these three measurements; no throughput advantage is established. The single-scale proxy process peak RSS was 3,972–4,000 KiB, but it excludes the multiscale path and must not be compared as an optimizer-only allocation figure.

The 32-second multiscale results reproduce the earlier fidelity and finality findings: maximum finality remains 2,048 seconds on every point; p50/p95/p99 are 2/24/64 seconds (right), 2/20/64 seconds (left), and 2/16/64 seconds (bottom). The peak pending-candidate proxy is 15–16. The three observation points are locations within one dataset, not independent validation datasets. No threshold is selected and no production code/API/schema changes are justified by this rerun.

## Current research conclusion

The fixed-horizon prototype establishes an exact but potentially horizon-long-delay baseline for the local interval optimizer. The fixed-count prototype demonstrates a separate horizon-independent policy with bounded run-decision delay, but it intentionally changes semantics and has only initial synthetic tests. Neither prototype justifies selecting a production policy. The next acceptance gate is exhaustive adversarial and cross-horizon prefix-finality testing, followed by physical-duration mapping, trace-level fidelity, runtime/memory measurements, and downstream simulation impact. The legacy optimizer remains unchanged.


## Corrected three-point finality results (2026-11-17)

The corrected implementation is validated by the passing regression test in PR #111 and the successful [three-point benchmark workflow 44082355832](https://github.com/vsimko/trafficmsd/actions/runs/44082355832), artifact `streaming-fixed-threshold-westermo`. This rerun uses the same three independently processed, checksum-verified Westermo observation points. The earlier finality tables above are historical and **superseded**; their p50/p95/p99/max values, including the 2,048-second maximum, must not be cited as current.

For a positive scale with source window `w = 2^s`, candidate `k` represents the legacy center-left event timestamp `event_time = k*w + w/2 - 1`. Candidate `j` becomes available only after its complete source window, at `available_at = (j+1)*w - 1`. Therefore the source-sample delay is:

```
available_at - event_time = (j-k)*w + w/2
```

At scale zero, delay remains the candidate/source-sample index difference. The half-window term is essential; omitting it undercounts delay at every positive scale.

For the exploratory `T_min = 32 s` policy:

| Observation point | Samples | Finality p50 | p95 | p99 | Maximum | Reconstruction MAE | RMSE | Volume error |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| right | 5,445 | 3 s | 26 s | 96 s | 4,096 s | 710.80 | 2,149.99 | +4.51% |
| left | 5,441 | 3 s | 24 s | 96 s | 4,096 s | 5,787.78 | 11,821.52 | +19.22% |
| bottom | 5,443 | 3 s | 24 s | 96 s | 4,096 s | 11,123.98 | 14,864.53 | −43.18% |

Across the full tested duration grid (1, 2, 4, 8, 16, 32, 64 s), the corrected aggregate p50 is 1–4 s depending on policy, p95 is 24–46 s, p99 is 96 s, and the maximum is 4,096 s on each observation point. The maximum is caused by the coarsest scale (window 8,192 s) and its center-left event-time convention; it is a genuine coarse-scale worst case, not a typical delay. At `T_min=32 s`, the coarsest scale contributes 4,096 s maximum, while scale 0 contributes at most 1 s on right/bottom and 2 s on left. Intermediate scale delays are reported in the JSON artifact.

The timestamp correction does not change coefficient values, reconstructions, optimizer runtime, pending-state counts, or process RSS. Those metrics in the earlier sections remain historical measurements from their stated CI runs. At `T_min=32 s`, the latest rerun reports multiscale elapsed times of 9.900 ms (right), 12.563 ms (left), and 14.116 ms (bottom), with peak pending-candidate proxies of 15, 16, and 16 respectively. These are single CI invocations, not medians or a performance claim.

The result remains exploratory: the three points belong to one dataset and may share duplicate packet observations; they are not independent held-out datasets. `T_min=32 s` is closest to the legacy offline reconstruction on these points, but that is not measured-trace fidelity and does not select a threshold. No production optimizer, API, or model schema is changed.
