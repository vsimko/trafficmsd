# Fixed-horizon streaming interval optimizer — research prototype

**Status:** test-only prototype; not a production API or selected model contract.  
**Issue:** #107  
**Reference:** `decomposition::optimize_intervals()` remains unchanged and is the offline oracle.

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

The accompanying tests cover threshold values L = 1, 2, 3, and 4, exact-threshold retention, shorter-run removal, and equality of already-emitted records with the completed fixed-threshold stream. This is an initial synthetic correctness check, not a full exhaustive proof or a performance/fidelity evaluation. In particular, no physical-duration adapter, trace-level calibration, multiscale integration, RSS measurement, or downstream simulation comparison is included yet.

## Current research conclusion

The fixed-horizon prototype establishes an exact but potentially horizon-long-delay baseline for the local interval optimizer. The fixed-count prototype demonstrates a separate horizon-independent policy with bounded run-decision delay, but it intentionally changes semantics and has only initial synthetic tests. Neither prototype justifies selecting a production policy. The next acceptance gate is exhaustive adversarial and cross-horizon prefix-finality testing, followed by physical-duration mapping, trace-level fidelity, runtime/memory measurements, and downstream simulation impact. The legacy optimizer remains unchanged.
