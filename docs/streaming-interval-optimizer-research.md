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
