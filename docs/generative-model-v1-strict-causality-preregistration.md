# Strictly causal decomposition — preregistration

**Status:** protocol proposal and code-level analysis only; no implementation, empirical result, or model selection  
**Related:** issue [#92](https://github.com/vsimko/trafficmsd/issues/92), [emission semantics audit](generative-model-v1-emission-audit.md), [prefix-sensitivity audit](generative-model-v1-decomposition-prefix-sensitivity.md), [generative model v1 design](generative-model-v1-design.md)

## 1. Research question

Can the existing multiscale decomposition be made operationally causal at base tick t, such that every value made available by the algorithm at t depends only on observations through t, while retaining an explicit and reproducible relationship to the existing offline reconstruction?

This question is separate from whether a stochastic occurrence or mark model is adequate. No HMM, occurrence probability, mark distribution, or predictive score is part of this protocol.

## 2. Why the existing decomposition is not yet causal

The current implementation has two distinct sources of look-ahead:

1. For scale s>0, a coefficient is associated with a globally aligned window of length 2^s, but the legacy emission filter places it at the center-left index, offset 2^(s-1)-1. That index occurs before the window is complete. The value cannot be known at its nominal emission tick if it depends on the complete window.
2. Candidate values are passed through `optimize_intervals()`. The current prefix-sensitivity experiment reruns interval optimization over the entire prefix available at each origin. It therefore uses later observations within that prefix; it is not an online update rule. Recomputing it at every tick would not by itself make the algorithm causal.

The offline event timestamp, the time at which the algorithm can know the event value, and the time at which a downstream generator can emit it must therefore be represented separately.

## 3. Definitions and non-negotiable information boundary

For every output record, distinguish:

- **event_time:** the base-tick index assigned by the chosen representation;
- **available_at:** the earliest input tick after which the algorithm can determine the record without future observations;
- **decision_time:** the tick at which a downstream process is permitted to consume the record.

A record is strictly causal only if its value and existence are invariant under every extension of the input after `available_at`. At processing tick t, the implementation may read observations with indices at most t and must not read, hash, parse, or derive state from any later observation.

All comparisons use the same checksum-verified training source and global sample origin. Validation/test data are prohibited. No padding, synthetic observations, fabricated events, or future-derived initialization are allowed.

## 4. Two distinct candidate contracts (not selected)

### A. Preserve legacy event time; delay availability

Keep the current center-left event_time convention. A higher-scale event may become available only when its complete globally aligned window has arrived, at the window's right boundary. For a window [k*2^s, (k+1)*2^s), this means the event's available_at is no earlier than (k+1)*2^s-1.

This preserves the nominal legacy event index but introduces scale-dependent detection latency. It does **not** solve the future-context dependence of `optimize_intervals()`; it is only a timing contract for a window-local candidate rule.

### B. Use a causal event time

Do not publish a higher-scale event at the center-left index. Assign event_time no earlier than the tick at which the complete window is available. This changes the event-time representation and is not backward-compatible with the current v1 reconstruction contract.

This option still requires a genuinely online candidate/interval rule. Merely shifting the output timestamp while retaining a full-prefix optimizer is not causal.

Neither contract is preferred by this preregistration. Any change to the deterministic model format requires a separate compatibility decision and must not silently rewrite existing v1 artifacts.

## 5. Required algorithmic decomposition

Before implementation, document and test each operation in the legacy path:

1. aligned window construction and candidate selection;
2. candidate interpolation/optimization in `optimize_intervals()`;
3. source-value removal from the residual;
4. subtraction at the center-left emission location;
5. scale-zero residual optimization;
6. additive reconstruction and output availability.

For each operation, classify it as:
- finite-window causal (depends only on a completed, declared input window);
- online-state causal (depends only on current input and previously committed state);
- non-causal (depends on later windows or future prefix context);
- unresolved (implementation dependency not yet characterized).

Do not label the full pipeline causal if any operation remains non-causal or unresolved.

A proposed online replacement for interval optimization must specify its state, update order, commitment/finality rule, and behavior when later observations would have changed an earlier offline optimum. It must not revise already committed outputs using future data. If it intentionally revises provisional outputs, those outputs must be marked provisional and excluded from causal scoring until finalized.

## 6. Preregistered audit protocol

### 6.1 Prefix-extension invariance

For every tick t in the permitted training interval:

1. Run the candidate algorithm on the prefix [0,t+1).
2. Record all outputs marked final/available by t, including event_time, available_at, scale, amplitude, and canonical ordering.
3. Extend the input by one or more later training observations and rerun from the same initial state.
4. Require every previously final output to remain byte-identical. Any mutation, disappearance, or retroactive insertion with available_at <= t is a causality failure.
5. Repeat from a clean process and compare canonical report bytes.

The implementation must consume the prefix incrementally in the primary test; a batch function that is handed the complete trace and merely masks future indices is insufficient evidence.

### 6.2 Availability and latency

Report, by scale:
- number of finalized records;
- distribution of event_time to available_at delay;
- maximum and median delay;
- records whose event_time precedes available_at;
- provisional records, if any, separately from final records.

Do not hide latency by shifting timestamps in the report. Preserve both clocks.

### 6.3 Offline compatibility (descriptive only)

Compare the causal candidate with the frozen legacy offline reconstruction on the same training-only input. Report event-key overlap, additions/removals, matched amplitude differences, and aggregate reconstruction differences by scale.

This comparison is descriptive. Do not select whichever causal rule most closely matches offline output if doing so violates the information boundary. No arbitrary acceptance threshold is introduced here.

### 6.4 Adversarial fixtures

Tests must include:
- a higher-scale window whose decisive maximum occurs at its final input tick;
- a later window that would alter a global interval optimum in the legacy implementation;
- tied and repeated maxima under the exact legacy tie-breaking rule;
- a candidate that is interpolated rather than matched to a source sample;
- an incomplete trailing window;
- a prefix extension that would have changed a previously computed offline coefficient;
- scale-zero residual behavior after higher-scale updates;
- simultaneous scale-zero and higher-scale additive emissions.

Tests must assert both event_time and available_at. They must fail if a future observation changes an already finalized output.

## 7. Reproducibility and stop conditions

Pin the source archive, derived trace, permitted training range, code revision, runtime, dependencies, and configuration. Hash canonical outputs and the complete report. Run the audit independently twice and require byte-identical reports.

Stop without fitting a stochastic model if:
- the input boundary cannot be enforced;
- any final output depends on an observation after its available_at;
- the online optimizer revises committed outputs;
- the legacy tie-breaking or residual update order cannot be reproduced;
- the event-time/availability-time distinction is lost;
- independent executions are not deterministic.

Do not inspect validation/test data to resolve a failure. Do not introduce a new cutoff, regime count, covariance prior, pooling rule, Gamma/lognormal choice, or event-support threshold.

## 8. Scope exclusions

This work does not:
- claim that the current offline decomposition is causal;
- select between candidate contracts A and B;
- implement or select an online optimizer;
- fit an HMM, occurrence process, or mark model;
- lower the independent Gamma threshold of 30 positive events per (scale, regime);
- infer `inactive_by_policy` from finite-prefix constant occupancy;
- modify deterministic model v1 artifacts or schemas;
- add a production fitter, stochastic generator, or generated QPN condition.

## 9. Decision gate

The next implementation proposal must first demonstrate, with code-level analysis and adversarial fixtures, whether the current `optimize_intervals()` semantics admit a finite-memory online equivalent without retroactive revisions. If not, the proposal must describe a new explicitly versioned causal decomposition contract and its compatibility implications. Only after this decision should an implementation experiment be preregistered.

No empirical result is claimed by this document.


## 10. Code-level analysis of the legacy interval optimizer

**Audited implementations:** `R/decomposition.R::optimize_intervals()` and `crates/trafficmsd-core/src/decomposition.rs::optimize_intervals()` on the repository's `master` revision inspected for this analysis.

The implementation has three distinct dependencies:

1. **Prefix-length-dependent cutoff (non-causal under prefix extension).** Both implementations calculate `cutoff = n * cutoff_fraction`, where `n` is the complete candidate-vector length. When `cutoff > 1`, a positive run is retained only when its length is strictly greater than that cutoff. Therefore, extending a prefix can change the classification of an earlier run even when no value inside that run changes.
2. **Run termination (bounded but delayed).** The run-length pass needs the end of a positive run to know its full length. A streaming implementation could keep the run provisional until a zero terminates it, but cannot finalize it at its first positive sample.
3. **One-sample right context (bounded but delayed).** The interpolation pass reads the next element and updates the current element in place. A streaming implementation needs at least one subsequent candidate before it can finalize the preceding interpolated value. This local delay does not repair dependency 1.

### Exact prefix-extension counterexample

Use `cutoff_fraction = 0.004` and a vector of zeros with three consecutive values equal to 5 at indices 100–102 (zero-based).

- At length 749, `cutoff = 2.996`; the three-sample positive run has length 3, so it survives the run filter.
- At length 750, `cutoff = 3.000`; the same run has length 3 and is removed because the implementation retains only run lengths strictly greater than the cutoff.
- The added observation is after the run and does not alter any of its three source values. The interpolation pass preserves the run in the first case and cannot restore it after the run filter zeros it in the second.

Thus, the value/existence of earlier optimized coefficients is not invariant when the declared candidate-vector horizon itself changes. The 749/750 example is a direct counterexample for the optimizer helper, but those two lengths are not valid complete-input lengths for `decompose_traffic()`, which requires powers of two.

A counterexample using valid power-of-two horizons is also available. With the same three positive values at indices 100–102 and zero elsewhere:
- at candidate-vector length 512, `cutoff = 2.048`, so the run of length 3 survives;
- at candidate-vector length 1024, `cutoff = 4.096`, so the same run is removed.

Both lengths are valid for the scale-zero optimizer input of a complete power-of-two decomposition. The appended observations are all zero and occur after the unchanged run.

This disproves prefix-extension invariance when the horizon is allowed to change. It does **not** by itself rule out exact causal processing when a finite total horizon is declared and fixed before processing starts. In that contract, `n * cutoff_fraction` is a known constant at each scale; run classification can be delayed until run termination, and interpolation can be delayed until its right-hand post-filter candidate is known. The remaining question is whether the complete multiscale residual dependency graph can be executed incrementally with bounded state and correct legacy ordering. That requires an explicit algorithm and adversarial implementation tests; it is not established by this static audit.

### Operation classification

| Operation | Classification | Reason |
|---|---|---|
| Aligned window candidate selection | finite-window causal after window completion | Reads only the current complete window; center-left legacy event time still precedes availability |
| Run-length removal | non-causal under arbitrary prefix extension | Threshold depends on total vector length; run finality also waits for termination |
| Gap interpolation | finite look-ahead, delayed-finalizable | Reads the next candidate and mutates in place |
| Source residual removal | finite-window causal after candidate is final | Uses the selected candidate and current window |
| Center-left residual subtraction | finite-window causal after candidate is final | Deterministic update at the legacy emission index |
| Scale-zero optimization | non-causal under arbitrary prefix extension | Operates on the full residual and uses its length-dependent cutoff |
| Additive reconstruction | deterministic once coefficients are final | Does not itself remove upstream look-ahead |

**Decision-gate conclusion:** do not implement a streaming wrapper around the existing optimizer and call it causal. First choose, in a separately reviewed and versioned proposal, whether to freeze the cutoff horizon, replace the run policy, or define a new causal representation. Preserve the current offline v1 behavior and artifacts unchanged.


## 11. Candidate contract C — fixed, declared finite horizon (not selected)

The static counterexample distinguishes a changing-horizon stream from a finite recording whose total length is known before the first observation. The latter deserves a separate feasibility test before concluding that every causal implementation requires a new decomposition representation.

### Contract

- Declare the complete recording length $N$ before consuming observations. Require the existing power-of-two input length and the existing global origin.
- At scale $s$, the candidate-vector length is $n_s=N/2^s$, and the legacy cutoff remains exactly $c_s=n_s f$, where $f$ is the unchanged v1 cutoff fraction.
- The declared $N$, $f$, tie-breaking, window alignment, center-left subtraction, and operation order are immutable for the run. A stream whose declared horizon changes is a different run and cannot inherit previously finalized outputs.
- Consume observations incrementally. Do not read or precompute from future sample values. The horizon is metadata, not permission to inspect future observations.
- Keep legacy `event_time`; report the actual `available_at` separately. Never report a center-left timestamp as if the coefficient were known at that tick.

### Required incremental execution strategy to investigate

1. At each scale, generate each raw window candidate only after the residual for that complete window has been finalized by all larger scales.
2. Apply the exact legacy run filter using the fixed scale-specific cutoff. A positive run remains provisional until a zero candidate or end-of-horizon establishes its complete length.
3. Apply interpolation in the exact legacy left-to-right, in-place order. Candidate $i$ cannot be finalized until the post-run-filter value at $i+1$ is known; retain the previous interpolated value because the legacy pass reads the already-updated $i-1$.
4. Once candidate $i$ is final, apply the legacy source-sample removal and center-left residual subtraction for its own disjoint window, then make its child windows eligible for the next lower scale.
5. Preserve the distinction between raw candidates, post-run-filter candidates, interpolated candidates, and committed residual updates. Do not recompute an earlier candidate using a later residual state.
6. At end-of-horizon, flush all pending runs and right-context dependencies using the exact batch boundary behavior. Compare every coefficient and reconstructed sample against the legacy batch implementation.

This is a dependency-order proposal, not a proven algorithm. In particular, higher-scale candidate optimization can delay residual availability for lower-scale windows; the implementation must establish that the resulting cross-scale schedule is acyclic, deterministic, and incrementally executable. Worst-case availability may be as late as the end of the recording. Such an implementation could be strictly causal but offer little or no low-latency benefit.

### Required tests and acceptance gate

- Test both $N=512$ and $N=1024$ with the three-value run above, proving that the fixed-horizon cutoff is used consistently and that changing the declared horizon starts a distinct run.
- Test positive runs that terminate at the final candidate, at a zero, and immediately before an interpolated gap.
- Test the sequential in-place interpolation behavior, including cases where the previous value was itself interpolated.
- Test ties/repeated maxima and exact source-removal/emission ordering across adjacent windows and scales.
- For each input prefix, assert that every output already marked final is byte-identical as more observations arrive under the same declared $N$.
- At EOF, require byte-for-byte equality of the full coefficient matrix and reconstruction against the existing R and Rust batch implementations.
- Report per-scale finalization latency and peak pending-state size. Do not claim useful real-time latency unless measured.

### Decision

Contract C is the first implementation experiment to attempt because it may preserve the complete finite-record v1 coefficients without selecting a new cutoff or changing the schema. It is not yet approved for implementation or selected as the stochastic model's decomposition. If exact incremental execution fails, stop and propose a versioned causal representation or explicitly different online interval policy. Unknown-horizon streaming remains a separate problem and cannot silently change $N$ after outputs have been committed.
