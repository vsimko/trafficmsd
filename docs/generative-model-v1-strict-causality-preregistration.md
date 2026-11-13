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

Thus, the value/existence of earlier optimized coefficients is not invariant under prefix extension. The example applies directly to scale zero, whose optimizer input has one element per observed tick; the same length-dependent rule applies to higher-scale candidate vectors, with one element per complete aligned window.

This is a constructive failure of prefix-extension invariance for the legacy semantics, not merely a theoretical concern about a future global optimum. It rules out an exact, prefix-extension-invariant online implementation that simultaneously reproduces the current `n * cutoff_fraction` rule for every prefix. A causal redesign would need an explicitly fixed cutoff/reference horizon, a different online run policy, or a versioned representation. Each changes or constrains the legacy semantics and must be evaluated as a separate contract; none is selected here.

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
