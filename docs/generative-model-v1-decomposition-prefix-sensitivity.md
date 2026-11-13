# Decomposition prefix-sensitivity audit — preregistration

**Status:** preregistration only; no experiment run and no result inspected  
**Related:** issue [#92](https://github.com/vsimko/trafficmsd/issues/92), [generative model v1 design](generative-model-v1-design.md), [pinned HMM research](../experiments/generative-model-v1-hmm-research/README.md)  
**Reference dataset:** checksum-verified Westermo v1.0 right-observation-point trace

## 1. Research question

The existing A/B/C HMM comparison computes the deterministic decomposition once over the complete training prefix [0,2048), then uses rolling-origin folds for HMM and occurrence fitting. That is a conditional training-only diagnostic, not a fold-local decomposition replay: interval optimization and the multiscale extraction can use context later than an individual fold's fit origin.

This audit asks a narrower question:

> How much do already-observed event locations and amplitudes change when the same deterministic decomposition is recomputed using only the training prefix available at each rolling origin, rather than using the frozen [0,2048) decomposition?

This is a decomposition-stability audit. It is **not** a causal real-time generator, a new model-fitting experiment, or a selection procedure.

## 2. Frozen inputs and boundaries

- Dataset: Westermo v1.0 right-observation-point, one-second captured-frame-byte trace.
- Source archive SHA-256: `8a1f49eb49747f3046eda8063b104834f41f0df30d4fc2357aea5609826a4445`.
- Derived trace SHA-256: `6429e3d6a84efdc13edc14e44b2de2e6cbd31452e1958116c271745069b3fbb7`.
- Declared split: train [0,3267), validation [3267,4356), test [4356,5445).
- The only permitted input is the header and checksum-verified training prefix [0,2048), SHA-256 `017e38220cc7590653c630455732c90c2c4c80e148a105e1dabe02e133ebe032`.
- Validation and test rows must not be read, parsed, hashed, or used for any decision.
- No padding, synthetic events, artificial zeros, or fabricated marks.

Use the existing four rolling origins without modification:

| Fold | Available prefix | Prefix-local decomposition input |
|---:|---:|---:|
| 1 | [0,1024) | exactly [0,1024) |
| 2 | [0,1280) | exactly [0,1280) |
| 3 | [0,1536) | exactly [0,1536) |
| 4 | [0,1792) | exactly [0,1792) |

The frozen reference is the existing decomposition computed on [0,2048). No fold endpoint, cutoff, preprocessing, source interval, or checksum may be changed after inspecting results.

## 3. Deterministic extraction protocol

Use the audited extraction semantics already documented in the generative-model design:

- global sample origin remains zero;
- cutoff fraction remains exactly 0.004;
- higher-scale windows remain globally aligned, with width 2^s and emission offset 2^(s-1)-1;
- scale-zero residual is retained at every sample;
- simultaneous scale contributions remain additive;
- the residual-removal and emission subtraction order remains unchanged;
- interval optimization is run only on the values available to that prefix.

A prefix-local run must not treat an incomplete trailing higher-scale window as a complete observation. For each scale s>0, process only windows wholly contained in the prefix; leave any trailing incomplete window unprocessed at that scale so its samples remain available to lower-scale extraction. Do not pad the window. Scale zero is evaluated over the complete available prefix using the existing scale-zero interval optimization.

This complete-window rule is required for a well-defined prefix comparison. The existing helper's length-derived maximum scale and partial final slices must not silently determine the scale set or create a partial high-scale event. Add explicit tests for:
1. a prefix shorter than one window at scale s produces no scale-s event;
2. a prefix ending exactly at a window boundary processes that window once;
3. a prefix with one complete and one incomplete window processes only the complete window;
4. emission ticks retain the globally aligned center-left offset;
5. scale-zero values remain present and no samples are padded.

For the [0,2048) reference, the complete-window implementation must reproduce the already pinned per-scale positive-mark counts `0,193,287,224,128,64,32,16,8,4,2,1` for scales 0–11. If it does not, stop: report the discrepancy and do not reinterpret or replace the existing experiment. The first successful complete-window implementation reproduced these counts and established the canonical reference event-map SHA-256 as `29a500dbcb42163c37260fafa3f67f421995ad72d3eeee5346d1afeab06d0ea6`. This hash is now pinned in the audit script. The script independently recomputes the reference event map twice and requires byte identity and the pinned hash before any fold comparison.

## 4. Comparison protocol

For each fold origin o, compare the prefix-local decomposition on [0,o) with the frozen reference decomposition on [0,2048), restricted to the common observable prefix [0,o).

Use event key (scale, emission_tick). Compare only complete globally aligned higher-scale windows wholly contained in [0,o); scale-zero comparison covers all ticks in [0,o).

Report, without selecting thresholds:

- eligible opportunities and positive-event counts by scale;
- exact event-key agreement count and Jaccard overlap by scale;
- additions and removals by scale;
- for keys present in both decompositions, absolute mark difference, relative mark difference, and log mark ratio summaries (count, median, mean, 5th/95th percentiles, maximum absolute difference);
- per-scale sum of positive marks and signed difference in that sum;
- an overall count-weighted summary, while retaining all per-scale results;
- deterministic hashes of the canonical event maps and complete JSON report.

Do not compare only the event counts: event-location agreement and mark-amplitude stability are separate outcomes. Do not use an arbitrary pass/fail threshold, choose a cutoff, or rank a decomposition method. All summaries are descriptive.

## 5. Interpretation boundaries

The prefix-local decomposition still uses interval optimization over the complete prefix available at its origin. It therefore measures sensitivity to adding later prefix context; it does **not** establish strict per-tick causality. In particular, the center-left emission convention can assign an event to a tick before all samples in its aligned window have been observed. This audit does not change that emission contract or claim that the emitted mark is available at its emission tick in a real-time system.

The audit does not fit an HMM, estimate occurrence probabilities, fit Gamma/lognormal marks, compare model scores, select a representation, select a regime count, tune a covariance prior, or alter any support threshold. It must not be used to promote the scale-aware representation or a mark family.

If the decomposition differs materially across origins, the result is evidence that the existing rolling-origin model scores are conditional on a frozen offline decomposition. It is not permission to choose whichever decomposition gives the best score. Any subsequent model experiment requires a separate preregistration that fixes how training features and held-out target labels are constructed without mixing incompatible event definitions.

## 6. Reproducibility and stop conditions

- Pin Python and numerical dependencies exactly as in the existing experiment environment.
- Record the pinned training-prefix, script, configuration, reference/fold event-map, and report SHA-256 values, together with Python and NumPy versions.
- Run the audit twice independently; canonical event-map and report bytes must match exactly.
- Preserve the original A/B/C HMM report and its checksum unchanged.
- Stop if the training-prefix checksum, pinned event counts, complete-window conformance, or independent-run determinism check fails.
- Do not inspect validation/test data to resolve any discrepancy.
- Keep this work under `experiments/` and `docs/`; do not add a production fitter, stochastic generator, schema parameters, generated QPN condition, or QPN result.

## 7. Preregistration status

The protocol was committed before the audit implementation and result generation. The
implementation is confined to
`experiments/generative-model-v1-decomposition-sensitivity/`. Its seven
complete-window and input-boundary regression tests pass. The dedicated
workflow ran the audit twice against the immutable artifact and verified
byte-identical JSON reports and SHA sidecars.

## 8. Execution record

- Workflow run: [43384748360](https://github.com/vsimko/trafficmsd/actions/runs/43384748360).
- Environment: Python 3.12.16, NumPy 1.26.4.
- Reference event-map SHA-256:
  `29a500dbcb42163c37260fafa3f67f421995ad72d3eeee5346d1afeab06d0ea6`.
- Report SHA-256:
  `181963574f889932194ed7ec6e9a37bd48b98a0337160894fc51026e5c0d82a2`.
- Script SHA-256:
  `c8d8754dd0ccc034aab1bd575c9d740f9c02eafd671d22268039892b74f85f20`.
- Pinned reference counts were reproduced exactly:
  `0,193,287,224,128,64,32,16,8,4,2,1`.
- Results and interpretation are recorded in
  [the experiment results summary](../experiments/generative-model-v1-decomposition-sensitivity/results.md);
  the full JSON report and checksum sidecar are retained in workflow artifact
  `14017784059`.

The observed prefix sensitivity is concentrated at scales 0–2, with scale 3
high but imperfect key overlap and identical event keys at scales 4–10 for
these origins. This confirms that the previous rolling-origin scores are
conditional on the frozen offline decomposition. It does not justify choosing
a different decomposition or fitting any model. No validation/test rows were
read, and no production fitter, stochastic generator, or QPN condition was
added.
