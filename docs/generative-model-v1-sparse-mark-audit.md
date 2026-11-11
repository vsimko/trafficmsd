# Sparse scale/regime mark audit — generative model v1

**Date:** 11 November 2026  
**Status:** training-only exploratory research; proposed direction, not a validated fitter or generator  
**Related:** [generative model v1 design](generative-model-v1-design.md), [issue #92](https://github.com/vsimko/trafficmsd/issues/92)

## 1. Question

The independent Gamma specification requires at least 30 positive training marks in every (scale, regime) cell. The Westermo training prefix has sparse upper scales and no positive scale-zero residuals. Lowering the threshold would produce unstable cell-wise estimates; filling missing cells from validation/test or synthetic observations is prohibited.

This audit asks whether the problem can instead be represented with (a) an explicit inactive-channel state where no positive mark is observed, and (b) a shared, scale-aware mark distribution with shrinkage across regimes.

## 2. Frozen data and extraction

The analysis uses only the immutable Westermo right-observation-point trace, with the already declared training-only prefix [0,2048). Validation [3267,4356) and test [4356,5445) were not read. The extraction checksum is recorded in the design proposal. Decomposition uses cutoff.fraction=0.004 and the code-audited center-left emission mapping.

Scale zero is the residual row after higher-scale extraction and interval optimization; it is not a copy of the original signal. In this prefix, the emitted positive-mark counts are:

| Scale | Positive marks |
|---:|---:|
| 0 | 0 |
| 1 | 193 |
| 2 | 287 |
| 3 | 224 |
| 4 | 128 |
| 5 | 64 |
| 6 | 32 |
| 7 | 16 |
| 8 | 8 |
| 9 | 4 |
| 10 | 2 |
| 11 | 1 |

The scale-zero observation is therefore **no positive mark in this particular training prefix**. This does not prove that scale zero is universally structural-zero for other traces or training windows. A model artifact must distinguish an explicitly disabled/inactive channel from an unfit channel; it must not invent a Gamma distribution for an empty sample.

## 3. Why independent Gamma cells fail

For scales 6–11, the total positive sample sizes are 32, 16, 8, 4, 2, and 1, respectively. Splitting those observations among three regimes makes cell-wise Gamma maximum likelihood estimates unavailable or unreliable. The existing 30-event threshold is therefore not the root problem: the model has too many independent amplitude parameters for the available evidence.

Pooling all scales into one unconditioned Gamma distribution is also inadequate because the mark magnitude changes with scale. In the training prefix, mean positive marks increase from 306.4 at scale 1 to 3,421.8 at scale 6 and 21,510 at scale 11 (the latter is a single observation). Scale identity must remain in the mark model.

## 4. Candidate: hierarchical scale-and-regime Gamma marks

A training-only diagnostic evaluated the following conditional mark model for scales with positive observations:

M(s,j) ~ Gamma(k, theta(s,j))

E[M(s,j)] = exp(alpha + beta*(s-3) + delta(j))

- one shared Gamma shape k across scales and regimes;
- a log-linear scale effect beta, retaining the scale-dependent amplitude trend;
- regime offsets delta(j), with regime 0 as the reference and L2 shrinkage on the other offsets;
- one shared set of mark parameters, rather than an independently fitted Gamma distribution for every (scale, regime) cell.

The diagnostic used the A/original-signal HMM selected separately inside each expanding training fold. Training events were assigned by the fold's Viterbi path. Test marks were scored using the one-step predictive regime probabilities, mixing the Gamma densities across regimes; the same-tick observation was not used to assign its own regime. The mark score is conditional on a positive event and is reported separately from event-occurrence likelihood.

For the exploratory run, the regime-offset penalty was fixed at 1.0. Sensitivity runs at penalties 0 and 10 gave similar fold-level results; this is a stability check, not a principled prior calibration.

### Rolling-origin results

All four scored windows are inside [0,2048). The numbers below are mean conditional log predictive density per positive held-out mark (higher is better):

| Fold | Positive marks scored | One pooled Gamma | Scale-only Gamma | Scale + regime shrinkage |
|---:|---:|---:|---:|---:|
| 1 | 127 | -7.4522 | -7.0945 | **-6.9110** |
| 2 | 121 | -7.3853 | -7.0263 | **-6.8525** |
| 3 | 115 | -7.2938 | -6.8325 | **-6.7346** |
| 4 | 109 | -8.6077 | -9.8066 | **-8.3054** |
| Weighted mean | 472 | -7.6633 | -7.6395 | **-7.1751** |

The scale-plus-regime model improves over the pooled-Gamma baseline in all four folds (0.30–0.56 nats per positive mark). The fitted scale slope was stable across folds (approximately 0.51–0.52 in log-mean units per scale step). The scale-only model deteriorated in fold 4; adding a shrinkage-controlled regime effect mitigated that deterioration.

These are exploratory, within-training-prefix comparisons. They do not establish distributional adequacy, upper-tail calibration, generalization to other traces, or a winner among regime-alignment candidates A/B/C. In particular, the upper scales remain supported by very few distinct marks; pooling reduces parameter variance but cannot create information that is absent from the data.

## 5. Scale-zero policy

Scale zero must not receive a fitted positive Gamma distribution when its training mark sample is empty.

The model contract should distinguish at least:
- **active:** positive training marks exist and are covered by the shared mark model;
- **inactive-by-policy:** the caller explicitly accepts that this scale emits no events in this fitted model; no mark parameters are present;
- **unfit:** the scale has no mark support but has not been explicitly disabled; fitting/generation must fail rather than silently invent amplitudes.

For the current Westermo prefix, scale zero is **unfit** unless an explicit inactive-by-policy decision is made. The zero count alone is not proof of a structural zero process. This is intentionally stricter than silently assigning event probability zero.

## 6. Recommended next model revision

1. Keep the scale-specific Bernoulli/Markov occurrence process separate from the shared conditional mark model.
2. Replace per-(scale, regime) Gamma parameters with the shared-shape, scale-dependent mean model above; retain regime offsets only with declared shrinkage.
3. Make inactive-by-policy and unfit states explicit in the model schema. An inactive scale has no amplitude distribution and emits zero by contract; an unfit scale blocks model acceptance.
4. Compare the shared-shape Gamma against a predeclared alternative (for example, shared-shape lognormal or a more flexible scale trend) using the same rolling-origin training folds. Do not choose the family from validation/test results.
5. Add per-scale and per-regime calibration diagnostics, including upper-tail and quantile coverage. The pooled log score alone is insufficient.
6. Re-run with an independently validated/canonical HMM implementation before using these results to select regime alignment or implement a production fitter.

The draft schema on this branch has now been revised to carry one shared Gamma mark model and per-scale active/inactive-by-policy/unfit status. The semantic validator still needs to enforce regime-offset alignment, supported-scale consistency, zero occurrence probabilities for inactive scales, and rejection of unfit scales. Do not serialize the exploratory parameter estimates as a production model.

## 7. Decision

**Recommended research direction:** explicit inactive/unfit scale status plus a shared, scale-aware, regime-shrunk mark distribution. Retain the 30-event rule only for the independent-cell baseline; do not lower it. Do not yet claim a production solution, select A/B/C, or proceed to generator/QPN implementation.


## 8. Gamma versus lognormal follow-up (11 November 2026)

A separate reproducible comparison is recorded in
[experiments/generative-model-v1-sparse-marks](../experiments/generative-model-v1-sparse-marks/README.md).
It uses the checksum-verified trace artifact and reads only the frozen
training prefix `[0,2048)`. The positive mark counts remain
`0,193,287,224,128,64,32,16,8,4,2,1`; no artificial events or validation/test
observations are used.

Both families were evaluated on the same four expanding folds
`[0,1024)->[1024,1280)`, `[0,1280)->[1280,1536)`,
`[0,1536)->[1536,1792)`, and `[0,1792)->[1792,2048)`. They share the
fold-specific exploratory diagonal-Gaussian HMM, one-step predictive regime
weights, log-linear scale mean, and L2 penalty 1.0 on regime offsets. The
lognormal location is `log(mean)-sigma^2/2`, so its scale/regime trend
parameterizes the same arithmetic mean as the Gamma model.

| Fold | Marks scored | Selected K | Shared Gamma | Shared lognormal |
|---:|---:|---:|---:|---:|
| 1 | 127 | 2 | -6.9644 | **-6.7790** |
| 2 | 121 | 3 | -6.8537 | **-6.6605** |
| 3 | 115 | 2 | -6.7257 | **-6.5340** |
| 4 | 109 | 2 | **-8.1052** | -8.3938 |
| Weighted mean | 472 | — | -7.1413 | **-7.0618** |

Lognormal improves the pooled conditional mark score by 0.0795 nats/mark and
wins three folds; Gamma wins fold 4. This is not stable enough to select a
family. The decomposition is frozen once on the complete 2,048-sample training prefix; interval optimization can therefore use neighboring-window context beyond an individual fold origin. The rolling origins apply to HMM and mark-parameter fitting, not to a strict end-to-end causal decomposition replay. The independently reimplemented HMM selected K=2,3,2,2, differing
from the earlier A/B/C exploratory run. The result is therefore a separate
diagnostic, not a canonical HMM reproduction.

Pooled predictive quantile coverage was:

| Quantile | Gamma | Lognormal | Nominal |
|---:|---:|---:|---:|
| 90% | 93.43% | 91.74% | 90% |
| 95% | 94.49% | 93.43% | 95% |
| 99% | 97.03% | 95.97% | 99% |

Fold 4 q99 coverage fell to 87.16% for Gamma and 82.57% for lognormal. The
pooled coverage therefore hides temporal tail-calibration failure. These
scores are conditional on positive events; they omit event-occurrence
likelihood and do not establish joint generative fidelity.

**Decision remains open:** do not select Gamma or lognormal, do not select an
A/B/C regime, and do not lower the 30-event independent-cell Gamma threshold.
Repeat with a canonical independently validated HMM and stronger upper-tail
diagnostics before any production fitter decision.
