# Gamma vs lognormal mark comparison — 11 November 2026

**Status:** exploratory, training-only; not model selection or validation  
**Input:** checksum-verified Westermo right-observation-point trace  
**Frozen prefix:** `[0,2048)`; validation/test rows not consumed or parsed  
**Training-prefix SHA-256:** `017e38220cc7590653c630455732c90c2c4c80e148a105e1dabe02e133ebe032`  
**Positive marks by scale 0–11:** 0, 193, 287, 224, 128, 64, 32, 16, 8, 4, 2, 1

## Protocol

Four expanding rolling-origin folds use fit/scoring ranges
`[0,1024) -> [1024,1280)`, `[0,1280) -> [1280,1536)`,
`[0,1536) -> [1536,1792)`, and `[0,1792) -> [1792,2048)`.

Both mark families share the same frozen event targets, fold-specific
diagonal-Gaussian HMM, BIC selection over K=1,2,3, predictive regime priors,
log-linear scale mean, and L2 regime-offset penalty 1.0. Gamma uses one shared
shape. Lognormal uses the same log arithmetic-mean trend, with location
`log(mean)-sigma^2/2`. Optimizers are SciPy L-BFGS-B; both fits converged in
all folds.

The same-tick observation is used only after its mark is scored. The
decomposition is computed once on the frozen 2,048-row prefix; each fold's HMM
and mark parameters use only earlier events. This is an independent exploratory
HMM implementation, not a canonical validated fitter.

## Conditional mark log scores

Higher is better; units are mean log predictive density per positive held-out
mark.

| Fold | Held-out marks | Selected K | Gamma | Lognormal |
|---:|---:|---:|---:|---:|
| 1 | 127 | 2 | -6.9644 | **-6.7790** |
| 2 | 121 | 3 | -6.8537 | **-6.6605** |
| 3 | 115 | 2 | -6.7257 | **-6.5340** |
| 4 | 109 | 2 | **-8.1052** | -8.3938 |
| Weighted | 472 | — | -7.1413 | **-7.0618** |

Lognormal's weighted advantage is 0.0794 nats per mark, but Gamma wins fold 4.
The result is not stable enough to select a family.

## Predictive quantile coverage

| Nominal quantile | Gamma pooled | Lognormal pooled |
|---:|---:|---:|
| 90% | 93.43% | 91.74% |
| 95% | 94.49% | 93.43% |
| 99% | 97.03% | 95.97% |

Fold 4 coverage was 81.65% / 79.82% at q90, 83.49% / 80.73% at q95,
and 87.16% / 82.57% at q99 (Gamma / lognormal). Both families under-cover
the upper tail in that fold. Pooled coverage hides this temporal instability.

## Interpretation and guardrails

- Scale 0 has zero positive marks in this training prefix; it remains
  `unfit` unless explicitly set `inactive_by_policy`. Zero observations do
  not prove a universal structural zero.
- Scales 6–11 have 32,16,8,4,2,1 marks. The independent Gamma threshold of
  30 positive events per scale/regime is unchanged and is not relaxed.
- No artificial events and no validation/test data were used.
- Conditional mark scores omit event-occurrence likelihood and do not prove
  joint generative fidelity.
- The independent HMM reimplementation selected K=2,3,2,2; its difference
  from the earlier A/B/C diagnostic is a reproducibility limitation.
- Sparse upper scales do not support reliable extreme-tail calibration.
- No A/B/C candidate or mark family is selected. No production fitter,
  generator, or QPN generated condition is implemented.

Reproduction code and instructions are in this directory. Results are
exploratory evidence only; repeat with a canonical independently validated HMM
before any production decision.
