# Sparse scale/regime occurrence-Markov support audit — 11 November 2026

**Status:** descriptive training-only diagnostic; not model selection or a fitted occurrence model  
**Input:** checksum-verified Westermo right-observation-point trace  
**Frozen prefix:** `[0,2048)` only  
**Training-prefix SHA-256:** `017e38220cc7590653c630455732c90c2c4c80e148a105e1dabe02e133ebe032`

## Protocol

The audit reads the CSV header and exactly the first 2,048 data rows, verifies the pinned prefix digest, and does not read validation `[3267,4356)` or test `[4356,5445)` rows. It reuses the audited event extraction and the same independent exploratory diagonal-Gaussian HMM implementation as the adjacent mark-family diagnostic. For each expanding fit range `[0,1024)`, `[0,1280)`, `[0,1536)`, and `[0,1792)`, K=1,2,3 are fit; the minimum-BIC path is used only to tabulate descriptive transition support.

For each scale, opportunities are the deterministic aligned emission ticks. `n00`, `n01`, `n10`, and `n11` count adjacent opportunity pairs `(E_previous,E_current)`, grouped by the HMM regime at the current emission tick. The first opportunity has no previous event state and is excluded. Regime labels are local to each fold.

## Full-prefix event occupancy

| Scale | Opportunities | Positive events | Zero events | Occupancy | Variation |
|---:|---:|---:|---:|---:|---|
| 0 | 2048 | 0 | 2048 | 0.0% | all_zero |
| 1 | 1024 | 193 | 831 | 18.8% | variable |
| 2 | 512 | 287 | 225 | 56.1% | variable |
| 3 | 256 | 224 | 32 | 87.5% | variable |
| 4 | 128 | 128 | 0 | 100.0% | all_one |
| 5 | 64 | 64 | 0 | 100.0% | all_one |
| 6 | 32 | 32 | 0 | 100.0% | all_one |
| 7 | 16 | 16 | 0 | 100.0% | all_one |
| 8 | 8 | 8 | 0 | 100.0% | all_one |
| 9 | 4 | 4 | 0 | 100.0% | all_one |
| 10 | 2 | 2 | 0 | 100.0% | all_one |
| 11 | 1 | 1 | 0 | 100.0% | all_one |

Scale 0 is all-zero in this prefix. Scales 4–11 are all-one at their opportunity ticks. Only scales 1–3 contain both zero and positive opportunities. This is a property of the current decomposition/event representation on this prefix, not proof that scale 0 is universally inactive or that scales 4–11 are universally always-on.

## Regime-conditioned transition support

Exploratory BIC-selected state counts by fit range were 2, 3, 2, and 2. These are descriptive assignments from the unvalidated local HMM, not a regime choice. The detailed table includes scale 0–3; the all-one scales 4–11 are summarized above.

`n00/n01/n10/n11` are the transition counts. `previous_zero_support = n00+n01` is the number of trials informing p01; `previous_one_support = n10+n11` informs p11.

| Fold | K | Scale | Regime | Opportunities | Positive | n00/n01/n10/n11 |
|---:|---:|---:|---:|---:|---:|---:|
| 1 | 2 | 0 | 0 | 950 | 0 | 949/0/0/0 |
| 1 | 2 | 0 | 1 | 74 | 0 | 74/0/0/0 |
| 1 | 2 | 1 | 0 | 475 | 101 | 359/13/14/88 |
| 1 | 2 | 1 | 1 | 37 | 7 | 30/1/0/6 |
| 1 | 2 | 2 | 0 | 242 | 118 | 108/14/15/104 |
| 1 | 2 | 2 | 1 | 14 | 12 | 1/2/1/10 |
| 1 | 2 | 3 | 0 | 108 | 102 | 3/3/2/99 |
| 1 | 2 | 3 | 1 | 20 | 19 | 1/0/0/19 |
| 2 | 3 | 0 | 0 | 1139 | 0 | 1138/0/0/0 |
| 2 | 3 | 0 | 1 | 122 | 0 | 122/0/0/0 |
| 2 | 3 | 0 | 2 | 19 | 0 | 19/0/0/0 |
| 2 | 3 | 1 | 0 | 567 | 113 | 437/15/16/98 |
| 2 | 3 | 1 | 1 | 63 | 17 | 44/3/2/14 |
| 2 | 3 | 1 | 2 | 10 | 0 | 10/0/0/0 |
| 2 | 3 | 2 | 0 | 285 | 146 | 118/20/20/126 |
| 2 | 3 | 2 | 1 | 31 | 24 | 5/3/2/21 |
| 2 | 3 | 2 | 2 | 4 | 4 | 0/0/0/4 |
| 2 | 3 | 3 | 0 | 132 | 123 | 5/4/3/119 |
| 2 | 3 | 3 | 1 | 25 | 24 | 1/0/0/24 |
| 2 | 3 | 3 | 2 | 3 | 3 | 0/0/0/3 |
| 3 | 2 | 0 | 0 | 1422 | 0 | 1421/0/0/0 |
| 3 | 2 | 0 | 1 | 114 | 0 | 114/0/0/0 |
| 3 | 2 | 1 | 0 | 709 | 137 | 553/18/18/119 |
| 3 | 2 | 1 | 1 | 59 | 11 | 46/2/2/9 |
| 3 | 2 | 2 | 0 | 361 | 192 | 141/26/27/166 |
| 3 | 2 | 2 | 1 | 23 | 21 | 1/2/1/19 |
| 3 | 2 | 3 | 0 | 164 | 155 | 5/4/3/151 |
| 3 | 2 | 3 | 1 | 28 | 27 | 1/0/0/27 |
| 4 | 2 | 0 | 0 | 1665 | 0 | 1664/0/0/0 |
| 4 | 2 | 0 | 1 | 127 | 0 | 127/0/0/0 |
| 4 | 2 | 1 | 0 | 831 | 157 | 652/21/21/136 |
| 4 | 2 | 1 | 1 | 65 | 13 | 50/2/2/11 |
| 4 | 2 | 2 | 0 | 423 | 228 | 164/30/30/198 |
| 4 | 2 | 2 | 1 | 25 | 23 | 1/2/1/21 |
| 4 | 2 | 3 | 0 | 191 | 175 | 10/6/5/169 |
| 4 | 2 | 3 | 1 | 33 | 30 | 3/0/0/30 |

## Interpretation

1. **Independent scale/regime Markov transitions are not supported across the scale range.** At scale 0 there are no positive events, so p11 has no previous-one trials. At scales 4–11 there are no zero events, so p01 has no previous-zero trials; most upper-scale cells also have very few total opportunities. A Beta(1,1) smoothing prior does not create data support: with zero trials, the corresponding posterior is just the prior.
2. **Scales 1–3 have some transition variation, but regime cells remain sparse.** Examples include fold 2 / scale 1 / regime 2 (10 opportunities, all zero), fold 2 / scale 2 / regime 2 (4 opportunities, all one), and fold 2 / scale 3 / regime 2 (3 opportunities, all one). Other scale-3 cells have very few zero-state transitions.
3. **The >=30 positive-event rule remains specific to independent Gamma estimates per (scale, regime).** This audit does not lower or repurpose that threshold. Markov transition support is a separate issue and must be reported using previous-state trial counts for p01 and p11.
4. **Do not convert observed degeneracy into policy.** A constant training-prefix sequence does not justify `inactive_by_policy`, p01=0, or p11=1 as a general process claim. Keep the occurrence process unresolved (`unfit`) unless a separate explicit policy decision is made.

## Research direction — not implemented

- Separate occurrence identifiability/status from mark-family support. A scale can have mark evidence for a shared mark model while its two-state occurrence chain remains unfit (notably scales 4–11 here).
- For each `(scale, regime)`, retain raw transition counts and separately track p01 trial support (`n00+n01`) and p11 trial support (`n10+n11`). Do not label a parameter fitted when its denominator is zero.
- Evaluate a predeclared hierarchical/logistic partial-pooling model for p01 and p11 only after a canonical, independently validated regime implementation is available. Pooling can stabilize sparse cells but cannot turn an empty denominator into empirical evidence; unsupported cells must remain visibly prior-dominated/unfit.
- Revisit whether the event representation should treat high-scale opportunities as always-present marks rather than Bernoulli events. This is a research question, not a selected redesign.
- Preserve the independent Gamma minimum of 30 positive events per scale/regime. Do not add events, lower thresholds, read validation/test rows, select A/B/C, or select Gamma/lognormal from this diagnostic.

## Reproduction

Download the checksum-verified `westermo-model-extraction-and-qpn-v1` artifact and extract `westermo-model-extraction-v1/trace.csv`, then run:

```sh
python experiments/generative-model-v1-sparse-marks/audit_occurrence_markov.py \
  artifacts/westermo-model-extraction-v1/trace.csv \
  --output experiments/generative-model-v1-sparse-marks/occurrence-markov-results.json
```

The script writes a machine-readable report to the requested output path. It reports support counts only and does not estimate or serialize p01/p11.

## Limitations

- The HMM is an independent exploratory diagonal-Gaussian reimplementation, not a canonical validated fitter.
- Viterbi states are used only for descriptive training transition counts, not predictive scoring or regime selection.
- Decomposition is frozen once on the full 2,048-sample training prefix; rolling fit ranges are not strict end-to-end causal decomposition replays.
- No occurrence probabilities, shrinkage strength, regime, or mark family is selected.
