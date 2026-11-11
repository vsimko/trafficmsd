# Sparse mark family comparison (training-only)

This experiment compares a shared-shape, log-linear scale-mean Gamma mark model
with a lognormal alternative on the same four expanding rolling-origin folds
inside the frozen Westermo training prefix `[0,2048)`.

## Reproduction

Requirements: Python 3.10+, NumPy, SciPy. Obtain the checksum-verified
`westermo-model-extraction-and-qpn-v1` workflow artifact (run 43079950800), extract
`westermo-model-extraction-v1/trace.csv`, then run:

```sh
python experiments/generative-model-v1-sparse-marks/compare_gamma_lognormal.py \
  artifacts/westermo-model-extraction-v1/trace.csv \
  --output experiments/generative-model-v1-sparse-marks/results.json
```

The script reads exactly the CSV header and first 2,048 rows using an unbuffered,
line-bounded reader, then verifies the training-prefix SHA-256
`017e38220cc7590653c630455732c90c2c4c80e148a105e1dabe02e133ebe032`.
It does not consume or parse validation `[3267,4356)` or test `[4356,5445)`
rows, pad the signal, add synthetic events, or fabricate marks.

The decomposition is computed once on the full frozen training prefix, so these are rolling-origin folds for HMM/mark fitting, not strict end-to-end causal decomposition replays. Neighbor-window interval optimization can use context elsewhere inside the same training prefix; treat scores as conditional diagnostics, not unbiased real-time forecast estimates. The decomposition port is checked against the frozen positive mark counts
`0,193,287,224,128,64,32,16,8,4,2,1`. Each fold fits a diagonal-Gaussian HMM
to the observations available before its origin, selects K=1,2,3 by BIC, fits
both mark families with the same scale trend and L2 regime-offset penalty
(`lambda=1`), and scores the same positive marks using one-step predictive
regime probabilities. The same-tick observation is applied only after scoring
that tick's mark.

The lognormal uses the same log arithmetic-mean trend as Gamma:
`log E[M|s,j] = alpha + beta*(s-3) + delta[j]`; its log-location is shifted
by `-sigma^2/2`. This makes the two families comparable on their conditional
mean scale.

## Results

See [results.md](results.md) for the committed result summary; rerunning the command writes a fresh machine-readable JSON report. The weighted conditional mark log score over
472 held-out-within-training marks was -7.1413 for Gamma and -7.0618 for
lognormal (lognormal advantage 0.0794 nats/mark). Lognormal was better in folds
1–3; Gamma was better in fold 4. The selected HMM state counts were 2,3,2,2,
which differ from the earlier A/B/C exploratory HMM run. This is an independent
diagnostic, not a reproduction of a canonical HMM implementation.

Predictive quantile coverage pooled across folds was:

| Quantile | Gamma | Lognormal | Nominal |
|---:|---:|---:|---:|
| 90% | 93.43% | 91.74% | 90% |
| 95% | 94.49% | 93.43% | 95% |
| 99% | 97.03% | 95.97% | 99% |

Fold 4 coverage at the 99% quantile fell to 87.16% (Gamma) and 82.57%
(lognormal). These diagnostics do not support a validated upper-tail claim.
Scores are conditional on positive events, not joint occurrence-plus-mark
scores. No A/B/C regime or mark family is selected; no production fitter,
generator, or QPN condition is implemented.


## Occurrence-Markov support audit

The companion [occurrence-Markov audit](../../docs/generative-model-v1-occurrence-markov-audit.md)
reports training-only event and transition support for each scale/regime. In the
frozen prefix, scale 0 is all-zero and scales 4–11 are all-one at their aligned
opportunities; only scales 1–3 have both event states. Several regime cells in
scales 1–3 also have very small transition denominators.

Reproduce it with:

```sh
python experiments/generative-model-v1-sparse-marks/audit_occurrence_markov.py \
  artifacts/westermo-model-extraction-v1/trace.csv \
  --output experiments/generative-model-v1-sparse-marks/occurrence-markov-results.json
```

The audit reports (n_{00}, n_{01}, n_{10}, n_{11}), plus the separate
previous-zero and previous-one trial counts for (p_{01}) and (p_{11}). It
does not fit occurrence probabilities or choose a regime. The current
`mark_status` field is not sufficient to express occurrence identifiability;
a separate occurrence status/support contract is required before any fitter.
