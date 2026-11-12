# Independent HMM validation for A/B/C regime alignment

**Status:** research-only training-prefix diagnostic. No TrafficMSD fitter or generator is implemented.

## Purpose

Re-run A/B/C using the independently maintained and exactly pinned hmmlearn 0.3.3 Gaussian HMM implementation instead of the exploratory hand-written EM routine.

- A: univariate log1p(X_t) from the original training signal.
- B: multivariate diagonal-Gaussian HMM over log1p(C_0,t), ..., log1p(C_S,t) emitted scale channels.
- C: univariate log1p(sum_s C_s,t) aggregate emitted signal.

All representations use the same frozen decomposition, event opportunities, positive-mark targets, four rolling-origin folds, fixed K values 1/2/3, and three deterministic initialization seeds. Every K is reported; no K is chosen. BIC is recorded only as a within-representation diagnostic.

## Immutable input and leakage boundary

Obtain artifact westermo-model-extraction-and-qpn-v1 from workflow run 43079950800, then use artifacts/westermo-model-extraction-v1/trace.csv.

The shared prefix reader consumes only the CSV header and first 2,048 rows and verifies SHA-256 017e38220cc7590653c630455732c90c2c4c80e148a105e1dabe02e133ebe032. The source trace is Westermo v1.0 right observation point, 1-second captured frame bytes. The declared split remains train [0,3267), validation [3267,4356), test [4356,5445). Validation and test rows are not read.

The decomposition is frozen over [0,2048) using the existing audited implementation. Interval optimization may use context elsewhere within this training prefix. The folds are rolling-origin for HMM and occurrence fitting, not strict causal decomposition replays. This is a conditional training-only comparison.

## Predictive protocol

Folds are (fit_end, predict_end): (1024,1280), (1280,1536), (1536,1792), (1792,2048).

For each fold, representation, and fixed K in {1,2,3}:

1. Fit GaussianHMM(covariance_type="diag") only on observations before fit_end. Use seeds 17, 29, 43, 200 EM iterations, tolerance 1e-4, and covariance floor 1e-3. Keep the highest training-likelihood converged restart for that fixed K only if every diagonal variance is finite and at least 1e-3; record and reject covariance-collapsed attempts.
2. Canonicalize univariate states by increasing emission mean. For B, use lexicographic ordering of the scale-wise mean vector.
3. Decode training states with Viterbi only to construct training occurrence transition counts and positive-mark support counts.
4. At each predictive tick t, compute the regime prior from observations strictly before t. Score the scale-specific event indicator conditioned on the previous event state and this prior. The current observation is applied only after its target is scored.
5. Use the same Beta(1,1) posterior-mean estimator for all representations. The fitted HMM covariance floor is checked after EM; a converged likelihood with a variance below 1e-3 is rejected as degenerate. A regime/previous-event cell with zero observed trials is unfit; a prior-only value is never substituted. A prediction is scored only when every regime with prior mass above 1e-8 has observed transition support.
6. Compare A/B/C on the intersection of identical supported occurrence targets for each fold and fixed K. Report coverage as well as log score.

Positive marks are support diagnostics. An independent scale/regime Gamma estimate is eligible only with at least 30 positive training events in that cell. This experiment does not fit Gamma or lognormal, compare their likelihoods, or introduce pooling. It reports how many positive predictive targets have the required mark support in every regime with non-negligible predictive mass across A/B/C.

## Reproduction

Python 3.12 is required. Install the exact dependency pins from requirements.txt, then run:

    python experiments/generative-model-v1-hmm-research/compare_hmm_representations.py artifacts/westermo-model-extraction-v1/trace.csv --output artifacts/generative-model-v1-hmm-research/report.json
    python -m unittest discover -s experiments/generative-model-v1-hmm-research/tests -v

The report includes prefix/source checksums, script/configuration checksums, library versions, seeds, convergence diagnostics, per-scale support, and common-target scores. Running twice in the same environment and against the same artifact must produce byte-identical JSON.

## Interpretation and decision rule

No A/B/C or K winner is selected. A representation can only be considered further if common-target occurrence scores are consistently better across folds, mark-support coverage is adequate without relaxing the 30-event rule, and convergence/state stability are acceptable. This diagnostic alone cannot select a mark family or pooling strategy. Fold-sensitive or unsupported results remain unresolved.

No validation/test data, synthetic events, or artificial padding are used. Unfit remains distinct from inactive_by_policy. No production fitter, stochastic generator, generated QPN condition, or QPN result is added.


## Observed pinned-HMM result (2026-11-12)

Workflow run 43202029718 completed successfully. The report JSON SHA-256 is
db2cd3702d54520894a55c7bd6988f2697a7ff4fe57d66502e20ffedb93ba3de. The report's
experiment script SHA-256 is b4da6f47529463f1831ced69ec1b6f440703bfdadb41a74e774b45f5df2e2990
and its configuration SHA-256 is 026b9b4d194250d5ba797c41f4737a7b89b781782bef8fe6c3a83121fe172578. The report
records Python 3.12.16, hmmlearn 0.3.3, NumPy 1.26.4, SciPy 1.13.1,
scikit-learn 1.5.2, joblib 1.4.2, and threadpoolctl 3.5.0.

The explicit 1e-3 diagonal covariance validity guard exposed substantial
degeneracy:

| Representation | K | Valid converged restarts | Total restarts |
|---|---:|---:|---:|
| A | 1 | 12 | 12 |
| A | 2 | 12 | 12 |
| A | 3 | 6 | 12 |
| B | 1 | 0 | 12 |
| B | 2 | 0 | 12 |
| B | 3 | 0 | 12 |
| C | 1 | 12 | 12 |
| C | 2 | 8 | 12 |
| C | 3 | 0 | 12 |

All B fits violated the covariance floor; their minimum diagonal variances
ranged from 5.58e-6 to 1.50e-5. C/K=3 also had no valid fit in any fold.
Some A/K=3 and C/K=2 restarts collapsed, although other restarts passed the
guard. A/K=1,2 and C/K=1 were numerically valid in all folds.

Because B had no valid fit, the common supported A/B/C target intersection was
empty for every K and fold. Therefore **no comparative predictive score and no
A/B/C winner is reported**. This is a meaningful negative result: the
12-dimensional diagonal-Gaussian B representation is not estimable under the
predeclared covariance guard on this sparse training prefix. Do not repair it
by silently dropping channels, adding noise, or introducing pooling; those
would be separate hypotheses requiring a predeclared experiment.

The frozen-prefix positive mark counts remain
0,193,287,224,128,64,32,16,8,4,2,1 for scales 0–11. The independent Gamma
minimum of 30 is unchanged. No Gamma/lognormal likelihood was fitted and no
mark family was selected. The absence of a valid B HMM is a numerical
fit failure, not an inactive-by-policy decision for any scale.


## Covariance implementation detail and collapse diagnostics

The pinned hmmlearn 0.3.3 source is inspected at
https://github.com/hmmlearn/hmmlearn/blob/0.3.3/src/hmmlearn/hmm.py.
In this version, \`min_covar\` is added during covariance initialization in
\`GaussianHMM._init\`; it is not a hard lower bound applied in the Gaussian
M-step. The M-step instead uses the inverse-gamma prior parameters
\`covars_prior\` and \`covars_weight\`. The experiment therefore explicitly
sets their 0.3.3 defaults (0.01 and 1.0) and separately rejects a converged
fit if any post-EM diagonal variance is below 1e-3. This distinction is
important: the guard is an experiment acceptance rule, not a guarantee
provided by hmmlearn's \`min_covar\`.

The report now records, for each training fold and each feature, the
training-only mean, variance, standard deviation, zero fraction, distinct
value count, and range. Every attempted HMM restart also records per-state,
per-feature diagonal variances, the feature(s) attaining the minimum, the
features below the guard, and posterior state occupancy. Valid selected
restarts additionally record canonical-state variances and occupancies.
These are diagnostics only: they do not remove channels, alter the guard,
select a covariance prior, or change the A/B/C/K decision rule.

The next regularization experiment, if justified by these diagnostics, must
predeclare a fixed prior-sensitivity grid and report all settings without
selecting one from validation/test performance. Any such setting remains a
research hypothesis until independently supported.


## Structural-zero feature sensitivity (2026-11-12)

The detailed per-feature report identifies B feature 0 (scale 0) as exactly
constant zero in all four training folds: variance 0, zero fraction 1, and one
distinct value. This follows the audited event decomposition, whose scale-0
positive-mark count is zero. Other B features are nonconstant but sparse; for
example the highest-scale channels have zero fractions around 0.997–0.999.

A diagnostic-only sensitivity variant is therefore added alongside (not in
place of) the original A/B/C experiment. For each fold it removes **only**
features with exactly zero variance in that fold's training observations.
Every nonconstant B channel is retained regardless of sparsity. It uses the
same fixed K values, seeds, convergence settings, covariance prior, and
post-fit covariance rejection guard. It reports fit/convergence/covariance
diagnostics only; it does not compute a predictive score or alter the original
A/B/C target intersection.

The scale-0 event target and its zero positive-mark support remain in the
reported target/support accounting. A zero training count does not by itself
convert a scale to \`inactive_by_policy\`; unresolved mark support remains
\`unfit\`. No events are synthesized, and no A/B/C or reduced-B representation
is selected. This sensitivity is solely to test whether the structurally
constant observation coordinate explains the HMM numerical failure.


### Structural-zero-excluded B predictive diagnostic

The diagnostic-only B variant retains the 11 nonconstant training features and
keeps the complete original scale/tick event target set. For fixed K=1, it is
scored on the exact common supported occurrence targets shared with A and C,
where all three candidate fits are valid. K=2 and K=3 remain reported as fit
diagnostics and are not scored if their covariance checks fail. This does not
replace the original A/B/C comparison: original B still includes scale 0 and
remains unfit under the original contract. No model or regime count is selected.
