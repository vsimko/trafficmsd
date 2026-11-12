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


## Predeclared covariance-prior sensitivity for multi-state models

The structural-zero-only experiment restores valid K=1 fits for B, but K=2
and K=3 still fail the post-EM covariance guard in every fold. K=1 produces
one state only; its identical A/C/B-variant scores are a non-discriminating
baseline, not evidence that the representations are equivalent.

A separate, fixed sensitivity grid tests inverse-gamma variance shrinkage
without selecting a setting:

- \`nu = 1, 10, 100\` (fixed logarithmic pseudo-count grid);
- per-feature prior mean = that feature's variance in the current fold's
  training observations only;
- \`covars_prior = nu * prior_mean\`;
- \`covars_weight = 1 + nu\`;
- same prior rule and same nu values for A, C, and B with only the exactly
  constant training feature excluded;
- fixed K=2 and K=3, all three initialization seeds, all four folds;
- same 1e-3 post-fit covariance rejection guard and predictive target logic.

This is a sensitivity analysis, not a tuning loop: all prior strengths and
all convergence failures are reported. Comparisons are scored only on the
common supported occurrence targets for A, C, and the reduced B variant under
the same nu and K. No validation/test observations are used, no prior strength
or regime count is selected, and no channels other than the exact constant
training feature are excluded.


## Observed covariance-prior sensitivity result (2026-11-12)

Workflow run 43206213536 completed successfully. Its artifact archive digest
is \`sha256:e7edd43ab89dcee04768c50916a77c6e6fab7bc7af4a16da2caa289680cd8026\`.
The report JSON SHA-256 is
\`17e1ebbc4ffc53c1e853dbf88f70e42d8d24f098f6c083a2cbe41298fb752ecc\`.
The experiment script SHA-256 is
\`2e83ec701ff9e207e5a6ff9d56cc4191e4871f1b5752770996778e82bcfc5a64\`;
configuration SHA-256 is
\`5b53fd2b28fcfa84adc4c40216d37156f75ff9a9427fbeed2e5ed77a4558f8cc\`.

The structural-zero-excluded B variant (all 11 nonconstant channels retained)
had the following valid initialization counts for K=2 and K=3:

| Prior strength \`nu\` | K=2 across four folds | K=3 across four folds |
|---:|---:|---:|
| 1 | 0/12 | 0/12 |
| 10 | 3/12 | 3/12 |
| 100 | 12/12 | 12/12 |

Here each fold contributes three fixed initialization seeds. Only \`nu=100\`
produced valid B-variant fits in all four folds for both multi-state candidates.
This is a numerical sensitivity finding, **not a selected covariance prior**.

For \`nu=100\`, all three representations (A, C, and structural-zero-excluded B)
had valid K=2/K=3 fits in all folds. The common supported occurrence targets
and mean log scores were:

| K | Fold | Common targets | A mean log score | C mean log score | B variant mean log score |
|---:|---:|---:|---:|---:|---:|
| 2 | 1 | 391 | -0.05549 | -0.05581 | -0.16235 |
| 2 | 2 | 416 | -0.04337 | -0.04885 | -0.20239 |
| 2 | 3 | 407 | -0.07468 | -0.08054 | -0.19772 |
| 2 | 4 | 416 | -0.11235 | -0.12236 | -0.20932 |
| 3 | 1 | 307 | -0.01952 | -0.02304 | -0.01931 |
| 3 | 2 | 311 | -0.01828 | -0.02251 | -0.01907 |
| 3 | 3 | 281 | -0.00482 | -0.00601 | -0.02013 |
| 3 | 4 | 297 | -0.02241 | -0.02034 | -0.01748 |

The K=2 B-variant occurrence score is consistently worse on these
training-prefix folds. K=3 is mixed by fold and does not show a consistent
B-variant advantage. The aggregate weighted mean log scores over the
corresponding common targets are:

| K | A | C | B variant |
|---:|---:|---:|---:|
| 2 | -0.07170 | -0.07719 | -0.19339 |
| 3 | -0.01646 | -0.01823 | -0.01899 |

For every reported \`nu=100\`, K, and fold, the number of common positive-mark
targets for which all required regimes had at least 30 positive training marks
was **zero**. Thus no independent Gamma estimate is supported by this
comparison. No Gamma/lognormal likelihood is fitted and no mark family is
selected.

Interpretation is deliberately limited: the strong prior makes the
structural-zero-excluded B HMM numerically estimable, but the K=2 occurrence
scores are materially worse, and K=3 does not establish a stable advantage.
The sensitivity grid does not justify choosing \`nu=100\`, K=2/3, a regime
representation, a mark family, or a pooling strategy. The original full B
representation including the scale-0 constant feature remains unfit under its
original specification. The K=1 identical scores remain a one-state,
non-discriminating baseline.

All results are conditional rolling-origin diagnostics within the frozen
training prefix [0,2048). No validation/test observations, synthetic events,
or artificial padding are involved.


## Restart-level state-structure diagnostics

The prior-sensitivity report now records canonicalized state means,
transition matrices, initial probabilities, transition-row sums, transition
entropy, nonzero outgoing transitions, and posterior state occupancy for
**every** initialization seed, including converged fits rejected by the
covariance guard. This is intended to distinguish a numerically valid
covariance matrix from a well-supported recurrent regime structure. No
transition topology, occupancy threshold, or state count is selected by this
diagnostic.


### Restart-level state-structure result

The final restart-diagnostics workflow run 43206780248 completed successfully.
Its artifact archive digest is
\`sha256:4dbdc535bce3ca97dac28c9eb357615d86f66d6f4312775c62dfc0712aa09f35\`;
the report JSON SHA-256 is
\`e49fe190dd4f091737c6d80bddbf561a5849147b6e2a06c4a22e36186db3b7c4\`.
The script SHA-256 is
\`44e9eb06026583279a2f97d93b44154fdd76fc3f770e2d3a7b75f3ef19f96189\`.

For B-variant with \`nu=100\`, K=2 had identical canonical transition
matrices across all three seeds within each fold. Posterior occupancy was
approximately 65/35, both states had two nonzero outgoing transitions, and
row entropies were approximately 0.59–0.66 nats. This is repeatable numerical
structure, but its common-target occurrence scores remained materially worse
than A and C.

K=3 had all 12/12 covariance-valid restarts, but its transition structure was
not stable across initialization. In each fold, the highest-training-
likelihood restart had two near-deterministic transition rows (row entropy
rounded to 0 nats), sending those states almost exclusively to the third
state; the third row had entropy around 0.84–0.89 nats. Other seeds produced
different, more diffuse transition matrices and different state occupancies.
The selected highest-likelihood restart was seed 29 or 43 in every fold,
whereas seed 17 (and in one fold seed 29) produced the alternative topology.
Thus covariance validity alone is not sufficient evidence for three stable,
interpretable regimes. No transition topology or K is selected.


### Cross-representation restart occupancy audit at nu=100

For completeness, the same restart-level occupancy/transition diagnostics were
summarized for A and C under the identical training-variance-scaled prior.
Across all four folds and three seeds, the minimum state occupancy fraction
and maximum state occupancy fraction observed in any run were:

| Representation | K | Minimum state fraction | Maximum state fraction | Transition-row entropy range (nats) |
|---|---:|---:|---:|---:|
| A | 2 | 1.9% | 98.1% | 0.008–0.681 |
| A | 3 | 1.1% | 92.5% | 0.079–1.007 |
| C | 2 | 3.9% | 96.1% | 0.000–0.672 |
| C | 3 | 8.6% | 78.4% | 0.000–0.793 |
| B without constant feature | 2 | 33.4% | 66.6% | 0.587–0.661 |
| B without constant feature | 3 | 11.4% | 76.1% | 0.000–0.941 |

These are descriptive ranges, not preselected acceptance thresholds. They
show that covariance-valid fits can still have strongly imbalanced states
(A and C), nearly deterministic transition rows (A/C/B-variant), or
seed-dependent transition topology (especially C/K=2 and B-variant/K=3).
B-variant/K=2 has the most repeatable occupancy/transition diagnostics but
does not perform well on the common occurrence targets. No representation
currently provides independent evidence sufficient to declare a stable
regime interpretation.


## Cross-run report serialization

Independent GitHub Actions runs exposed floating-point differences around
1e-12 in otherwise identical pinned HMM executions. The within-job duplicate
run comparison had passed, but independent runner executions were not
byte-identical. To make the published artifact reproducible across runners,
the report serializer now canonicalizes every finite floating-point field to
7 significant decimal digits before JSON encoding. This does not alter model
fitting, predictions before serialization, event counts, thresholds, or
selection rules. The precision is part of the hashed experiment configuration
and is covered by a regression test. Non-finite values fail report generation.
