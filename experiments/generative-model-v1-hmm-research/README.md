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

1. Fit GaussianHMM(covariance_type="diag") only on observations before fit_end. Use seeds 17, 29, 43, 200 EM iterations, tolerance 1e-4, and covariance floor 1e-3. Keep the highest training-likelihood converged restart for that fixed K; record all attempts.
2. Canonicalize univariate states by increasing emission mean. For B, use lexicographic ordering of the scale-wise mean vector.
3. Decode training states with Viterbi only to construct training occurrence transition counts and positive-mark support counts.
4. At each predictive tick t, compute the regime prior from observations strictly before t. Score the scale-specific event indicator conditioned on the previous event state and this prior. The current observation is applied only after its target is scored.
5. Use the same Beta(1,1) posterior-mean estimator for all representations. A regime/previous-event cell with zero observed trials is unfit; a prior-only value is never substituted. A prediction is scored only when every regime with prior mass above 1e-8 has observed transition support.
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
