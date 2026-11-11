# Generative model v1 — design proposal

**Status:** research proposal; not a production generator or validated model  
**Related:** issue [#92](https://github.com/vsimko/trafficmsd/issues/92), evaluator [generative-fidelity-v1](../experiments/generative-fidelity-v1/README.md), QPN gate [#82](https://github.com/vsimko/trafficmsd/issues/82)

## 1. Scope and non-goals

TrafficMSD currently has deterministic extracted representations in schemas/model/v1 and schemas/model/v2. Their components encode a reconstruction: a scale, a time window, an emission position, and an amplitude. They are not probability distributions and must remain unchanged.

This proposal defines a separate fitted stochastic layer. It does not claim packet-level generation, protocol semantics, or Internet representativeness. The first target is a new sequence of non-negative observations at the same declared abstraction as the training signal (for the current Westermo experiment: captured-frame bytes per one-second bin).

The generator must not be described as a random perturbation of a deterministic reconstruction. It samples event occurrence and event amplitude at each scale, then sums emitted marks into the output bins.

## 2. Candidate v1 process

The proposed model is a **multiscale regime-conditioned marked-event process**.

### 2.1 Regime process

Let Z_t be a discrete latent regime at base sampling tick t. It is a first-order Markov chain with initial probabilities pi_j and transition matrix P_ij.

Fit the regime process to log(1 + X_t), where X_t is the training-only observed byte count. Each regime has a Gaussian emission distribution with fitted mean and standard deviation in log space. Candidate counts are K=1,2,3; select the lowest BIC, breaking ties in favour of the smaller K. The HMM is fitted by a documented deterministic implementation and a recorded fitting seed. Regime labels are canonicalized by ascending emission mean.

This is an intentionally simple regime model. Its geometric dwell-time assumption is a known limitation and must be tested against burst-run metrics; a hidden semi-Markov extension is out of v1 scope.


### 2.1.1 Regime-alignment alternatives and training-only comparison

The HMM observation used to define Z_t is not yet selected. Compare these three alternatives on the same contiguous training prefix, with identical scale extraction and event targets:

| Candidate | Regime observation | What it preserves | Main risk |
|---|---|---|---|
| A — original-signal HMM | Scalar log1p(X_t); use the predicted regime distribution at each scale's emission tick | The original measured base-tick level and its aggregate bursts | The signal includes contributions that decomposition later relocates or removes; it can assign a regime from a sample whose original value is not the emitted mark |
| B — scale-aware emitted HMM | Per-tick vector (log1p(C_0,t), ..., log1p(C_S,t)), where C_s,t is the emitted contribution of scale s at tick t; use a multivariate diagonal-Gaussian HMM | Which scale contributed at each tick, including scale-zero residual versus higher-scale marks | Sparse higher-scale channels and many near-zero dimensions can make covariance estimates and rare-regime occupancy unstable |
| C — aggregate-emission HMM | Scalar log1p(R_t), where R_t = sum_s C_s,t; use the predicted regime distribution at each scale's emission tick | The exact base-tick signal represented by the emitted reconstruction | Summation discards scale identity; different scale combinations can produce the same aggregate |

Here C_s,t is zero except at the audited emission opportunities. It must be extracted from the training-only decomposition; do not reconstruct the features from the full trace. Candidate B is a distinct multivariate observation model, not an HMM fitted independently to each scale.

**Common training-only evaluation protocol**

1. Freeze the dataset checksum, preprocessing and global sample origin. Use the already declared contiguous training segment only. Select the largest power-of-two prefix beginning at global sample zero that fits inside that segment; do not pad observed training values with synthetic zeros. Record the exact half-open range. Validation and test samples must not be read by this comparison.
2. Build X_t, C_s,t, and R_t from that same prefix. Use identical deterministic decomposition settings for B and C.
3. Fit K=1,2,3 regime candidates for each observation representation, recording convergence and parameter counts. Do not compare raw HMM BIC values across A/B/C as if they were on a common likelihood scale: the observation spaces and dimensions differ.
4. To compare the alternatives on a common target, use blocked rolling-origin folds wholly inside the training prefix. For each fold, fit the regime model on the preceding training block, then obtain the one-step predictive regime distribution at tick t from observations only through t-1. Do not use the current tick's observation or a smoothed full-sequence posterior to condition the event at t; either would leak the target emission into its own regime assignment.
5. With that predictive regime distribution, score the same scale-specific event indicators and positive marks for all candidates. Use the same Beta(1,1)-smoothed event-transition estimator and the same Gamma mark family for every candidate. Report joint held-out-within-training log predictive density per opportunity, separately for occurrence and positive amplitude, plus per-scale coverage and calibration diagnostics. Folds, minimum event counts and all tie-breakers must be fixed before inspecting scores.
6. Report regime occupancy, transition matrices, convergence failures and state stability across folds as secondary diagnostics. Canonicalize labels by the fitted emission mean for univariate candidates; for B, use a deterministic ordering based on the vector of scale-wise emission means. Do not treat numeric state labels as comparable across fits.
7. Prefer an alternative only if its common-target predictive score improves consistently across training folds without material instability or unfit scale/regime cells. If evidence is tied or fold-sensitive, retain the simplest adequate representation and document the uncertainty; do not consult the final holdout to break the tie.

The predictive-prior rule is important for all three candidates: regime inference at tick t must not condition on the same-tick observed event or mark whose conditional distribution is being evaluated. At generation time the regime is sampled from its Markov transition law, not inferred from a future or contemporaneous generated observation.

**Current comparison result (11 November 2026): not empirically determined.** The completed PR CI validates the deterministic emission mask and package behavior, but its model-extraction smoke artifact contains only a 32-sample synthetic fixture. It is not a Westermo training trace and cannot support regime selection. The immutable Westermo source is published as a release archive, but no extracted training-signal artifact from the Westermo extraction workflow is currently attached to this PR's CI runs. Therefore no A/B/C scores, selected K, or regime winner are claimed. The comparison is blocked on producing and checksum-verifying the training-only Westermo signal and emitted scale channels. This is an explicit data-availability blocker, not evidence in favour of any candidate.

### 2.2 Scale-specific event process

For scale s, define the event opportunity interval and emission phase using the deterministic decomposition convention:

- s=0: one opportunity per base sample, interval 1, phase offset 0;
- s>0: interval 2^s base samples, phase offset 2^(s-1)-1 samples from the start of each aligned scale window.

At opportunity k, the binary event indicator E_(s,k) is a first-order Markov chain conditioned on the regime at its emission tick. For regime j, parameters are:
- p01_(s,j) = P(E_(s,k)=1 | E_(s,k-1)=0, Z=j);
- p11_(s,j) = P(E_(s,k)=1 | E_(s,k-1)=1, Z=j).

The initial event probability is recorded separately for every scale. Regime conditioning shares traffic-state changes across scales; the per-scale Markov state adds local persistence and burst clustering.

### 2.3 Amplitude marks and emission

When E_(s,k)=1, draw a strictly positive amplitude mark from a Gamma distribution with shape a_(s,j)>0 and scale b_(s,j)>0, conditioned on the current regime j. When E=0, emit zero.

Emit the mark at the scale's deterministic emission tick. If multiple scales emit at the same base tick, sum their marks. The generated output is therefore non-negative and uses the same value unit and base sampling interval as the source. The model does not add random within-bin timing: that would require a different observation and workload contract.

This construction explicitly samples both event occurrence and amplitude. Temporal dependence comes from the shared regime chain and scale-specific event chains; it is not inferred from marginal amplitude distributions alone.

## 3. Fitting protocol

1. Freeze the source dataset, preprocessing, binning, observation units, and source checksum before fitting.
2. Select one contiguous training interval before looking at holdout metrics. For the current decomposition implementation, use a training length that is a power of two; do not pad the training interval with artificial zeros for fitting. Record zero-based inclusive start and exclusive end sample indices.
3. Run the deterministic decomposition on training data only. Extract one event opportunity sequence per scale using the scale interval and emission phase above. Do not fit on a reconstruction of the entire trace and then claim a held-out evaluation.
4. Fit the Gaussian-emission HMM to log1p(X_t) on the training interval only, evaluate K=1,2,3 by BIC, and canonicalize regime labels by increasing emission mean.
5. Decode the training regime path using Viterbi. For each scale and regime, estimate p01 and p11 from the corresponding event transitions, with a declared Beta(1,1) smoothing prior. Estimate positive Gamma marks by maximum likelihood from event amplitudes assigned to that regime.
6. A scale/regime pair with fewer than 30 positive training events is not fitted: fail with a diagnostic rather than silently borrowing holdout samples or fabricating parameters. A later protocol revision may define principled partial pooling.
7. Record algorithm/library versions, fitting seed, convergence diagnostics, selected K, BIC values, training interval, source and derived-input checksums, decomposition parameters, and all units.

The fitted process is a proposal, not a claim that the current deterministic coefficients are unbiased event observations. The exact center-left emission mapping is now audited against compute_emits() and synthetic conformance tests are included; see [the emission semantics audit](generative-model-v1-emission-audit.md). Regime alignment at those emission ticks remains an open modelling choice and must be compared using training data only before fitter implementation.

## 4. Generation and reproducibility

- The generator accepts only a validated generative-model v1 artifact, requested sample count, and a non-negative integer seed.
- A replicate is one independently seeded run. Repeatedly writing the same realization under different filenames is invalid.
- Use a documented RNG algorithm and deterministic seed handling. Independent replicates use unique seeds.
- Output is a CSV with exactly sample_index,value, contiguous zero-based indices, finite non-negative values, and exactly the requested number of samples.
- The evaluator manifest records generator name/version, model checksum, configuration checksum, source dataset identifier, source checksum, training range, preprocessing version, sampling interval, time/value units, and filename-to-seed mapping.
- Generated duration and regime policy must be predeclared. v1 starts each realization from the fitted initial regime distribution and scale-specific initial event probabilities; no source holdout state is carried into generation.

## 5. Validation gates

### Statistical gate

Use a contiguous held-out interval that was not used for fitting, model selection, or threshold tuning. Compare generated replicates with that interval using the existing generative-fidelity-v1 evaluator. Report the complete metric vector: marginal distribution, volume, ACF, burst exceedance/run lengths, and replicate spread.

Include two separately labelled reference baselines:
- IID empirical resampling of training observations;
- moving-block bootstrap of training observations, with block length selected using training data only and recorded.

These are controls, not TrafficMSD generators. Evaluate at least 20 unique seeds for exploratory characterization. Predeclare use-case primary metrics and tolerances before inspecting holdout results. Do not treat replicate p05/p95 as confidence intervals.

A marginal match with poor temporal or burst metrics fails the temporal-fidelity claim. Passing the evaluator's mechanical checks alone is not a scientific pass.

### Simulation-outcome gate

Only after the statistical gate passes, add a generated condition to issue #82. Keep the QPN model, 1,000-byte abstract-job mapping, cyclic replay semantics, warm-up, termination, replication policy, and metrics identical to the measured/replay experiment. Report simulation-outcome fidelity separately from statistical fidelity.

## 6. Schema contract

The JSON Schema in schemas/generative-model/v1/model.schema.json is a structural contract. A semantic validator must additionally enforce:

- unique contiguous scale indices from zero and scale interval/phase consistency;
- exactly one parameter set for every (scale_index, regime_id) pair;
- all regime transition rows and initial probabilities sum to one within a documented tolerance;
- probabilities are in [0,1], Gamma parameters are positive, and all numbers are finite;
- the training range is contiguous, non-empty, and within the declared source signal;
- source model schema kind is deterministic reconstruction, never another generative model;
- checksum and fitting provenance are present and consistent.

## 7. Known risks and decision points

- The HMM's geometric regime durations may underfit long-tailed bursts.
- Gamma marks may be inadequate for heavy-tailed amplitudes; a lognormal alternative should be evaluated only as a predeclared model comparison, not chosen after holdout inspection.
- A minimum 30 events per scale/regime may make fine scales or rare regimes unfit. Failures must be reported; partial pooling is not silently applied.
- Training intervals must preserve global scale-window alignment: for a power-of-two training length N, require the global training start index to be divisible by N. The emission mask itself is covered by the accompanying conformance tests.
- A power-of-two training interval limits split flexibility; a future decomposition-window protocol may relax this without changing the v1 schema semantics.

## 8. Implementation sequence

1. Review the code-confirmed emission mapping and settle the still-open regime alignment using training-only comparisons.
2. Implement schema validation and semantic invariant tests only.
3. Implement fitting with synthetic fixtures and deterministic reproducibility tests.
4. Implement generation and output/manifest contract.
5. Run training-only baseline fitting and untouched contiguous holdout evaluation.
6. Connect to QPN only after the statistical gate passes.

No generator implementation is included in this proposal.
