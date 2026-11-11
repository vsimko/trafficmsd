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

**Current comparison result (11 November 2026): no candidate selected.** The immutable Westermo v1.0 right-observation-point extraction artifact was recovered from workflow run [42001977735](https://github.com/vsimko/trafficmsd/actions/runs/42001977735) and checksum-verified. The source archive SHA-256 is `8a1f49eb49747f3046eda8063b104834f41f0df30d4fc2357aea5609826a4445`; the derived 1-second captured-frame-byte trace SHA-256 is `6429e3d6a84efdc13edc14e44b2de2e6cbd31452e1958116c271745069b3fbb7` (5,445 samples, right observation point). The declared split is train `[0,3267)`, validation `[3267,4356)`, test `[4356,5445)`.

An exploratory comparison used only the first 2,048 rows of the training segment (`[0,2048)`), the largest power-of-two prefix beginning at global sample zero. The script reads only those rows; validation and test rows were not loaded. Four expanding rolling-origin folds were wholly inside this prefix. A local diagonal-Gaussian HMM diagnostic (K=1,2,3; deterministic initialization; 100 EM iterations) produced:

| Candidate | Selected K, folds 1–4 | Occurrence mean log score | Positive marks scored |
|---|---|---:|---:|
| A — original signal | 3, 3, 3, 3 | -0.12485 | 42 / 472 (8.9%) |
| B — emitted channels | 2, 3, 3, 3 | -0.25663 | 0 / 472 (0%) |
| C — aggregate emissions | 3, 3, 3, 3 | -0.20665 | 0 / 472 (0%) |

These are exploratory diagnostics, **not selection evidence**. The occurrence-only scores omit the positive-mark likelihood and are not a common joint score. Complete Gamma mark coverage is insufficient for all three candidates; the existing minimum of 30 positive training events per (scale, regime) remains unchanged. The local HMM implementation is not yet a canonical independently validated fitter, so no A/B/C winner or validated generative fidelity is claimed. Do not use validation/test data, lower the event threshold, or implement a production fitter/generator on this evidence.

The extraction port was separately checked against the archived R-produced full-trace reconstruction (maximum absolute difference 0). This is a conformance check, not a claim that the emitted reconstruction equals the original observation: interval optimization can make the deterministic decomposition lossy, as documented in [model format v1](model-format-v1.md).
### 2.2 Scale-specific event process

For scale s, define the event opportunity interval and emission phase using the deterministic decomposition convention:

- s=0: one opportunity per base sample, interval 1, phase offset 0;
- s>0: interval 2^s base samples, phase offset 2^(s-1)-1 samples from the start of each aligned scale window.

At opportunity k, the binary event indicator E_(s,k) is a first-order Markov chain conditioned on the regime at its emission tick. For regime j, parameters are:
- p01_(s,j) = P(E_(s,k)=1 | E_(s,k-1)=0, Z=j);
- p11_(s,j) = P(E_(s,k)=1 | E_(s,k-1)=1, Z=j).

The initial event probability is recorded separately for every scale. Regime conditioning shares traffic-state changes across scales; the per-scale Markov state adds local persistence and burst clustering.

### 2.3 Amplitude marks and emission

When E_(s,k)=1, draw a strictly positive amplitude mark. The independent per-(scale, regime) Gamma distribution with shape a_(s,j)>0 and scale b_(s,j)>0 is only the baseline specification: the Westermo training-only audit shows that it is not estimable for most fine-scale/regime cells. The proposed replacement is a shared-shape Gamma model with a scale-dependent log mean and shrinkage-controlled regime offsets; see [the sparse-mark audit](generative-model-v1-sparse-mark-audit.md). A scale with no positive training marks is unfit unless explicitly disabled by policy; it must not be assigned fabricated Gamma parameters. When E=0, emit zero.

Emit the mark at the scale's deterministic emission tick. If multiple scales emit at the same base tick, sum their marks. The generated output is therefore non-negative and uses the same value unit and base sampling interval as the source. The model does not add random within-bin timing: that would require a different observation and workload contract.

This construction explicitly samples both event occurrence and amplitude. Temporal dependence comes from the shared regime chain and scale-specific event chains; it is not inferred from marginal amplitude distributions alone.

## 3. Fitting protocol

1. Freeze the source dataset, preprocessing, binning, observation units, and source checksum before fitting.
2. Select one contiguous training interval before looking at holdout metrics. For the current decomposition implementation, use a training length that is a power of two; do not pad the training interval with artificial zeros for fitting. Record zero-based inclusive start and exclusive end sample indices.
3. Run the deterministic decomposition on training data only. Extract one event opportunity sequence per scale using the scale interval and emission phase above. Do not fit on a reconstruction of the entire trace and then claim a held-out evaluation.
4. Fit the Gaussian-emission HMM to log1p(X_t) on the training interval only, evaluate K=1,2,3 by BIC, and canonicalize regime labels by increasing emission mean.
5. Decode the training regime path using Viterbi. For each scale and regime, estimate p01 and p11 from the corresponding event transitions, with a declared Beta(1,1) smoothing prior. Estimate positive Gamma marks by maximum likelihood from event amplitudes assigned to that regime.
6. Fit the shared-shape, scale-trend Gamma mark model over positive training events, with declared shrinkage on regime offsets. The former independent-cell baseline still requires at least 30 events per (scale, regime); do not lower that threshold. A scale with no positive marks is unfit unless explicitly disabled by policy; fail rather than silently borrowing holdout samples or fabricating parameters. Record the shared mark fit's event count and supported scales.
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

The JSON Schema in schemas/generative-model/v1/model.schema.json is a structural contract. It now represents one shared Gamma mark model (shape, log-mean scale trend, regime offsets and shrinkage penalty) plus per-scale active/inactive-by-policy/unfit status. The Python validator in scripts/validate_generative_model_v1.py and its regression tests now enforce the cross-field invariants below (in addition to structural JSON Schema validation):

- unique contiguous scale indices from zero and scale interval/phase consistency;
- exactly one occurrence parameter set for every (scale_index, regime_id) pair;
- the shared mark-model regime-offset vector has exactly one entry per regime and uses regime 0 as the zero-offset reference;
- fitted_scale_indices exactly match scale processes whose mark_status is active; unfit scales remain explicit and are excluded rather than coerced to policy-inactive;
- inactive_by_policy scales have zero initial/event probabilities; their explicit policy status is distinct from insufficient evidence (unfit);
- all regime transition rows and initial probabilities sum to one within a documented tolerance;
- probabilities are in [0,1], shared Gamma shape is positive, and all numbers are finite;
- shared mark mean is exp(log_mean_intercept + log_mean_scale_slope * (scale_index - 3) + regime_log_mean_offsets[regime_id]); Gamma scale is that mean divided by the shared shape;
- the training range is contiguous, non-empty, and its declared sample count matches its endpoints; checking the end against the source signal length remains an external provenance check because the source length is not part of this schema;
- source model schema kind is deterministic reconstruction, never another generative model;
- checksum and fitting provenance are present and consistent.

## 7. Known risks and decision points

- The HMM's geometric regime durations may underfit long-tailed bursts.
- Gamma marks may be inadequate for heavy-tailed amplitudes. A same-fold training-only Gamma/lognormal diagnostic on 472 positive marks slightly favored lognormal in weighted conditional mark log score (-7.0618 vs -7.1413), but Gamma won fold 4 and both families under-covered the upper tail there (q99 coverage 87.16% Gamma, 82.57% lognormal). The independent HMM reimplementation is not canonical; neither family is selected. See the [sparse-mark audit](generative-model-v1-sparse-mark-audit.md) and [reproducibility bundle](../experiments/generative-model-v1-sparse-marks/README.md).
- A minimum 30 events per scale/regime makes fine scales or rare regimes unfit under independent-cell Gamma estimation. The shared scale-and-regime mark model is a research candidate, not yet a validated replacement; its schema and calibration gates must be completed before fitting/generation.
- Training intervals must preserve global scale-window alignment: for a power-of-two training length N, require the global training start index to be divisible by N. The emission mask itself is covered by the accompanying conformance tests.
- A power-of-two training interval limits split flexibility; a future decomposition-window protocol may relax this without changing the v1 schema semantics.

## 8. Implementation sequence

1. Review the code-confirmed emission mapping and settle the still-open regime alignment using training-only comparisons.
2. Implement schema validation and semantic invariant tests only. (The initial semantic validator and 15-case regression suite are present; the dedicated schema/semantic CI workflow passed on the current PR head.)
3. Implement fitting with synthetic fixtures and deterministic reproducibility tests.
4. Implement generation and output/manifest contract.
5. Run training-only baseline fitting and untouched contiguous holdout evaluation.
6. Connect to QPN only after the statistical gate passes.

No generator implementation is included in this proposal.
