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

The fitted process is a proposal, not a claim that the current deterministic coefficients are unbiased event observations. Before implementation, test the extraction-to-opportunity mapping against the existing reconstruction/export semantics on synthetic fixtures, including odd/even scale boundaries and simultaneous emissions.

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
- The current decomposition's emission mapping must be verified against the actual reconstruction helper before a generator implementation is accepted.
- A power-of-two training interval limits split flexibility; a future decomposition-window protocol may relax this without changing the v1 schema semantics.

## 8. Implementation sequence

1. Review this proposal and settle the event extraction mapping.
2. Implement schema validation and semantic invariant tests only.
3. Implement fitting with synthetic fixtures and deterministic reproducibility tests.
4. Implement generation and output/manifest contract.
5. Run training-only baseline fitting and untouched contiguous holdout evaluation.
6. Connect to QPN only after the statistical gate passes.

No generator implementation is included in this proposal.
