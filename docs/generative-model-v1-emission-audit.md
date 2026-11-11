# Emission semantics audit for generative model v1

**Audited against:** master at 8a7b9c11251d1a26f6658e838342f4a735f4611e  
**Implementation:** R/plotting.R::compute_emits(), R/decomposition.R::decompose_traffic(), R/reconstruction.R::reconstruct_traffic(), and R/model-export.R::export_model_v1()  
**Status:** code-path audit; synthetic conformance tests are added in tests/testthat/test-emission-semantics.R.

## 1. Exact emission filter

For a coefficient matrix with rows indexed by scale plus one:

- Row 1 (scale 0) is retained at every sample.
- For scale s>0, each globally aligned window has length L=2^s.
- Exactly one coefficient is retained per window, at one-based local position L/2, equivalent to zero-based offset L/2-1.
- The window starts are 0,L,2L,... . The position is therefore globally aligned; it is not a phase that restarts at an arbitrary trace crop.

The implementation constructs a row filter with wfilter[wlen / 2] <- 1 and multiplies the coefficient matrix by that filter. reconstruct_traffic() sums the resulting emitted matrix column-wise. Contributions from different scales at the same sample are additive.

For a 32-sample matrix, example emission indices (zero-based) are:

| Scale | Window length | Emission indices |
|---:|---:|---|
| 0 | 1 | 0 through 31 |
| 1 | 2 | 1, 3, 5, ..., 31 |
| 2 | 4 | 1, 5, 9, ..., 29 |
| 3 | 8 | 3, 11, 19, 27 |
| 4 | 16 | 7, 23 |
| 5 | 32 | 15 |

This yields simultaneous emissions: for example, sample 1 receives scales 0, 1, and 2; sample 3 receives scales 0, 1, and 3. The reconstruction is the sum of those contributions.

## 2. How decomposition populates the rows

For each scale s>0, decompose_traffic() partitions the current residual into aligned windows. In each window it selects the largest value whose value occurs exactly once in that window (after sorting descending). The vector of window candidates is then passed through optimize_intervals().

For a positive optimized candidate c, the decomposition:

1. writes c into every coefficient column of that scale's window;
2. finds the first matching occurrence in the current residual and zeros that source position if a match exists;
3. subtracts c at the center-left emission position, clamped at zero.

The second and third actions are intentionally different positions when the selected source occurrence is not at center-left. This prevents the residual row from duplicating the higher-scale contribution at the location where reconstruction emits it. If interval optimization interpolates a candidate and no matching source value exists, step 2 is skipped but step 3 still occurs.

After all higher scales, row 1 is produced by applying optimize_intervals() to the remaining residual. Thus scale-zero values are residual samples, not a raw copy of the input.

## 3. Consequences for generative fitting

- For s>0, read at most one event opportunity per aligned window, from its center-left coefficient. Do not count the repeated coefficient in all 2^s columns as 2^s events.
- A zero at the center-left position means no emitted event for that scale/window, even if the original unique source value was at another position in the window.
- A non-zero center-left coefficient is the emitted mark amplitude. It is not a timestamp of the original source packet.
- For scale 0, every sample is an opportunity; a positive residual is an emission and zero is no emission.
- Sum emissions from all scales at each base sample. Do not overwrite simultaneous contributions.
- Keep the global sample origin and window alignment in the training/holdout split. A crop that restarts scale windows at its own index changes the model's event assignment.
- Candidate optimization can interpolate between windows and change candidate values. Higher-scale event marks are therefore properties of the extracted reconstruction, not necessarily unchanged observed packet sizes.
- Reconstruction is deterministic but not guaranteed to equal the original input sample by sample. The generative evaluation must continue to distinguish reconstruction fidelity from generated statistical fidelity.

## 4. Regime alignment remains an open modelling choice

The emission timing is now code-confirmed. The proposed HMM regime path, however, is not validated by this audit. Fitting a base-tick regime HMM directly to the original X_t and looking up the state at a center-left emission tick may assign a regime using a sample whose original observation was shifted/removed by decomposition.

Before implementing the fitter, compare at least these predeclared alternatives using training data only:

1. base-tick HMM on the original binned signal, sampled at each emission tick;
2. regime model on a scale-aware signal derived from the emitted reconstruction;
3. a regime process fitted to a base-tick aggregate of all emitted scale contributions.

Do not select among them using holdout metrics. Record the choice and its training-only criterion in the model artifact. This audit does not establish which regime alignment is statistically best.

## 5. Synthetic conformance coverage

The accompanying test file uses a 6-row, 32-column coefficient matrix with distinct values to assert the exact emission mask at all scales. A second fixture places unit coefficients at every scale and checks simultaneous emissions by column sum. These tests validate the legacy filter contract; they do not validate a stochastic model, fitting procedure, or empirical fidelity.
