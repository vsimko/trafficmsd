# Model-extraction evaluation protocol v1

**Status:** proposed protocol for implementation and review  
**Scope:** deterministic model extraction and reconstruction  
**Not covered:** claims of statistically realistic synthetic generation or simulator validity

This protocol defines how to evaluate the first TrafficMSD model-extraction representation. It is deliberately limited to properties that can be tested against the measured input trace. A good reconstruction score does not establish that the model is a stochastic traffic generator.

## 1. Evaluation unit and input contract

The initial observation is transferred packet length aggregated into fixed-width time bins:

- value: sum of selected packet lengths in one bin;
- historical compatibility interval: one second;
- empty bins: zero;
- time origin, interval width, and value unit: explicit metadata;
- bin convention: left-closed, right-open;
- selected packet population, direction, and length field: documented by the dataset-specific preprocessing manifest.

The one-second interval is a historical compatibility baseline, not a universal requirement for all datasets. Metrics are computed on the unpadded observed interval. Padding added only to satisfy the decomposition's power-of-two requirement must not be included in the primary score.

The protocol assumes a source vector (x_1,\ldots,x_n) and a reconstructed vector \(\hat{x}_1,\ldots,\hat{x}_n\) with identical length, bin origin, and units. Invalid or missing observations are not silently dropped or imputed; preprocessing decisions must be recorded before evaluation.

## 2. Primary fidelity metrics

Report all primary metrics per trace. Do not pool samples across traces before calculating per-trace scores.

Let \(e_t=\hat{x}_t-x_t\).

| Metric | Definition | Unit / interpretation |
|---|---|---|
| MAE | \(n^{-1}\sum_t |e_t|\) | bytes/bin; average absolute reconstruction error |
| RMSE | \(\sqrt{n^{-1}\sum_t e_t^2}\) | bytes/bin; penalizes large errors |
| Bias | \(n^{-1}\sum_t e_t\) | bytes/bin; signed systematic error |
| Relative L1 error | \(\sum_t |e_t| / \sum_t x_t\) | dimensionless; undefined when total source volume is zero |
| Volume error | \((\sum_t\hat{x}_t-\sum_t x_t)/\sum_t x_t\) | dimensionless; undefined when source volume is zero |

For an all-zero source trace, report absolute metrics and mark relative metrics as not applicable. Never replace an undefined denominator with an arbitrary epsilon.

## 3. Distribution and temporal-structure metrics

These are secondary diagnostics; none is sufficient alone.

### 3.1 Marginal distribution

- Report source and reconstruction mean, standard deviation, coefficient of variation (when mean is nonzero), and quantiles at p50, p90, p95, and p99.
- Report the absolute quantile difference in bytes/bin.
- Report the empirical 1-Wasserstein distance between the two sample distributions in bytes/bin.
- A two-sample KS distance may be reported descriptively. Do not interpret its conventional independent-sample p-value as valid for autocorrelated time series.

### 3.2 Temporal dependence

Compute the sample autocorrelation function after subtracting each series' own mean. Report absolute ACF differences at predeclared lags:

`1, 2, 4, 8, 16, 32, 64, 128, 256` bins, limited to lags below \(n/4\).

Also report the mean absolute ACF difference over the eligible lags. If a series has zero variance, ACF is undefined; report it as not applicable rather than returning zero.

### 3.3 Burst diagnostics

Using a threshold fixed from the source trace's p95, report for source and reconstruction:

- fraction of bins above the threshold;
- number of contiguous exceedance runs;
- median and 95th-percentile run length in bins;
- maximum run length in bins.

The threshold is computed once from the source trace and applied unchanged to the reconstruction. Runs are contiguous in the original, unpadded time axis. If no bins exceed the threshold, run-length statistics are not applicable.

Frequency-domain diagnostics may be added in a later protocol revision once the intended traffic regimes and minimum trace durations are established. They are not a v1 acceptance gate.

## 4. Model compactness

Report:

- number of serialized model components;
- number of non-zero scale components and scale-zero residual samples;
- serialized model size in bytes;
- model-size ratio: serialized model bytes divided by the size of the source numeric vector encoded as float64.

Compactness is reported alongside fidelity, never as a standalone success criterion. Do not claim compression if metadata, JSON formatting, or source encoding are inconsistently counted.

## 5. Baselines and controls

The initial extraction comparison uses deterministic controls:

1. **Raw-trace identity control:** \(\hat{x}=x\). This is the zero-error reference, not a model and not a fair compression baseline.
2. **Constant-mean control:** reconstruct every bin as the source trace mean. This is a deliberately weak baseline that tests whether the multiscale representation improves on preserving only average volume.
3. **TrafficMSD v1 extraction:** the proposed multiscale representation with its cutoff and all preprocessing parameters fixed and recorded.

Machine-readable results place these controls under `results[].metrics.baselines`:
- `scope = full_trace_diagnostic`: both controls are evaluated descriptively on the full observed trace; the constant value is the full-trace mean. This is not out-of-sample evidence.
- `scope = contiguous_holdout`: compute the constant-mean value from the training segment only, then score it and the model against the contiguous test segment. Never estimate the baseline mean from the test segment.

The identity control must have zero primary fidelity errors (within numeric precision). The constant-mean control is a diagnostic reference, not a claim that constant traffic is a realistic generator.

Poisson, ON/OFF, and Markov-modulated traffic generators are not v1 extraction baselines. They belong to the later synthetic-generation evaluation because comparing a deterministic reconstruction to a stochastic generator answers a different question. When generation is implemented, that phase must define fitted-parameter procedures, independent random seeds, repeated runs, uncertainty summaries, and a measured-trace simulator reference condition.

## 6. Data separation and reproducibility

- Prefer evaluation on multiple independent captures or sessions.
- When only one long trace is available, define contiguous train/validation/test segments before tuning any extraction parameter. Do not randomly shuffle time bins.
- Do not tune cutoff or preprocessing on the final test segment.
- Record dataset identifier, source citation, license status, exact source file names, checksums, packet selection, timestamp and length semantics, binning parameters, and preprocessing version.
- Record TrafficMSD version/commit, schema version, configuration, and all numeric units.
- Emit one machine-readable record per trace and a summary containing per-trace values, median, interquartile range, and the number of successful/failed traces.
- Preserve failed runs and exclusions with explicit reasons.

The raw trace and the derived fixture must have documented redistribution permissions. Restricted or unverified raw traces remain outside the repository.

## 7. Initial acceptance gates

A model-extraction implementation can claim protocol conformance only when:

1. all primary metrics above are emitted with units and defined missing-value behavior;
2. marginal, temporal, and burst diagnostics are emitted where mathematically defined;
3. the raw-trace identity control returns zero fidelity error within numeric precision;
4. the constant-mean control is computed from the source training segment only;
5. model compactness is measured with the same serialization convention for every model;
6. the experiment records all provenance and configuration fields;
7. results are reported per trace and failures are not silently omitted.

No universal numeric quality threshold is imposed in this protocol. Thresholds must be justified from the intended simulation use case and validated datasets; they must not be selected after inspecting test results.

## 8. Relationship to downstream work

This protocol supports the model-extraction stage in the research roadmap. It does not close the dataset acquisition requirement, the model schema's trace-derived example requirement, or the later traffic-generation and simulator-adapter work. Those remain separate gates.
