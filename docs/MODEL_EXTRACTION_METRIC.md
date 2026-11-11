# Model-extraction fidelity and complexity metric v1

**Status:** proposed operational metric specification  
**Scope:** deterministic reconstruction of PCAP-derived byte-rate time series  
**Companion protocol:** [Model-extraction evaluation protocol v1](model-extraction-evaluation-v1.md)

## 1. Evaluation object

The initial observation is a non-negative time series
\[
X=(x_1,\ldots,x_n),
\]
where \(x_t\) is the sum of captured packet/frame lengths in bin \(t\). A candidate extraction configuration \(\theta\) (initially the decomposition cutoff) produces a serialized model \(M_\theta\) and its deterministic reconstruction
\[
\hat X_\theta=R(M_\theta)=(\hat x_1,\ldots,\hat x_n).
\]

All comparisons for one trace must use the same observed bins, bin origin, interval, packet-selection policy, and units. Power-of-two padding used internally by decomposition is excluded from scoring and from the source sample count.

This v1 metric evaluates **reconstruction**, not stochastic generation. It does not establish that a model can generate statistically independent or realistic future traffic.

## 2. Fidelity vector

No single scalar is designated as the universal fidelity score. Report the following measures separately for every trace and candidate.

### 2.1 Pointwise and volume fidelity

Let \(e_t=\hat x_t-x_t\).

- Mean absolute error:
  \[
  D_{MAE}=\frac{1}{n}\sum_{t=1}^{n}|e_t|.
  \]
  Unit: bytes/bin.

- Root mean squared error:
  \[
  D_{RMSE}=\sqrt{\frac{1}{n}\sum_{t=1}^{n}e_t^2}.
  \]
  Unit: bytes/bin. This penalizes large local deviations.

- Relative L1 error:
  \[
  D_{L1}=\frac{\sum_t|e_t|}{\sum_t x_t}.
  \]
  Dimensionless; undefined when the source volume is zero.

- Signed volume error:
  \[
  D_V=\frac{\sum_t\hat x_t-\sum_t x_t}{\sum_t x_t}.
  \]
  Dimensionless; undefined when the source volume is zero. Keep its sign because over- and under-estimation have different meanings.

MAE and RMSE are scale-dependent and should not be compared across traces with substantially different traffic volumes without normalization. Relative L1 is the default cross-trace pointwise measure when source volume is non-zero. Do not replace undefined denominators with an arbitrary epsilon.

### 2.2 Marginal-distribution fidelity

Report the empirical 1-Wasserstein distance between the source and reconstructed bin-value distributions, along with the source/reconstruction coefficient of variation when the corresponding mean is non-zero:
\[
D_{W1}=W_1(\{x_t\},\{\hat x_t\}).
\]
Unit: bytes/bin. Also report source/reconstruction mean, standard deviation, and p50/p90/p95/p99 quantiles. The two-sample KS distance may be included descriptively; its independent-sample p-value must not be interpreted as valid for autocorrelated observations.

### 2.3 Temporal fidelity

For each series, calculate its sample autocorrelation after subtracting that series' own mean. For the predeclared eligible lag set
\[
L=\{1,2,4,8,16,32,64,128,256\},
\]
retain only lags below \(n/4\), and report
\[
D_{ACF}=\frac{1}{|L'|}\sum_{\ell\in L'}
|\rho_X(\ell)-\rho_{\hat X}(\ell)|.
\]
This is dimensionless. Report the per-lag differences as well as their mean. If either series has zero variance, the ACF comparison is not applicable; it must not be silently converted to zero.

### 2.4 Burst fidelity

For a full-trace diagnostic, compute the threshold once as the source trace's p95 and apply it unchanged to both series. For a contiguous holdout, estimate the threshold from the training segment's p95 and apply it unchanged to validation and test; never estimate it from the evaluated segment. Report:
- absolute difference in the fraction of bins above the threshold;
- source and reconstruction exceedance-run counts;
- median, p95, and maximum exceedance-run length;
- absolute differences in those run-length summaries where both are defined.

These are diagnostics by default. The holdout report includes exceedance-fraction difference, run counts, median/p95/max run lengths, and run-length differences where defined. Promote a burst measure into the primary fidelity constraint only when the target simulation use case is demonstrably sensitive to burst structure. Runs are measured on the observed, unpadded time axis.

### 2.5 Use-case-specific primary fidelity set

The default extraction report contains all metrics above. The **primary fidelity vector** used for model selection must be declared before final holdout evaluation:
- always include relative L1 error when source volume is non-zero (otherwise use MAE);
- include ACF discrepancy for the initial time-series extraction profile;
- include burst/distribution measures as primary objectives only when the target use case requires them.

MAE, RMSE, bias, volume error, and all secondary diagnostics remain visible even when they are not selection objectives. This prevents a selected model from appearing good merely because an irrelevant metric was optimized.

## 3. Complexity vector

For every candidate, report at least:

1. \(C_{components}\): number of serialized model components;
2. \(C_{bytes}\): exact byte length of the serialized model file.

Use the same schema version, serializer, formatting, metadata policy, and encoding for every candidate. Measure the final artifact written to disk, not an in-memory object estimate. Component count and serialized bytes are separate objectives: a model with fewer components can still serialize larger due to metadata or verbose parameters.

Report the number of scale-zero residual entries separately. A free-parameter count may be added once the model schema defines which fields are independent fitted parameters; do not equate component count with parameter count without justification.

The ratio
\[
C_{ratio}=C_{bytes}/(8n)
\]
may be reported as a reference against a float64 encoding of the binned signal. It is **not** a compression ratio against the original PCAP and must be labelled accordingly. If PCAP compression is claimed, compare complete model storage against the same source PCAP bytes and include required metadata.

## 4. Pareto analysis

For a fixed trace, preprocessing contract, and evaluation split, represent each candidate by
\[
V_\theta=(D_1,\ldots,D_k,C_{components},C_{bytes}),
\]
where \(D_1,\ldots,D_k\) are the predeclared primary fidelity measures.

Candidate \(a\) dominates candidate \(b\) if it is no worse on every objective and strictly better on at least one. A candidate is Pareto-optimal if no other candidate dominates it. Minimize every fidelity error and both complexity measures. Report the non-dominated candidates and plot/report fidelity against serialized bytes; do not collapse the vector into an unexplained weighted sum.

Use a documented numerical tolerance when comparing floating-point metrics. The tolerance must be small relative to the metric's reporting precision and identical across candidates. Missing/not-applicable metrics cannot be treated as zero; candidates with incomparable required metrics must be reported as incomparable or excluded under a predeclared rule.

## 5. Selecting one model for a use case

A Pareto frontier is the primary result. If an experiment requires one selected model, use a constrained rule:

\[
\min_\theta C_{bytes}(M_\theta)
\quad\text{subject to}\quad
D_j(M_\theta)\leq\varepsilon_j
\quad\forall j\in J,
\]
with component count as the tie-breaker.

The tolerances \(\varepsilon_j\) are use-case acceptance limits, not values to tune until a preferred candidate wins. Justify them from the downstream question, measurement uncertainty, or an agreed operational tolerance; freeze them before inspecting the final test results. If no defensible tolerances exist, report the Pareto frontier without claiming a unique best model. If no candidate satisfies all constraints, report that no feasible candidate was found rather than silently relaxing a threshold.

## 6. Validation and cutoff-selection protocol

For a single long trace, use contiguous train/validation/test segments and never shuffle time bins. Freeze the split before tuning. Evaluate the candidate cutoff grid on validation, calculate its fidelity and complexity vector, and derive the validation Pareto frontier. If tolerances have been declared, select the smallest serialized model satisfying them on validation. Freeze the selected cutoff and evaluate it once on the final test segment.

The current Westermo experiment independently decomposes each evaluated segment. Therefore, the test result measures whether the selected extraction configuration transfers to a later segment; it is **not** a model trained on the first 60% that predicts or generates the final 20%. The training segment may estimate baseline parameters and burst thresholds, but must not leak test information into cutoff selection.

For independent captures, report per-capture frontiers and summarize per-capture metrics (for example, median and interquartile range). Do not pool all time bins across captures before scoring.

## 7. Controls and interpretation

- **Identity control:** source compared with itself; expected zero pointwise and temporal reconstruction error. It is an upper-bound control, not a compact model.
- **Training-mean control:** constant value estimated on training data only and scored on validation/test. It is a deliberately simple baseline, not a realistic traffic generator.
- **TrafficMSD candidates:** evaluated at each declared extraction configuration with actual serialized complexity.

A low reconstruction error does not prove generative fidelity. A compact deterministic model does not prove simulation-outcome fidelity. Those claims require separate generation and simulator experiments with their own repeated-run and uncertainty protocols.

## 8. Minimum reporting table

For each trace and candidate, publish:

| Group | Required fields |
|---|---|
| Identity | dataset/trace ID, split, bin width, units, preprocessing version |
| Configuration | extraction version, cutoff, software commit |
| Pointwise | MAE, RMSE, bias, relative L1, volume error |
| Distribution | Wasserstein-1, source/reconstruction quantiles |
| Temporal | per-lag ACF differences, mean absolute ACF difference |
| Bursts | source threshold, exceedance fractions, run counts and run lengths |
| Complexity | component count, residual count, serialized bytes, float64-reference ratio |
| Selection | Pareto status, declared tolerances, feasible/infeasible status |

Retain failed and excluded candidates with reasons. Report the full candidate grid and the rule used to derive the frontier so that the selection can be independently reproduced.

## 9. Open decisions for the next implementation iteration

1. Confirm which temporal and burst measures are primary for the first stated simulation use case.
2. Set defensible, predeclared fidelity tolerances or keep the result as a Pareto frontier only.
3. **Implemented in this PR:** calculate the validation Pareto frontier and evaluate every non-dominated candidate on the untouched test segment. A future iteration may add a configuration interface for explicit use-case tolerances.
4. Keep full-trace cutoff sensitivity explicitly diagnostic; use validation for selection and test only for final reporting.
5. Repeat the protocol on independent captures before making generality claims.
