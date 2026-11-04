# Research Readiness Specification

**Status:** working technical specification  
**Primary direction:** network traffic model extraction

This document defines the technical decisions needed to extend `trafficmsd` from multiscale signal analysis toward extraction of reusable network traffic models. It is a working specification, not a claim that the proposed method has already been validated. Scientific publication is not part of the current implementation phase.

## 1. Immediate objective

Develop a method that extracts a compact, interpretable, reusable model from measured network traffic.

The initial objective is model extraction, not synthetic traffic generation or simulator integration. Those are downstream capabilities that depend on a well-defined and evaluated model representation.

The central distinction is:

- **Decomposition/reconstruction:** represent a measured signal at multiple scales and reconstruct that signal from its coefficients.
- **Model extraction:** infer a reusable parameterized description from one or more measured traces.
- **Traffic generation:** sample new traffic sequences from that description.
- **Simulation suitability:** assess whether generated sequences preserve properties relevant to a target performance model.

Success at one stage does not imply success at the next.

## 2. Initial technical questions

The first implementation phase should answer these questions:

- **What is being modeled?** Bytes per time interval, packet counts, packet sizes, inter-arrival times, or another traffic quantity?
- **What does the extracted model describe?** Temporal scales, amplitudes/rates, recurring structures, bursts, trends, residual variation, or a defined subset?
- **What information must be retained?** Which properties are essential for the intended analysis or later simulation use?
- **What assumptions are made?** Sampling interval, stationarity, segmentation, missing data, and trace duration.
- **How is extraction quality measured?** What information loss or approximation error is acceptable for the intended use case?

These decisions should be made before implementing a new generator.

## 3. Proposed processing pipeline

1. **Trace acquisition:** identify source, capture conditions, time span, sampling interval, units, and redistribution rights.
2. **Preprocessing:** document filtering, aggregation, missing-value handling, normalization, and segmentation.
3. **Multiscale analysis:** apply the existing decomposition implementation and retain all parameters required to reproduce it.
4. **Model extraction:** convert decomposition and clustering results into a documented model representation.
5. **Model inspection:** provide diagnostics that explain model components and information retained or discarded.
6. **Model evaluation:** compare relevant properties of the extracted representation and the source trace.
7. **Later generation:** once the model contract is stable, define how new sequences can be sampled from it.
8. **Later simulation:** assess the effect of generated traffic in a selected performance simulator.

Each stage should have a versioned input/output contract so that extraction, generation, and evaluation can be tested independently.

## 4. Candidate model representation

The existing historical notation describes components using a tuple of scale, amplitude, and temporal bounds. This is a useful starting point, but the production model format must be specified and tested before implementation.

A model component may need to capture:

- scale or temporal resolution;
- amplitude or rate contribution, including units;
- start/end position or a rule for temporal placement;
- component type (for example, trend, periodic component, burst, or residual);
- extraction parameters and confidence/quality diagnostics, where meaningful;
- provenance linking the component to the source trace and extraction configuration.

The model as a whole should define its time unit, rate unit, sampling interval, duration semantics, preprocessing metadata, and schema version. Stochastic generation parameters should be added only when generation semantics are designed.

Do not assume that a deterministic reconstruction tuple is already a generative stochastic model.

## 5. Model-extraction evaluation

The first evaluation should focus on whether the extracted representation captures the information it claims to represent. Metric selection depends on the traffic quantity and use case selected in the initial scope.

Candidate evaluation dimensions include:

| Dimension | Candidate measurements |
|---|---|
| Reconstruction fidelity | absolute/relative error, RMSE, normalized error |
| First-order properties | mean, variance, quantiles, coefficient of variation |
| Distributional properties | empirical CDF distance, Wasserstein distance, KS statistic |
| Temporal structure | autocorrelation at selected lags, correlation decay |
| Burst structure | peak-to-mean ratio, burst duration and inter-burst distributions |
| Frequency structure | power spectral density and dominant periodicities |
| Model compactness | number of components, parameter count, encoded model size |
| Robustness | sensitivity to trace duration, sampling interval, and traffic regime |

No single metric is sufficient. Some metrics may be inappropriate for a particular traffic quantity; the final protocol should justify each selected metric. Report per-trace results as well as aggregated results.

### Comparisons and controls

- Compare against the original trace and, where relevant, a reconstruction from the full decomposition coefficients.
- Use held-out segments or traces where feasible.
- Record all preprocessing and extraction parameters.
- Fix and record random seeds for stochastic steps such as clustering.
- Include sensitivity analyses for important parameters.
- Report failures, excluded traces, and limitations rather than silently dropping them.

Baseline traffic generators such as Poisson, ON/OFF, and Markov-modulated models are more relevant to the later generation phase; they should not distract from the first extraction milestone.

## 6. Data and reproducibility requirements

For every trace, record at least:

- dataset name and source citation;
- license and redistribution restrictions;
- capture environment and traffic direction, if known;
- time range, sampling interval, units, and aggregation procedure;
- preprocessing steps and exclusions;
- checksum and immutable version or release identifier.

Large or restricted raw traces should not be committed to the package repository. Store them in an appropriate data repository or institutional storage and provide scripts that validate their expected checksums. Commit small, redistributable fixtures only when licensing permits.

Every evaluation should have a machine-readable configuration and produce machine-readable metrics, logs, and figures. Record software versions and random seeds so that results can be reproduced from documented inputs.

## 7. Architecture boundaries

Keep the following concerns separate:

- **Package core:** decomposition, reconstruction, clustering, and stable reusable functions.
- **Model layer:** schema, serialization, validation, diagnostics, and conversion from analysis output to model parameters.
- **Extraction layer:** algorithms and parameter-selection rules that construct model objects from trace analysis.
- **Evaluation layer:** datasets, configurations, quality metrics, and result aggregation.
- **Future generator:** sequence sampling from a stable model representation.
- **Future simulator adapter:** mapping generated traffic into one selected simulator's input format.

This separation allows the model representation and extraction algorithms to be tested without coupling them to a generator or a particular simulator.

## 8. Decisions required before implementation

The first implementation iteration should explicitly decide:

1. The initial traffic domain and traffic quantity to model.
2. The target time resolution and any segmentation/stationarity assumptions.
3. Which multiscale components and properties the extracted model must retain.
4. The model schema, units, temporal semantics, and provenance fields.
5. The initial trace datasets and their licenses.
6. The extraction-quality metrics and acceptance criteria.
7. The minimal example that demonstrates the complete extraction path.

These decisions should be captured before substantial implementation begins.

## 9. Initial definition of done

The model-extraction foundation is ready when:

- the target traffic quantity and use case are explicit;
- the model representation is documented and versioned;
- extraction from a trace to a model is reproducible;
- model contents can be inspected and validated;
- quality metrics and acceptance criteria are defined;
- a minimal example works with a redistributable dataset;
- limitations and assumptions are documented.

The research stages are tracked in [Research and Development Roadmap](RESEARCH_ROADMAP.md); Rust migration milestones are tracked in the technical [ROADMAP](../ROADMAP.md).
