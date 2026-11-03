# Research Readiness Specification

**Status:** proposal for discussion  
**Strategic direction:** network traffic simulation

This document defines the minimum scientific and technical decisions needed before extending `trafficmsd` into a traffic-model extraction and generation framework. It is a working specification, not a claim that the proposed method has already been validated.

## 1. Research objective

Develop and evaluate a method that extracts a compact, interpretable model from measured network traffic and uses that model to generate synthetic traffic suitable for performance simulation.

The central distinction is:

- **Decomposition/reconstruction:** represent a measured signal at multiple scales and reconstruct that signal from its coefficients.
- **Model extraction:** infer a reusable parameterized description from one or more measured traces.
- **Traffic generation:** sample new traffic sequences from that description.
- **Simulation suitability:** demonstrate that the generated sequences preserve the properties relevant to a target performance model.

Success at one stage does not imply success at the next.

## 2. Candidate research questions

The first study should select one primary question and a small number of secondary questions.

- **RQ1 — Representation:** Can multiscale decomposition produce a compact traffic-model representation that preserves the temporal structure relevant to performance simulation?
- **RQ2 — Fidelity:** How closely do sequences generated from the extracted model match measured traces across distributional, temporal, burstiness, and spectral properties?
- **RQ3 — Utility:** Do generated sequences produce comparable performance outcomes to measured traces when used as input to a selected queueing or network simulator?
- **RQ4 — Generalization:** How sensitive is model quality to trace duration, sampling interval, traffic regime, and the presence of non-stationarity?

A first paper should avoid attempting to answer all four questions at once. RQ1 and RQ2 provide a manageable initial study; RQ3 can become the main contribution if a simulator integration is available.

## 3. Proposed processing pipeline

1. **Trace acquisition:** identify source, capture conditions, time span, sampling interval, units, and redistribution rights.
2. **Preprocessing:** document filtering, aggregation, missing-value handling, normalization, and segmentation.
3. **Multiscale analysis:** apply the existing decomposition implementation and retain all parameters required to reproduce it.
4. **Model extraction:** convert decomposition and clustering results into a documented model representation.
5. **Generation:** produce synthetic sequences from the model, including an explicit description of deterministic and stochastic components.
6. **Validation:** compare synthetic and measured traces using fixed metrics and baseline models.
7. **Simulation:** feed measured and generated traffic into the same simulator configuration and compare output performance measures.

Each stage should have a versioned input/output contract so that extraction, generation, and evaluation can be tested independently.

## 4. Candidate model representation

The existing historical notation describes components using a tuple of scale, amplitude, and temporal bounds. This is a useful starting point, but the production model format must be specified and tested before implementation.

A model component may need to capture:

- scale or temporal resolution;
- amplitude or rate contribution, including units;
- start/end position or a rule for temporal placement;
- component type (for example, trend, periodic component, burst, or residual);
- stochastic parameters and sampling procedure, where applicable;
- provenance linking the component to the source trace and extraction configuration.

The model as a whole should define its time unit, rate unit, sampling interval, duration semantics, random seed behavior, and schema version. Do not assume that a deterministic reconstruction tuple is already a generative stochastic model.

## 5. Validation protocol

Use a common preprocessing pipeline and evaluate on held-out segments or traces where feasible. Report results per trace and aggregated across traces; avoid relying only on a single average.

### Candidate metric families

| Property | Candidate measurements |
|---|---|
| First-order statistics | mean, variance, quantiles, coefficient of variation |
| Distributional similarity | empirical CDF distance, Wasserstein distance, KS statistic |
| Temporal dependence | autocorrelation at selected lags, correlation decay |
| Burstiness | peak-to-mean ratio, burst duration and inter-burst distributions |
| Frequency structure | power spectral density and dominant periodicities |
| Long-range dependence | Hurst-related estimates, with estimator and limitations stated |
| Operational behavior | queue length, delay, loss, utilization, throughput in a fixed simulator |

No single metric is sufficient. Metric selection should be justified by the target network/performance use case, and uncertainty should be reported where sample size permits.

### Baselines

Select baselines according to the characteristics of the traces and the intended claim. Candidate baselines include:

- Poisson arrivals for a simple memoryless reference;
- ON/OFF sources for bursty on/off behavior;
- Markov-modulated Poisson processes for regime-switching behavior;
- an appropriate wavelet or multiscale method where a comparable implementation is available;
- replay of the original measured trace as the reference condition in simulation.

Document parameter fitting and ensure that baselines receive comparable access to training data.

### Experimental controls

- Separate fitting and evaluation data where possible.
- Fix and record random seeds; repeat stochastic runs with multiple seeds.
- Keep simulator configuration identical between measured and generated input conditions.
- Record software versions, configuration files, and dataset checksums.
- Report failures, excluded traces, and preprocessing decisions.
- Include ablations for key model components when making claims about their contribution.

## 6. Data and reproducibility requirements

For every trace, record at least:

- dataset name and source citation;
- license and redistribution restrictions;
- capture environment and traffic direction, if known;
- time range, sampling interval, units, and aggregation procedure;
- preprocessing steps and exclusions;
- checksum and immutable version or release identifier.

Large or restricted raw traces should not be committed to the package repository. Store them in an appropriate data repository or institutional storage and provide scripts that validate their expected checksums. Commit small, redistributable fixtures only when licensing permits.

Every experiment should have a machine-readable configuration and produce machine-readable metrics, logs, and figures. A clean environment should be able to reproduce the reported evaluation from the documented inputs.

## 7. Architecture boundaries

Keep the following concerns separate:

- **Package core:** decomposition, reconstruction, clustering, and stable reusable functions.
- **Model layer:** schema, serialization, validation, and conversion from analysis output to model parameters.
- **Generator:** sequence sampling from a model, independent of the evaluation framework.
- **Experiment layer:** datasets, configurations, baselines, metric calculation, and result aggregation.
- **Simulator adapter:** mapping generated traffic into one selected simulator's input format.

This separation will make it possible to test the model and generator without coupling the package to a particular simulator.

## 8. Decisions required before implementation

The first research iteration should explicitly decide:

1. The primary research question and target traffic domain.
2. The traffic quantity to model (for example, packet counts, bytes, packet sizes, or inter-arrival times).
3. The time resolution and stationarity/segmentation assumptions.
4. The model schema and stochastic generation semantics.
5. The initial trace datasets and their licenses.
6. The baseline models and metrics.
7. The first simulation target and the performance outputs of interest.

These decisions should be captured in an experiment protocol before substantial generator implementation begins.

## 9. Initial definition of done

The research-readiness phase is complete when:

- the primary research question and target use case are agreed;
- the model representation is documented sufficiently for an independent implementation;
- datasets and their legal status are identified;
- baseline methods and evaluation metrics are fixed;
- the experiment pipeline and result format are specified;
- the selected simulator integration has a clear input/output contract.

The roadmap in [ROADMAP.md](../ROADMAP.md) tracks the subsequent implementation and validation phases.
