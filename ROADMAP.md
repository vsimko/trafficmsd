# Research and Development Roadmap

## Strategic direction

The primary direction for `trafficmsd` is **network traffic model extraction**. The immediate goal is to turn measured network traffic into a compact, interpretable, reusable model using the package's multiscale analysis capabilities.

Traffic generation and performance-simulation integration are downstream capabilities. They should be developed only after the extracted model has a clear representation and its quality can be assessed.

The current R package provides multiscale decomposition, reconstruction, and amplitude clustering. These capabilities are a foundation for model extraction; they do not, by themselves, establish that an extracted model is complete or statistically representative.

## Guiding principles

- Preserve the existing package as the reusable algorithmic core.
- Make model extraction and model representation the first-class development concerns.
- Separate analysis, model representation, generation, and simulator integration into explicit stages.
- Validate each stage with documented data, assumptions, and measurable criteria.
- Keep implementation reproducible through versioned configurations, data provenance, fixed seeds, and machine-readable results.
- Treat scientific publications as a possible later outcome, not a current project deliverable.

## Work plan

### Phase 0 — Define the extraction problem

- Select the initial traffic domain and the traffic quantity to model.
- Define the purpose and expected contents of an extracted model.
- Document preprocessing, temporal resolution, and segmentation assumptions.
- Identify suitable traces and their licenses.

**Exit criteria:** the extraction task, inputs, outputs, assumptions, and initial evaluation criteria are explicit.

### Phase 1 — Formalize the traffic-model representation

- Define a versioned schema for a traffic model and its components.
- Specify scale, amplitude/rate, units, temporal placement, component types, and residual handling.
- Distinguish descriptive parameters from stochastic generation parameters.
- Define serialization, validation, and compatibility rules.

**Exit criteria:** a model can be created, serialized, loaded, and validated independently of the extraction implementation.

### Phase 2 — Implement and evaluate model extraction

- Map multiscale decomposition and clustering outputs to the model representation.
- Define extraction algorithms and parameter-selection rules.
- Evaluate information retained and lost during extraction.
- Test robustness across traces, trace lengths, sampling intervals, and traffic regimes.
- Provide inspectable examples and diagnostics.

**Exit criteria:** extraction produces documented models from representative traces, with measured quality and known limitations.

### Phase 3 — Add traffic generation

- Define which model components are deterministic and which require stochastic sampling.
- Implement generation from the versioned model representation.
- Compare generated sequences with source traces using appropriate statistical metrics.
- Add suitable baseline models and repeatability tests.

**Exit criteria:** generated sequences can be produced and evaluated independently of the extraction implementation.

### Phase 4 — Validate simulation suitability

- Select one performance simulator or queueing model based on the target use case.
- Define an adapter from the model/generator output to simulator input.
- Compare simulation outcomes driven by measured and generated traffic.
- Document where the approach is useful and where it is not.

**Exit criteria:** simulation suitability is assessed using controlled and reproducible comparisons.

## Repository responsibilities

- **`trafficmsd`**: maintained R package, extraction algorithms, model representation, user documentation, tests, and stable public API.
- **Experiment assets**: scripts, configurations, derived results, and plots can initially live in a clearly separated repository or directory when their scope justifies it. No paper-specific repository is needed yet.
- **Dataset storage**: use external or institutional storage for large or restricted traces. Keep metadata, provenance, licenses, and checksums in version control rather than committing data that cannot legally or practically be redistributed.

## Immediate priorities

1. Agree on the first model-extraction use case and target traffic quantity.
2. Specify the model representation before implementing generation.
3. Define how extraction quality will be evaluated.
4. Implement a minimal end-to-end extraction example using a redistributable trace.
5. Keep repository maintenance and release preparation tracked through issues [#10](https://github.com/vsimko/trafficmsd/issues/10) and [#11](https://github.com/vsimko/trafficmsd/issues/11).

See [Research Readiness](docs/RESEARCH_READINESS.md) for the proposed extraction scope, model boundaries, and evaluation requirements.
