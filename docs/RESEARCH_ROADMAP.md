# Research and Development Roadmap

## Strategic direction

The primary direction for `trafficmsd` is **measurement-driven extraction of compact, simulation-ready network traffic models from PCAP-derived observations**. The central research question is: given a measured trace and a defined simulation use case, what is the smallest model that preserves the traffic properties that materially affect the simulation?

The intended contribution is not a general-purpose packet analyser, traffic generator, network emulator, or network simulator. It is the method and evaluation workflow between packet capture and simulation: extract a parameterized model, quantify its fidelity and complexity, expose the trade-off, and realize the selected model as a reproducible simulator workload. See [Related work and research positioning](RELATED_WORK.md) for the comparison inventory and scope boundaries.

Traffic generation and simulator integration remain downstream stages of the same model-extraction objective. A deterministic reconstruction is not automatically a stochastic generator, and a successful simulator run does not establish fidelity to the source capture.

The current R package provides multiscale decomposition, reconstruction, and amplitude clustering. These capabilities are a foundation for model extraction; they do not, by themselves, establish that an extracted model is complete or statistically representative.

## Guiding principles

- Preserve the existing package as the reusable algorithmic core.
- Make model extraction and model representation the first-class development concerns.
- Separate analysis, model representation, generation, and simulator integration into explicit stages.
- Evaluate fidelity and model complexity together; report the Pareto frontier or predeclared fidelity-constrained model selection rather than optimizing reconstruction error alone.
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
- Measure model complexity consistently (component/parameter count and serialized size at minimum).
- Define a fidelity–complexity Pareto analysis and predeclared holdout selection rule.
- Test robustness across traces, trace lengths, sampling intervals, and traffic regimes.
- Provide inspectable examples and diagnostics.

**Exit criteria:** extraction produces documented models from representative traces, with measured quality and known limitations; candidate models are compared on a fidelity–complexity frontier, with selection rules fixed before final holdout evaluation.

### Phase 3 — Add traffic generation

- Define which model components are deterministic and which require stochastic sampling.
- Implement generation from the versioned model representation.
- Compare generated sequences with source traces using appropriate statistical metrics.
- Add suitable baseline models and repeatability tests.

**Exit criteria:** generated sequences can be produced and evaluated independently of the extraction implementation.

### Phase 4 — Establish LINE as the first QPN backend

- Verify the exact LINE release, license, documented headless interface, and machine-readable result export.
- Verify the QPN constructs required by the first workload case; record semantic gaps explicitly.
- Define backend-neutral workload, experiment, capability, and result contracts.
- Implement a minimal source → queue → service proof of concept.
- Compare a deterministic case with hand-calculated expectations.

**Exit criteria:** a minimal LINE experiment runs reproducibly without GUI automation, and its supported QPN subset and limitations are documented.

### Phase 5 — Cross-validate with QPME/SimQPN

- Add QPME/SimQPN only after the LINE proof of concept and neutral contracts are stable.
- Verify its version, license, headless/batch interface, and supported constructs.
- Run equivalent schedules and QPN semantics through both backends on their common supported subset.
- Compare throughput, queue population, utilization, residence-time distributions, runtime, and uncertainty.
- Investigate disagreements; do not designate either simulator as ground truth.

**Exit criteria:** backend differences and equivalences are documented on reproducible shared experiments.

### Phase 6 — Evaluate simulation relevance of generated traffic

- Compare measured-trace input, deterministic replay controls, and stochastic generated workloads.
- Keep QPN model, workload mapping, warm-up, termination, and replication policy fixed.
- Predeclare primary metrics and report uncertainty across replications.
- Separate workload-level QPN conclusions from packet-level network behavior.

**Exit criteria:** the usefulness and limitations of generated traffic for the stated performance-simulation use case are supported by controlled results.

## Repository responsibilities

- **`trafficmsd`**: maintained R package, extraction algorithms, model representation, user documentation, tests, and stable public API.
- **Experiment assets**: scripts, configurations, derived results, and plots can initially live in a clearly separated repository or directory when their scope justifies it. No paper-specific repository is needed yet.
- **Dataset storage**: use external or institutional storage for large or restricted traces. Keep metadata, provenance, licenses, and checksums in version control rather than committing data that cannot legally or practically be redistributed.

## Immediate priorities

1. Agree on the first model-extraction use case and target traffic quantity.
2. Specify the model representation before implementing generation.
3. Define how extraction quality will be evaluated.
4. Implement a minimal end-to-end extraction example using a redistributable trace.
5. Establish LINE's QPN construct coverage and a minimal headless proof of concept ([#79](https://github.com/vsimko/trafficmsd/issues/79)).
6. Freeze backend-neutral workload and result contracts ([#80](https://github.com/vsimko/trafficmsd/issues/80)).
7. Add QPME/SimQPN as a later independent backend ([#81](https://github.com/vsimko/trafficmsd/issues/81)).
8. Evaluate generated traffic through controlled QPN outcomes ([#82](https://github.com/vsimko/trafficmsd/issues/82)).
9. Keep repository maintenance and release preparation tracked through issues [#10](https://github.com/vsimko/trafficmsd/issues/10) and [#11](https://github.com/vsimko/trafficmsd/issues/11).

See [Research Readiness](docs/RESEARCH_READINESS.md) for the proposed extraction scope, model boundaries, and evaluation requirements. The simulator selection is recorded in [ADR 0002 — LINE-first simulation backend strategy](adr/0002-line-first-simulator.md), with the [backend-neutral adapter contract](simulator-adapter-v1.md).
