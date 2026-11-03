# Research and Development Roadmap

## Strategic direction

The primary research direction for `trafficmsd` is **network traffic simulation**. The goal is to develop a reproducible method that transforms measured network traffic into statistically validated traffic models and generators suitable for performance simulation.

The current R package provides multiscale decomposition, reconstruction, and amplitude clustering. These capabilities are a foundation for model extraction; they do not, by themselves, establish that generated traffic is statistically realistic or suitable for a target simulator.

## Guiding principles

- Preserve the existing package as the reusable algorithmic core.
- Separate signal decomposition, traffic-model representation, traffic generation, and simulator integration into explicit stages.
- Evaluate generated traffic against measured traces and relevant baseline models.
- Make experiments reproducible through versioned configurations, data provenance, fixed seeds, and machine-readable results.
- Keep research claims proportional to the evidence and document limitations.

## Work plan

### Phase 0 — Research readiness

- Agree on research questions, scope, terminology, and intended simulation use cases.
- Define the formal representation of an extracted traffic model.
- Specify datasets, baselines, metrics, and acceptance criteria.
- Decide how experiment code, results, and paper sources will be versioned.

**Exit criteria:** the research protocol is specific enough that an independent researcher can implement and reproduce the first evaluation.

### Phase 1 — Reproducible evaluation

- Build a repeatable pipeline for loading and preprocessing traces.
- Record dataset provenance, licenses, checksums, preprocessing parameters, and train/test splits.
- Implement baseline models and common evaluation metrics.
- Export machine-readable results and figures from scripted runs.

**Exit criteria:** a clean checkout can reproduce the baseline evaluation and its reported metrics.

### Phase 2 — Traffic model and generator

- Formalize the mapping from multiscale decomposition outputs to a traffic-model description.
- Specify how model parameters represent scale, amplitude, temporal placement, and stochastic variation.
- Implement a generator that can produce new traffic sequences from the model.
- Test numerical correctness, parameter sensitivity, and repeatability.

**Exit criteria:** generated sequences can be produced from a documented model format and evaluated independently of the extraction implementation.

### Phase 3 — Statistical and simulation validation

- Compare generated and measured traces across first-order, temporal, burstiness, and frequency-domain properties.
- Compare against appropriate baselines, such as Poisson, ON/OFF, and Markov-modulated models where their assumptions fit the trace.
- Evaluate the impact on a selected performance simulator or queueing model.
- Document where the method performs well and where it does not.

**Exit criteria:** claims about statistical fidelity and simulation usefulness are supported by controlled experiments.

### Phase 4 — Publication and dissemination

- Prepare a paper with a reproducible artifact and clearly stated limitations.
- Archive datasets or permitted derived artifacts with persistent identifiers where possible.
- Publish software releases and experiment snapshots associated with each paper.
- Consider simulator adapters and additional traffic domains based on evidence from the first evaluation.

**Exit criteria:** the paper, code, experiment configuration, and accessible artifacts are mutually traceable.

## Repository responsibilities

- **`trafficmsd`**: maintained R package, algorithms, user documentation, tests, and stable public API.
- **Paper/experiment repositories**: paper-specific LaTeX, experiment orchestration, derived results, plots, and exact dependency/configuration snapshots. These may remain private during development; publish or archive appropriate artifacts when ready.
- **Dataset storage**: use external or institutional storage for large or restricted traces. Keep metadata, provenance, licenses, and checksums in version control rather than committing data that cannot legally or practically be redistributed.

## Immediate priorities

1. Complete the research-readiness specification and agree on the first research question.
2. Define the traffic-model representation before implementing a generator.
3. Establish the evaluation protocol and reproducible experiment pipeline.
4. Select one initial simulator integration target after the model and evaluation requirements are clear.
5. Address repository maintenance and release preparation through the existing issues [#10](https://github.com/vsimko/trafficmsd/issues/10) and [#11](https://github.com/vsimko/trafficmsd/issues/11).

See [Research Readiness](docs/RESEARCH_READINESS.md) for the proposed questions, model boundaries, and validation protocol.
