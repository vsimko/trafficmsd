# TrafficMSD Technical Roadmap

**Last updated:** 2026-11-03  
**Architecture decision:** [ADR 0001 — Rust as the TrafficMSD simulation core](docs/adr/0001-rust-core.md)

## Direction

TrafficMSD is evolving toward a **Rust-based network traffic simulation engine** with a thin R package for visualization and statistical analysis.

The repository remains a single project. Rust owns computationally intensive traffic processing, model extraction, and simulation. The R package remains a supported client and presentation/analysis layer; it is not the implementation home for the simulation engine.

The first cross-language interface is a native command-line executable. It must be usable independently of R. Direct language bindings are deferred until there is evidence that the CLI boundary is insufficient.

This technical roadmap complements (and does not replace) research planning and research-readiness documents. Scientific validation, datasets, and research questions remain separately tracked.

## Target architecture

```text
                 TrafficMSD Rust workspace
        +-----------------------------------------+
        | trafficmsd-core                         |
        | domain types, algorithms, simulation,   |
        | validation, deterministic execution    |
        +----------------------+------------------+
                               |
        +----------------------v------------------+
        | trafficmsd CLI                          |
        | config, batch runs, input/output, errors|
        +----------------------+------------------+
                               |
                +--------------+--------------+
                |                             |
        +-------v--------+             +------v-------+
        | R package      |             | Other tools  |
        | thin adapter   |             | shell, etc.  |
        | plots + stats  |             | CLI clients  |
        +----------------+             +--------------+
```

## Principles

1. **One computational source of truth:** domain algorithms live in Rust core, not duplicated in CLI or R wrappers.
2. **CLI first:** establish a documented, versioned, machine-readable boundary before adding language-specific bindings.
3. **Keep R useful:** retain plotting, diagrams, and downstream statistical analysis in R.
4. **Incremental migration:** preserve the current R implementation as a reference until Rust behavior is validated.
5. **Reproducibility:** explicit configuration, units, versions, and random seeds; stable machine-readable outputs.
6. **Evidence-led optimization:** benchmark representative workloads before introducing concurrency or lower-level optimizations.
7. **No premature bindings:** defer FFI and other language bindings until justified by measured limitations.

## Milestones

### M0 — Architecture and migration plan

- [x] Record Rust-core decision in ADR 0001.
- [x] Establish this technical roadmap.
- [x] Create implementation issues and agree their dependencies.
- [ ] Define supported platforms and initial distribution expectations.

### M1 — Rust workspace foundation

- [x] Add a Cargo workspace.
- [x] Create a domain-focused core library crate and a separate CLI crate.
- [x] Establish formatting, linting, unit/integration testing, and CI for stable Rust.
- [ ] Define error handling, logging, and version reporting conventions.
- [x] Keep the core independent of R and CLI concerns.

### M2 — Domain and CLI contracts

- [x] Inventory the current computational R API and its documented input/unit gaps in [docs/cli-contract.md](docs/cli-contract.md).
- [x] Define versioned configuration and normalized traffic-data contracts in [docs/cli-contract.md](docs/cli-contract.md) and [config.schema.json](schemas/cli/v1/config.schema.json).
- [x] Specify initial CLI commands, exit codes, diagnostics, and output conventions.
- [ ] Add small, redistributable fixtures and golden/reference results.
- [x] Document deterministic v1 execution and explicit seed requirements for future stochastic operations.

### M3 — Model extraction in Rust

- [ ] Port the existing multiscale decomposition and reconstruction behavior.
- [ ] Port the necessary validation and clustering behavior, based on the algorithm inventory.
- [ ] Keep algorithm semantics separate from serialization and CLI concerns.
- [ ] Add numerical, invariant, edge-case, and reproducibility tests.
- [ ] Compare Rust outputs against the existing R reference implementation.

### M4 — Native CLI

- [ ] Implement CLI commands on top of the Rust core.
- [ ] Support configuration-driven batch runs.
- [ ] Provide documented JSON and CSV input/output where appropriate.
- [ ] Add end-to-end tests from input file to machine-readable result.
- [ ] Verify the executable works without an R installation.

### M5 — Thin R adapter

- [x] Define the initial R/Rust responsibility boundary in [docs/cli-contract.md](docs/cli-contract.md).
- [x] Implement executable discovery and invocation through the standalone CLI.
- [x] Handle paths, process errors, and temporary files safely.
- [x] Preserve the existing R API; expose explicit Rust-backed adapter functions.
- [x] Keep plots, diagrams, and statistical post-processing in R.
- [ ] Add bounded process timeouts and explicit cancellation behavior.
- [ ] Test clean binary installation and adapter execution on every supported platform.

### M6 — Binary distribution and release

- [x] Define supported OS/architecture targets and minimum runtime requirements in [docs/cli-binary-distribution.md](docs/cli-binary-distribution.md).
- [x] Automate native builds and SHA-256 release artifact checks.
- [x] Add the R installer that selects and verifies the matching executable.
- [ ] Test offline/error/fallback behavior and executable version compatibility on all supported platforms.
- [x] Document installation, upgrades, and troubleshooting in [docs/cli-binary-distribution.md](docs/cli-binary-distribution.md).

### M7 — Performance and future interfaces

- [ ] Establish representative benchmarks against the current R implementation.
- [ ] Measure core runtime, end-to-end CLI overhead, memory use, and scaling.
- [ ] Optimize only measured bottlenecks.
- [ ] Reassess direct R bindings only if process overhead or packaging constraints justify them.
- [ ] Consider Python or other bindings only when there is a concrete consumer and maintenance plan.

## Migration acceptance gates

The R implementation is not removed merely because a Rust port exists. A computational operation is eligible to become Rust-backed by default when:

- its inputs, outputs, units, and semantics are documented;
- reference fixtures and edge cases are available;
- Rust tests cover normal, invalid, and boundary inputs;
- cross-implementation comparisons pass under justified numerical tolerances;
- deterministic behavior is verified for fixed seeds where applicable;
- the CLI contract is documented and exercised end to end;
- the R adapter reports failures clearly and preserves agreed public behavior.

## Out of scope for the initial migration

- Rewriting visualization and plotting in Rust.
- Building a network simulator integration before the traffic-generation contract is stable.
- Direct FFI, Python bindings, WASM, or a server mode.
- Large-scale scientific experiments or paper preparation as part of the code migration itself.

## Initial implementation work

The migration is tracked through these GitHub issues:

1. [#32 — Rust workspace and independent core crate](https://github.com/vsimko/trafficmsd/issues/32)
2. [#33 — Versioned traffic inputs, configuration, and CLI contract](https://github.com/vsimko/trafficmsd/issues/33)
3. [#34 — Extract existing multiscale algorithms into Rust core](https://github.com/vsimko/trafficmsd/issues/34)
4. [#35 — Standalone Rust CLI](https://github.com/vsimko/trafficmsd/issues/35)
5. [#36 — R-versus-Rust conformance and reproducibility tests](https://github.com/vsimko/trafficmsd/issues/36)
6. [#37 — Thin R adapter over the Rust CLI](https://github.com/vsimko/trafficmsd/issues/37)
7. [#38 — Cross-platform Rust CLI binary distribution](https://github.com/vsimko/trafficmsd/issues/38)
8. [#39 — Rust versus R performance benchmarks](https://github.com/vsimko/trafficmsd/issues/39)

The recommended dependency order is workspace and contracts first, followed by algorithm extraction and CLI implementation. Conformance testing should gate the R adapter rollout; binary distribution must be designed together with the adapter. Benchmarking begins once equivalent core and CLI paths exist.
