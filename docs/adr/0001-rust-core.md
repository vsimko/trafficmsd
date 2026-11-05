# ADR 0001: Rust as the TrafficMSD simulation core

- **Status:** Accepted
- **Date:** 2026-11-03
- **Decision owners:** TrafficMSD maintainers

## Context

TrafficMSD is currently distributed as an R package. R has been useful for exploratory analysis and for producing plots, but it is not the desired implementation environment for computationally intensive traffic analysis and simulation.

The project's long-term technical direction is network traffic simulation. The core algorithms should be efficient, independently testable, reproducible, and usable from more than one programming environment. The R package remains valuable for visualization, statistical analysis, and integration into established R workflows; it should not own the computationally intensive simulation implementation.

The existing R implementation is also an important behavioral reference during migration. Replacing it in one step would make it difficult to distinguish intended algorithmic changes from migration regressions.

## Decision

TrafficMSD will adopt **Rust as its primary implementation language for the computational and simulation core**.

The repository remains a single project and a single repository. The existing R package is retained as a client/adapter and will progressively become a thin interface around the Rust engine.

The initial architecture is:

- **Rust core library:** owns traffic data structures, validation, model extraction, traffic generation/simulation algorithms, deterministic random-number handling, and core computations. It must not depend on R or the CLI.
- **Rust CLI:** the first stable, language-neutral interface to the core. It supports batch execution, explicit configuration, and machine-readable input/output.
- **R package:** initially invokes the native CLI and reads its outputs. It retains plotting, visualization, and downstream statistical-analysis functionality. Direct native bindings may be considered later if there is a demonstrated need.
- **Other environments:** can use the CLI without an R runtime. Future language bindings may call the Rust library directly, subject to a separate design decision.

A Cargo workspace will separate at least the core library and CLI executable (for example, `crates/trafficmsd-core` and `crates/trafficmsd-cli`).

## Interface and data contracts

The CLI will accept documented configuration and data inputs and produce documented outputs. JSON is the initial control/configuration and interchange format; CSV may be used for tabular traffic data where appropriate. The exact schema, units, time semantics, validation rules, error behavior, and versioning policy must be specified before the CLI is treated as stable.

Core operations must be deterministic when given the same inputs, configuration, and explicit random seed, wherever the algorithm permits deterministic execution. Randomness must not silently depend on or mutate a caller's global random state.

The CLI is an interface, not the owner of domain logic. Business logic belongs in the Rust core.

The choice of an external performance-simulation backend is a separate architectural decision. It must not couple the traffic-model schema or Rust core domain types to one simulator. See [ADR 0002 — LINE-first simulation backend strategy](0002-line-first-simulator.md).

## Migration and compatibility

Migration will be incremental:

1. Establish architecture, workspace, and interface contracts.
2. Extract existing algorithms into Rust while retaining the R implementation as a reference oracle.
3. Add fixtures and cross-implementation tests that compare numerical outputs and documented invariants.
4. Implement the CLI on top of the core.
5. Move R-facing computational functions to the CLI while keeping visualization and statistical-analysis helpers in R.
6. Establish supported binary distribution for the relevant operating systems and CPU architectures.

The migration must preserve existing documented behavior unless a deliberate, documented change is approved. Comparisons should use appropriate numerical tolerances where exact floating-point identity is not realistic. Edge cases, input validation, units, random-seed semantics, and reconstruction invariants must be covered.

The R implementation will not be removed until the Rust implementation has sufficient behavioral coverage and the replacement path is usable by package users.

## Consequences

### Positive

- Computationally intensive work can use Rust's performance and memory-safety properties.
- The engine can be used without installing or embedding an R runtime.
- The CLI creates an early, testable boundary between computation and presentation.
- R remains available for mature plotting and statistical workflows.
- The core can later support additional language bindings without duplicating algorithms.

### Costs and risks

- The project will temporarily maintain two implementations of some algorithms.
- Cross-platform executable builds, packaging, and installation add release engineering work.
- A versioned CLI contract and data schemas require explicit compatibility discipline.
- Numerical differences between R and Rust implementations must be investigated rather than hidden by overly broad tolerances.

## Explicitly deferred

- Direct R FFI (including extendr), Python bindings, Node bindings, and WebAssembly.
- A stable public Rust library API beyond the internal workspace boundary.
- A general plugin system or remote service architecture.
- Rewriting plotting and statistical-analysis functionality in Rust.
- Removing the existing R implementation before parity and migration criteria are met.

## Revisit when

Revisit this decision if binary distribution proves impractical for supported R platforms, if CLI process overhead is material for common workloads, or if a direct binding offers a clear user and maintenance benefit. Any such change should be supported by measured evidence and a new ADR.
