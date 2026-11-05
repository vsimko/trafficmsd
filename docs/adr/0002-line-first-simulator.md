# ADR 0002: LINE-first simulation backend strategy

- **Status:** Accepted
- **Date:** 2026-11-05
- **Decision owners:** TrafficMSD maintainers
- **Related:** [ADR 0001 — Rust as the TrafficMSD simulation core](0001-rust-core.md)

## Context

TrafficMSD's selected product direction is network traffic simulation, with the first integration scoped to workload-level performance simulation using Queueing Petri Nets (QPNs). The existing simulator adapter contract originally selected QPME/SimQPN first. A review of available systems identified LINE Solver as a promising alternative because it combines queueing-network analysis and discrete-event simulation, documents QPN support, and provides programmatic integration paths.

A simulator is an external execution backend, not the owner of TrafficMSD's traffic-model semantics. The model, generated workload, and experiment configuration must remain independent of the selected backend. A second implementation is valuable for checking whether observed results depend on a particular simulator.

LINE's advertised QPN support does not establish complete semantic equivalence with QPME/SimQPN. Exact construct coverage and headless execution must be verified before the backend is treated as supported.

## Decision

1. **Use LINE Solver as the first backend to investigate and implement.** The first deliverable is a minimal reproducible proof of concept, not a claim that LINE is universally superior.
2. **Retain QPME/SimQPN as a planned second backend.** Integrate it after the LINE proof of concept and backend-neutral contracts are stable. Its purpose is independent cross-validation, not designation as ground truth.
3. **Keep simulator integration behind a backend-neutral contract.** TrafficMSD owns versioned traffic models, generated series/arrival schedules, workload mapping, experiment metadata, and normalized result records. Backend bridges own serialization, invocation, capability checks, and output extraction.
4. **Require documented headless/programmatic operation.** Do not build a supported integration on undocumented GUI automation.
5. **Reject unsupported semantics explicitly.** Each backend must declare/validate the QPN constructs used by an experiment. No silent approximation or model weakening to fit a backend.
6. **Keep the initial scope at workload-level QPN performance simulation.** Packet-level protocol, routing, congestion-control, and link-layer behavior require a separate network simulator and a separate decision.

## Initial LINE proof of concept

The first experiment will use a deliberately small authored QPN model: source/arrival schedule → queue → service → completion. It must:

- pin the LINE release and runtime;
- verify license and documented headless invocation;
- establish a construct-coverage matrix for places, transitions, token classes/colours, queue disciplines, routing, service-time distributions, batch arrivals, and required metrics;
- use a backend-neutral, checksummed arrival schedule;
- compare deterministic output with hand-calculated expectations;
- export machine-readable observations and record configuration/model hashes.

The exact QPN subset is determined by the target use case, not by whichever subset is easiest to express in LINE.

## Cross-validation with QPME/SimQPN

Once LINE and the neutral contracts are reproducible, implement QPME/SimQPN as a second bridge. Compare both systems only on a common, explicitly documented semantic subset and identical schedules, model assumptions, warm-up, termination, and replication policies. Investigate differences rather than averaging or selecting the preferred output.

## Consequences

### Positive

- Programmatic experiments and CI can be developed against LINE first.
- The simulator choice remains replaceable because model and schedule contracts are backend-neutral.
- QPME/SimQPN provides a later independent cross-check.
- Unsupported QPN features and semantic differences become explicit research evidence.

### Costs and risks

- LINE's QPN support may not cover every construct needed by the intended experiment.
- Different simulators may implement or interpret constructs and metrics differently.
- Maintaining multiple bridges adds code and test burden.
- Workload-level QPN simulation does not validate packet-level networking behavior.

## Deferred

- QPME/SimQPN implementation until LINE and neutral contracts pass their initial gates.
- OMNeT++/INET or ns-3 integration; revisit only if the research question requires packet/protocol/network-topology behavior.
- Automatic inference of QPN topology, service rates, or routing from traffic traces.
- Claims of simulator equivalence or scientific validity before controlled comparison.

## Revisit when

Revisit this decision if LINE cannot express the required QPN model without semantic compromises, lacks a stable documented headless interface, or fails the deterministic proof-of-concept checks. The fallback is to reassess QPME/SimQPN or another suitable backend while preserving the backend-neutral contracts.
