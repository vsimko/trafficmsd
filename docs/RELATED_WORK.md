# Related work and research positioning

**Status:** living research inventory; literature review is not exhaustive  
**Last reviewed:** 2026-11-11  
**Purpose:** preserve relevant approaches for a future paper and guide technical scope decisions.

This document distinguishes direct methodological precedents from complementary simulators, replay tools, and traffic generators. A tool is not called a direct competitor merely because it reads PCAP or can emit packets.

## 1. TrafficMSD research focus

The proposed central contribution is:

> **TrafficMSD aims to derive compact, simulation-ready models of measured network traffic from packet captures, while making the trade-off between model fidelity and model complexity explicit and measurable.**

The research question is not simply how to analyse PCAP files, replay packets, or build another general-purpose network simulator. It is:

> Given a packet trace and a defined simulation use case, what is the smallest model that preserves the traffic properties that materially affect the simulation outcomes?

The intended pipeline is:

1. **Capture input:** ingest a documented PCAP trace and preserve capture provenance, timestamps, packet lengths, direction, and relevant flow metadata.
2. **Feature/signal construction:** derive the explicitly selected traffic representation (initially, measured traffic time series; packet- and flow-level attributes are included only when required by the chosen use case).
3. **Model extraction:** infer a parameterized representation using the TrafficMSD multiscale methods and any justified additional structure.
4. **Compact model selection:** evaluate candidate representations over a fidelity–complexity trade-off and retain a model on the Pareto frontier or satisfying predeclared fidelity constraints.
5. **Simulation realization:** map the selected model into a reproducible workload supported by a documented simulator backend.
6. **Validation:** compare both trace-level properties and simulation-relevant outcomes against the measured reference.

The stages must remain separable. A good deterministic reconstruction is not automatically a stochastic generator; a simulator that correctly executes a supplied workload does not prove that the workload model is faithful to the source capture.

### 1.1 What is in scope

- PCAP-derived network traffic as the empirical source.
- Automatic extraction of a reusable, interpretable, parameterized model.
- Fidelity to explicitly selected temporal, size/rate, burst, and (where applicable) flow characteristics.
- Compactness measured consistently (at least component/parameter count and serialized bytes; runtime and memory can be added where relevant).
- A model representation with clear semantics that can be realized as a simulator workload.
- Reproducible evaluation on held-out portions of traces and independent captures.

### 1.2 What is not the primary goal

Unless a concrete research question requires it, TrafficMSD should not try to become:

- a packet dissector or full protocol-analysis suite;
- a general-purpose packet crafting or line-rate hardware traffic generator;
- a replacement for ns-3, OMNeT++, or other network simulators;
- a general network emulator or record-and-replay environment;
- a full network digital twin covering topology discovery, routing, congestion control, and every protocol layer.

Such tools can be used as upstream parsers, downstream generators, simulators, or validation infrastructure. Reusing them is preferable to rebuilding their mature functionality.

## 2. Fidelity–complexity trade-off

Model quality must not be judged by reconstruction fidelity alone. A model that stores every observed event may reproduce one trace exactly but offer little abstraction or compression. Conversely, a tiny model may discard burstiness, temporal dependence, or flow structure that matters to the intended simulation.

Let:
- (X) be the measured PCAP-derived reference;
- (M) be an extracted model;
- (G(M)) be the trace/workload realized from that model;
- (D_j(X,G(M))) be a predeclared fidelity measure;
- (C(M)) be a consistently measured complexity measure.

Candidate fidelity measures depend on the selected traffic abstraction and may include packet/byte rate, packet-size distribution, inter-arrival distribution, burst statistics, autocorrelation, flow-level characteristics, and simulation-output discrepancies. Candidate complexity measures include component count, free parameter count, serialized size, generation cost, and memory footprint.

The preferred initial formulation is **multi-objective/Pareto evaluation**, not an arbitrary single weighted score:

[
min_M ig(D_1,ldots,D_k, C(M)ig)
]

A model is Pareto-dominated if another model is no worse on every declared objective and strictly better on at least one. Report the Pareto frontier so that the cost of additional fidelity is visible.

For a concrete deployment or experiment, a constrained formulation may be more actionable:

[
min_M C(M) quad 	ext{subject to} quad D_j(X,G(M)) leq epsilon_j
]

The tolerances (epsilon_j) must be declared before final holdout evaluation and justified by the target simulation use case. A weighted objective can be added later if the weights have an interpretable operational meaning; it must not hide the individual metrics.

**Important:** reconstruction fidelity, statistical/generative fidelity, and simulation-outcome fidelity are separate evaluation layers. A model may perform well on one and poorly on another. The paper should state which layer each claim addresses.

## 3. Related approaches

### 3.1 Measurement-driven traffic modelling and generation

#### Harpoon — flow-level traffic generation

- **Reference:** Joel Sommers, Hyungsuk Kim, and Paul Barford, “Harpoon: A Flow-Level Traffic Generator for Router and Network Tests,” ACM SIGMETRICS 2004. [Paper](https://cs.colgate.edu/~jsommers/pubs/harpoon_abstract.pdf) · [Project documentation](https://jsommers.github.io/harpoon/) · [Author's software page](https://pages.cs.wisc.edu/~pb/software_data.html)
- **Approach:** extracts distributional parameters from NetFlow/packet-trace observations and generates application-independent TCP/UDP flows intended to reproduce byte, packet, temporal, and spatial flow characteristics.
- **Relation to TrafficMSD:** one of the closest historical precedents for measurement-derived traffic models and synthetic traffic generation.
- **Difference:** Harpoon focuses on constructive flow-level generation (including file-transfer/session abstractions); TrafficMSD's proposed focus is compact model extraction from PCAP-derived observations, with an explicit fidelity–complexity selection objective and simulation-oriented validation.
- **Possible connection:** compare extracted flow/time-scale summaries with Harpoon-style distributions; use Harpoon as a baseline generator if its input requirements and implementation are suitable. It may also inform which temporal and spatial properties should be preserved.

#### MINT — measurement-driven GenAI traffic modelling

- **Reference:** Andrew Nguyen et al., “MINT: Modeling GenAI Impact on Network Traffic,” ACM WiNTECH 2026. [Paper/preprint](https://arxiv.org/abs/2609.35742) · [Project page](https://wayslab.github.io/mint/)
- **Approach:** measures client-side traffic for GenAI services and modalities, identifies burst regimes, and validates the resulting traffic model in ns-3. The work highlights upload/download asymmetry and burst variability.
- **Relation to TrafficMSD:** a particularly close contemporary example of the measurement → model → network simulation workflow.
- **Difference:** MINT targets GenAI traffic and its workload-specific burst regimes. TrafficMSD should target a more general, explicitly bounded PCAP-to-compact-model problem and make model complexity versus fidelity a first-class optimization/evaluation axis.
- **Possible connection:** reproduce a compatible MINT dataset or trace as an external benchmark; compare burst and temporal descriptors; use ns-3 as an optional downstream validation backend. Do not claim general superiority from a comparison on unrelated datasets.

### 3.2 Network simulation frameworks

#### ns-3

- **Reference:** [Official project overview](https://www2.nsnam.org/about/what-is-ns-3/) · [Documentation](https://www2.nsnam.org/documentation/)
- **Approach:** open-source discrete-event simulator for networking research and education, with protocol/device models, trace collection, and real-time/emulation capabilities.
- **Relation to TrafficMSD:** downstream simulator and possible independent validation target, not a direct PCAP model-extraction competitor.
- **Difference:** ns-3 provides simulation infrastructure and network models; it does not by itself define TrafficMSD's proposed automatic compact model extraction and Pareto selection method.
- **Possible connection:** implement a TrafficMSD workload adapter or use an existing trace/application interface; compare simulator-relevant outcomes against LINE/QPN where semantics can be matched.

#### OMNeT++ and INET

- **Reference:** [OMNeT++ overview](https://omnetpp.org/intro/) · [INET documentation](https://inet.omnetpp.org/Introduction.html)
- **Approach:** OMNeT++ is a modular discrete-event simulation framework. INET supplies reusable wired, wireless, Internet protocol, mobility, and application models.
- **Relation to TrafficMSD:** alternative downstream simulation ecosystem and possible cross-validation backend.
- **Difference:** these projects provide simulation infrastructure and protocol models rather than the specific automatic PCAP-to-compact-model extraction method proposed for TrafficMSD.
- **Possible connection:** export a TrafficMSD-generated workload into an INET/OMNeT++ experiment; use the common supported scenario to check whether conclusions depend on the simulator backend.

#### LINE Solver / QPN

- **Reference:** repository decision and validation records: [LINE-first backend ADR](adr/0002-line-first-simulator.md) · [QPN simulator adapter contract](simulator-adapter-v1.md) · [LINE validation](line-solver-validation.md)
- **Approach:** current selected backend for queueing-network performance evaluation, including the repository's QPN workload experiments.
- **Relation to TrafficMSD:** part of the intended implementation/validation stack, not an external competitor.
- **Boundary:** a valid QPN run proves that the authored QPN/workload was executed consistently; it does not by itself prove packet-level network fidelity or validate the PCAP extraction algorithm.

### 3.3 Record/replay and network emulation

#### Mahimahi

- **Reference:** Ravi Netravali et al., “Mahimahi: A Lightweight Toolkit for Reproducible Web Measurement,” ACM SIGCOMM 2014. [Paper](https://mahimahi.mit.edu/mahimahi_atc.pdf) · [Project](https://mahimahi.mit.edu/)
- **Approach:** records HTTP application traffic and replays it under emulated network conditions using composable shells.
- **Relation to TrafficMSD:** relevant to reproducible measurement and replay experiments.
- **Difference:** it preserves and replays application/web activity under controlled network conditions; it is not primarily a compact, general-purpose model extractor from arbitrary PCAP traces.
- **Possible connection:** use it to create controlled capture conditions or evaluate an extracted workload in a repeatable web/network experiment.

#### tcpreplay

- **Reference:** [Official documentation](https://tcpreplay.appneta.com/) · [Timing and speed control](https://tcpreplay.appneta.com/guide/concepts/timing-and-speed.html)
- **Approach:** replays captured packets onto a network interface, preserving original packet timing or applying a selected rate; it also supports packet rewriting.
- **Relation to TrafficMSD:** useful baseline for exact trace replay and a reference point for the value added by abstraction.
- **Difference:** replay reproduces a recorded packet sequence; it does not infer a smaller parameterized model or generalize to new traffic sequences.
- **Possible connection:** compare direct PCAP replay with TrafficMSD-generated workloads in a controlled testbed; use it as the high-detail reference where replay is feasible.

#### Ostinato

- **Reference:** [Features](https://ostinato.org/features) · [Documentation](https://ostinato.org/docs/)
- **Approach:** packet crafting, L2/L3 traffic generation, PCAP editing/replay, stream scheduling, and protocol/performance testing.
- **Relation to TrafficMSD:** a practical downstream traffic-generation/test tool.
- **Difference:** emphasizes explicit packet/stream construction and test execution, rather than automatic compact model inference from measured traces.
- **Possible connection:** export a generated packet/stream profile when a hardware or lab test is needed; do not duplicate its packet crafting functionality inside TrafficMSD.

#### Cisco TRex

- **Reference:** [TRex documentation](https://trex-tgn.cisco.com/trex/doc/) · [Stateless mode and PCAP support](https://trex-tgn.cisco.com/trex/doc/trex_stateless.html)
- **Approach:** high-performance stateful/stateless traffic generation, programmable packet streams, and PCAP-based traffic profiles.
- **Relation to TrafficMSD:** possible high-rate execution backend for workloads derived from measured traces.
- **Difference:** focuses on generating traffic at scale and testing network devices; it does not target the extraction and compactness optimization problem.
- **Possible connection:** translate selected TrafficMSD model outputs into TRex profiles and test whether the compact representation can drive realistic lab traffic.

## 4. Positioning summary

| Approach | Main abstraction | Main role | Relationship to TrafficMSD |
|---|---|---|---|
| Harpoon | Flow/session distributions | Measurement-derived flow generation | Closest historical modelling/generation precedent |
| MINT | GenAI burst regimes | Measurement-derived model + ns-3 validation | Closest contemporary workflow precedent |
| ns-3 | Protocols, nodes, links, events | Network simulation | Downstream simulator |
| OMNeT++ / INET | Modular network/protocol models | Network simulation | Downstream simulator |
| LINE / QPN | Queueing-network workload and service model | Performance simulation | Current selected backend |
| Mahimahi | Recorded HTTP sessions and network conditions | Record/replay and emulation | Complementary experimental infrastructure |
| tcpreplay | Original captured packet sequence | Exact packet replay | High-detail replay baseline |
| Ostinato | Explicit packet streams | Traffic crafting and testing | Optional execution backend |
| TRex | Programmable high-rate streams/flows | Traffic generation and testing | Optional execution backend |

The most important distinction is that TrafficMSD's intended contribution lies **between packet capture and simulation**: infer a compact representation from measurements, quantify what information is retained or discarded, and provide a controlled route from the selected model to a simulator workload.

## 5. Proposed research claims and evidence requirements

The following are hypotheses to test, not claims already established:

1. TrafficMSD can extract a model automatically from documented PCAP-derived input.
2. A model can be substantially smaller than the raw trace or a lossless event representation while preserving predeclared traffic properties.
3. Fidelity can be controlled through an explicit model-selection procedure, producing an interpretable fidelity–complexity Pareto frontier.
4. The selected model can drive a reproducible workload in at least one simulator.
5. The resulting workload preserves the traffic characteristics and simulation outcomes relevant to the chosen use case.
6. Against appropriate baselines (including exact replay, simple statistical models, and relevant measurement-driven generators), TrafficMSD offers a useful compactness/fidelity trade-off.

To support these claims, experiments should use identical source traces, explicit train/validation/holdout separation, consistent units and time windows, predeclared metrics, repeated stochastic runs where applicable, and reported uncertainty. A single Westermo trace and a single FCFS QPN experiment are not sufficient to establish generality.

## 6. Literature-review maintenance

For each newly identified work, add:
- full bibliographic reference and stable DOI/arXiv/project URL;
- publication year and current project status where verifiable;
- input data type (PCAP, NetFlow, application logs, synthetic workload, etc.);
- model abstraction (packet, flow, burst, time series, queueing model);
- whether it extracts, generates, replays, emulates, or simulates;
- fidelity and complexity metrics, if any;
- what can be reused or integrated;
- precise difference from the TrafficMSD research question.

Before using this inventory in a paper, verify the final publication metadata, version, licensing, and exact technical claims from the primary paper or official project documentation. The inventory is a starting point, not a completed systematic literature review.
