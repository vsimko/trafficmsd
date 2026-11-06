# TrafficMSD generator and simulator adapter contract v1

**Status:** neutral v1 schemas and fixture validator implemented; generator/adapter conformance remains in progress  
**Initial target:** LINE Solver (QPN-capable simulation backend)  
**Planned second backend:** QPME / SimQPN for independent cross-validation  
**Scope:** workload-level performance simulation using Queueing Petri Nets (QPNs)

The machine-readable v1 contract and validation rules are specified in [simulator-contract-v1.md](simulator-contract-v1.md), with JSON Schemas under `schemas/simulator/v1/` and deterministic fixtures under `experiments/simulator-contract-v1/`.

## 1. Purpose and boundary

TrafficMSD's first simulator integration will evaluate whether a generated workload derived from an extracted traffic model changes the performance results of a QPN system model. It is not a packet-level network simulator and does not model TCP/IP, routing, congestion control, or link-layer behavior.

The adapter is deliberately downstream of model extraction and traffic generation:

1. The model layer stores the versioned deterministic description.
2. The generator turns that description into a reproducible time series or arrival schedule.
3. The adapter maps the generated workload into a simulator-neutral arrival schedule.
4. A simulator-specific bridge feeds that schedule to the selected backend (LINE first; QPME/SimQPN later).
5. The evaluation layer compares measured-trace and generated-trace runs under identical QPN configurations.

No simulator-specific fields belong in the traffic-model schema.

## 2. Simulator strategy

Use **LINE Solver** as the first implementation and experimental backend. Add **QPME/SimQPN** later as a second, independent QPN backend.

### Why LINE first

- LINE is an actively maintained queueing-network analysis and simulation framework with native discrete-event simulation support.
- It documents support for Queueing Petri Nets (QPNs) and stochastic Petri nets, alongside conventional queueing networks.
- It provides programmatic integration paths (including Python-facing and process/JSON interfaces), which are better suited to reproducible experiments and CI than GUI-only automation.
- Its open-source BSD-3-Clause license is compatible with evaluating it as an optional external backend; TrafficMSD must still record the exact LINE release and its own dependency/licensing notices.

LINE's advertised QPN support is not proof that every QPME/SimQPN construct has identical semantics. Before relying on LINE for a scientific comparison, build a small compatibility matrix for the exact QPN constructs required by the experiment (places, transitions, token colours/classes, queue disciplines, routing, service-time distributions, batch arrivals, and measured outputs). Unsupported constructs must be recorded, not silently approximated.

### Why retain QPME/SimQPN as the second backend

QPME/SimQPN is a QPN-specialist environment and provides a useful independent reference implementation. It will be integrated only after the LINE path has a reproducible minimal experiment. The purpose is cross-validation of model semantics and outcomes, not to designate either simulator as ground truth.

### Backend decision gates

1. Verify the exact LINE version, license, documented headless invocation, model format, QPN construct coverage, and machine-readable result export.
2. Implement a minimal LINE proof of concept with one source, one queue, one service transition, and a measurable completion/throughput result.
3. Compare results with hand-calculated expectations for a deliberately simple deterministic case.
4. Freeze and validate the backend-neutral schedule and result contracts before implementing the QPME/SimQPN bridge. The initial schema/fixture set is now versioned; actual LINE/QPME adapter conformance remains a separate gate.
5. Add QPME/SimQPN and compare both backends on the same supported model subset, input schedule, and experiment settings.

If LINE cannot represent a required construct without changing its semantics, stop and document the gap. Do not weaken the QPN model silently to fit the backend.

References:
- LINE Solver: https://line-solver.sourceforge.net/
- LINE interoperability: https://line-solver.sourceforge.net/interop.html
- QPME: https://github.com/DescartesResearch/QPME

## 3. Generator API contract

The first generator interface consumes a validated TrafficMSD model and an explicit generation configuration.

Conceptual signature:

```text
generate_traffic(model, config) -> TrafficSeries
```

Required configuration:

- `schema_version`: generator configuration version.
- `duration`: positive finite duration.
- `sampling_interval`: positive finite interval.
- `time_unit`: explicit time unit.
- `value_unit`: explicit model quantity unit.
- `seed`: explicit unsigned integer for stochastic generation; null is allowed only for a deterministic generator.
- `generation_mode`: explicit deterministic or stochastic mode.

The returned `TrafficSeries` contains:

- strictly increasing, regular sample indices beginning at zero;
- finite, non-negative values;
- declared time and value units;
- sampling interval and observed duration;
- source model identifier and schema version;
- generator name/version, configuration, and seed;
- no implicit unit conversion or silent clipping.

A deterministic v1 replay generator may reproduce the serialized reconstruction. It is a baseline, not evidence of stochastic realism. A later stochastic generator must document its distributions, temporal dependence, fitting procedure, and fixed-seed semantics separately.

## 4. Simulator-neutral arrival schedule

The adapter must not pass a byte-valued time series directly into a QPN backend. QPN arrivals represent workload entities (tokens/jobs), so conversion from bytes to jobs requires an explicit workload mapping.

The first supported mapping is backend-neutral and consists of:

- `bytes_per_job`: positive finite number of bytes represented by one arriving job;
- `arrival_placement`: `bin_start` for v1;
- `remainder_policy`: `carry_forward` for v1.

For each input bin, compute the number of newly released jobs from the cumulative byte total:

```text
jobs_through_bin(k) = floor(cumulative_bytes_through_bin(k) / bytes_per_job)
jobs_in_bin(k) = jobs_through_bin(k) - jobs_through_bin(k - 1)
```

This cumulative quantization preserves total represented volume up to the final fractional remainder and avoids independently rounding every bin. The final remainder must be reported in the schedule metadata. Negative values, non-finite values, unit mismatches, and overflow are errors.

The v1 schedule is a backend-neutral table with:

| Field | Meaning |
|---|---|
| `time` | Arrival time in declared simulation time units |
| `job_count` | Non-negative integer batch size |
| `bytes_per_job` | Workload mapping used for this schedule |
| `source_bin` | Zero-based source bin index |

The adapter must preserve empty bins as no-arrival intervals. It must not invent within-bin arrival times. `bin_start` means all jobs in a batch become available at the start of the source bin; alternative placement policies require a later schema version and separate evaluation.

This mapping is a workload abstraction. It does not claim that one job equals one packet, flow, request, or application transaction unless the experiment explicitly defines that interpretation.

## 5. Simulator backend bridge

A backend-specific bridge consumes the neutral arrival schedule and a user-authored QPN model. The first implementation targets LINE; a later implementation targets QPME/SimQPN.

Responsibilities:

- validate simulator version and model compatibility;
- validate that every QPN construct used by the experiment is supported with the intended semantics;
- map each scheduled job to the configured QPN entry place and token color;
- preserve batch time and count without reordering;
- use the QPN model's own service-time and routing definitions;
- record the exact simulator model and configuration hashes;
- run the requested replications and warm-up policy;
- export raw observations and summary metrics in a machine-readable form.

The bridge must not create or infer queue topology, service rates, routing probabilities, or resource capacities from the TrafficMSD model. Those belong to the QPN system model and its experiment configuration.

The LINE integration must use a documented programmatic/headless interface. Do not depend on undocumented GUI automation. The later QPME/SimQPN bridge is subject to the same requirement; if no stable headless interface exists, stop and reassess rather than automating GUI interactions.

## 6. Backend-neutral experiment and results

The experiment definition must be independent of backend-specific command-line flags and file formats. It records the workload schedule checksum, authored QPN model identifier/checksum, backend identifier and exact version, simulation clock/time unit, warm-up policy, termination condition, replication count, random seeds, and requested metrics.

A normalized result record must preserve backend identity and version and must not erase backend-specific raw observations. At minimum, retain raw per-replication observations where available, aggregate estimates, uncertainty/confidence intervals, and explicit missing/unsupported metric markers. A backend must fail validation when it cannot provide a required metric or model construct; it must not substitute a different metric without documenting the mapping.

## 7. Simulation outputs and comparison

At minimum, retain:

- offered workload volume and job arrivals;
- completed jobs and throughput;
- mean and distribution of queue population where available;
- queue utilization where applicable;
- token/job residence time, including mean, standard deviation, and confidence interval where supported;
- run duration, warm-up, replication count, and termination reason.

Compare two input conditions under the same QPN model and simulator configuration:

1. measured-trace reference schedule;
2. TrafficMSD-generated schedule.

Use identical workload mapping, system model, warm-up, termination policy, and replication strategy. For stochastic generation, use independent documented seeds and report between-run uncertainty. A deterministic replay must be kept as a separate control.

Do not compare simulator results from different QPN configurations and attribute the difference to the traffic model.

## 8. Reproducibility and validation gates

Every run records:

- TrafficMSD commit, model schema version, model identifier, and model checksum;
- generator version, full configuration, and seed;
- source trace provenance and preprocessing manifest;
- workload mapping and generated arrival-schedule checksum;
- backend name and exact version, QPN model checksum, capability-validation result, and simulator settings;
- operating system/runtime versions, replication count, warm-up, and output artifacts.

Required tests before calling the adapter supported:

- deterministic schedule generation for a fixed model and configuration;
- cumulative byte-to-job conversion, including fractional remainder and empty bins;
- invalid units, invalid values, overflow, and boundary-time handling;
- stable schedule serialization and checksum;
- simulator bridge conformance against a small hand-calculated arrival schedule;
- measured-trace and generated-trace reference runs under identical QPN settings.

## 9. Explicit limitations and deferred work

- No packet-level protocol, network topology, link, or congestion simulation.
- No inference of jobs/requests from packet bytes without a declared workload mapping.
- No automatic construction of QPN system models.
- No simulator-specific stochastic service-time fitting in TrafficMSD.
- QPME/SimQPN is intentionally deferred until the LINE proof of concept and backend-neutral contracts are reproducible.

Implementation tracking: [#79 — LINE QPN compatibility and headless integration](https://github.com/vsimko/trafficmsd/issues/79), [#80 — simulator-neutral workload/result contracts](https://github.com/vsimko/trafficmsd/issues/80), [#81 — QPME/SimQPN second backend](https://github.com/vsimko/trafficmsd/issues/81), and [#82 — simulation-outcome evaluation](https://github.com/vsimko/trafficmsd/issues/82).

The contract can be revised after the LINE proof of concept verifies a concrete QPN workload case and its documented headless interface.