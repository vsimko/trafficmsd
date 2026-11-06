# Simulator-neutral contract v1 (issue #80)

**Status:** initial versioned schemas and fixture validator; adapter conformance remains separate work.
**Scope:** workload arrival batches, experiment intent, backend capabilities, and normalized run results.
**Schema dialect:** JSON Schema Draft 2020-12.

## Separation of concerns

The TrafficMSD model and experiment do not contain LINE flags, QPME file formats, solver class names, or backend command lines.

- **Arrival schedule** is an ordered series of time-binned job batches. It declares simulation time units, bin width, bin-start placement, byte-to-job mapping, remainder policy, and generator provenance.
- **Experiment** identifies the authored QPN model and schedule by SHA-256, then declares warm-up, termination, replication seeds, required metrics, and required model capabilities.
- **Backend capabilities** are reported separately by an adapter. They identify backend/version and explicitly enumerate supported constructs, metrics, warm-up policies, and termination kinds.
- **Normalized result** records backend/version/engine, input checksums, per-replication seeds and raw observations, and per-metric estimates or explicit unavailable states.

The schemas are under [schemas/simulator/v1](../schemas/simulator/v1/). The fixture set is under [experiments/simulator-contract-v1](../experiments/simulator-contract-v1/).

## Arrival schedule semantics

Version 1 supports a regular binned schedule with these fixed semantics:

- `time_unit` is one of `s`, `ms`, `us`, or `ns`; `bin_width` is a positive duration in that unit.
- `batches` contains one record for every source bin, including bins with `job_count: 0`. `source_bin` starts at zero, is contiguous, and `time = source_bin * bin_width`.
- `arrival_placement: bin_start` makes the batch available at the beginning of its source bin. No within-bin timing is inferred.
- The only v1 quantity conversion is bytes to jobs, with positive `bytes_per_job` and `remainder_policy: carry_forward`. The fractional final byte remainder is reported and must be smaller than one job.
- Deterministic generation uses `seed: null`; stochastic generation requires an explicit unsigned seed.

The schedule describes workload entities, not packets, flows, requests, or transactions unless the experiment author explicitly defines that interpretation. It does not infer QPN topology, service rates, routing, or queue capacities.

## Experiment and capability negotiation

The experiment is backend-neutral. `warmup` supports `none`, `fixed_time`, and `mser5`; fixed-time warm-up requires a positive duration. Termination is either a simulated-time limit or a completion count. Replication count must equal the number of explicitly supplied seeds.

Before execution, the adapter must compare every requested construct, metric, warm-up policy, and termination kind with its capability declaration. Any mismatch is a hard validation error; no silent substitution is permitted. The experiment's `required_metrics` must be included in the declared metric requirements.

Capability declarations are descriptive, not proof of correctness. An adapter must only advertise semantics that it has tested and documented for its exact backend version.

## Results and failure semantics

A result includes backend identity/version and engine, checksums of the model/schedule/experiment, per-replication termination reason and raw observations, and normalized metric records.

Every requested metric must appear. A metric that cannot be produced is represented as `unsupported` or `not_recorded`, with a reason and null estimate. If any required metric is unavailable, `validation` must be `failed`. A backend error or incomplete replication must remain visible; it must not be converted into a successful aggregate. Raw observations are retained rather than replaced by aggregates.

## Stable serialization and checksums

The reference validator uses canonical JSON: UTF-8, recursively sorted object keys, compact separators, no NaN/Infinity, and one trailing newline. SHA-256 is computed over those bytes. Input files themselves remain versioned artifacts; consumers must use the same canonicalization rule when producing logical-object hashes. A file-byte checksum and a canonical-object checksum are distinct and must not be interchanged.

JSON Schema validates shape and basic ranges. The semantic validator additionally checks contiguous bins and preserved empty bins, exact bin-start times, remainder bounds, seed/mode consistency, replication/seed counts, warm-up requirements, capability compatibility, required metric presence, and result checksum consistency.

## Validation

Run locally with Python 3.11+:

```sh
python -m pip install "jsonschema>=4.20,<5"
python scripts/validate_simulator_contracts.py
python -m unittest discover -s tests -p "test_simulator_contracts.py" -v
```

GitHub Actions runs the same checks in [Simulator-neutral contract checks](../.github/workflows/simulator-contracts.yml).

The fixtures are contract tests, not simulator output and not evidence of LINE/QPME equivalence. Existing LINE integration remains pinned and tested independently in [LINE Solver validation](line-solver-validation.md).

## Deliberate v1 limits

- No arbitrary quantity units or implicit unit conversion.
- No sub-bin arrival placement, irregular bins, or alternative remainder policies.
- No automatic model translation or backend command configuration.
- No inferred confidence interval method; backend adapters must document the estimator and interval method in run metadata when adding statistical summaries.
- No claim that a capability declaration alone establishes semantic parity.

Changes to these semantics require a new schema version or an explicitly compatible extension. Issue #81 can implement a second backend against this contract after adapter conformance tests are added.
