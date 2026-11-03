# TrafficMSD CLI and data contract — schema version 1

**Status:** Initial contract for implementation in the Rust workspace  
**Scope:** File-based, language-neutral interface for regularized traffic signals

This document defines the first CLI boundary. It deliberately separates the transport
contract (files, metadata, commands, errors) from the scientific traffic-model
representation tracked in [research issue #27](https://github.com/vsimko/trafficmsd/issues/27).

## 1. Existing R API inventory relevant to the CLI

The current public computational boundary has these behaviors:

| R function | Current responsibility and observed contract | Migration note |
|---|---|---|
| `read_traffic()` | Reads CSV columns `Time` and `Length`; both must be finite numeric vectors. Applies `use.expfactor` to timestamps, aggregates on integer time keys (default `sum`), converts to a regular time series, optionally pads to a power of two, then replaces remaining missing values with zero. | Raw-event ingestion is not part of CLI schema v1. Timestamp units, length units, and the historical aggregation details need an explicit compatibility decision before porting. |
| `pad_power2()` | Requires at least 16 finite numeric observations; appends zeros to the next power-of-two length and never truncates. | Padding must remain explicit and preserve the original length as metadata where later trimming is required. |
| `decompose_traffic()` | Requires at least 16 finite numeric observations and a power-of-two length. Uses `cutoff.fraction` in (0,1), default 0.004. Returns `length`, `maxscale`, and a coefficient matrix; row 1 is the remainder and row `scale + 1` stores that scale. | Algorithmic details and numerical equivalence belong to issue #34/#36, not the transport schema. |
| `reconstruct_traffic()` | Accepts a coefficient matrix or an object containing `coef`; computes emitted contributions and returns their column sums. | Reconstruction invariants and coefficient interchange must be established against the R reference. |
| `estimate_clusters()` | Uses k-means within-cluster sums of squares; default `seed = 1`, `nstart = 25`; validates finite numeric input and preserves the caller's RNG state when a local seed is supplied. | Stochastic operations need explicit seed and RNG-version metadata in a future versioned command contract. |
| `cluster_by_amplitude()` | Quantizes numeric samples using k-means; default `seed = 1`, `nstart = 25`; returns fitted clusters, centers, quantized data, absolute errors, and MAE. | Cluster-label ordering and cross-language RNG equivalence need dedicated conformance tests. |
| `optimize_intervals()` (internal) | Removes short isolated intervals according to `cutoff.fraction`, then interpolates small gaps in the coefficient vector. | Part of the decomposition algorithm; preserve exact boundary and interpolation behavior in the Rust port. |

The current package does not consistently declare the physical units of
`Time` and `Length` in the raw-event reader. Therefore the normalized v1
contract requires callers to supply `sampling_interval`, `time_unit`, and
`value_unit`; it does not infer them from legacy packet files.

## 2. Scope and non-goals

Version 1 accepts an already regularized, uniformly sampled numeric signal. It
does not yet define raw packet/event ingestion, trace timestamp normalization,
model extraction, stochastic traffic generation, or simulator-specific adapters.

This is intentional. The existing R `read_traffic()` function accepts packet
CSV files with `Time` and `Length`, applies a time scaling and integer-time
aggregation, and optionally pads the result. Its timestamp and aggregation
semantics must be migrated separately after a dedicated compatibility review.
They must not be silently inferred from the normalized-signal format below.

The first computational operations are:

- `validate`: validate configuration and input signal without producing a result.
- `decompose`: produce multiscale coefficients from a regularized signal.
- `reconstruct`: reconstruct a signal from a previously serialized coefficient
  matrix. The exact coefficient interchange representation will be finalized
  with the algorithm extraction work in issue #34.

The initial executable also supports `--help` and `--version`.

## 3. File and path conventions

The CLI is invoked as:

```sh
trafficmsd <command> --config path/to/config.json [--force]
```

The configuration file is UTF-8 JSON. Paths inside it are resolved relative
to the directory containing the configuration file, not the process working
directory. Absolute paths are accepted.

Input and output are regular files. The CLI must not write diagnostics or
progress messages to standard output; diagnostics go to standard error.
Commands that create output files must not overwrite existing files unless
`--force` is supplied. Output creation should be atomic where the platform
allows it (write a temporary sibling file, then rename).

## 4. Configuration schema v1

A configuration is a JSON object with these required fields:

| Field | Type | Meaning |
|---|---|---|
| `schema_version` | integer | Must be `1`. |
| `command` | string | `validate`, `decompose`, or `reconstruct`. |
| `input_csv` | string | Path to the normalized signal CSV. |
| `output_csv` | string | Output path for commands that produce tabular results. |
| `signal` | object | Required time and value metadata. |
| `parameters` | object | Command-specific parameters; may be empty for validation. |

The `output_csv` field is required for `decompose` and `reconstruct`, and omitted for `validate`.

The `signal` object contains:

- `sampling_interval`: positive finite number, expressed in `time_unit`.
- `time_unit`: one of `s`, `ms`, `us`, or `ns`.
- `value_unit`: non-empty string naming the measured quantity, for example
  `bytes`, `packets`, or `bits`. The CLI treats this as metadata and does
  not convert units.

For `decompose`, `parameters.cutoff_fraction` is required and must be finite
and strictly between zero and one. Its initial default-compatible value is
`0.004`, matching the current R API; callers should nevertheless provide it
explicitly for reproducible experiment configurations.

All operations in schema v1 are deterministic and therefore accept no random
seed. A future stochastic operation must introduce an explicit unsigned
64-bit seed in a versioned contract; it must never silently consume a
process-global random stream or choose an undocumented seed. The seed, RNG
algorithm/version, and operation version must be recorded in result metadata.
Reproducibility is guaranteed within the same TrafficMSD release and supported
platform; cross-release bitwise identity must not be assumed unless explicitly
documented.

Unknown configuration fields are rejected in schema v1. A change that alters
meaning, required fields, units, or interpretation requires a new schema
version. A newer unsupported schema version must fail clearly; the CLI must
not guess, silently coerce, or auto-upgrade it.

## 5. Normalized signal CSV

For `validate` and `decompose`, the input CSV must be UTF-8, have a header,
and contain exactly these columns:

```csv
sample_index,value
0,12
1,18.5
2,7
3,25
```

Rules:

- `sample_index` is a zero-based integer sequence beginning at zero, with
  no gaps, duplicates, or reordering.
- `value` is a finite base-10 number. Missing values, NaN, positive/negative
  infinity, and empty cells are invalid.
- Negative and fractional values are accepted by the transport layer; an
  operation may impose stricter domain constraints and must report them.
- At least one observation is required by the transport format. The
  decomposition operation currently requires at least 16 observations and a
  power-of-two length, matching the existing R implementation.
- Sampling is uniform. The timestamp of sample `i` is
  `i * sampling_interval`; no separate timestamp column is accepted in v1.
- Signal duration is derived from sample count and sampling interval. It is
  not an independent input field.

The CLI must preserve numeric values without integer coercion. Locale-specific
decimal separators and thousands separators are not accepted.

For `reconstruct`, `input_csv` is instead the coefficient table described in
section 6. The signal metadata describes the reconstructed output. The
coefficient table must be rectangular and complete according to the rules in
that section.

## 6. Output conventions

Signal outputs use the same `sample_index,value` CSV layout. The input to
`reconstruct` and the output from `decompose` use a long-form coefficient
table with columns:

```csv
scale,sample_index,coefficient
0,0,0
0,1,0
1,0,12
1,1,12
```

Scale zero denotes the remainder row; higher scale numbers denote the
corresponding multiscale component. Each scale must contain exactly one row
for every sample index from zero to sample_count minus one. Rows are ordered
by ascending scale, then ascending sample index; duplicate or missing pairs
are invalid. Exact coefficient semantics and reconstruction invariants are
specified alongside the Rust algorithm implementation and its R conformance
fixtures in issues #34 and #36. The
transport schema must not be treated as a scientific model schema.

Outputs must contain only finite numeric values. Metadata such as schema
version, TrafficMSD version, operation, signal units, sampling interval,
sample count, and relevant parameters must be made available alongside
machine-readable results. The implementation issue #35 will settle the
sidecar naming and exact output envelope before the CLI is declared stable.

## 7. Commands and validation behavior

- `validate` checks the complete config, input schema, numeric values, sample
  ordering, and operation-independent signal metadata. It creates no output.
- `decompose` validates first, then applies the multiscale decomposition and
  writes the coefficient table.
- `reconstruct` validates the coefficient input and writes a normalized
  signal CSV. The coefficient input shape must match the representation
  produced by the matching supported TrafficMSD schema/version.

All validation must complete before replacing or creating final output files.
Errors must identify the configuration field or input row/column when
possible. Error messages must not include unstable Rust debug dumps.

### Exit codes

| Code | Meaning |
|---:|---|
| 0 | Success |
| 2 | CLI usage or unsupported command/option |
| 3 | Invalid configuration or input data |
| 4 | File-system or input/output failure |
| 5 | Algorithm execution failure or violated numerical invariant |

The process exits non-zero on any failure. A failure must not leave a
partially written final output file.

## 8. Valid and invalid examples

Valid configuration for a 32-sample signal:

```json
{
  "schema_version": 1,
  "command": "decompose",
  "input_csv": "input/signal.csv",
  "output_csv": "output/coefficients.csv",
  "signal": {
    "sampling_interval": 0.001,
    "time_unit": "s",
    "value_unit": "bytes"
  },
  "parameters": {
    "cutoff_fraction": 0.004
  }
}
```

Invalid configuration (unsupported schema and non-positive sampling interval):

```json
{
  "schema_version": 2,
  "command": "decompose",
  "input_csv": "signal.csv",
  "output_csv": "coefficients.csv",
  "signal": {
    "sampling_interval": 0,
    "time_unit": "seconds",
    "value_unit": "bytes"
  },
  "parameters": {
    "cutoff_fraction": 1.5
  }
}
```

The invalid example must be rejected. The CLI should report the unsupported
schema version first; after that is corrected, it should report invalid
sampling interval and time unit, then invalid cutoff fraction.

## 9. Compatibility policy

Schema version 1 is strict: unknown fields and unsupported enum values are
errors. Any incompatible change increments the schema version. The CLI
reports its own version through `trafficmsd --version` and rejects
unsupported input schema versions with exit code 3. It must not silently
reinterpret old files.

The CLI contract is independent of the research model schema in issue #27.
A future model file may be an input to a generation command, but it must have
its own schema/version and explicit conversion boundary rather than being
embedded as undocumented fields in this CLI configuration.

## 10. Open implementation decisions

The following are intentionally deferred to the implementation/conformance
issues, not left implicit:

- exact coefficient CSV ordering and serialization details after comparison
  with the R implementation;
- output metadata sidecar filename and exact JSON envelope;
- supported maximum sample count and resource limits;
- raw packet/event ingestion and legacy `read_traffic()` compatibility;
- stochastic generation commands and their RNG implementation;
- cross-platform path and atomic-rename edge cases.
