# CI caching

TrafficMSD uses separate cache strategies for Rust builds, R package dependencies, and
the external Westermo dataset inputs. Caches are performance optimizations only: a
cache miss must always fall back to a clean, correct build or download.

## Rust / Cargo

The Rust workflows use `Swatinem/rust-cache@v2`:

- `rust.yml`
- `r-rust-conformance.yml`
- `performance-benchmark.yml`
- `cli-binaries.yml`

The action caches Cargo registry and Git dependencies as well as build artefacts.
Its cache keys account for the runner/toolchain and Rust dependency inputs. The CLI
matrix additionally sets a target-specific `shared-key`; the action's runner
partitioning keeps operating systems separate. Do not share compiled `target/`
artefacts across incompatible operating systems or targets.

When changing Rust toolchain configuration or cache behavior, verify both a cache
hit and a cache miss. Cache contents must never be required for correctness.

## R package dependencies

R workflows use
`r-lib/actions/setup-r-dependencies@v2`, which manages the R dependency cache.
The current workflows consistently set `cache-version: 1`:

- `R-CMD-check.yaml`
- `R-unit-tests.yaml`
- `R-release-validation.yaml`
- `model-extraction-smoke.yml`
- `r-rust-conformance.yml`
- `performance-benchmark.yml`

Keep the R dependency declarations and cache configuration aligned. Increment
`cache-version` deliberately when a cache reset is needed; do not add a second
overlapping cache for the same R library without measured justification. R release
and devel jobs must retain separate compatible dependency environments.

## Westermo dataset inputs

`westermo-dataset.yml` resolves the upstream `main` branch to a commit SHA and
uses that immutable revision in both the download URLs and the Actions cache key.
The cache also includes the dataset validation script hash. This prevents a cache
entry for an earlier upstream revision from being silently reused after upstream
changes.

All restored files are still passed through the normal dataset validation step.
The workflow regenerates SHA-256 checksums and records the upstream commit in the
release manifest. A cache miss downloads the same files from the resolved commit.

## Measuring effectiveness

### Cold run (2026-11-04, first PR run)

All Rust cache actions reported a cache miss and saved a cache at job completion.
Observed wall-clock durations:

| Workflow/job | Cold duration | Rust cache |
|---|---:|---|
| Rust stable checks/build | 31 s | Miss |
| CLI Linux x86_64 | 80 s | Miss |
| CLI Windows x86_64 | 83 s | Miss |
| CLI Linux aarch64 | 91 s | Miss |
| CLI macOS aarch64 | 104 s | Miss |
| CLI macOS x86_64 | 178 s | Miss |
| R↔Rust conformance | 141 s | Miss |
| R-versus-Rust benchmark | 105 s | Miss |

R dependency caches were already warm in the benchmark and conformance jobs; these
measurements isolate the Cargo cache cold path. The CLI matrix uses target-specific
cache scopes, and all five target builds reported misses on this first run.

### Warm run

A second same-branch run is required to confirm Cargo cache hits and compare elapsed
times. Results will be recorded here after that run completes.

Avoid treating a cache hit alone as proof of an improvement; the relevant metric is
reduced end-to-end CI time without weakening validation. Cache misses must continue
to produce clean, correct builds.
