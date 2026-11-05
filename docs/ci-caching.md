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

Compare representative cold and warm workflow runs using GitHub Actions job
durations and cache restore/save logs. Record the workflow, runner, cache hit/miss,
and elapsed time. Avoid treating a cache hit alone as proof of an improvement;
the relevant metric is reduced end-to-end CI time without weakening validation.

### Recorded Rust cold/warm comparison (2026-11-04 to 2026-11-05)

Two successful runs of the same Rust stable job on `master` were compared.
Both used `ubuntu-latest` on x86_64 Linux and the same commit
(`b3380204480d3798806c36fa6385bc84fa8e8aed`).

| Run | Cache evidence | Cache restore | Cargo dev | Cargo test | Cargo release |
|---|---|---:|---:|---:|---:|
| [Cold push run #41905502836](https://github.com/vsimko/trafficmsd/actions/runs/41905502836) | `No cache found` | no entry (restore lookup ~0.12 s) | 11.14 s | 3.14 s | 4.44 s |
| [Warm manual run #41983699070](https://github.com/vsimko/trafficmsd/actions/runs/41983699070) | Full cache-key match; 56 MB restored successfully | ~1.7 s to unpack after cache download | 1.86 s | 0.95 s | 1.12 s |

The three reported Cargo profile times sum to 18.72 s for the cold run and
3.93 s for the warm run (about 79% lower for these measured build phases).
This is an observed comparison of two runs, not a controlled benchmark or a
guarantee of future savings. Runner provisioning, cache transfer, and other
workflow steps are not included in those Cargo profile totals. The cold run
was a push-triggered run; the warm run was manually dispatched on the same
commit and branch. Both jobs completed successfully.
