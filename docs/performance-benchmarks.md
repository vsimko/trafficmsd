# Rust versus R performance benchmark protocol

**Status:** reproducible engineering benchmark harness  
**Scope:** multiscale decomposition and reconstruction only  
**Not a claim:** no speedup or performance advantage is asserted until results are collected and reviewed.

## Workloads

The benchmark runner uses deterministic synthetic non-negative signals at three power-of-two sizes:

- small: 1,024 samples;
- medium: 8,192 samples;
- large: 32,768 samples.

The same signal formula and cutoff fraction (0.004) are used by R and Rust. The benchmark checks that the CLI decomposition coefficients and reconstructed output agree with the R reference within a relative tolerance of 1e-12 before recording timings.

The fixtures are synthetic and are not representative measured packet traces. They isolate algorithm and process overhead; dataset-specific benchmarks must be added after the provenance-backed trace in issue #41 is validated.

## Measurements

For both decomposition and reconstruction, the runner records:

- median elapsed time for the R reference implementation;
- cumulative R memory allocations from Rprofmem for one operation;
- mean and total elapsed nanoseconds for the in-process Rust core benchmark;
- median end-to-end Rust CLI elapsed time, including R file serialization, process startup, CLI parsing, CSV I/O, and result serialization;
- maximum resident set size for the Rust core benchmark and CLI process when GNU time is available.

The in-process Rust core measurement separates algorithm runtime from the end-to-end CLI path. The CLI timing is intentionally not described as core-only performance. R allocation totals and process RSS are different memory measures and must not be compared as if they were equivalent. RSS includes process/runtime overhead.

## Reproduction

From a clean checkout with stable Rust and R dependencies installed:

\`\`\`sh
cargo build --release --bin trafficmsd
cargo build --release -p trafficmsd-core --example benchmark
R CMD INSTALL .
Rscript benchmarks/benchmark.R \\
  target/release/trafficmsd \\
  target/release/examples/benchmark \\
  benchmarks/results 5
\`\`\`

The runner writes:

- \`benchmark-results.csv\`: one row per workload size and operation;
- \`benchmark-results.json\`: the same measurements plus OS, architecture, R/Rust versions, commit, configuration, and measurement notes.

The GitHub Actions workflow \`Performance benchmarks\` runs this protocol on Ubuntu and uploads the result files as a workflow artifact. Results are tied to the commit and CI environment; they are not committed as fixed performance claims.

## Interpretation rules

- Compare equivalent operations and identical signal sizes/configuration.
- Use repeated runs and report the median for R and end-to-end CLI.
- Treat the Rust core example's mean as an in-process microbenchmark, not a CLI measurement.
- Inspect scaling across all three input sizes; do not extrapolate from one size.
- Treat CI timings as environment-specific and potentially noisy.
- Do not claim a speedup without repeated measurements on representative hardware and, later, validated trace workloads.
- Use memory measurements only with their method and scope stated. Rprofmem reports cumulative R allocations; GNU time reports process maximum RSS.

## Limitations

The current harness benchmarks the first migrated deterministic operations only. It does not benchmark raw PCAP preprocessing, clustering, stochastic generation, simulator integration, concurrency, or production-scale traces. Those require separate workloads and contracts.
