# Rust development

TrafficMSD's Rust workspace lives at the repository root. It currently contains:

- `trafficmsd-core`: domain types and simulation algorithms; independent of CLI
  and R integration.
- `trafficmsd-cli`: the standalone `trafficmsd` executable, depending on the
  core crate.

The workspace uses the Rust stable channel. The toolchain file installs the
minimal profile plus `rustfmt` and `clippy`. The workspace MSRV is currently
Rust 1.75 (edition 2021); update this policy deliberately when a dependency or
language feature requires a newer compiler.

## Local commands

From the repository root:

```sh
rustup show
cargo fmt --all -- --check
cargo clippy --workspace --all-targets -- -D warnings
cargo test --workspace
cargo build --workspace --release
cargo run -p trafficmsd-cli -- --help
cargo run -p trafficmsd-cli -- --version
```

The CLI is intentionally minimal at this stage. Its simulation commands and
input/output contracts will be added in the follow-up work. Keep simulation
logic in `trafficmsd-core`; do not add dependencies from the core crate to the
CLI or to R.

## R package boundary

The repository remains an R package during the migration. Rust workspace files
and sources are excluded from R package builds through `.Rbuildignore`.
Existing R checks remain in their own workflows and are not replaced by Rust
checks.
