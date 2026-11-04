# TrafficMSD CLI binary distribution v1

**Status:** release automation and R installer contract  
**Supported v1 targets:**

| Platform | Rust target | GitHub Actions runner |
|---|---|---|
| Linux x86-64 (glibc) | \`x86_64-unknown-linux-gnu\` | \`ubuntu-24.04\` |
| Linux ARM64 (glibc) | \`aarch64-unknown-linux-gnu\` | \`ubuntu-24.04-arm\` |
| macOS Intel | \`x86_64-apple-darwin\` | \`macos-15-intel\` |
| macOS Apple Silicon | \`aarch64-apple-darwin\` | \`macos-15\` |
| Windows x86-64 | \`x86_64-pc-windows-msvc\` | \`windows-2025\` |

## Release assets

The \`CLI binary distribution\` workflow builds the standalone \`trafficmsd\`
executable natively on each target runner when a GitHub Release is published.
It attaches one raw executable per target and a
\`trafficmsd-cli-manifest.json\` to that release.

The manifest schema is versioned at
\`schemas/cli-binaries/v1/manifest.schema.json\`. It records the release/CLI
version, target triple, asset filename, and SHA-256 digest. The publish job
fails unless every required platform asset is present. Release assets are
uploaded only after all platform builds have succeeded.

The Linux artifacts target glibc; they are not musl/static binaries. Linux
systems must provide a compatible glibc runtime. Windows ARM64, Linux
architectures other than x86-64/ARM64, and macOS versions/architectures outside
the listed targets are not supported by this first distribution increment.

## R installation

After a CLI-enabled GitHub Release has been published:

\`\`\`r
install.packages(c("jsonlite", "digest"))
trafficmsd::install_trafficmsd_cli()
\`\`\`

The installer:

1. selects the matching target for the current OS and architecture;
2. downloads the latest release manifest (or a requested version);
3. downloads the matching executable;
4. verifies its SHA-256 digest before installation;
5. stores it in the per-user R cache; and
6. sets \`TRAFFICMSD_CLI\` for the current R session.

A Rust toolchain and administrator permissions are not required. To pin a
release, pass its version, for example
\`install_trafficmsd_cli(version = "0.1.0")\`. Use \`force = TRUE\` to replace
a cached binary. The binary is not installed into the package library and
does not require write access to the R installation.

The explicit R adapters \`decompose_traffic_rust()\` and
\`reconstruct_traffic_rust()\` discover the executable through
\`TRAFFICMSD_CLI\` or \`PATH\`. The existing R implementations remain the
reference and default; the installer does not silently switch existing
functions to Rust.

## Security and compatibility

- Only HTTPS GitHub release URLs are used.
- The release manifest is parsed as schema version 1.
- The downloaded asset must match the manifest SHA-256 before it is copied to
  the cache.
- The asset name must be a basename, not a path.
- Each executable is versioned with the GitHub Release; updates do not
  overwrite binaries from another version.
- The CLI still validates its own input schema and reports its version.
- A checksum protects against corruption and mismatched assets; it is not a
  substitute for verifying the repository/release publisher.

## Reproduction and validation

The \`CLI binary distribution\` workflow also runs for pull requests that
change the Rust CLI/core or distribution workflow. It builds all five targets
and uploads short-lived workflow artifacts without publishing a release.

The first user-installable binary release is deliberately separate from this
engineering change. It must be published through the release-readiness process
tracked by issue #11, after the outstanding blocking research/data gates have
been reviewed.
