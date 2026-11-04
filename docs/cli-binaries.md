# Standalone CLI binaries

The standalone Rust CLI is built for the following initial platform matrix:

| Platform | Rust target | Asset name |
|---|---|---|
| Linux x86_64 (GNU) | `x86_64-unknown-linux-gnu` | `trafficmsd-x86_64-unknown-linux-gnu` |
| macOS x86_64 | `x86_64-apple-darwin` | `trafficmsd-x86_64-apple-darwin` |
| macOS ARM64 | `aarch64-apple-darwin` | `trafficmsd-aarch64-apple-darwin` |
| Windows x86_64 (MSVC) | `x86_64-pc-windows-msvc` | `trafficmsd-x86_64-pc-windows-msvc.exe` |

## CI artifacts and releases

The `CLI binary builds` workflow tests the Rust workspace on each runner, builds each target, and uploads the executable with a SHA-256 checksum file. Pull-request and branch artifacts are temporary CI artifacts.

Pushing a version tag matching `vX.Y.Z` runs the full platform matrix and publishes the binaries and checksum files as GitHub Release assets. Do not create a version tag until the release validation checklist in [RELEASE.md](../RELEASE.md) has passed.

## Verify an asset

On Linux or macOS, download the executable and its matching `.sha256` file, then run:

```sh
shasum -a 256 -c trafficmsd-<target>.sha256
chmod +x trafficmsd-<target>
./trafficmsd-<target> --version
```

On Windows, compare the downloaded file hash with the value in its `.sha256` file using:

```powershell
(Get-FileHash .\trafficmsd-x86_64-pc-windows-msvc.exe -Algorithm SHA256).Hash
```

## Compatibility and current limitation

The CLI reports its own version with `--version`. The R adapter must check that version before invoking it and provide actionable diagnostics for missing or incompatible executables.

The binary workflow does not yet install or download a binary automatically from the R package. That runtime discovery/download and checksum-verification behavior belongs to issue #37. The current binaries are not code-signed or notarized; macOS users may need to follow local Gatekeeper policy.
