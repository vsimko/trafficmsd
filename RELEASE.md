# Release procedure

This repository uses semantic versioning for package releases. The package version in `DESCRIPTION`, the `NEWS.md` heading, the Git tag, and the GitHub release title must agree.

## Pre-release checklist

1. Start from an up-to-date `master` branch.
2. Update `Version` in `DESCRIPTION` and add a matching section to `NEWS.md`.
3. Review user-visible changes, compatibility notes, dependencies, and documentation.
4. Run `devtools::document()`, `devtools::test()`, and `devtools::check()`.
5. Run the **R release validation** workflow manually against the intended commit. Confirm that it builds, installs, and checks the source archive successfully.
6. Review the workflow artifact and confirm that the package version and source archive name are correct.

## Publishing

After validation passes:

1. Create an annotated Git tag named `vX.Y.Z` on the validated commit.
2. The `CLI binary builds` workflow runs for the tag and creates the GitHub Release with platform binaries and SHA-256 files.
3. Attach the validated R source archive from the release-validation workflow artifact and add the corresponding `NEWS.md` notes.
4. Verify the release page, binary checksums, and installation instructions.

Do not tag or publish a release before the validation workflow succeeds. CRAN submission is a separate decision and is not implied by creating a GitHub release.

## Reproducible local validation

```sh
R CMD build .
R CMD check --as-cran trafficmsd_X.Y.Z.tar.gz
```
