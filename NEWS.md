# trafficmsd 0.1.0

## Compatibility and correctness

- Validate traffic signals and preserve numeric precision during decomposition.
- Zero-pad signals to power-of-two lengths instead of truncating them.
- Handle degenerate signals during cluster estimation.
- Make k-means clustering reproducible through explicit seed and `nstart` arguments.
- Validate traffic CSV input and document its required `Time` and `Length` columns.

## Maintenance

- Add GitHub Actions package checks and unit tests.
- Modernize package metadata and refresh user documentation.
- Expand regression and public API edge-case tests.
