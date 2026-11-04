# trafficmsd

[![R package checks](https://github.com/vsimko/trafficmsd/actions/workflows/R-CMD-check.yaml/badge.svg?branch=master)](https://github.com/vsimko/trafficmsd/actions/workflows/R-CMD-check.yaml)
[![R unit tests](https://github.com/vsimko/trafficmsd/actions/workflows/R-unit-tests.yaml/badge.svg?branch=master)](https://github.com/vsimko/trafficmsd/actions/workflows/R-unit-tests.yaml)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)

**trafficmsd** provides multiscale decomposition of network traffic time series,
with an emphasis on traffic models suitable for performance simulation (for
example, using Queuing Petri Nets).

## Technical direction

TrafficMSD is transitioning toward a Rust-based network traffic simulation
engine. The R package will remain the visualization and statistical-analysis
interface, while computationally intensive operations move into a standalone
Rust core and CLI. See the [technical roadmap](ROADMAP.md) and
[ADR 0001: Rust core architecture](docs/adr/0001-rust-core.md).

## Installation

Install the development version from GitHub:

```r
install.packages("remotes")
remotes::install_github("vsimko/trafficmsd")
```

The package is tested with current R release and R-devel on GitHub Actions.
The package requires R 3.6.0 or later.

## Quick start

The package includes small example signals. A decomposition input must be a
finite numeric vector with at least 16 observations and a length that is a
power of two. Use `pad_power2()` when zero-padding is appropriate for the
analysis.

```r
library(trafficmsd)

data(sample32)
decomposition <- decompose_traffic(sample32)
reconstructed <- reconstruct_traffic(decomposition)
print_stats(decomposition)
plot_spectrum(decomposition)
```

For a signal whose length is not a power of two:

```r
signal <- c(12, 18, 7, 25, 14, 9, 30, 11, 8, 20, 16, 6, 22, 10, 13, 5, 17)
padded_signal <- pad_power2(signal)
decomposition <- decompose_traffic(padded_signal)
```

Padding appends zeros; it does not truncate observations. The returned
reconstruction therefore has the padded length. Keep the original length if
you need to trim the reconstructed vector back to the unpadded signal.

## Main functions

- `read_traffic()` reads packet timestamps and lengths from a CSV containing
  `Time` and `Length` columns and regularizes the signal.
- `pad_power2()` appends zeros to reach the next power-of-two length.
- `decompose_traffic()` computes the multiscale decomposition.
- `reconstruct_traffic()` reconstructs a traffic vector from decomposition
  coefficients.
- `estimate_clusters()` estimates an amplitude cluster count.
- `cluster_by_amplitude()` quantizes amplitudes using k-means.
- `plot_spectrum()`, `plot_scaleogram()`, and `plot_traffic()` provide
  visualization helpers.

Clustering uses a local random seed by default so repeated calls are
reproducible without changing the caller's global random-number state. Pass
`seed = NULL` to use the current random-number stream, or set `seed` and
`nstart` explicitly.

See the function help pages (for example, `?decompose_traffic`) for arguments,
return values, and constraints.

## Research direction

The research roadmap focuses on network traffic model extraction, followed by
traffic generation and performance-simulation validation.

- [Research and development roadmap](docs/RESEARCH_ROADMAP.md)
- [Research readiness specification](docs/RESEARCH_READINESS.md)
- [Performance benchmark protocol](docs/performance-benchmarks.md)

## Research reference

The package accompanies work on automated extraction of network traffic models:

```bibtex
@inproceedings{Rygielski2016,
  title={Automated Extraction of Network Traffic Models Suitable for Performance Simulation},
  author={Rygielski, Piotr and Simko, Viliam and Sittner, Felix and Aschenbrenner, Doris and Kounev, Samuel and Schilling, Klaus},
  booktitle={Proceedings of the 7th ACM/SPEC on International Conference on Performance Engineering},
  pages={27--35},
  year={2016},
  organization={ACM}
}
```

## Development

Run the package checks and test suite from a clean checkout:

```sh
R CMD build .
R CMD check --no-manual trafficmsd_*.tar.gz
Rscript -e 'testthat::test_local()'
```

Issues and contributions: https://github.com/vsimko/trafficmsd/issues
