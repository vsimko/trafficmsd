# TrafficMSD extracted model format v1

Initial specification for a fixed-interval aggregate traffic signal and its multiscale reconstruction. This scientific artifact is separate from the CLI configuration schema.

## Observation

The first target is the sum of `Length` values in fixed-width bins. The legacy `read_traffic()` implementation uses `as.integer(Time)` and `sum`, with regularization and empty-bin replacement by zero. Its documentation describes the bin width as one second. Preserve this as the compatibility default while keeping the interval configurable.

The code establishes the legacy interpretation, not independently verified source units. The physical unit of `Length` and the original trace clock unit remain unknown until dataset provenance is confirmed. Store unknown units explicitly; do not infer bytes or seconds from values alone.

## Reconstruction components

Version 1 stores the observation, provenance, coverage, extraction parameters, and reconstruction components. Scale zero is the residual and emits at each sample. For scale `s > 0`, the window length is `2^s`; the legacy emission filter places the component at the window's center-left sample (zero-based offset `window_length / 2 - 1`). Amplitudes use the same unit as the observed signal.

These are reconstruction semantics, not a stochastic traffic generator. Future generation parameters must separately define distributions, temporal dependence, duration, and RNG behavior. Do not relabel reconstruction coefficients as generative parameters.

## Provenance and validation

Record trace identity, source reference, license status, extraction software, preprocessing, time unit and its verification status, value unit and its verification status, sampling interval, and bin origin. Keep observed sample count distinct from power-of-two padding. Semantic validation must check scale/window consistency, emission positions, coverage, and provenance/unit consistency.

### Legacy local trace

The bundled `data-raw/02-heatup-and-linkcap.csv` has `Time` and `Length` columns. The maintainer confirms that it was manually captured on a personal laptop during ordinary use (possibly while watching YouTube), solely to obtain sample data for an earlier article. The exact application/session, capture procedure, timestamp clock, physical units, and redistribution rights are unknown.

Treat this file as an undocumented personal development fixture only. It may support local smoke tests and legacy compatibility checks, but must not be described as a public dataset, used as a scientific benchmark, or used to support claims about real-world traffic. Do not redistribute it as a research artifact. Keep provenance and units explicitly unknown.

### Candidate for reproducible experiments

The UNB CIC VPN-nonVPN dataset (ISCXVPN2016) is a candidate because its documented traffic categories include YouTube/Vimeo streaming and the release includes PCAP captures plus derived flow CSV files. The original paper describes 28 GB of captures generated with Wireshark/tcpdump, and the official page describes YouTube HTML5/Flash and Vimeo streaming captured using Chrome and Firefox. UNB's dataset FAQ states that datasets may be redistributed, republished, and mirrored provided the dataset and associated paper are cited. This is the publisher's stated reuse condition, not an SPDX license identifier; preserve the source terms and citation with any derived artifact.

Important limitations: the dataset is not guaranteed to be a clean YouTube-only trace; exact application/session-to-file mapping is not documented by UNB; and flow CSVs are derived flow-level data, not a substitute for packet-level time series. The PCAP's captured packet length must not be mislabeled as application payload bytes or wire bytes without checking truncation and link-layer semantics. The official download form returned a server error during the initial assessment, so no files have yet been downloaded or inspected.

Before adoption, inspect the archive and identify exact files from metadata rather than filename guesses. Confirm packet timestamps, captured/original lengths, link-layer type, and background traffic. Define direction policy and fixed-bin aggregation explicitly. Keep raw PCAPs outside Git. The detailed assessment and extraction gates are in [docs/datasets/iscxvpn2016-assessment.md](datasets/iscxvpn2016-assessment.md), tracked by [issue #42](https://github.com/vsimko/trafficmsd/issues/42).

Sources:
- https://www.unb.ca/cic/datasets/vpn.html
- https://www.unb.ca/cic/datasets/
- https://www.scitepress.org/papers/2016/57407/57407.pdf

The schema is in `schemas/model/v1/model.schema.json`.


## Reference implementation details confirmed during schema review

The current R implementation applies the emission mask to the coefficient matrix, then sums the filtered rows by sample. For scale s > 0, the period is P = 2^s and the zero-based emission offset within each globally aligned period is P/2 - 1. Scale zero is the residual row and is emitted at every sample. The phase is anchored at sample index zero; it is not restarted at a component's begin position.

During decomposition, when a higher-scale coefficient c is selected in a window, the algorithm removes the selected source occurrence and subtracts c from the residual at that window's emission position (clamped at zero). This is required because the higher-scale component and scale-zero residual would otherwise emit the same contribution twice. A serialized model must therefore retain the residual contribution, including its sample positions; it must not treat residual as optional metadata.

The schema's `components` are reconstruction records, not an already-generalized stochastic generator. For a lossless mapping, each record must identify its scale, globally aligned window, emission sample, and amplitude. Records at scale zero represent residual samples. The current schema does not yet enforce cross-field constraints such as `window_length_samples = 2^scale_index`, the midpoint formula, uniqueness of scale-zero sample positions, or a single consistent sample extent. Those constraints belong in the semantic validator and must be tested against R-generated fixtures before the format is called stable.
