# TrafficMSD extracted model format v1

Initial specification for a fixed-interval aggregate traffic signal and its multiscale reconstruction. This scientific artifact is separate from the CLI configuration schema.

## Observation

The first target is the sum of `Length` values in fixed-width bins. The legacy `read_traffic()` implementation uses `as.integer(Time)` and `sum`, with regularization and empty-bin replacement by zero. Its documentation describes the bin width as one second. Preserve this as the compatibility default while keeping the interval configurable.

The code establishes the legacy interpretation, not independently verified source units. The physical unit of `Length` and the original trace clock unit remain unknown until dataset provenance is confirmed. Store unknown units explicitly; do not infer bytes or seconds from values alone.

## Reconstruction components

Version 1 stores the observation, provenance, coverage, extraction parameters, and reconstruction components. Scale zero is the residual and emits at each sample. For scale `s > 0`, the window length is `2^s`; the legacy emission filter places the component at the window's center-left sample (zero-based offset `window_length / 2 - 1`). Amplitudes use the same unit as the observed signal.

These are reconstruction semantics, not a stochastic traffic generator. Future generation parameters must separately define distributions, temporal dependence, duration, and RNG behavior. Do not relabel reconstruction coefficients as generative parameters.

## Provenance and validation

Record trace identity, source reference, license status, extraction software, preprocessing, time unit and its verification status, value unit and its verification status, sampling interval, and bin origin. Keep observed sample count distinct from power-of-two padding. Semantic validation checks scale/window consistency, emission positions, uniqueness, and residual coverage. JSON Schema and the exporter enforce field shape and basic metadata; additional cross-field provenance/unit checks remain a validation task.

### Legacy local trace

The bundled `data-raw/02-heatup-and-linkcap.csv` has `Time` and `Length` columns. The maintainer confirms that it was manually captured on a personal laptop during ordinary use (possibly while watching YouTube), solely to obtain sample data for an earlier article. The exact application/session, capture procedure, timestamp clock, physical units, and redistribution rights are unknown.

Treat this file as an undocumented personal development fixture only. It may support local smoke tests and legacy compatibility checks, but must not be described as a public dataset, used as a scientific benchmark, or used to support claims about real-world traffic. Do not redistribute it as a research artifact. Keep provenance and units explicitly unknown.

### Candidate for reproducible experiments

The UNB CIC VPN-nonVPN dataset (ISCXVPN2016) is a candidate because its documented traffic categories include YouTube/Vimeo streaming and the release includes PCAP captures plus derived flow CSV files. The original paper describes 28 GB of captures generated with Wireshark/tcpdump, and the official page describes YouTube HTML5/Flash and Vimeo streaming captured using Chrome and Firefox. UNB's dataset FAQ states that datasets may be redistributed, republished, and mirrored provided the dataset and associated paper are cited. This is the publisher's stated reuse condition, not an SPDX license identifier; preserve the source terms and citation with any derived artifact.

Important limitations: the dataset is not guaranteed to be a clean YouTube-only trace; exact application/session-to-file mapping is not documented by UNB; and flow CSVs are derived flow-level data, not a substitute for packet-level time series. The PCAP's captured packet length must not be mislabeled as application payload bytes or wire bytes without checking truncation and link-layer semantics. The official download form returned a server error during the initial assessment, so no files have yet been downloaded or inspected.

Before adoption, inspect the archive and identify exact files from metadata rather than filename guesses. Confirm packet timestamps, captured/original lengths, link-layer type, and background traffic. Define direction policy and fixed-bin aggregation explicitly. Keep raw PCAPs outside Git. The detailed assessment and extraction gates are in [docs/datasets/iscxvpn2016-assessment.md](datasets/iscxvpn2016-assessment.md), tracked by [issue #41](https://github.com/vsimko/trafficmsd/issues/42).

Sources:
- https://www.unb.ca/cic/datasets/vpn.html
- https://www.unb.ca/cic/datasets/
- https://www.scitepress.org/papers/2016/57407/57407.pdf

The schema is in `schemas/model/v1/model.schema.json`.


## Semantic validation and reconstruction fidelity

The Rust core provides a semantic validator in `trafficmsd_core::model` in addition to JSON Schema validation. The semantic validator checks:

- observed and decomposition sample counts, power-of-two extent, and padding arithmetic;
- scale/window length consistency and global window alignment;
- center-left emission position for each higher-scale component;
- uniqueness of each scale/window component;
- exactly one scale-zero residual component for every sample index.

The model is a deterministic reconstruction description, but it is **not necessarily a lossless encoding of the observed signal**. The R extraction algorithm applies `optimize_intervals()` to higher-scale candidate vectors and to the final residual vector. It also subtracts selected higher-scale amplitudes from the residual at their emission positions (clamped at zero). Consequently, interval optimization can change coefficient values and reconstruction can differ from the input signal. The existing R test suite checks output shape and finiteness and permits a relative difference in total sum; it does not establish sample-wise equality.

Therefore:
- semantic validation answers whether a model is internally well-formed;
- reconstruction produces the signal defined by the serialized components;
- reconstruction quality must be evaluated separately against the original observation using explicit metrics and tolerances (tracked in issue #28).

Do not describe a semantically valid model as exact or lossless unless a particular extraction result has independently passed a sample-wise conformance check.

## R reference exporter

The R API `export_model_v1()` converts the output of `decompose_traffic()`
into this representation. It preserves every scale-zero residual sample,
including zeros, and serializes higher-scale components only at the
center-left emission position selected by the legacy `compute_emits()` filter.
The decomposition result now retains its `cutoff.fraction` so the exporter
can record the actual extraction setting. When a coefficient matrix is passed
directly, callers must provide `cutoff_fraction` explicitly.

The exporter defaults time and value units to unknown and does not infer bytes
or seconds. Supply verified units only when supported by dataset provenance.
The optional `extracted_at` timestamp defaults to null so repeated exports can
be byte-for-byte reproducible apart from JSON formatting/runtime metadata.
Writing JSON requires the optional `jsonlite` package.

Example:

```r
x <- rep(c(0, 4, 0, 2), 8)
d <- decompose_traffic(x)
model <- export_model_v1(
  d,
  observed_sample_count = length(x),
  model_id = "synthetic-periodic-32",
  dataset_id = "synthetic-periodic-32"
)
export_model_v1(
  d,
  observed_sample_count = length(x),
  model_id = "synthetic-periodic-32",
  dataset_id = "synthetic-periodic-32",
  path = "synthetic-periodic-32.json"
)
```

The exporter and its conformance tests are a reference-generation mechanism;
a committed golden fixture must be produced by running this code under R and
reviewing the resulting JSON, not by hand-transcribing coefficients.
