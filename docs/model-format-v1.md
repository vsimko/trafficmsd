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

The UNB CIC VPN-nonVPN dataset (ISCXVPN2016) is a candidate because its documented traffic categories include YouTube/Vimeo streaming and the release includes PCAP captures plus derived flow CSV files. UNB's dataset FAQ states that datasets may be redistributed, republished, and mirrored provided the dataset and associated paper are cited. This is a dataset-specific reuse statement, not an SPDX license identifier; preserve the source terms and citation with any derived artifact.

Before adoption, inspect the PCAP/CSV contents and confirm that packet timestamps and captured lengths support a defensible fixed-interval byte-volume series. Record the exact subset, download date/version, citation, source URL, transformations, and any applicable terms. Do not commit the full dataset or assume the flow CSV is a substitute for packet-level aggregation.

Sources:
- https://www.unb.ca/cic/datasets/vpn.html
- https://www.unb.ca/cic/datasets/

The schema is in `schemas/model/v1/model.schema.json`.
