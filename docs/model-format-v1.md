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

The bundled `data-raw/02-heatup-and-linkcap.csv` has `Time` and `Length` columns, but repository metadata does not currently establish the physical units or redistribution license. A trace-derived validated example remains blocked on provenance confirmation. The schema is in `schemas/model/v1/model.schema.json`.
