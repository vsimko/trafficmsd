# TrafficMSD sparse extracted model format v2

Model v2 preserves the observation, provenance, coverage, extraction, and higher-scale reconstruction semantics of model v1, but uses a sparse scale-zero residual.

## Sparse residual semantics

For scale zero, a component emits at its own sample index with a one-sample window. A missing scale-zero component means an implicit residual amplitude of exactly zero. A present scale-zero component must have a non-zero amplitude. This removes zero-valued records that contribute nothing to reconstruction.

For scales greater than zero, the window length remains (2^s), windows remain globally aligned, and emission remains at the center-left sample (zero-based offset (2^s / 2 - 1)). Component amplitudes remain non-negative and use the observed signal's value unit.

## Compatibility

- v1 is unchanged and continues to require exactly one scale-zero residual component for every decomposition sample, including zero amplitudes.
- v2 has schema version `2.0`; it must be validated with `schemas/model/v2/model.schema.json` and the v2 semantic validator.
- A v1 model can be converted to v2 by dropping only scale-zero components whose amplitude is exactly zero and changing the schema version. Do not drop non-zero residuals or higher-scale components.
- Reconstructing a v2 model initializes the signal to zero and adds all declared emissions. For the same decomposition, v1 and v2 reconstructions must be numerically identical.

## Scope

This is a storage and representation optimization only. It does not improve the decomposition algorithm, pointwise fidelity, temporal dependence, volume preservation, or traffic-generation validity. Report both actual serialized JSON bytes and component counts when benchmarking compactness.