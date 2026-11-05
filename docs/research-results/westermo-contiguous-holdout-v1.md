# Westermo contiguous holdout: fidelity and compactness report

**Status:** exploratory result; not an independent scientific validation  
**Dataset:** Westermo Network Traffic Dataset v1.0, right observation point only  
**Capture:** 35,067 packets; 5,445 one-second bins  
**Dataset archive SHA-256:** `8a1f49eb49747f3046eda8063b104834f41f0df30d4fc2357aea5609826a4445`  
**Aggregated trace SHA-256:** `6429e3d6a84efdc13edc14e44b2de2e6cbd31452e1958116c271745069b3fbb7`  
**CI run:** GitHub Actions run 42001977735 (Westermo model extraction)  
**Source commit:** `1b72845fb611eee9faa203c03c6a580464a689d3`

## Scope and split

The trace is binned into one-second left-closed/right-open intervals using captured frame lengths in bytes. It is not application payload or wire-length traffic. The split is contiguous, without shuffling:

| Segment | Zero-based half-open range | Samples |
|---|---:|---:|
| Train | [0, 3267) | 3,267 |
| Validation | [3267, 4356) | 1,089 |
| Test | [4356, 5445) | 1,089 |

The cutoff is selected by validation MAE from `0.0001, 0.00025, 0.0005, 0.001, 0.002, 0.004, 0.008, 0.016`; ties are resolved by the smallest cutoff. The selected cutoff is 0.0001.

**Interpretation constraint:** this is not a forecasting experiment. The deterministic decomposition is applied independently to each evaluated segment. The train segment supplies the constant-mean baseline and burst threshold; it does not train a predictive model that generates the test segment. Results measure lossy reconstruction fidelity on the supplied segment, not future-traffic generalization.

## Test-segment fidelity

| Metric | TrafficMSD | Training-mean baseline |
|---|---:|---:|
| MAE | 580.69 | 729.74 |
| RMSE | 1,974.46 | 1,764.31 |
| Bias (estimate − source) | +162.39 | −9.28 |
| Relative L1 error | 1.0374 | 1.3037 |
| Volume error | +29.01% | −1.66% |

TrafficMSD reduces MAE by 20.42% against the constant-mean control, but increases RMSE by 11.91%. The model therefore improves average absolute error while worsening squared error and total-volume preservation. The raw observed identity control has zero reconstruction error by definition; it is an upper-bound control, not a deployable predictive baseline.

Independent recomputation from the CI artifact's `trace.csv` and `model-v2-test.json` gives:

- Pearson correlation between source and reconstruction: 0.4891.
- Source ACF at lags 1, 2, 4, 8, 16, 32: 0.8452, 0.6942, 0.6723, 0.5768, 0.4107, 0.2712.
- Reconstructed ACF at the same lags: 0.2839, 0.4384, 0.6098, 0.6714, 0.6523, 0.5486.
- Mean absolute ACF difference across those lags: 0.2489.
- Source burst runs above the training p95 threshold (2,843 bytes): 9; reconstructed runs: 30.
- Maximum source/reconstructed burst run lengths: 20 / 21 samples.
- Zero-valued samples: source 0%; reconstruction 14.97%.
- Source/reconstruction p95: 2,078 / 4,456 bytes; p99: 10,017 / 11,840 bytes.
- Source/reconstruction total: 609,583 / 786,429 bytes.

These diagnostics show material distortion in temporal dependence, burst counts, zero occupancy, and volume. Matching a maximum burst length does not compensate for the other discrepancies.

## Sparse representation v2

For the selected test segment:

| Metric | Model v1 | Model v2 |
|---|---:|---:|
| Components | 2,899 | 986 |
| Serialized JSON | 474,918 bytes | 163,181 bytes |

Model v2 reduces serialized size by 65.64% while preserving the v1 reconstruction exactly in the experiment's equivalence check. It remains 18.73 times the size of the raw float64 test vector (8,712 bytes). This is a representation-efficiency result, not a fidelity improvement.

## Reproducibility and provenance

The CI job downloaded the immutable TrafficMSD release asset, verified the archive checksum and member inventory, generated the trace, ran the contiguous evaluation, and validated the v1, v2, and evaluation JSON documents against their schemas. The archived workflow artifact contains the manifest, checksums, trace, model exports, validation sweep, and evaluation JSON.

- Workflow run: https://github.com/vsimko/trafficmsd/actions/runs/42001977735
- Artifact: `westermo-model-extraction-v1` (140,469 bytes ZIP; SHA-256 `06263c7eda54a50ec95541e5ac3ed57257822f2c0478824437319f2507fbaa12`)
- Dataset registry: [docs/datasets/README.md](../datasets/README.md)

## Conclusion and next validation

This single industrial-network observation point does not establish a generally faithful traffic model. The current cutoff and decomposition preserve some coarse values but distort important temporal and volume properties. No scientific-success claim is made.

Next experiments should test (1) alternative loss functions that penalize volume and temporal-structure errors, (2) burst-aware and distribution-aware diagnostics, (3) sensitivity across multiple contiguous windows and operating regimes, and (4) an independently sourced dataset after its terms and acquisition procedure are reviewed. MAWI remains metadata-only; do not acquire it until its current research-use/privacy terms have been reviewed and the exact file checksum can be recorded. Any acquisition over 50 MB must be explicitly on-demand, checksum-verified, and cached only where the terms permit.
