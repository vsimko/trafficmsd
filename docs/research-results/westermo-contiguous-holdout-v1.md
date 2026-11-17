# Westermo contiguous holdout: fidelity and compactness report

**Status:** exploratory result; not an independent scientific validation  
**Dataset:** Westermo Network Traffic Dataset v1.0, right observation point only  
**Capture:** 35,067 packets; 5,445 one-second bins  
**Dataset archive SHA-256:** `8a1f49eb49747f3046eda8063b104834f41f0df30d4fc2357aea5609826a4445`  
**Aggregated trace SHA-256:** `6429e3d6a84efdc13edc14e44b2de2e6cbd31452e1958116c271745069b3fbb7`  
**CI run:** GitHub Actions run [44124933173](https://github.com/vsimko/trafficmsd/actions/runs/44124933173)  
**Artifact:** `westermo-model-extraction-and-qpn-v1`, artifact ID `14347336777`, 681,122 bytes, SHA-256 `e7b85833424a4e3d4a44b1b4a610d143b8fd15db25763e1de8a8a5c7d3c10a51`  
**Source commit:** `aaf3766f6c159f4284481ef4a2dd2b58418cd6e3`

## Scope and split

The trace is binned into one-second left-closed/right-open intervals using captured frame lengths in bytes. It is not application payload or wire-length traffic. The split is contiguous, without shuffling:

| Segment | Zero-based half-open range | Samples |
|---|---:|---:|
| Train | [0, 3267) | 3,267 |
| Validation | [3267, 4356) | 1,089 |
| Test | [4356, 5445) | 1,089 |

The artifact evaluates seven cutoffs: `0.0001, 0.0005, 0.001, 0.002, 0.004, 0.008, 0.016`. Validation retains all seven as non-dominated candidates under the predeclared objectives (mean absolute ACF difference, relative L1 error, component count, and serialized model bytes). The selection rule explicitly returns no unique cutoff without predeclared use-case tolerances. All seven candidates are then evaluated on the untouched test segment; test results must not be used to select a winner.

**Interpretation constraint:** this is not a forecasting experiment. The deterministic decomposition is applied independently to each evaluated segment. The train segment supplies the constant-mean baseline and burst threshold; it does not train a predictive model that generates the test segment. Results measure lossy reconstruction fidelity on the supplied segment, not future-traffic generalization.

## Test-segment fidelity

All seven validation-frontier candidates were evaluated on the same untouched 1,089-bin test segment. The table reports the test metrics and sparse-v2 representation size from the archived evaluation JSON. No candidate is selected.

| Cutoff | MAE | RMSE | Bias | Volume error | v2 components | v2 JSON bytes |
|---:|---:|---:|---:|---:|---:|---:|
| 0.0001 | 580.69 | 1,974.46 | +162.39 | +29.01% | 986 | 163,204 |
| 0.0005 | 580.69 | 1,974.46 | +162.39 | +29.01% | 986 | 163,204 |
| 0.001 | 601.36 | 2,002.03 | +142.14 | +25.39% | 893 | 147,997 |
| 0.002 | 639.62 | 2,045.09 | +108.72 | +19.42% | 706 | 117,469 |
| 0.004 | 674.96 | 2,077.67 | +38.98 | +6.96% | 489 | 81,972 |
| 0.008 | 692.59 | 2,124.68 | −3.39 | −0.60% | 378 | 63,784 |
| 0.016 | 720.01 | 2,212.69 | −71.81 | −12.83% | 252 | 43,136 |

The training-mean baseline on this test segment has MAE 729.74 and RMSE 1,764.31. Every Pareto candidate lowers MAE relative to that baseline but has higher RMSE; the cutoff therefore trades pointwise absolute error against squared error, volume preservation, and representation size. The 0.0001 and 0.0005 candidates produce identical test metrics and representation size in this run. The raw observed identity control has zero reconstruction error by definition; it is an upper-bound control, not a deployable predictive baseline.

The following additional diagnostics describe the 0.0001 test candidate (also numerically identical to 0.0005 in this artifact); they are not evidence that this cutoff was selected:

- Source ACF at lags 1, 2, 4, 8, 16, 32: 0.8452, 0.6942, 0.6723, 0.5768, 0.4107, 0.2712.
- Reconstructed ACF at the same lags: 0.2839, 0.4384, 0.6098, 0.6714, 0.6523, 0.5486.
- Mean absolute ACF difference across those lags: 0.2489.
- Source burst runs above the training p95 threshold (2,843 bytes): 9; reconstructed runs: 30.
- Maximum source/reconstructed burst run lengths: 20 / 21 samples.
- Source/reconstruction p95: 2,078 / 4,456 bytes; p99: 10,017 / 11,840 bytes.
- Source/reconstruction total: 609,583 / 786,429 bytes.

These diagnostics show material distortion in temporal dependence, burst counts, zero occupancy, and volume. Matching a maximum burst length does not compensate for the other discrepancies.

## Sparse representation v2

For the 0.0001 test candidate (also identical in output to 0.0005):

| Metric | Model v1 | Model v2 |
|---|---:|---:|
| Components | 2,899 | 986 |
| Serialized JSON | 474,941 bytes | 163,204 bytes |

Model v2 reduces serialized size by 65.64% while preserving the v1 reconstruction within the experiment's numerical equivalence check. It remains 18.73 times the size of the raw float64 test vector (8,712 bytes). This is a representation-efficiency result, not a fidelity improvement.

## Reproducibility and provenance

The CI job downloaded the immutable TrafficMSD release asset, verified the archive checksum and member inventory, generated the trace, ran the contiguous evaluation, and validated the v1, v2, and evaluation JSON documents against their schemas. The archived workflow artifact contains the manifest, checksums, trace, model exports, validation sweep, and evaluation JSON.

- Workflow run: https://github.com/vsimko/trafficmsd/actions/runs/44124933173
- Artifact: `westermo-model-extraction-and-qpn-v1` (artifact ID `14347336777`; 681,122 bytes; GitHub artifact SHA-256 `e7b85833424a4e3d4a44b1b4a610d143b8fd15db25763e1de8a8a5c7d3c10a51`)
- Holdout evaluation: `westermo-model-extraction-v1/model/contiguous-holdout-evaluation.json` inside the artifact.
- Dataset registry: [docs/datasets/README.md](../datasets/README.md)

## Conclusion and next validation

This single industrial-network observation point does not establish a generally faithful traffic model. The cutoff sweep exposes a genuine trade-off, but the validation Pareto rule selects no unique candidate. Test results show material temporal and volume distortion across the candidate set. No scientific-success claim or selected cutoff is made.

Next experiments should test (1) alternative loss functions that penalize volume and temporal-structure errors, (2) burst-aware and distribution-aware diagnostics, (3) sensitivity across multiple contiguous windows and operating regimes, and (4) an independently sourced dataset after its terms and acquisition procedure are reviewed. MAWI remains metadata-only; do not acquire it until its current research-use/privacy terms have been reviewed and the exact file checksum can be recorded. Any acquisition over 50 MB must be explicitly on-demand, checksum-verified, and cached only where the terms permit.
