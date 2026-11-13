# Decomposition prefix-sensitivity results

**Status:** Reproduced twice byte-identically

**Workflow run:** [43384596620](https://github.com/vsimko/trafficmsd/actions/runs/43384596620)  
**Artifact:** `generative-model-v1-decomposition-prefix-sensitivity` (ID `14018330713`)  
**Report SHA-256:** `181963574f889932194ed7ec6e9a37bd48b98a0337160894fc51026e5c0d82a2`  
**Script SHA-256:** `c8d8754dd0ccc034aab1bd575c9d740f9c02eafd671d22268039892b74f85f20`  
**Reference event-map SHA-256:** `29a500dbcb42163c37260fafa3f67f421995ad72d3eeee5346d1afeab06d0ea6`  
**Environment:** Python 3.12.16, NumPy 1.26.4  
**Regression tests:** 7/7 passed

## Reference conformance

The complete-window implementation reproduced the pinned reference positive-event counts exactly for scales 0–11:

`0, 193, 287, 224, 128, 64, 32, 16, 8, 4, 2, 1`.

The canonical reference event map was independently recomputed twice before fold comparison. Both byte content and the pinned SHA-256 matched.

## Fold-level event-key stability

| Fold origin | Reference events | Prefix events | Agreed keys | Union keys | Jaccard | Prefix-only additions | Reference-only removals |
|---:|---:|---:|---:|---:|---:|---:|---:|
| 1,024 | 486 | 665 | 459 | 692 | 0.6633 | 206 | 27 |
| 1,280 | 612 | 815 | 571 | 856 | 0.6671 | 244 | 41 |
| 1,536 | 733 | 861 | 691 | 903 | 0.7652 | 170 | 42 |
| 1,792 | 847 | 985 | 795 | 1,037 | 0.7666 | 190 | 52 |

Jaccard overlap by scale:

| Scale | Fold 1 | Fold 2 | Fold 3 | Fold 4 |
|---:|---:|---:|---:|---:|
| 0 | 0.000 | 0.000 | 0.000 | 1.000 |
| 1 | 0.381 | 0.343 | 0.489 | 0.486 |
| 2 | 0.686 | 0.705 | 0.711 | 0.701 |
| 3 | 0.984 | 0.968 | 0.973 | 0.967 |
| 4–10 | 1.000 | 1.000 | 1.000 | 1.000 |
| 11 | n/a | n/a | n/a | n/a |

Scale 11 has no eligible opportunities in any fold. Scale 0 has no positive events in the frozen reference; prefix-local extraction has 39, 31, 7, and 0 positive events respectively.

## Matched-key mark-amplitude stability

Absolute mark differences for matched event keys:

| Fold origin | Matched marks | Mean absolute difference | Median | 95th percentile | Maximum |
|---:|---:|---:|---:|---:|---:|
| 1,024 | 459 | 84.71 | 0 | 0 | 17,672 |
| 1,280 | 571 | 50.60 | 0 | 0 | 17,672 |
| 1,536 | 691 | 27.77 | 0 | 60 | 2,230 |
| 1,792 | 795 | 38.05 | 0 | 72.6 | 9,176 |

The zero medians and (in early folds) zero 95th percentiles coexist with large maxima: most matched amplitudes are unchanged, but a small number of matched events change substantially. The report also contains relative absolute differences, signed log mark ratios, and positive-mark sums by scale.

## Interpretation boundaries

- The largest event-key instability is concentrated at scales 0–2; scale 3 is highly but not perfectly stable, and scales 4–10 have identical event keys in these prefixes.
- The frozen full-prefix decomposition has no scale-0 positive marks, while earlier prefix-local decompositions leave positive scale-0 residuals. This is a decomposition-context effect, not a discovered event process or a reason to change the model.
- The audit confirms that the existing rolling-origin HMM/mark results are conditional on the frozen offline decomposition. It does not establish a strict per-tick causal generator: interval optimization sees all context in each available prefix, and center-left emissions can precede completion of their aligned windows.
- No HMM, occurrence probability, Gamma/lognormal mark model, or predictive score was fit or compared. No representation, regime count, cutoff, pooling rule, or model family was selected.
- Validation/test rows were not read; no padding or synthetic events were used. The 30-positive-event Gamma support rule and the original A/B/C report remain unchanged.

The full JSON report and its SHA sidecar are retained in the workflow artifact listed above.
