# Decomposition cutoff sensitivity — occurrence support

**Status:** descriptive training-only sensitivity audit; not model selection  
**Input:** checksum-verified Westermo right-observation-point trace  
**Frozen prefix:** `[0,2048)` only  
**Training-prefix SHA-256:** `017e38220cc7590653c630455732c90c2c4c80e148a105e1dabe02e133ebe032`

## Question

The occurrence audit finds scale 0 all-zero and scales 4–11 all-one at cutoff fraction `0.004`. This audit checks sensitivity to the decomposition interval cutoff; it does not select a cutoff or change the event definition.

## Protocol

- Read only the CSV header and first 2,048 rows; verify the pinned prefix digest.
- Re-run deterministic extraction at six descriptive cutoff fractions: `0.001`, `0.002`, `0.004`, `0.008`, `0.016`, `0.032`.
- Count positive emissions at globally aligned opportunity ticks for each scale.
- Verify that `0.004` reproduces `[0,193,287,224,128,64,32,16,8,4,2,1]`.
- Do not fit occurrence probabilities, choose a regime, or inspect validation/test rows.

## Results

Each cell is positive events / opportunities:

| Scale | 0.001 | 0.002 | 0.004 | 0.008 | 0.016 | 0.032 |
|---:|---:|---:|---:|---:|---:|---:|
| 0 | 134/2048 | 64/2048 | 0/2048 | 0/2048 | 0/2048 | 0/2048 |
| 1 | 609/1024 | 360/1024 | 193/1024 | 43/1024 | 0/1024 | 0/1024 |
| 2 | 442/512 | 403/512 | 287/512 | 217/512 | 56/512 | 0/512 |
| 3 | 234/256 | 234/256 | 224/256 | 155/256 | 126/256 | 32/256 |
| 4 | 128/128 | 128/128 | 128/128 | 128/128 | 128/128 | 123/128 |
| 5 | 64/64 | 64/64 | 64/64 | 64/64 | 64/64 | 64/64 |
| 6 | 32/32 | 32/32 | 32/32 | 32/32 | 32/32 | 32/32 |
| 7 | 16/16 | 16/16 | 16/16 | 16/16 | 16/16 | 16/16 |
| 8 | 8/8 | 8/8 | 8/8 | 8/8 | 8/8 | 8/8 |
| 9 | 4/4 | 4/4 | 4/4 | 4/4 | 4/4 | 4/4 |
| 10 | 2/2 | 2/2 | 2/2 | 2/2 | 2/2 | 2/2 |
| 11 | 1/1 | 1/1 | 1/1 | 1/1 | 1/1 | 1/1 |

## Interpretation

1. **Scales 5–11 remain fully occupied across the entire tested grid.** Their event indicators do not vary in this prefix, so `p01` has no observed previous-zero trials at any tested cutoff. This is an identifiability limitation, not evidence that these scales are universally always-on.
2. **Scale 4 is nearly invariant but not completely:** `128/128` through cutoff `0.016`, then `123/128` at `0.032`.
3. **Scales 0–3 are highly cutoff-sensitive.** Scale 1 ranges from `609/1024` at `0.001` to `0/1024` at `0.016` and `0.032`; scale 2 ranges from `442/512` to `0/512` at `0.032`. Fine-scale occurrence support is therefore conditional on the decomposition cutoff.
4. The baseline `0.004` reproduces the committed occurrence audit exactly. This is a conformance check, not a justification for that cutoff.
5. No cutoff is selected, no probability or pooling strength is estimated, and no scale is relabelled `inactive_by_policy`. The `>=30` independent Gamma mark threshold is unchanged.

## Reproduction

```sh
python experiments/generative-model-v1-sparse-marks/audit_cutoff_sensitivity.py \
  artifacts/westermo-model-extraction-v1/trace.csv \
  --output experiments/generative-model-v1-sparse-marks/cutoff-sensitivity-results.json
```

The script reads exactly the header plus `[0,2048)`, verifies the prefix SHA-256, and refuses non-matching inputs. Validation/test rows are never read.

## Limitations and next decision

- The grid is descriptive on one fixed training prefix, not a hyperparameter search.
- Only the cutoff fraction varies; extraction semantics remain unchanged.
- The analysis does not establish which cutoff best represents traffic or supports prediction.
- Freeze and justify the decomposition cutoff independently of validation/test data before fitting occurrence transitions. Do not add events, lower the Gamma threshold, or convert unresolved states into policy decisions.
