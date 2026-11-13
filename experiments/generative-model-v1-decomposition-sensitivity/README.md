# Decomposition prefix-sensitivity audit

This is a descriptive, training-prefix-only audit of deterministic multiscale
event-map stability when the same extraction is recomputed at the four
predeclared rolling origins. It does not fit or select a generative model.

## Reproduction

Requirements: Python 3.12 and NumPy 1.26.4. Obtain the immutable
`westermo-model-extraction-and-qpn-v1` artifact from workflow run
[43079950800](https://github.com/vsimko/trafficmsd/actions/runs/43079950800),
then run:

```sh
python -m pip install numpy==1.26.4
python experiments/generative-model-v1-decomposition-sensitivity/audit_decomposition_prefix_sensitivity.py \
  artifacts/westermo-model-extraction-v1/trace.csv \
  --output artifacts/decomposition-prefix-sensitivity/report.json
python -m unittest discover \
  -s experiments/generative-model-v1-decomposition-sensitivity/tests -v
```

The reader consumes only the CSV header and first 2,048 rows and verifies
training-prefix SHA-256
`017e38220cc7590653c630455732c90c2c4c80e148a105e1dabe02e133ebe032`.
No validation/test rows are read, no padding is used, and no synthetic events
are introduced. Higher-scale extraction processes only globally aligned
windows wholly contained in the available prefix; incomplete trailing windows
remain in the residual for lower scales.

The script first recomputes the frozen [0,2048) reference twice and requires
byte-identical canonical event maps, the pinned per-scale counts, and reference
event-map SHA-256
`29a500dbcb42163c37260fafa3f67f421995ad72d3eeee5346d1afeab06d0ea6`.
It then computes all four fold comparisons. The JSON report records script,
configuration, event-map, and report SHA-256 values plus Python/NumPy versions.
The report SHA-256 is also written to a sidecar file.

## Results

See [results.md](results.md) for the run summary and immutable workflow
artifact details. The complete machine-readable report is uploaded as a
GitHub Actions artifact by the dedicated
[decomposition sensitivity workflow](https://github.com/vsimko/trafficmsd/actions/workflows/generative-model-v1-decomposition-sensitivity.yml).

## Interpretation

This is a decomposition-stability audit, not a strict per-tick causal replay.
Interval optimization at each origin still sees the complete prefix available
at that origin, and center-left emission can precede completion of its
observation window. Results do not select a decomposition, representation,
cutoff, regime count, mark family, pooling strategy, or model score. They do
not authorize a production fitter, stochastic generator, or generated QPN
condition.
