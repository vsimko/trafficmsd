# Generative fidelity v1 — evaluation harness

**Status:** evaluation protocol and reference evaluator; no production stochastic generator is included.

## Purpose and separation of claims

TrafficMSD evaluations must report three independent layers:

1. **Reconstruction fidelity:** deterministic reconstruction compared with the same original PCAP-derived binned signal. Use the extraction metric and contiguous-holdout protocol in `docs/MODEL_EXTRACTION_METRIC.md` and `docs/model-extraction-evaluation-v1.md`.
2. **Statistical/generative fidelity:** multiple newly generated realizations compared with the empirical reference at the same declared traffic abstraction, units, sampling interval, duration, and regime. This evaluator implements this layer.
3. **Simulation-outcome fidelity:** measured-trace, deterministic replay, and stochastic-generated workloads compared under the same authored QPN model and workload mapping. Follow issue [#82](https://github.com/vsimko/trafficmsd/issues/82) and `docs/simulator-adapter-v1.md`.

Passing one layer does not imply passing another. The evaluator deliberately does not calculate pointwise MAE between a stochastic realization and the original trace.

## Input contract

- `REFERENCE.csv`: CSV with exactly `sample_index,value`.
- `GENERATED_DIR/replicate-*.csv`: at least two independent realization files with the same columns and sample count.
- `MANIFEST.json`: experiment/dataset identifiers, sampling interval and units, preprocessing version, generator name/version, and a filename-to-unique-seed mapping. See [manifest.example.json](manifest.example.json) for the required structure.
- Sample indices are contiguous zero-based integers; values are finite and non-negative.
- All files use the same time origin, regular bin width, value unit, packet/flow selection, and preprocessing contract. These semantics must be recorded in the experiment manifest.
- Each replicate must have its own recorded random seed and generator/configuration version in the experiment manifest. Do not treat repeated copies of one realization as independent replicates.

## Metrics

Per replicate, report:

- mean, standard deviation, coefficient of variation, total volume, zero-bin fraction, and p50/p90/p95/p99;
- Wasserstein-1 and KS distance between empirical bin-value distributions (no independent-sample KS p-value);
- absolute and relative mean difference and standard-deviation difference;
- ACF differences at eligible predeclared lags `1,2,4,8,16,32,64,128,256`;
- burst exceedance fraction and contiguous run counts/median/p95/maximum run lengths, using one p95 threshold estimated from the reference and reused for every replicate.

Across replicates, report median, empirical p05/p95, and IQR for key discrepancy metrics. These percentiles describe variation among supplied realizations; they are not confidence intervals. For scientific uncertainty intervals, use independent seeds and a predeclared bootstrap or other justified inferential procedure. A practical target is at least 20 independent seeds for exploratory characterization and more when uncertainty is material.

## Reproduction

From the repository root:

```sh
Rscript experiments/generative-fidelity-v1/tests.R
Rscript experiments/generative-fidelity-v1/evaluate.R \
  path/to/reference.csv path/to/generated-replicates path/to/manifest.json \
  artifacts/generative-fidelity-v1/evaluation.json
```

The evaluator and end-to-end CLI tests require `jsonlite`; the underlying metric functions use base R. The evaluator validates a unique non-negative seed for every replicate and embeds the complete experiment manifest and each replicate seed in the result JSON. It fails on malformed input, mismatched lengths, invalid provenance, or fewer than two replicates.

## Controls and interpretation

- A deterministic reconstruction/replay is a control and must be labelled as such, not counted as stochastic generation.
- Include simple synthetic controls such as IID empirical resampling and a block-resampling control when they answer the declared use case. These are baselines, not TrafficMSD generation.
- A marginal match with poor ACF or burst fidelity is not a temporal match.
- A close statistical match does not establish simulation-outcome fidelity.
- For non-stationary captures, predeclare regimes/windows and compare corresponding regimes; do not hide regime changes by pooling the entire trace.
- Choose primary metrics and acceptable tolerances from the target use case before looking at final holdout results. Otherwise report the metric vector without naming a winner.

## Current limitation and next gate

The repository currently defines a generator contract but does not yet provide a production stochastic generator. This PR therefore adds a generator-independent evaluator and synthetic metric tests only. The next gate is to implement one explicitly documented generator against the existing model/config contract, emit independent seeded realizations, and evaluate them with this harness before connecting them to issue #82's QPN experiment.
