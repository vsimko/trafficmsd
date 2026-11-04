# Westermo model-extraction v1 experiment

This experiment produces the first real-trace deterministic TrafficMSD model
from the Westermo Network Traffic Dataset. It deliberately uses only the
**right** observation-point archive; left and bottom are not combined because
the dataset has multiple observation points and repeated observations may
represent the same packets.

## Reproduce from a clean checkout

Requirements: Python 3.10+, Wireshark/tshark, R, and the package dependencies.

```sh
sudo apt-get install tshark
Rscript -e 'install.packages(c("jsonlite", "digest"))'
R CMD INSTALL .
python scripts/datasets/extract_westermo.py
Rscript experiments/model-extraction-v1/run-westermo.R \
  artifacts/westermo-model-extraction-v1/trace.csv \
  artifacts/westermo-model-extraction-v1/manifest.json \
  artifacts/westermo-model-extraction-v1/model
```

The acquisition script downloads the immutable `dataset-westermo-v1.0`
release, verifies the archive against `SHA256SUMS`, checks the exact PCAP
inventory against `validation-report.json`, and processes every PCAP/PCAPNG
member in `westermo-right.zip`. It uses `frame.time_epoch` and
`frame.cap_len` from tshark. Values are therefore **captured frame bytes**,
not application payload bytes or original wire lengths.

Bins are left-closed/right-open, one second wide by default, aligned to the
first packet timestamp. The model time origin is relative zero; the exact
Unix timestamp origin is preserved separately in the extraction manifest. Packet lengths are summed and empty bins are zero.
Truncated packets are retained using their captured length and counted in the
manifest. Malformed packet metadata causes a hard failure rather than silent
packet loss. The output manifest records release provenance, checksums,
selected observation point, member checksums, packet counts, timestamp
precision, truncation count, and preprocessing choices.

The R stage zero-pads only for decomposition, preserves the observed sample
count in model metadata, and evaluates reconstruction only over the observed
interval. It emits model v1 JSON, evaluation-result v1 JSON, and source and
reconstructed CSV files. CI validates both JSON documents against the checked-in
schemas.

## Interpretation limits

Westermo is an industrial-network dataset. This single-observation-point
experiment is a reproducibility and model-extraction demonstration, not
evidence that the trace represents general Internet traffic. The event log
contains time-window annotations, not packet-level labels. The extracted
components describe deterministic reconstruction; they are not stochastic
traffic-generator parameters. Full-trace metrics are descriptive diagnostics,
not out-of-sample generalization estimates.
