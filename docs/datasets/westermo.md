# Westermo Network Traffic Dataset

TrafficMSD acquires the reduced packet captures from the upstream Westermo
Network Traffic Dataset and republishes the source archives as assets of the
GitHub Release `dataset-westermo-v1.0`. Raw captures are not committed to the
source tree or Git history.

## Provenance and license

- Upstream repository: https://github.com/westermo/network-traffic-dataset
- Upstream release/version: 1.0 (April 2023)
- License: Creative Commons Attribution 4.0 International (CC BY 4.0)
- License text: https://creativecommons.org/licenses/by/4.0/
- Citation: Per Erik Strandberg et al., *Westermo Network Traffic Dataset*,
  Westermo Network Technologies AB, RISE, Mälardalen University and Tietoevry,
  April 2023.

The upstream dataset describes approximately 1.8 million packets over about
90 minutes, observed at multiple network locations. Reduced captures omit
physical-world simulator traffic. Observation points can contain duplicate
packets; event labels identify time windows, not packet-level ground truth.

## Acquisition

The GitHub Actions workflow
[`westermo-dataset.yml`](../../.github/workflows/westermo-dataset.yml)
downloads the three reduced PCAP archives and the event log from the upstream
repository. It validates ZIP CRCs, checks for PCAP/PCAPNG members, inspects
capture metadata with Wireshark `capinfos`, calculates SHA-256 checksums and
publishes the files to the GitHub Release:

https://github.com/vsimko/trafficmsd/releases/tag/dataset-westermo-v1.0

The release includes `SHA256SUMS`, `manifest.yaml` and a generated
`validation-report.json`. The validation report is authoritative for the
exact packet counts, capture timestamps and link-layer metadata observed by
the workflow.

## Download for experiments

Download the release assets into a local, ignored data directory:

```sh
mkdir -p data/external/westermo
cd data/external/westermo
base="https://github.com/vsimko/trafficmsd/releases/download/dataset-westermo-v1.0"
curl -fL "$base/westermo-right.zip" -o westermo-right.zip
curl -fL "$base/westermo-left.zip" -o westermo-left.zip
curl -fL "$base/westermo-bottom.zip" -o westermo-bottom.zip
curl -fL "$base/events.txt" -o events.txt
curl -fL "$base/SHA256SUMS" -o SHA256SUMS
sha256sum --check SHA256SUMS
```

## Scientific limitations

This is industrial network traffic, not a representative sample of general
Internet traffic. Results must be described within that scope. Packet lengths
are captured frame lengths unless a preprocessing stage explicitly derives a
different byte measure. Do not interpret event-window labels as per-packet
ground truth or count repeated observations at different capture points as
independent packets without an explicit deduplication policy.
