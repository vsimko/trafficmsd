# Research dataset registry

This registry records research datasets used by TrafficMSD or considered for future validation. A registry entry is not evidence that the dataset has been downloaded, validated, or approved for redistribution.

The governing storage and CI rules are in [Dataset storage and CI policy](../data-management.md).

## Westermo Network Traffic Dataset v1.0

| Field | Value |
|---|---|
| Registry ID | `westermo-v1` |
| Source | [Westermo Network Traffic Dataset](https://github.com/westermo/network-traffic-dataset) |
| Upstream release | v1.0 (April 2023) |
| Traffic type | Industrial network traffic; multiple observation points |
| Citation | Per Erik Strandberg et al., *The Westermo Network Traffic Dataset*, Data in Brief 50 (2023), 109512. [DOI: 10.1016/j.dib.2023.109512](https://doi.org/10.1016/j.dib.2023.109512) |
| License | CC BY 4.0, as declared in the TrafficMSD dataset release manifest |
| TrafficMSD release | [`dataset-westermo-v1.0`](https://github.com/vsimko/trafficmsd/releases/tag/dataset-westermo-v1.0) |
| Selected archive | `westermo-right.zip` |
| Selected archive size | 703,780 bytes |
| Three reduced capture archives | 28,920,226 bytes combined (bottom, left, right) |
| Selected archive SHA-256 | `8a1f49eb49747f3046eda8063b104834f41f0df30d4fc2357aea5609826a4445` |
| Acquisition status | Acquired, checksum-verified, and used by the routine extraction workflow |

### Reproduction notes

The experiment selects only the right observation-point archive; it does not combine observation points because duplicate observations may occur. The extraction script downloads the immutable TrafficMSD release assets into the ignored `data/external/westermo/` directory and verifies the archive against `SHA256SUMS` before processing.

The selected archive is below the 50 MB threshold, so it is eligible for ordinary CI. The complete set of three reduced capture archives is also below the threshold, but is not needed by the current extraction job.

Scientific limitations: this is industrial traffic, not a representative sample of general Internet traffic. Captured frame lengths are not application payload lengths, and event annotations are time windows rather than packet-level labels.

## MAWI samplepoint-F: 2006-10-03 14:00 UTC

| Field | Value |
|---|---|
| Registry ID | `mawi-samplepoint-f-200610031400` |
| Source archive | [MAWI Traffic Archive](https://mawi.nezu.wide.ad.jp/mawi/) |
| Trace metadata | [200610031400](https://mawi.nezu.wide.ad.jp/mawi/samplepoint-F/2006/200610031400.html) |
| File | `200610031400.dump.gz` |
| Capture window | 2006-10-03 14:00–14:15 UTC |
| Duration | 899.66 seconds |
| Packet count | 16,581,271 |
| Reported average rate | 94.23 Mbps |
| Snap length | 96 bytes |
| Compressed size | Approximately 437.10 MB |
| Uncompressed size | Approximately 1,173.37 MB |
| License / terms | No general redistribution license established in this review. MAWI research-use and privacy guidelines apply; confirm current terms before acquisition or redistribution. |
| SHA-256 | Unknown — file has not been acquired |
| Acquisition status | Metadata reviewed; bytes not downloaded or processed |
| CI policy | On-demand only; never fetch in ordinary pull-request CI |

### Research suitability and limitations

This is a broad backbone trace and may support cross-dataset evaluation of aggregate traffic characteristics. It is not application-labeled and does not provide application-level ground truth. MAWI anonymizes/scrambles IP addresses; retain its privacy constraints and do not attempt re-identification.

The 2006 capture belongs to the historical samplepoint-F capture period. MAWI documents a 100 Mbps to 1 Gbps link upgrade in 2007, so this trace must not be treated as representative of later capture conditions.

### Acquisition requirements

Before using this trace:

1. Recheck the official metadata page and current MAWI terms. The registry does not establish permission to cache or redistribute the raw capture.
2. Acquire the exact file only in a local research run or through the explicitly dispatched [MAWI acquisition workflow](../../.github/workflows/research-dataset-mawi.yml). Ordinary CI and pull-request workflows must never fetch it.
3. The on-demand workflow requires the registry ID, an externally verified 64-character SHA-256, explicit confirmation that current terms were reviewed, and confirmation that those terms permit the private Actions cache. It verifies SHA-256 after both download and cache restore, then verifies the gzip stream.
4. The checksum remains unknown until the exact bytes are acquired and hashed; do not invent or prefill it from metadata. The workflow deliberately fails closed while the checksum or terms approval is missing.
5. Verify the PCAP/dump structure before parsing.
6. Keep the raw file outside Git, in ignored `data/external/mawi-samplepoint-f-200610031400/`.
7. Publish only compact derived aggregates and evaluation results, subject to the dataset terms.
## Adding a dataset

Add a new section with a stable registry ID and the fields above. Explicitly distinguish metadata review from actual acquisition and validation. Datasets over 50 MB require an on-demand acquisition path before they can become CI inputs.


## Additional candidates reviewed for independent validation

The following sources were screened on 2026-11-17. They are retained as documented alternatives, not as approved or acquired TrafficMSD inputs. "Too large" refers to the complete raw dataset/archive or to the lack of a verified small independent capture; it does not mean that a carefully selected individual trace could never be used. Any selected trace still needs its own immutable identity, exact byte size, checksum, license/terms review, and preprocessing record.

### UNSW-IoTraffic

| Field | Value |
|---|---|
| Registry ID | `unsw-iotraffic-dryad-2025` |
| Source | [Dryad dataset DOI 10.5061/dryad.w0vt4b94b](https://doi.org/10.5061/dryad.w0vt4b94b) |
| Traffic type | Consumer IoT traffic from a lab testbed; 27 devices |
| Capture period | Approximately 203 days; timestamps in UTC |
| Contents | 27 per-device raw PCAPs with full headers and payloads; separate flow-level CSVs and protocol parameter data |
| Published scale | 95,543,405 packets; 4,944,041 flows; approximately 26.9 GB of PCAP data |
| Published archive sizes | `pcaps.zip` 13.92 GB; `flows.zip` 95.51 MB; `protocols.zip` 186.96 MB; root archive 14.24 GB |
| License / terms | Review the current Dryad record and included README/license before acquisition or reuse; no redistribution permission is inferred here |
| Acquisition status | Metadata reviewed only; no files downloaded; per-device PCAP sizes and checksums not verified |
| TrafficMSD suitability | **Promising independent raw-traffic candidate**, with packet timestamps and full packet captures. Complete archive is far too large for the 50 MB ordinary-CI budget. A single device PCAP may be usable if its exact size is independently confirmed at or below the budget, or through a separately approved on-demand research workflow if larger. |

This is a strong candidate for a future independent-capture comparison because it provides raw packet traces rather than only flow aggregates. Do not download the full archive for this purpose. First inspect the published per-device summary and determine whether one device trace is small enough and scientifically suitable. The 95.51 MB flow archive is also above the ordinary-CI threshold and its flow aggregates are not a substitute for packet-level time-series extraction.

### RIT General Packet Datasets for Network Research

| Field | Value |
|---|---|
| Registry ID | `rit-general-packet-datasets-2023` |
| Source | [RIT repository DOI 10.57673/gccis-yg55](https://repository.rit.edu/data/1/) |
| Traffic type | General packet datasets with curated class-balanced subsets and one raw capture |
| Small curated files | `dataset1.txt`: 4,136 kB / 7,000 packets; `dataset0.txt`: 61,728 kB / 70,000 packets |
| Raw capture | `capture1-10-11-23.pcapng`: 952,256 kB / 978,827 packets |
| License / terms | Confirm the repository's current reuse terms before redistribution or caching |
| Acquisition status | Metadata reviewed only; no files downloaded; checksums not verified |
| TrafficMSD suitability | **Raw capture is potentially usable**, but is much larger than 50 MB and would require explicit on-demand/local research acquisition. The small text datasets are curated and class-balanced, so they are not valid substitutes for a chronological raw packet trace. |

RIT remains a possible independent packet source if the full raw capture is acceptable for a local/on-demand experiment. Its curated subsets can still be useful for classification-oriented or parser smoke tests, but not as the independent temporal validation requested in issue #108.

### NetMetria public sample PCAPs

| Field | Value |
|---|---|
| Registry ID | `netmetria-public-scenarios-2026` |
| Source | [NetMetria public sample library](https://netmetria.com/samples) |
| Traffic type | Three generated, post-compromise defensive-security scenarios |
| NM-SAMPLE-001 | 230.2 KiB; 1,592 packets; 11m 59s |
| NM-SAMPLE-002 | 622.6 KiB; 4,134 packets; 27m 59s |
| NM-SAMPLE-003 | 877.5 KiB; 6,109 packets; 51m 59s |
| Published checksums | SHA-256 values are published per sample on the source page |
| License / terms | Source permits defensive analysis, research, training, and testing; it does not grant rights to repackage or commercially redistribute the samples as a dataset product. Review current terms for the intended use |
| Ground truth | Not included in the public downloads |
| Acquisition status | Metadata reviewed only; no files downloaded or checksums independently verified |
| TrafficMSD suitability | **Small enough for local experiments and parser/integration tests**, but not a substitute for an independent operational capture: the samples are generated scenarios and public ground truth is unavailable. |

These files are not too large for the 50 MB budget. They are recorded because they are usable for supplementary robustness and tooling tests, not because they satisfy the independent real-capture validation gate.

### Measurement Lab (M-Lab) Packet Header Data Set

| Field | Value |
|---|---|
| Registry ID | `mlab-packet-header-dataset-2019-present` |
| Source | [M-Lab Packet Header Data Set](https://www.measurementlab.net/data/pcap/) |
| Traffic type | Incoming TCP measurement flows, stored as per-flow PCAP files |
| Time range | 2019–present; continuously updated |
| Contents | Packet headers only; payloads are deliberately excluded |
| License | CC0 1.0 Universal, as declared by M-Lab |
| Published total size | No single bounded archive size; data is distributed across service-specific archives in Google Cloud Storage |
| Acquisition status | Metadata reviewed only; no archive or individual flow acquired; selected-flow sizes/checksums not verified |
| TrafficMSD suitability | **Potentially usable with careful selection**, because an individual flow file may be small and timestamps are retained. The full continuously updated corpus is not a bounded CI input. Header-only captures do not contain captured payload/frame lengths, so they cannot directly reproduce the current TrafficMSD byte-volume signal. |

M-Lab may support a distinct header/timing-focused experiment or a flow-level robustness check. It should not be treated as directly equivalent to the current packet-length workload without defining and validating a separate measurement target. A small per-flow sample is not an independent aggregate network capture.

### Candidate disposition

| Candidate | Size relative to 50 MB | Can it be used? | Appropriate role |
|---|---|---|---|
| MAWI samplepoint-F | 437 MB compressed; 1.17 GB expanded | Yes, with terms review and on-demand/local acquisition | Independent backbone trace; too large for ordinary CI |
| UNSW-IoTraffic | 13.92 GB PCAP archive; per-device sizes unverified | Yes; first identify one suitably sized device PCAP | Independent IoT raw capture; avoid full archive |
| RIT raw capture | 952 MB | Yes, with on-demand/local acquisition | Independent raw capture; curated text subsets only for other tests |
| RIT curated subsets | 4.1 MB to 247.8 MB depending on file | Yes, for limited non-temporal tests; larger files exceed CI budget | Classification/parser tests, not chronological validation |
| NetMetria scenarios | 0.23–0.88 MB each | Yes | Parser, integration, and generated-scenario robustness tests |
| M-Lab | No bounded total; per-flow sizes unknown | Yes, for a separately defined header/timing experiment | Per-flow TCP header analysis; not current byte-volume target |

None of these entries changes the 50 MB repository/ordinary-CI policy. Large candidates may be used in local research or explicitly dispatched workflows when their terms, exact file selection, checksum, and storage controls are approved. Do not split, truncate, or curate a large capture merely to make it appear to meet the CI limit; any subset must be scientifically justified and described as a distinct sample.


## Additional UNSW IoT flow-level candidates (screened 2026-11-17)

These two Dryad releases are useful independent IoT *flow-level* candidates, but neither is a raw PCAP corpus. They are recorded separately from the packet-capture candidates above because their observation unit and available packet detail differ from the current TrafficMSD one-second captured-frame-byte signal.

### UNSW DATA2025v1 / DATA2025v2 cross-environment flow datasets

| Field | Value |
|---|---|
| Registry ID | `unsw-data2025-customflows-v1-v2` |
| Source | [Dryad DOI 10.5061/dryad.7h44j1098](https://doi.org/10.5061/dryad.7h44j1098) |
| Published archives | `DATA2025v1.tar.gz`: 12.64 GB; `DATA2025v2.tar.gz`: 7.66 GB; full deposit: 22.52 GB |
| Dataset scale | Approximately 16.5 million Custom Flow records; v1 about 7.7 million records from 18 device types; v2 about 8.8 million records from 10 overlapping device types |
| Capture context | Physical IoT devices in live 2025 deployments; v2 is a separate deployment with a different network topology, background traffic, and user activity |
| Format / fields | Daily Parquet flow records with flow timestamps, total flow bytes, packet counts, and offsets/sizes/directions for the first 10 packets; encoded payload fields are also present |
| License / terms | Review the dataset README and Dryad record for the applicable data license, privacy, and redistribution terms before acquisition; no permission is inferred from the page's general content-reuse notice |
| Acquisition status | Metadata reviewed only; no files downloaded; daily member sizes and checksums not verified |
| TrafficMSD suitability | **Potentially useful for independent cross-environment flow-level validation**, especially v1 versus v2. Both full archives are far above the 50 MB ordinary-CI budget. It is not a raw packet trace and cannot directly reproduce the current packet-level captured-frame-byte signal. |

The v2 deployment is particularly interesting as a separate environment, not just another device/observation point. A scientifically defensible experiment could derive a flow-level signal from daily Parquet members and compare flow sizes, packet counts, packet-size/time-offset summaries, and inter-flow activity. It must be labelled as a separate flow-level measurement target. A single daily member may be smaller than the full archive, but its exact size, checksum, and representativeness must be checked before selecting it. Local or explicit on-demand research acquisition is possible under the repository's large-dataset policy; ordinary CI must not fetch the complete archives.

### UNSW DATA2016 custom-flow representation

| Field | Value |
|---|---|
| Registry ID | `unsw-data2016-customflows` |
| Source | [Dryad DOI 10.5061/dryad.6q573n6c1](https://doi.org/10.5061/dryad.6q573n6c1) |
| Published archives | Bidirectional: 1.78 GB; unidirectional: 794.91 MB; total: 2.57 GB |
| Dataset scale | More than 5.9 million custom-flow records from 60 days of IoT PCAP captures, covering 22 device types |
| Format / fields | Daily Parquet custom flows; total flow bytes and packet counts plus time offsets, packet sizes, and directions for the first 10 packets; selected payload bytes |
| License / terms | Review the README and Dryad record for the applicable data license, privacy, and redistribution terms before acquisition |
| Acquisition status | Metadata reviewed only; no files downloaded; daily member sizes and checksums not verified |
| TrafficMSD suitability | **Usable for a separate flow-level temporal/size experiment**, but the complete archives exceed 50 MB and the derived custom-flow representation is not equivalent to a raw packet capture or the current one-second byte series. |

DATA2016 is based on earlier UNSW IoT packet captures and provides a structured hybrid of flow metadata and selected packet-level features. It may be useful for exploratory flow-level workload analysis, but it is not an independent raw capture for the current packet-level acceptance gate. Prefer DATA2025v1/v2 if cross-environment validation is the goal; prefer a raw PCAP candidate when validating the existing packet-length signal.

Both candidates are metadata-only and unapproved for acquisition at this point. Any selected Parquet member still requires an immutable version, exact member path and byte size, SHA-256, license/terms review, and a documented flow-to-time-series transformation. Do not commit the full archives or fetch them in ordinary CI.
