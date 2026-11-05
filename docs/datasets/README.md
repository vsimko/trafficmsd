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

1. Recheck the official metadata page and current MAWI terms.
2. Acquire the exact file only in a local research run or an explicitly dispatched CI workflow.
3. Record the retrieval date, final byte size, and SHA-256; do not fill in a checksum from metadata alone.
4. Verify the gzip stream and PCAP/dump structure before parsing.
5. Keep the raw file outside Git, in ignored `data/external/mawi-samplepoint-f-200610031400/`.
6. If CI caching is enabled, key it by the registry ID, exact source version/file, and verified SHA-256. Re-verify after restoring the cache.
7. Publish only compact derived aggregates and evaluation results, subject to the dataset terms.

## Adding a dataset

Add a new section with a stable registry ID and the fields above. Explicitly distinguish metadata review from actual acquisition and validation. Datasets over 50 MB require an on-demand acquisition path before they can become CI inputs.
