# Dataset storage and CI policy

**Status:** repository policy  
**Maximum committed test-data footprint:** 50 MB (50,000,000 bytes) total

## 1. Purpose

Keep source history, ordinary development, and pull-request CI small and reproducible while preserving traceable access to larger research datasets.

## 2. Repository size limit

- The combined stored size of all test and fixture datasets committed to Git must not exceed **50,000,000 bytes (50 MB) total across the repository**. Splitting one dataset into multiple files or directories does not bypass this limit.
- The limit applies to the dataset as stored in the repository, not to each individual file.
- Prefer deterministic synthetic fixtures when they adequately test an algorithm. Keep real-data fixtures small, representative, redistributable, and documented.
- A dataset at or below 50 MB may be committed only when its license and provenance permit redistribution. The size limit is not a license grant.
- Do not commit generated experiment output, caches, temporary downloads, or derived files unless they are small, intentional, and required as versioned fixtures.
- Datasets larger than 50 MB must never be committed to the Git repository, including Git LFS. Do not split, compress, or otherwise repackage a large dataset to evade this rule.

## 3. Large external datasets

For every dataset larger than 50 MB, record an entry in [the dataset registry](datasets/README.md) before using it in a reproducible experiment. Record, where available:

- canonical dataset name, citation, source URL, and immutable release/version;
- license, terms of use, redistribution restrictions, and privacy constraints;
- expected compressed and uncompressed size;
- exact file/member selection and preprocessing scope;
- retrieval procedure and required tools;
- SHA-256 for each acquired file (unknown until bytes are actually obtained);
- acquisition status: metadata reviewed, not acquired, acquired, validated, or retired.

Never invent a checksum or claim acquisition before the bytes have been downloaded and verified.

## 4. CI policy

### Ordinary CI and pull requests

- Must run using repository fixtures, generated synthetic data, and external datasets whose total size is at most 50 MB.
- Must not fetch, unpack, or process datasets larger than 50 MB.
- Must not depend on credentials or external services to access large datasets.
- Should produce compact, deterministic test output.

### Explicit on-demand research workflows

A workflow that needs a dataset larger than 50 MB must be started explicitly (for example, through `workflow_dispatch`). It must:

1. identify the dataset by a registry ID and immutable version;
2. download only the required files into an ignored directory such as `data/external/<dataset-id>/`;
3. verify SHA-256 and any source-provided manifest before processing, including after a cache restore;
4. use a GitHub Actions cache only when licensing, privacy, and repository access rules permit it; key the cache by dataset ID, immutable version, and checksum;
5. fail closed on missing or mismatched checksums, unexpected files, or changed source metadata;
6. upload only necessary derived results as workflow artifacts, with an explicit retention period;
7. never commit downloaded data, extracted PCAPs, or large generated outputs.

A cache is an optimization, not the authoritative copy or provenance record. A cache miss must be safe and reproduce the same acquisition. Restricted or sensitive data must not be placed in a cache or artifact accessible to unauthorized workflows.

## 5. Local development

The `data/external/` directory is ignored by Git. Keep downloaded data there and follow the acquisition and checksum instructions in the dataset registry. Do not remove the ignore rule to make a dataset appear in a commit.

## 6. Current application

- The Westermo right-observation-point archive used by routine extraction CI is 703,780 bytes. All three reduced Westermo capture archives total approximately 28.9 MB. They are distributed as versioned GitHub Release assets, not committed to Git history, and are below the 50 MB threshold.
- The MAWI samplepoint-F candidate is approximately 437.1 MB compressed and 1,173.4 MB uncompressed. It is metadata-only at present and must be acquired only by an explicit on-demand workflow or a local research run.

See [the dataset registry](datasets/README.md) for provenance and acquisition status.
