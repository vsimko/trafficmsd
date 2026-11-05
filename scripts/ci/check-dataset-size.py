#!/usr/bin/env python3
"""Enforce the repository's aggregate 50 MB committed-dataset policy.

Dataset files are discovered under directories named data, datasets, fixtures,
testdata, or extdata (excluding documentation and tooling directories). Each
dataset directory is measured as a whole; splitting files does not evade the
limit. Git LFS pointers and tracked data/external downloads are rejected.
"""

from __future__ import annotations

import subprocess
import sys
from collections import defaultdict
from pathlib import PurePosixPath

MAX_DATASET_BYTES = 50_000_000  # 50 MB, decimal
DATA_DIRECTORY_NAMES = {"data", "datasets", "fixtures", "testdata", "extdata"}
NON_DATA_ROOTS = {"docs", ".github", "scripts"}
EXTERNAL_PREFIX = "data/external/"


def tracked_blobs() -> list[tuple[str, str, int]]:
    result = subprocess.run(
        ["git", "ls-tree", "-r", "-l", "-z", "HEAD"],
        check=True,
        stdout=subprocess.PIPE,
    )
    entries: list[tuple[str, str, int]] = []
    for record in result.stdout.split(b"\0"):
        if not record:
            continue
        metadata, raw_path = record.split(b"\t", 1)
        mode, object_type, sha, size_text = metadata.decode("ascii").split()
        if object_type != "blob" or size_text == "-":
            continue
        entries.append((raw_path.decode("utf-8", errors="surrogateescape"),
                        sha, int(size_text)))
    return entries


def dataset_bucket(path: str) -> str | None:
    parts = PurePosixPath(path).parts
    if not parts or parts[0] in NON_DATA_ROOTS:
        return None
    for index, part in enumerate(parts[:-1]):
        if part.lower() in DATA_DIRECTORY_NAMES:
            root = "/".join(parts[: index + 1])
            remainder = parts[index + 1 :]
            # Root-level files form one dataset; each immediate subdirectory
            # is a separate dataset with all of its descendants aggregated.
            return root if len(remainder) == 1 else f"{root}/{remainder[0]}"
    return None


def is_lfs_pointer(sha: str) -> bool:
    blob = subprocess.run(
        ["git", "cat-file", "blob", sha],
        check=True,
        stdout=subprocess.PIPE,
    ).stdout
    return blob.startswith(b"version https://git-lfs.github.com/spec/v1\n")


def main() -> int:
    groups: dict[str, list[tuple[str, int]]] = defaultdict(list)
    errors: list[str] = []

    for path, sha, size in tracked_blobs():
        if path.startswith(EXTERNAL_PREFIX):
            errors.append(
                f"{path}: downloaded external data must remain untracked"
            )
            continue

        bucket = dataset_bucket(path)
        if bucket is None:
            continue
        if size <= 1024 and is_lfs_pointer(sha):
            errors.append(
                f"{path}: Git LFS is not permitted for committed datasets"
            )
        groups[bucket].append((path, size))

    for bucket, files in sorted(groups.items()):
        total = sum(size for _, size in files)
        print(f"{bucket}: {total:,} bytes across {len(files)} tracked file(s)")
        if total > MAX_DATASET_BYTES:
            errors.append(
                f"{bucket}: aggregate size {total:,} bytes exceeds "
                f"{MAX_DATASET_BYTES:,}-byte limit"
            )

    if errors:
        print("\nDataset policy violations:", file=sys.stderr)
        for error in errors:
            print(f"- {error}", file=sys.stderr)
        return 1

    print("Dataset storage policy passed.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
