#!/usr/bin/env python3
"""Enforce the repository-wide aggregate 50 MB committed-dataset policy.

Dataset files are discovered under directories named data, datasets, fixtures,
testdata, or extdata (excluding documentation and tooling directories). The
total across all such tracked files is limited, so splitting one dataset into
multiple files or directories cannot evade the limit. Git LFS pointers and
tracked data/external downloads are rejected.
"""

from __future__ import annotations

import subprocess
import sys
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


def is_dataset_path(path: str) -> bool:
    parts = PurePosixPath(path).parts
    if not parts or parts[0] in NON_DATA_ROOTS:
        return False
    return any(part.lower() in DATA_DIRECTORY_NAMES for part in parts[:-1])


def is_lfs_pointer(sha: str) -> bool:
    blob = subprocess.run(
        ["git", "cat-file", "blob", sha],
        check=True,
        stdout=subprocess.PIPE,
    ).stdout
    return blob.startswith(b"version https://git-lfs.github.com/spec/v1\n")


def main() -> int:
    dataset_files: list[tuple[str, int]] = []
    errors: list[str] = []

    for path, sha, size in tracked_blobs():
        if path.startswith(EXTERNAL_PREFIX):
            errors.append(
                f"{path}: downloaded external data must remain untracked"
            )
            continue

        if not is_dataset_path(path):
            continue
        if size <= 1024 and is_lfs_pointer(sha):
            errors.append(
                f"{path}: Git LFS is not permitted for committed datasets"
            )
        dataset_files.append((path, size))

    total = sum(size for _, size in dataset_files)
    print(
        f"Committed dataset/test data: {total:,} bytes across "
        f"{len(dataset_files)} tracked file(s)"
    )
    if total > MAX_DATASET_BYTES:
        errors.append(
            f"repository-wide aggregate size {total:,} bytes exceeds "
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
