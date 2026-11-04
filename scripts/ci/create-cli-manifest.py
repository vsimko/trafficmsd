#!/usr/bin/env python3
"""Create the deterministic manifest for a complete TrafficMSD CLI release."""

from __future__ import annotations

import hashlib
import json
import re
import sys
from pathlib import Path

EXPECTED_TARGETS = {
    "x86_64-unknown-linux-gnu",
    "aarch64-unknown-linux-gnu",
    "x86_64-apple-darwin",
    "aarch64-apple-darwin",
    "x86_64-pc-windows-msvc",
}


def main() -> int:
    if len(sys.argv) != 3:
        print("usage: create-cli-manifest.py <version> <dist-directory>", file=sys.stderr)
        return 2

    version = sys.argv[1]
    dist = Path(sys.argv[2])
    if not re.fullmatch(r"[A-Za-z0-9._-]+", version):
        print(f"invalid release version: {version!r}", file=sys.stderr)
        return 2
    if not dist.is_dir():
        print(f"distribution directory does not exist: {dist}", file=sys.stderr)
        return 2

    targets = {}
    prefix = f"trafficmsd-{version}-"
    for path in sorted(dist.iterdir()):
        if not path.is_file() or not path.name.startswith(prefix):
            continue
        target = path.name[len(prefix):]
        if target.endswith(".exe"):
            target = target[:-4]
        if target not in EXPECTED_TARGETS:
            print(f"unexpected binary asset: {path.name}", file=sys.stderr)
            return 1
        if target in targets:
            print(f"duplicate binary asset for target: {target}", file=sys.stderr)
            return 1
        sha256 = hashlib.sha256(path.read_bytes()).hexdigest()
        targets[target] = {"asset": path.name, "sha256": sha256}

    missing = sorted(EXPECTED_TARGETS - set(targets))
    if missing:
        print(f"missing platform binaries: {', '.join(missing)}", file=sys.stderr)
        return 1

    manifest = {
        "schema_version": 1,
        "release_version": version,
        "cli_version": version,
        "targets": targets,
    }
    manifest_path = dist / "trafficmsd-cli-manifest.json"
    manifest_path.write_text(
        json.dumps(manifest, indent=2, sort_keys=True) + "\n",
        encoding="utf-8",
    )
    print(f"Created {manifest_path} for {len(targets)} targets")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
