#!/usr/bin/env python3
"""Validate Westermo reduced PCAP ZIPs before publishing a dataset release."""

from __future__ import annotations

import json
import subprocess
import sys
import zipfile
from pathlib import Path, PurePosixPath

PCAP_MAGICS = {
    bytes.fromhex("d4c3b2a1"),
    bytes.fromhex("a1b2c3d4"),
    bytes.fromhex("4d3cb2a1"),
    bytes.fromhex("a1b23c4d"),
    bytes.fromhex("0a0d0d0a"),  # pcapng
}
ARCHIVES = (
    "westermo-right.zip",
    "westermo-left.zip",
    "westermo-bottom.zip",
)


def inspect_archive(archive: Path, output_dir: Path) -> dict:
    capture_dir = output_dir / archive.stem
    capture_dir.mkdir(parents=True, exist_ok=True)
    captures = []

    with zipfile.ZipFile(archive) as zipped:
        bad_member = zipped.testzip()
        if bad_member:
            raise RuntimeError(f"{archive.name}: CRC failure in {bad_member}")

        for info in zipped.infolist():
            if info.is_dir():
                continue
            member_path = PurePosixPath(info.filename)
            if member_path.is_absolute() or ".." in member_path.parts:
                raise RuntimeError(f"Unsafe ZIP member path: {info.filename}")

            with zipped.open(info) as source:
                magic = source.read(4)
            if magic not in PCAP_MAGICS:
                continue

            destination = capture_dir.joinpath(*member_path.parts)
            destination.parent.mkdir(parents=True, exist_ok=True)
            with zipped.open(info) as source, destination.open("wb") as target:
                while chunk := source.read(1024 * 1024):
                    target.write(chunk)

            result = subprocess.run(
                ["capinfos", "-c", "-a", "-e", "-E", "-I", str(destination)],
                check=True,
                capture_output=True,
                text=True,
            )
            captures.append({
                "archive_member": info.filename,
                "uncompressed_bytes": info.file_size,
                "capinfos": result.stdout.strip(),
            })

    if not captures:
        raise RuntimeError(f"{archive.name}: no PCAP/PCAPNG captures found")

    return {
        "archive": archive.name,
        "archive_bytes": archive.stat().st_size,
        "capture_count": len(captures),
        "captures": captures,
    }


def main() -> int:
    if len(sys.argv) != 2:
        print("Usage: validate_westermo.py DIRECTORY", file=sys.stderr)
        return 2

    directory = Path(sys.argv[1]).resolve()
    inspection_dir = directory / "_inspection"
    inspection_dir.mkdir(parents=True, exist_ok=True)
    reports = []

    for filename in ARCHIVES:
        archive = directory / filename
        if not archive.is_file() or archive.stat().st_size == 0:
            raise RuntimeError(f"Missing or empty archive: {filename}")
        reports.append(inspect_archive(archive, inspection_dir))

    report = {
        "dataset": "Westermo Network Traffic Dataset",
        "validation_tool": "capinfos",
        "archives": reports,
    }
    (directory / "validation-report.json").write_text(
        json.dumps(report, indent=2) + "\n", encoding="utf-8"
    )
    print(json.dumps(report, indent=2))
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except Exception as error:
        print(f"Dataset validation failed: {error}", file=sys.stderr)
        raise SystemExit(1)
