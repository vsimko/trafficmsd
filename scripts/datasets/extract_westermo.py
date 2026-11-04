#!/usr/bin/env python3
"""Reproduce a one-observation-point Westermo packet-length time series."""

from __future__ import annotations

import argparse
import csv
import hashlib
import json
import re
import shutil
import subprocess
import tempfile
import urllib.request
import zipfile
from decimal import Decimal, ROUND_FLOOR
from pathlib import Path, PurePosixPath

RELEASE = "dataset-westermo-v1.0"
BASE_URL = f"https://github.com/vsimko/trafficmsd/releases/download/{RELEASE}"
ARCHIVE = "westermo-right.zip"
ASSETS = ("westermo-right.zip", "SHA256SUMS", "manifest.yaml", "validation-report.json")


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def download_assets(directory: Path) -> dict[str, str]:
    directory.mkdir(parents=True, exist_ok=True)
    for name in ASSETS:
        destination = directory / name
        if not destination.exists():
            request = urllib.request.Request(f"{BASE_URL}/{name}", headers={"User-Agent": "trafficmsd-research-pipeline"})
            with urllib.request.urlopen(request, timeout=120) as response, destination.open("wb") as output:
                shutil.copyfileobj(response, output)
        if destination.stat().st_size == 0:
            raise RuntimeError(f"Downloaded asset is empty: {name}")

    expected = {}
    for line in (directory / "SHA256SUMS").read_text(encoding="utf-8").splitlines():
        checksum, name = line.split(maxsplit=1)
        expected[name.strip().lstrip("*")] = checksum.lower()
    release_manifest = (directory / "manifest.yaml").read_text(encoding="utf-8")
    if "license: CC-BY-4.0" not in release_manifest:
        raise RuntimeError("Dataset release manifest does not declare the expected CC-BY-4.0 license")
    actual = sha256(directory / ARCHIVE)
    if expected.get(ARCHIVE) != actual:
        raise RuntimeError(f"SHA-256 mismatch for {ARCHIVE}: expected {expected.get(ARCHIVE)}, got {actual}")
    return {
        "archive_sha256": actual,
        "release_manifest_sha256": sha256(directory / "manifest.yaml"),
        "validation_report_sha256": sha256(directory / "validation-report.json"),
    }


def aggregate_packets(packet_rows: list[tuple[Decimal, int]], bin_width: Decimal):
    """Aggregate timestamp/length pairs into deterministic left-closed bins."""
    if bin_width <= 0:
        raise ValueError("bin_width must be positive")
    if not packet_rows:
        raise ValueError("packet_rows must not be empty")
    ordered = sorted(packet_rows, key=lambda item: item[0])
    origin = ordered[0][0]
    last_timestamp = ordered[-1][0]
    bin_totals: dict[int, int] = {}
    for timestamp, length in ordered:
        if timestamp < 0 or length < 0:
            raise ValueError("packet timestamps and lengths must be non-negative")
        index = int(((timestamp - origin) / bin_width).to_integral_value(rounding=ROUND_FLOOR))
        bin_totals[index] = bin_totals.get(index, 0) + length
    sample_count = max(bin_totals) + 1
    signal = [bin_totals.get(index, 0) for index in range(sample_count)]
    return signal, origin, last_timestamp


def capinfos_packet_count(report_text: str) -> int:
    """Return the exact per-interface count, not capinfos' rounded summary."""
    matches = re.findall(r"Number of packets\\s*[:=]\\s*([\\d,]+)", report_text)
    if not matches:
        raise ValueError("capinfos report does not contain an exact packet count")
    return int(matches[-1].replace(",", ""))


def extract_packets(archive_path: Path, work_dir: Path, bin_width: Decimal):
    report_path = archive_path.parent / "validation-report.json"
    report = json.loads(report_path.read_text(encoding="utf-8"))
    archive_report = next((item for item in report["archives"] if item["archive"] == ARCHIVE), None)
    if archive_report is None:
        raise RuntimeError(f"{ARCHIVE} is absent from validation-report.json")

    packet_rows = []
    member_records = []
    capinfos_by_member = {
        item["archive_member"]: item["capinfos"]
        for item in archive_report["captures"]
    }
    with zipfile.ZipFile(archive_path) as zipped:
        bad_member = zipped.testzip()
        if bad_member:
            raise RuntimeError(f"ZIP CRC failure: {bad_member}")
        expected_members = {item["archive_member"] for item in archive_report["captures"]}
        actual_members = set()
        for info in sorted(zipped.infolist(), key=lambda item: item.filename):
            if info.is_dir():
                continue
            member_path = PurePosixPath(info.filename)
            if member_path.is_absolute() or ".." in member_path.parts:
                raise RuntimeError(f"Unsafe ZIP member path: {info.filename}")
            with zipped.open(info) as source:
                magic = source.read(4)
            if magic not in {bytes.fromhex("d4c3b2a1"), bytes.fromhex("a1b2c3d4"),
                             bytes.fromhex("4d3cb2a1"), bytes.fromhex("a1b23c4d"),
                             bytes.fromhex("0a0d0d0a")}:
                continue
            actual_members.add(info.filename)
            capture_path = work_dir.joinpath(*member_path.parts)
            capture_path.parent.mkdir(parents=True, exist_ok=True)
            with zipped.open(info) as source, capture_path.open("wb") as target:
                shutil.copyfileobj(source, target)

            command = [
                "tshark", "-r", str(capture_path), "-T", "fields",
                "-e", "frame.time_epoch", "-e", "frame.cap_len", "-e", "frame.len",
                "-E", "separator=\t", "-E", "quote=n", "-E", "occurrence=f",
            ]
            completed = subprocess.run(command, check=True, capture_output=True, text=True)
            count = 0
            truncated = 0
            for line_number, line in enumerate(completed.stdout.splitlines(), start=1):
                fields = line.split("\t")
                if len(fields) != 3 or not all(fields):
                    raise RuntimeError(f"Malformed tshark output in {info.filename}:{line_number}")
                timestamp = Decimal(fields[0])
                captured_length = int(fields[1])
                original_length = int(fields[2])
                if timestamp < 0 or captured_length < 0 or original_length < captured_length:
                    raise RuntimeError(f"Invalid packet metadata in {info.filename}:{line_number}")
                packet_rows.append((timestamp, captured_length))
                count += 1
                truncated += int(original_length > captured_length)
            if count == 0:
                raise RuntimeError(f"No packets decoded from {info.filename}")
            capinfos_text = capinfos_by_member[info.filename]
            try:
                reported_count = capinfos_packet_count(capinfos_text)
            except ValueError as error:
                raise RuntimeError(
                    f"Published capinfos packet count is missing for {info.filename}: "
                    f"{capinfos_text!r}"
                ) from error
            if reported_count != count:
                raise RuntimeError(
                    f"Packet count mismatch for {info.filename}: tshark decoded {count}, "
                    f"published capinfos report records {reported_count}; "
                    f"capinfos={capinfos_text!r}"
                )
            member_records.append({
                "archive_member": info.filename,
                "sha256": sha256(capture_path),
                "packet_count": count,
                "truncated_packet_count": truncated,
                "captured_bytes": sum(length for _, length in packet_rows[-count:]),
                "validation_capinfos": capinfos_text,
            })

    if actual_members != expected_members:
        missing = sorted(expected_members - actual_members)
        unexpected = sorted(actual_members - expected_members)
        raise RuntimeError(f"PCAP inventory differs from validation report; missing={missing}, unexpected={unexpected}")
    if not packet_rows:
        raise RuntimeError("No packet records found in selected observation point")

    signal, origin, last_timestamp = aggregate_packets(packet_rows, bin_width)
    sample_count = len(signal)
    return signal, {
        "archive": ARCHIVE,
        "observation_point": "right",
        "capture_members": member_records,
        "packet_count": len(packet_rows),
        "truncated_packet_count": sum(item["truncated_packet_count"] for item in member_records),
        "time_origin_unix": str(origin),
        "model_time_origin": 0,
        "last_packet_time_unix": str(last_timestamp),
        "timestamp_decimal_places": max(max(0, -value.as_tuple().exponent) for value, _ in packet_rows),
        "sampling_interval": str(bin_width),
        "sample_count": sample_count,
        "aggregation": "sum",
        "quantity": "captured_frame_length",
        "value_unit": "byte",
        "bin_convention": "left_closed_right_open",
        "empty_bin_value": 0,
        "deduplication": "none; only the right observation-point archive is processed",
    }


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir", type=Path, default=Path("artifacts/westermo-model-extraction-v1"))
    parser.add_argument("--cache-dir", type=Path, default=Path("data/external/westermo"))
    parser.add_argument("--bin-width", type=Decimal, default=Decimal("1"))
    args = parser.parse_args()
    if args.bin_width <= 0:
        parser.error("--bin-width must be positive")

    hashes = download_assets(args.cache_dir)
    tshark_version = subprocess.run(
        ["tshark", "--version"], check=True, capture_output=True, text=True
    ).stdout.splitlines()[0]
    with tempfile.TemporaryDirectory(prefix="trafficmsd-westermo-") as temporary:
        signal, details = extract_packets(args.cache_dir / ARCHIVE, Path(temporary), args.bin_width)

    args.output_dir.mkdir(parents=True, exist_ok=True)
    trace_path = args.output_dir / "trace.csv"
    with trace_path.open("w", newline="", encoding="utf-8") as output:
        writer = csv.writer(output)
        writer.writerow(["sample_index", "value"])
        writer.writerows(enumerate(signal))

    manifest = {
        "dataset_id": "westermo-network-traffic-dataset-v1.0",
        "dataset_release": RELEASE,
        "source_reference": "https://github.com/westermo/network-traffic-dataset",
        "source_publication": "Per Erik Strandberg et al., The Westermo network traffic data set, Data in Brief 50 (2023), 109512; DOI:10.1016/j.dib.2023.109512",
        "license": "CC-BY-4.0",
        "license_reference": "https://creativecommons.org/licenses/by/4.0/",
        "retrieval_date_utc": __import__("datetime").datetime.now(__import__("datetime").timezone.utc).isoformat(),
        **hashes,
        **details,
        "tool": "tshark",
        "tool_version": tshark_version,
        "python_version": __import__("platform").python_version(),
        "preprocessor_version": "1.0.0",
        "trace_file": "trace.csv",
        "trace_sha256": sha256(trace_path),
        "limitations": [
            "Industrial-network traffic; not representative of general Internet traffic.",
            "Values are captured frame lengths in bytes, not application payload bytes.",
            "Only the right observation-point archive is processed; no cross-observation-point deduplication is performed.",
            "Event annotations are time windows, not packet-level labels.",
        ],
    }
    (args.output_dir / "manifest.json").write_text(json.dumps(manifest, indent=2) + "\n", encoding="utf-8")
    print(json.dumps({"trace": str(trace_path), "manifest": str(args.output_dir / "manifest.json"),
                      "packet_count": details["packet_count"], "sample_count": details["sample_count"]}, indent=2))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
