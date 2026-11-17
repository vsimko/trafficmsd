#!/usr/bin/env python3
"""Compare offline and test-only streaming multiscale coefficient amplitudes."""

from __future__ import annotations

import argparse
import csv
import hashlib
import json
import math
from pathlib import Path
from typing import Any


def read_json(path: Path) -> tuple[dict[str, Any], bytes]:
    raw = path.read_bytes()
    value = json.loads(
        raw,
        parse_constant=lambda token: (_ for _ in ()).throw(ValueError(token)),
    )
    if not isinstance(value, dict):
        raise ValueError(f"{path} must contain a JSON object")
    return value, raw


def sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def read_signal(path: Path, expected_count: int) -> list[float]:
    values: list[float] = []
    with path.open(newline="", encoding="utf-8") as handle:
        reader = csv.DictReader(handle)
        if reader.fieldnames != ["sample_index", "value"]:
            raise ValueError(f"{path} must have sample_index,value columns")
        for expected_index, row in enumerate(reader):
            if int(row["sample_index"]) != expected_index:
                raise ValueError(f"{path} sample indices must be contiguous from zero")
            value = float(row["value"])
            if not math.isfinite(value) or value < 0:
                raise ValueError(f"{path} values must be finite and non-negative")
            values.append(value)
    if len(values) != expected_count:
        raise ValueError(f"{path} has {len(values)} samples; expected {expected_count}")
    return values


def load_offline_candidates(model_path: Path) -> tuple[list[list[float]], dict[str, Any], bytes]:
    model, raw = read_json(model_path)
    coverage = model["coverage"]
    padded_count = int(coverage["decomposition_sample_count"])
    observed_count = int(coverage["observed_sample_count"])
    if padded_count <= 0 or padded_count & (padded_count - 1):
        raise ValueError("offline model decomposition length must be a positive power of two")
    if not 0 < observed_count <= padded_count:
        raise ValueError("offline model observed sample count is invalid")
    max_scale = padded_count.bit_length() - 1
    candidates = [
        [0.0] * (padded_count // (1 << scale))
        for scale in range(max_scale + 1)
    ]
    seen: set[tuple[int, int]] = set()
    for component in model["components"]:
        scale = int(component["scale_index"])
        start = int(component["window_start_sample"])
        width = int(component["window_length_samples"])
        emission = int(component["emission_sample_index"])
        amplitude = float(component["amplitude"])
        if not 0 <= scale <= max_scale:
            raise ValueError("offline component scale is out of range")
        expected_width = 1 << scale
        if width != expected_width or start < 0 or start % width:
            raise ValueError("offline component window does not match its scale")
        expected_emission = start if scale == 0 else start + width // 2 - 1
        if emission != expected_emission or emission >= padded_count:
            raise ValueError("offline component emission index is inconsistent")
        if not math.isfinite(amplitude) or amplitude < 0:
            raise ValueError("offline coefficient amplitudes must be finite and non-negative")
        key = (scale, emission)
        if key in seen:
            raise ValueError("offline model contains duplicate scale/emission components")
        seen.add(key)
        candidates[scale][emission // width] = amplitude
    return candidates, {
        "observed_sample_count": observed_count,
        "padded_sample_count": padded_count,
        "max_scale": max_scale,
        "model_id": model.get("model_id"),
    }, raw


def load_streaming_candidates(
    matrix_path: Path, padded_count: int, max_scale: int
) -> tuple[list[list[float]], bytes]:
    matrix = [
        [0.0] * padded_count
        for _ in range(max_scale + 1)
    ]
    seen = bytearray((max_scale + 1) * padded_count)
    row_count = 0
    with matrix_path.open(newline="", encoding="utf-8") as handle:
        reader = csv.DictReader(handle)
        if reader.fieldnames != ["scale_index", "sample_index", "value"]:
            raise ValueError(
                f"{matrix_path} must have scale_index,sample_index,value columns"
            )
        for row in reader:
            scale = int(row["scale_index"])
            sample = int(row["sample_index"])
            value = float(row["value"])
            if not 0 <= scale <= max_scale or not 0 <= sample < padded_count:
                raise ValueError("streaming coefficient matrix index is out of range")
            if not math.isfinite(value) or value < 0:
                raise ValueError("streaming coefficients must be finite and non-negative")
            offset = scale * padded_count + sample
            if seen[offset]:
                raise ValueError("streaming coefficient matrix contains duplicate cells")
            seen[offset] = 1
            matrix[scale][sample] = value
            row_count += 1
    expected_rows = (max_scale + 1) * padded_count
    if row_count != expected_rows:
        raise ValueError(
            f"streaming matrix has {row_count} cells; expected {expected_rows}"
        )
    return [
        [
            matrix[scale][
                index if scale == 0 else index * (1 << scale) + (1 << (scale - 1)) - 1
            ]
            for index in range(padded_count // (1 << scale))
        ]
        for scale in range(max_scale + 1)
    ], matrix_path.read_bytes()


def reconstruct(candidates: list[list[float]], padded_count: int) -> list[float]:
    output = [0.0] * padded_count
    for scale, row in enumerate(candidates):
        width = 1 << scale
        for index, value in enumerate(row):
            emission = index * width if scale == 0 else index * width + width // 2 - 1
            output[emission] += value
    return output


def compare_vectors(left: list[float], right: list[float]) -> dict[str, float | int]:
    if len(left) != len(right) or not left:
        raise ValueError("coefficient vectors must have equal non-zero lengths")
    errors = [b - a for a, b in zip(left, right)]
    absolute = [abs(value) for value in errors]
    return {
        "candidate_count": len(left),
        "offline_positive_count": sum(value > 0 for value in left),
        "streaming_positive_count": sum(value > 0 for value in right),
        "positive_count_difference": sum(value > 0 for value in right)
        - sum(value > 0 for value in left),
        "mae": sum(absolute) / len(absolute),
        "rmse": math.sqrt(sum(value * value for value in errors) / len(errors)),
        "max_absolute_error": max(absolute),
        "mean_signed_error": sum(errors) / len(errors),
    }


def compare_signals(actual: list[float], expected: list[float]) -> dict[str, float]:
    if len(actual) != len(expected) or not actual:
        raise ValueError("reconstructed signals must have equal non-zero lengths")
    errors = [a - b for a, b in zip(actual, expected)]
    absolute = [abs(value) for value in errors]
    return {
        "mae": sum(absolute) / len(absolute),
        "rmse": math.sqrt(sum(value * value for value in errors) / len(errors)),
        "max_absolute_error": max(absolute),
    }


def analyze(
    offline_model_path: Path,
    streaming_matrix_path: Path,
    offline_signal_path: Path,
    streaming_signal_path: Path,
    output_dir: Path,
) -> dict[str, Any]:
    offline, metadata, model_raw = load_offline_candidates(offline_model_path)
    padded = metadata["padded_sample_count"]
    observed = metadata["observed_sample_count"]
    max_scale = metadata["max_scale"]
    streaming, matrix_raw = load_streaming_candidates(
        streaming_matrix_path, padded, max_scale
    )
    offline_signal = read_signal(offline_signal_path, observed)
    streaming_signal = read_signal(streaming_signal_path, observed)

    per_scale = []
    for scale, (offline_row, streaming_row) in enumerate(zip(offline, streaming)):
        full = compare_vectors(offline_row, streaming_row)
        width = 1 << scale
        observed_count = sum(
            1 for index in range(len(offline_row))
            if (index * width if scale == 0 else index * width + width // 2 - 1) < observed
        )
        full["observed_candidate_count"] = observed_count
        full["observed"] = compare_vectors(
            offline_row[:observed_count], streaming_row[:observed_count]
        )
        per_scale.append({"scale": scale, "window_seconds": width, **full})

    offline_reconstructed = reconstruct(offline, padded)[:observed]
    streaming_reconstructed = reconstruct(streaming, padded)[:observed]
    offline_round_trip = compare_signals(offline_reconstructed, offline_signal)
    streaming_round_trip = compare_signals(streaming_reconstructed, streaming_signal)
    tolerance = 1e-8
    if offline_round_trip["max_absolute_error"] > tolerance:
        raise ValueError(
            "offline model components do not reconstruct the supplied offline signal"
        )
    if streaming_round_trip["max_absolute_error"] > tolerance:
        raise ValueError(
            "streaming coefficient matrix does not reconstruct the supplied streaming signal"
        )

    result = {
        "schema_version": "1.0",
        "comparison_id": "westermo-offline-vs-streaming-coefficients-v1",
        "status": "completed",
        "scope": "right observation point; unpadded observed bins for the primary comparison",
        "policy": "test-only fixed-duration multiscale, T_min=32 seconds",
        "dataset": "Westermo Network Traffic Dataset v1.0",
        "observed_sample_count": observed,
        "padded_sample_count": padded,
        "offline_model_sha256": hashlib.sha256(model_raw).hexdigest(),
        "streaming_coefficient_matrix_sha256": hashlib.sha256(matrix_raw).hexdigest(),
        "offline_signal_sha256": sha256(offline_signal_path),
        "streaming_signal_sha256": sha256(streaming_signal_path),
        "reconstruction_round_trip": {
            "offline_model_vs_offline_signal": offline_round_trip,
            "streaming_matrix_vs_streaming_signal": streaming_round_trip,
        },
        "aggregate": {
            "offline_positive_candidates": sum(item["offline_positive_count"] for item in per_scale),
            "streaming_positive_candidates": sum(item["streaming_positive_count"] for item in per_scale),
            "positive_candidate_difference": sum(item["positive_count_difference"] for item in per_scale),
        },
        "per_scale": per_scale,
        "limitations": [
            "Westermo right is one observation point from one industrial-network capture.",
            "T_min=32 seconds is exploratory and has not been selected on held-out captures.",
            "Candidate amplitudes are compared at the center-left emission position used by reconstruction.",
            "This is deterministic reconstruction evidence, not stochastic generator validation.",
            "The streaming optimizer remains test-only; no production API or model schema is changed.",
        ],
    }
    output_dir.mkdir(parents=True, exist_ok=True)
    json_path = output_dir / "coefficient-comparison.json"
    json_path.write_text(
        json.dumps(result, indent=2, sort_keys=True, allow_nan=False) + "\n",
        encoding="utf-8",
    )
    lines = [
        "# Offline versus streaming coefficient comparison",
        "",
        f"- Observed/padded samples: {observed} / {padded}",
        f"- Offline model SHA-256: `{result['offline_model_sha256']}`",
        f"- Streaming coefficient matrix SHA-256: `{result['streaming_coefficient_matrix_sha256']}`",
        f"- Positive candidates, offline/streaming: {result['aggregate']['offline_positive_candidates']} / {result['aggregate']['streaming_positive_candidates']}",
        "",
        "| Scale | Window (s) | Offline positive | Streaming positive | MAE | RMSE | Max abs. error | Observed MAE | Observed RMSE |",
        "|---:|---:|---:|---:|---:|---:|---:|---:|---:|",
    ]
    for item in per_scale:
        observed_metrics = item["observed"]
        lines.append(
            f"| {item['scale']} | {item['window_seconds']} | "
            f"{item['offline_positive_count']} | {item['streaming_positive_count']} | "
            f"{item['mae']:.6g} | {item['rmse']:.6g} | "
            f"{item['max_absolute_error']:.6g} | {observed_metrics['mae']:.6g} | "
            f"{observed_metrics['rmse']:.6g} |"
        )
    lines.extend([
        "",
        "The comparison uses candidate amplitudes at each scale's center-left emission position. "
        "Observed-only metrics exclude candidates whose emission positions lie in power-of-two padding.",
        "",
        "Both coefficient representations were reconstructed and checked against their corresponding "
        "observed signal CSVs (maximum absolute round-trip error must be at most 1e-8).",
        "",
        "This is a test-only comparison on one Westermo observation point and one exploratory threshold. "
        "It does not establish independent-capture compatibility or authorize production changes.",
    ])
    (output_dir / "coefficient-comparison.md").write_text(
        "\n".join(lines) + "\n", encoding="utf-8"
    )
    return result


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--offline-model", type=Path, required=True)
    parser.add_argument("--streaming-matrix", type=Path, required=True)
    parser.add_argument("--offline-signal", type=Path, required=True)
    parser.add_argument("--streaming-signal", type=Path, required=True)
    parser.add_argument("--output-dir", type=Path, required=True)
    args = parser.parse_args()
    result = analyze(
        args.offline_model,
        args.streaming_matrix,
        args.offline_signal,
        args.streaming_signal,
        args.output_dir,
    )
    print(json.dumps(result["aggregate"], sort_keys=True))


if __name__ == "__main__":
    main()
