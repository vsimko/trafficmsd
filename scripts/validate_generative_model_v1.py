#!/usr/bin/env python3
"""Structural and cross-field validation for TrafficMSD generative model v1.

This validates model artifacts only. It deliberately does not fit parameters,
infer inactive scales, or generate traffic.
"""
from __future__ import annotations

import argparse
import json
import math
from pathlib import Path
from typing import Any

from jsonschema import Draft202012Validator, FormatChecker


ROOT = Path(__file__).resolve().parents[1]
DEFAULT_SCHEMA = ROOT / "schemas/generative-model/v1/model.schema.json"


def _finite_numbers(value: Any, path: str = "$") -> list[str]:
    errors: list[str] = []
    if isinstance(value, bool) or value is None or isinstance(value, str):
        return errors
    if isinstance(value, (int, float)):
        if not math.isfinite(value):
            errors.append(f"{path}: number must be finite")
        return errors
    if isinstance(value, list):
        for index, item in enumerate(value):
            errors.extend(_finite_numbers(item, f"{path}[{index}]"))
    elif isinstance(value, dict):
        for key, item in value.items():
            errors.extend(_finite_numbers(item, f"{path}.{key}"))
    return errors


def _sum_to_one(values: list[float], path: str, tolerance: float) -> list[str]:
    total = math.fsum(values)
    if not math.isfinite(total) or abs(total - 1.0) > tolerance:
        return [f"{path}: probabilities must sum to 1 (got {total:.12g})"]
    return []


def semantic_errors(model: dict[str, Any], tolerance: float = 1e-8) -> list[str]:
    """Return cross-field invariant violations for a structurally valid model."""
    errors = _finite_numbers(model)

    fit = model["fit"]
    start = fit["training_start_sample"]
    end = fit["training_end_sample_exclusive"]
    count = fit["training_sample_count"]
    if end <= start or end - start != count:
        errors.append(
            "fit.training_end_sample_exclusive - training_start_sample "
            "must equal training_sample_count and be positive"
        )
    if count > 0 and count & (count - 1) == 0 and start % count != 0:
        errors.append("power-of-two training ranges must start on a globally aligned sample boundary")

    selection = fit["regime_selection"]
    candidates = selection["candidate_counts"]
    selected = selection["selected_count"]
    bic_counts = [entry["regime_count"] for entry in selection["bic_by_count"]]
    if selected not in candidates:
        errors.append("fit.regime_selection.selected_count must be a candidate")
    if len(bic_counts) != len(set(bic_counts)) or set(bic_counts) != set(candidates):
        errors.append(
            "fit.regime_selection.bic_by_count must contain exactly one row "
            "for every candidate_count"
        )

    regimes = model["regimes"]
    states = regimes["states"]
    regime_count = len(states)
    expected_ids = list(range(regime_count))
    state_ids = [state["regime_id"] for state in states]
    if state_ids != expected_ids:
        errors.append("regimes.states regime_id values must be ordered and contiguous from 0")
    initial = regimes["initial_probabilities"]
    if selection["selected_count"] != regime_count:
        errors.append("fit.regime_selection.selected_count must equal the number of regime states")
    if len(initial) != regime_count:
        errors.append("regimes.initial_probabilities length must equal the number of states")
    else:
        errors.extend(_sum_to_one(initial, "regimes.initial_probabilities", tolerance))
    for index, state in enumerate(states):
        row = state["transition_probabilities"]
        if len(row) != regime_count:
            errors.append(
                f"regimes.states[{index}].transition_probabilities length "
                "must equal the number of states"
            )
        else:
            errors.extend(
                _sum_to_one(
                    row,
                    f"regimes.states[{index}].transition_probabilities",
                    tolerance,
                )
            )

    mark_model = model["mark_model"]
    offsets = mark_model["regime_log_mean_offsets"]
    if len(offsets) != regime_count:
        errors.append("mark_model.regime_log_mean_offsets length must equal the number of regimes")
    elif offsets[0] != 0:
        errors.append("mark_model.regime_log_mean_offsets[0] must be the zero reference offset")

    scales = model["scale_processes"]
    scale_ids = [item["scale_index"] for item in scales]
    if scale_ids != list(range(len(scales))):
        errors.append("scale_processes scale_index values must be unique, ordered, and contiguous from 0")

    active_scales: list[int] = []
    for index, scale in enumerate(scales):
        scale_id = scale["scale_index"]
        expected_interval = 1 if scale_id == 0 else 2 ** scale_id
        expected_phase = 0 if scale_id == 0 else 2 ** (scale_id - 1) - 1
        if scale["opportunity_interval_samples"] != expected_interval:
            errors.append(
                f"scale {scale_id}: opportunity_interval_samples must be {expected_interval}"
            )
        if scale["phase_offset_samples"] != expected_phase:
            errors.append(f"scale {scale_id}: phase_offset_samples must be {expected_phase}")

        parameters = scale["regime_parameters"]
        parameter_ids = [item["regime_id"] for item in parameters]
        if parameter_ids != expected_ids:
            errors.append(
                f"scale {scale_id}: regime_parameters must contain exactly one "
                "entry for every contiguous regime_id"
            )

        status = scale["mark_status"]
        if status == "active":
            active_scales.append(scale_id)
        elif status == "inactive_by_policy":
            for regime in parameters:
                for field in (
                    "initial_event_probability",
                    "event_p01",
                    "event_p11",
                ):
                    if regime[field] != 0:
                        errors.append(
                            f"scale {scale_id}: inactive_by_policy requires "
                            f"{field}=0 for every regime"
                        )
        elif status == "unfit":
            errors.append(
                f"scale {scale_id}: unfit scales are not acceptable in a fitted model; "
                "mark inactive_by_policy only after an explicit policy decision"
            )

    fitted_scales = mark_model["fitted_scale_indices"]
    if fitted_scales != active_scales:
        errors.append(
            "mark_model.fitted_scale_indices must exactly match active scale_processes "
            "in ascending order"
        )
    if not active_scales:
        errors.append("at least one active scale is required by the shared mark model")

    return errors


def validate_model(
    model: dict[str, Any],
    schema_path: Path | str = DEFAULT_SCHEMA,
    tolerance: float = 1e-8,
) -> None:
    """Raise ValueError with all structural and semantic errors."""
    schema = json.loads(Path(schema_path).read_text(encoding="utf-8"))
    validator = Draft202012Validator(schema, format_checker=FormatChecker())
    structural = [
        f"{'.'.join(map(str, error.absolute_path)) or '$'}: {error.message}"
        for error in validator.iter_errors(model)
    ]
    if structural:
        raise ValueError("Structural validation failed:\n- " + "\n- ".join(sorted(structural)))
    semantic = semantic_errors(model, tolerance=tolerance)
    if semantic:
        raise ValueError("Semantic validation failed:\n- " + "\n- ".join(semantic))


def _reject_nonfinite(token: str) -> None:
    raise ValueError(f"non-standard JSON numeric constant is not allowed: {token}")


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("model", type=Path, help="generative model JSON artifact")
    parser.add_argument("--schema", type=Path, default=DEFAULT_SCHEMA)
    args = parser.parse_args()
    try:
        model = json.loads(
            args.model.read_text(encoding="utf-8"),
            parse_constant=_reject_nonfinite,
        )
        validate_model(model, args.schema)
    except (OSError, json.JSONDecodeError, ValueError) as exc:
        parser.exit(1, f"validation failed: {exc}\n")
    print(f"valid generative model v1: {args.model}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
