import copy
import json
import unittest
from pathlib import Path

from validate_generative_model_v1 import DEFAULT_SCHEMA, validate_model


def valid_model():
    return {
        "schema_version": "1.0",
        "model_id": "semantic-fixture",
        "model_kind": "multiscale_regime_marked_event_process",
        "provenance": {
            "dataset_id": "synthetic-unit-fixture",
            "source_sha256": "a" * 64,
            "preprocessing_version": "fixture-1",
            "fitted_with": "unit-test-only",
        },
        "observation": {
            "quantity": "bytes",
            "sampling_interval": 1,
            "time_unit": "second",
            "value_unit": "byte",
            "bin_convention": "left_closed_right_open",
        },
        "source_model": {
            "schema_version": "2.0",
            "model_id": "deterministic-source",
            "model_kind": "multiscale_reconstruction",
            "sha256": "b" * 64,
        },
        "fit": {
            "algorithm": "hmm_regime_plus_scale_markov_gamma_v1",
            "algorithm_version": "fixture-1",
            "training_start_sample": 0,
            "training_end_sample_exclusive": 64,
            "training_sample_count": 64,
            "training_signal_sha256": "c" * 64,
            "decomposition_cutoff_fraction": 0.004,
            "fitting_seed": 7,
            "regime_selection": {
                "candidate_counts": [1, 2],
                "selected_count": 2,
                "criterion": "BIC",
                "bic_by_count": [
                    {"regime_count": 1, "bic": 123.0},
                    {"regime_count": 2, "bic": 100.0},
                ],
            },
        },
        "regimes": {
            "initial_probabilities": [0.6, 0.4],
            "states": [
                {
                    "regime_id": 0,
                    "transition_probabilities": [0.9, 0.1],
                    "log_volume_mean": 1.0,
                    "log_volume_sd": 0.5,
                },
                {
                    "regime_id": 1,
                    "transition_probabilities": [0.2, 0.8],
                    "log_volume_mean": 2.0,
                    "log_volume_sd": 0.7,
                },
            ],
        },
        "mark_model": {
            "family": "gamma",
            "shape": 2.0,
            "log_mean_intercept": 3.0,
            "log_mean_scale_slope": 0.5,
            "regime_log_mean_offsets": [0.0, 0.1],
            "regime_offset_penalty": 1.0,
            "fitted_mark_count": 42,
            "fitted_scale_indices": [1],
        },
        "scale_processes": [
            {
                "scale_index": 0,
                "opportunity_interval_samples": 1,
                "phase_offset_samples": 0,
                "mark_status": "inactive_by_policy",
                "regime_parameters": [
                    {
                        "regime_id": 0,
                        "initial_event_probability": 0.0,
                        "event_p01": 0.0,
                        "event_p11": 0.0,
                    },
                    {
                        "regime_id": 1,
                        "initial_event_probability": 0.0,
                        "event_p01": 0.0,
                        "event_p11": 0.0,
                    },
                ],
            },
            {
                "scale_index": 1,
                "opportunity_interval_samples": 2,
                "phase_offset_samples": 0,
                "mark_status": "active",
                "regime_parameters": [
                    {
                        "regime_id": 0,
                        "initial_event_probability": 0.2,
                        "event_p01": 0.1,
                        "event_p11": 0.8,
                    },
                    {
                        "regime_id": 1,
                        "initial_event_probability": 0.3,
                        "event_p01": 0.2,
                        "event_p11": 0.7,
                    },
                ],
            },
        ],
    }


class GenerativeModelSemanticValidationTests(unittest.TestCase):
    def assert_invalid(self, model, expected):
        with self.assertRaisesRegex(ValueError, expected):
            validate_model(model, DEFAULT_SCHEMA)

    def test_valid_model_with_explicit_policy_inactive_scale(self):
        validate_model(valid_model(), DEFAULT_SCHEMA)

    def test_unfit_scale_is_rejected_not_silently_zeroed(self):
        model = valid_model()
        model["scale_processes"][0]["mark_status"] = "unfit"
        self.assert_invalid(model, "unfit scales")

    def test_inactive_scale_requires_zero_occurrence_probabilities(self):
        model = valid_model()
        model["scale_processes"][0]["regime_parameters"][1]["event_p11"] = 0.01
        self.assert_invalid(model, "inactive_by_policy requires event_p11=0")

    def test_scale_indices_must_be_contiguous_from_zero(self):
        model = valid_model()
        model["scale_processes"][1]["scale_index"] = 2
        self.assert_invalid(model, "contiguous from 0")

    def test_scale_interval_and_phase_must_match_emission_contract(self):
        model = valid_model()
        model["scale_processes"][1]["phase_offset_samples"] = 1
        self.assert_invalid(model, "phase_offset_samples must be 0")

    def test_each_scale_must_cover_each_regime_exactly_once(self):
        model = valid_model()
        model["scale_processes"][1]["regime_parameters"].pop()
        self.assert_invalid(model, "regime_parameters must contain exactly one")

    def test_initial_and_transition_probabilities_must_sum_to_one(self):
        model = valid_model()
        model["regimes"]["initial_probabilities"] = [0.6, 0.6]
        self.assert_invalid(model, "probabilities must sum to 1")

    def test_mark_offsets_must_align_and_anchor_regime_zero(self):
        model = valid_model()
        model["mark_model"]["regime_log_mean_offsets"] = [0.2]
        self.assert_invalid(model, "offsets length must equal")
        model = valid_model()
        model["mark_model"]["regime_log_mean_offsets"][0] = 0.2
        self.assert_invalid(model, "zero reference offset")

    def test_supported_scales_must_match_active_scales(self):
        model = valid_model()
        model["mark_model"]["fitted_scale_indices"] = [0, 1]
        self.assert_invalid(model, "must exactly match active")

    def test_training_range_must_match_sample_count(self):
        model = valid_model()
        model["fit"]["training_end_sample_exclusive"] = 65
        self.assert_invalid(model, "must equal training_sample_count")

    def test_bic_rows_must_cover_each_candidate_once(self):
        model = valid_model()
        model["fit"]["regime_selection"]["bic_by_count"].pop()
        self.assert_invalid(model, "exactly one row for every candidate_count")

    def test_non_finite_numbers_are_rejected(self):
        model = valid_model()
        model["mark_model"]["shape"] = float("inf")
        self.assert_invalid(model, "number must be finite")

    def test_schema_rejects_unknown_properties(self):
        model = valid_model()
        model["unexpected"] = True
        self.assert_invalid(model, "Additional properties are not allowed")


if __name__ == "__main__":
    unittest.main()
