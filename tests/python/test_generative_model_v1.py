import sys
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "scripts"))
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
                "positive_training_mark_count": 0,
                "mark_status": "unfit",
                "regime_parameters": [
                    {
                        "regime_id": 0,
                        "initial_event_probability": 0.0,
                        "event_p01": 0.0,
                        "event_p11": None,
                        "occurrence_status": {
                            "initial": "fitted",
                            "p01": "fitted",
                            "p11": "unfit",
                        },
                        "occurrence_support": {
                            "initial_opportunity_count": 1,
                            "p01_trial_count": 10,
                            "p11_trial_count": 0,
                        },
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
                "positive_training_mark_count": 42,
                "mark_status": "active",
                "regime_parameters": [
                    {
                        "regime_id": 0,
                        "initial_event_probability": 0.2,
                        "event_p01": 0.1,
                        "event_p11": 0.8,
                        "occurrence_status": {
                            "initial": "fitted",
                            "p01": "fitted",
                            "p11": "fitted",
                        },
                        "occurrence_support": {
                            "initial_opportunity_count": 1,
                            "p01_trial_count": 8,
                            "p11_trial_count": 3,
                        },
                    },
                    {
                        "regime_id": 1,
                        "initial_event_probability": 0.3,
                        "event_p01": 0.2,
                        "event_p11": 0.7,
                        "occurrence_status": {
                            "initial": "fitted",
                            "p01": "fitted",
                            "p11": "fitted",
                        },
                        "occurrence_support": {
                            "initial_opportunity_count": 1,
                            "p01_trial_count": 7,
                            "p11_trial_count": 2,
                        },
                    },
                ],
            },
        ],
    }


class GenerativeModelSemanticValidationTests(unittest.TestCase):
    def assert_invalid(self, model, expected):
        with self.assertRaisesRegex(ValueError, expected):
            validate_model(model, DEFAULT_SCHEMA)

    def test_valid_model_keeps_mark_and_occurrence_status_separate(self):
        validate_model(valid_model(), DEFAULT_SCHEMA)

    def test_mark_policy_does_not_force_occurrence_policy(self):
        model = valid_model()
        model["scale_processes"][0]["mark_status"] = "inactive_by_policy"
        validate_model(model, DEFAULT_SCHEMA)

    def test_unfit_mark_scale_remains_distinct_from_occurrence_status(self):
        model = valid_model()
        model["scale_processes"][0]["mark_status"] = "unfit"
        validate_model(model, DEFAULT_SCHEMA)
        self.assertEqual(model["mark_model"]["fitted_scale_indices"], [1])

    def test_unfit_occurrence_parameter_must_be_null(self):
        model = valid_model()
        model["scale_processes"][0]["regime_parameters"][0]["event_p11"] = 0.5
        self.assert_invalid(model, "unfit occurrence parameter must be null")

    def test_fitted_occurrence_parameter_requires_observed_trials(self):
        model = valid_model()
        regime = model["scale_processes"][0]["regime_parameters"][0]
        regime["occurrence_status"]["p11"] = "fitted"
        regime["event_p11"] = 0.5
        self.assert_invalid(model, "fitted occurrence parameter requires at least one")

    def test_policy_inactive_occurrence_requires_zero_probability(self):
        model = valid_model()
        regime = model["scale_processes"][0]["regime_parameters"][0]
        regime["occurrence_status"]["p01"] = "inactive_by_policy"
        regime["event_p01"] = 0.1
        self.assert_invalid(model, "inactive_by_policy occurrence parameter requires probability 0")

    def test_occurrence_can_remain_unfit_despite_nonzero_trial_support(self):
        model = valid_model()
        regime = model["scale_processes"][0]["regime_parameters"][0]
        regime["occurrence_support"]["p11_trial_count"] = 4
        validate_model(model, DEFAULT_SCHEMA)

    def test_zero_event_scale_cannot_be_marked_active(self):
        model = valid_model()
        model["scale_processes"][0]["mark_status"] = "active"
        model["mark_model"]["fitted_scale_indices"] = [0, 1]
        self.assert_invalid(model, "active mark status requires at least one")

    def test_fitted_mark_count_matches_active_scale_counts(self):
        model = valid_model()
        model["mark_model"]["fitted_mark_count"] = 41
        self.assert_invalid(model, "must equal the sum of positive_training_mark_count")

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
        model["mark_model"]["regime_log_mean_offsets"] = [0.0]
        self.assert_invalid(model, "offsets length must equal")
        model = valid_model()
        model["mark_model"]["regime_log_mean_offsets"][0] = 0.2
        self.assert_invalid(model, "0 was expected")

    def test_supported_scales_must_match_active_scales(self):
        model = valid_model()
        model["mark_model"]["fitted_scale_indices"] = [0, 1]
        self.assert_invalid(model, "must exactly match active")

    def test_training_range_must_match_sample_count(self):
        model = valid_model()
        model["fit"]["training_end_sample_exclusive"] = 65
        self.assert_invalid(model, "must equal training_sample_count")

    def test_selected_regime_count_must_match_state_count(self):
        model = valid_model()
        model["fit"]["regime_selection"]["selected_count"] = 1
        self.assert_invalid(model, "selected_count must equal the number of regime states")

    def test_power_of_two_training_range_must_be_globally_aligned(self):
        model = valid_model()
        model["fit"]["training_start_sample"] = 1
        model["fit"]["training_end_sample_exclusive"] = 65
        self.assert_invalid(model, "globally aligned sample boundary")

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
