import sys
import unittest
from pathlib import Path
import numpy as np

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
import compare_hmm_representations as experiment


class PredictivePriorTests(unittest.TestCase):
    def test_current_observation_cannot_change_its_own_regime_prior(self):
        start = np.array([0.5, 0.5])
        transition = np.array([[0.9, 0.1], [0.1, 0.9]])
        means = np.array([[-2.0], [2.0]])
        variances = np.ones((2, 1))
        first = np.array([[0.0], [1.0], [0.5]])
        changed = first.copy()
        changed[1, 0] = 100.0
        a = experiment.predictive_priors(start, transition, means, variances, first)
        b = experiment.predictive_priors(start, transition, means, variances, changed)
        np.testing.assert_allclose(a[1], b[1], rtol=0, atol=1e-14)
        self.assertGreater(np.max(np.abs(a[2] - b[2])), 1e-6)

    def test_canonical_order_is_lexicographic(self):
        means = np.array([[2.0, 0.0], [1.0, 9.0], [1.0, 2.0]])
        np.testing.assert_array_equal(experiment.canonical_state_order(means), [2, 1, 0])

    def test_observation_feature_diagnostics_are_training_only_and_explicit(self):
        values = np.array([[0.0, 1.0], [0.0, 2.0], [1.0, 3.0]])
        diagnostics = experiment.observation_feature_diagnostics(values)
        self.assertEqual(diagnostics[0]["zero_fraction"], 2 / 3)
        self.assertEqual(diagnostics[0]["distinct_value_count"], 2)
        self.assertAlmostEqual(diagnostics[1]["variance"], 2 / 3)

    def test_covariance_diagnostics_locate_collapsed_feature(self):
        covars = np.array([
            [[1e-4, 0.0], [0.0, 0.5]],
            [[2e-4, 0.0], [0.0, 0.7]],
        ])
        diagnostics = experiment.covariance_diagnostics(covars)
        self.assertEqual(diagnostics["minimum_variance_feature_indices"], [0])
        self.assertEqual(diagnostics["features_below_covariance_floor"], [0])
        self.assertEqual(diagnostics["feature_minimum_variance_state_indices"], [0, 0])

    def test_nonconstant_feature_filter_excludes_only_exact_constants(self):
        values = np.array([
            [0.0, 0.0, 1.0],
            [0.0, 2.0, 1.0],
            [0.0, 0.0, 1.0],
            [0.0, 3.0, 1.0],
        ])
        np.testing.assert_array_equal(
            experiment.nonconstant_feature_indices(values), [1]
        )

    def test_comparison_accepts_explicit_representation_set(self):
        predictions = {
            "A": {"predictions": {(0, 1): {"probability": 0.25, "mark_eligible": False, "observed_event": 0}}, "scale_support": [{}]},
            "C": {"predictions": {(0, 1): {"probability": 0.25, "mark_eligible": False, "observed_event": 0}}, "scale_support": [{}]},
            "B_without_constant": {"predictions": {(0, 1): {"probability": 0.25, "mark_eligible": False, "observed_event": 0}}, "scale_support": [{}]},
        }
        result = experiment.summarize_comparison(
            1, 1, 2, predictions, {},
            representations=("A", "C", "B_without_constant"),
        )
        self.assertEqual(result["common_occurrence_targets_across_representations"], 1)
        self.assertEqual(
            set(result["comparisons"]),
            {"A", "C", "B_without_constant"},
        )

    def test_comparison_scores_only_identical_supported_targets(self):
        predictions = {
            "A": {"predictions": {
                (0, 1): {"probability": 0.25, "mark_eligible": False, "observed_event": 1},
                (0, 2): {"probability": 0.99, "mark_eligible": False, "observed_event": 0},
            }, "scale_support": [{}]},
            "C": {"predictions": {
                (0, 1): {"probability": 0.75, "mark_eligible": False, "observed_event": 1},
                (0, 3): {"probability": 0.01, "mark_eligible": False, "observed_event": 1},
            }, "scale_support": [{}]},
            "B_without_constant": {"predictions": {
                (0, 1): {"probability": 0.50, "mark_eligible": False, "observed_event": 1},
                (0, 2): {"probability": 0.01, "mark_eligible": False, "observed_event": 0},
            }, "scale_support": [{}]},
        }
        result = experiment.summarize_comparison(
            1, 0, 4, predictions, {(0, 1): 1, (0, 2): 0, (0, 3): 1},
            representations=("A", "C", "B_without_constant"),
        )
        self.assertEqual(result["common_occurrence_targets_across_representations"], 1)
        self.assertEqual(result["target_opportunities_all_scales"], 4)
        self.assertEqual(
            {name: item["common_occurrence_target_count"]
             for name, item in result["comparisons"].items()},
            {"A": 1, "C": 1, "B_without_constant": 1},
        )
        self.assertAlmostEqual(
            result["comparisons"]["A"]["common_occurrence_mean_log_score"],
            np.log(0.25),
        )
        self.assertAlmostEqual(
            result["comparisons"]["C"]["common_occurrence_mean_log_score"],
            np.log(0.75),
        )
        self.assertAlmostEqual(
            result["comparisons"]["B_without_constant"]["common_occurrence_mean_log_score"],
            np.log(0.50),
        )

    def test_comparison_rejects_inconsistent_observed_target_labels(self):
        predictions = {
            "A": {"predictions": {
                (0, 1): {"probability": 0.25, "mark_eligible": False, "observed_event": 1},
            }, "scale_support": [{}]},
            "C": {"predictions": {
                (0, 1): {"probability": 0.75, "mark_eligible": False, "observed_event": 0},
            }, "scale_support": [{}]},
        }
        with self.assertRaisesRegex(ValueError, "inconsistent observed event label"):
            experiment.summarize_comparison(
                1, 0, 2, predictions, {(0, 1): 1}, representations=("A", "C")
            )

    def test_scaled_variance_prior_uses_only_supplied_training_matrix(self):
        values = np.array([[1.0, 0.0], [3.0, 2.0], [5.0, 4.0]])
        prior, weight, variances = experiment.scaled_variance_prior(values, 2, 10.0)
        np.testing.assert_allclose(variances, [8 / 3, 8 / 3])
        np.testing.assert_allclose(prior, np.full((2, 2), 80 / 3))
        self.assertEqual(weight, 11.0)

    def test_transition_diagnostics_report_stochastic_rows(self):
        matrix = np.array([[0.5, 0.5], [0.0, 1.0]])
        result = experiment.transition_diagnostics(matrix)
        np.testing.assert_allclose(result["row_sums"], [1.0, 1.0])
        self.assertEqual(result["nonzero_transitions_per_row"], [2, 1])
        self.assertAlmostEqual(result["row_entropy_nats"][1], 0.0)

    def test_report_precision_normalizes_runner_level_float_noise(self):
        first = {
            "score": 1.234567891234,
            "small": 1.23456789e-120,
            "count": 7,
        }
        second = {
            "score": 1.234567891235,
            "small": 1.2345678901e-120,
            "count": 7,
        }
        self.assertEqual(
            experiment.normalize_report_precision(first),
            experiment.normalize_report_precision(second),
        )
        self.assertEqual(
            experiment.normalize_report_precision({"x": 1.234567891234})["x"],
            1.234568,
        )

    def test_restart_selection_uses_stable_likelihood_tie_break(self):
        restarts = [
            (1.2345678912, 43, "seed-43"),
            (1.2345678913, 17, "seed-17"),
            (1.2345678911, 29, "seed-29"),
        ]
        selected = experiment.select_best_restart(restarts)
        self.assertEqual(selected[1], 17)
        self.assertEqual(selected[2], "seed-17")

    def test_gamma_minimum_remains_thirty(self):
        self.assertFalse(experiment.mark_support_sufficient(29))
        self.assertTrue(experiment.mark_support_sufficient(30))
        self.assertEqual(experiment.MARK_MINIMUM, 30)

    def test_covariance_collapse_is_rejected(self):
        collapsed = np.array([[[1e-4, 0.0], [0.0, 1.0]]])
        valid = np.array([[[1e-3, 0.0], [0.0, 1.0]]])
        self.assertEqual(experiment.covariance_floor_status(collapsed), (False, 1e-4))
        self.assertEqual(experiment.covariance_floor_status(valid), (True, 1e-3))

    def test_unsupported_occurrence_cell_is_unfit_not_prior_only(self):
        prior = np.array([0.5, 0.5])
        trials = np.array([[10, 0], [0, 0]])
        successes = np.array([[2, 0], [0, 0]])
        self.assertIsNone(experiment.occurrence_probability(prior, trials, successes, 0))

    def test_beta_estimator_requires_observed_support(self):
        prior = np.array([1.0, 0.0])
        trials = np.array([[8, 0], [0, 0]])
        successes = np.array([[2, 0], [0, 0]])
        self.assertAlmostEqual(experiment.occurrence_probability(prior, trials, successes, 0), 0.3)


if __name__ == "__main__":
    unittest.main()
