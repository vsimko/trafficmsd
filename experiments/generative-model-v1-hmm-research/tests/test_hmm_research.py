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
