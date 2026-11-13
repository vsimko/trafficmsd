#!/usr/bin/env python3
"""Training-prefix-only A/B/C regime-alignment comparison with pinned hmmlearn."""
from __future__ import annotations
import argparse
import hashlib
import importlib.metadata
import json
import platform
import sys
from pathlib import Path
import numpy as np
from hmmlearn.hmm import GaussianHMM
from scipy.special import logsumexp

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent / "generative-model-v1-sparse-marks"))
from compare_gamma_lognormal import TRAINING_PREFIX_SHA256, extract_events, read_prefix

EXPECTED_VERSIONS = {
    "hmmlearn": "0.3.3", "numpy": "1.26.4", "scipy": "1.13.1",
    "scikit-learn": "1.5.2", "joblib": "1.4.2", "threadpoolctl": "3.5.0",
}
FOLDS = ((1024, 1280), (1280, 1536), (1536, 1792), (1792, 2048))
K_VALUES = (1, 2, 3)
FIT_SEEDS = (17, 29, 43)
MAX_ITER, TOL, MIN_COVAR = 200, 1e-4, 1e-3
# Explicit hmmlearn 0.3.3 inverse-gamma covariance prior parameters.
COVARS_PRIOR, COVARS_WEIGHT = 0.01, 1.0
MARK_MINIMUM = 30
REGIME_MASS_EPSILON = 1e-8
COVARIANCE_PRIOR_STRENGTHS = (1.0, 10.0, 100.0)
COVARIANCE_PRIOR_K_VALUES = (2, 3)
REPORT_FLOAT_SIGNIFICANT_DIGITS = 7
RESTART_SELECTION_SIGNIFICANT_DIGITS = 6


def normalize_report_precision(value):
    """Canonicalize insignificant runner-level floating-point noise."""
    if isinstance(value, dict):
        return {key: normalize_report_precision(item) for key, item in value.items()}
    if isinstance(value, (list, tuple)):
        return [normalize_report_precision(item) for item in value]
    if isinstance(value, (float, np.floating)):
        numeric = float(value)
        if not np.isfinite(numeric):
            raise ValueError("report contains a non-finite floating-point value")
        return float(format(numeric, f".{REPORT_FLOAT_SIGNIFICANT_DIGITS}g"))
    if isinstance(value, np.integer):
        return int(value)
    return value


def dependency_versions():
    return {name: importlib.metadata.version(name) for name in EXPECTED_VERSIONS}


def assert_pinned_dependencies():
    actual = dependency_versions()
    mismatches = {n: {"expected": v, "actual": actual.get(n)}
                  for n, v in EXPECTED_VERSIONS.items() if actual.get(n) != v}
    if mismatches:
        raise RuntimeError(f"pinned dependency mismatch: {mismatches}")
    return actual


def build_representations(signal, events):
    max_scale = int(round(np.log2(len(signal))))
    components = np.zeros((len(signal), max_scale + 1), dtype=float)
    event_values = {}
    for scale, tick, mark in events:
        components[tick, scale] = mark
        event_values[(scale, tick)] = mark
    observations = {
        "A": np.log1p(signal).reshape(-1, 1),
        "B": np.log1p(components),
        "C": np.log1p(components.sum(axis=1)).reshape(-1, 1),
    }
    return observations, components, event_values


def canonical_state_order(means):
    means = np.asarray(means, dtype=float)
    if means.ndim != 2:
        raise ValueError("means must be a state-by-feature matrix")
    if means.shape[1] == 1:
        return np.argsort(means[:, 0], kind="stable")
    return np.lexsort(tuple(means[:, j] for j in reversed(range(means.shape[1]))))


def raw_diagonal_variances(covars):
    covars = np.asarray(covars, dtype=float)
    result = np.diagonal(covars, axis1=1, axis2=2) if covars.ndim == 3 else covars
    if result.ndim != 2:
        raise ValueError("unexpected GaussianHMM covariance shape")
    return np.asarray(result, dtype=float)


def diagonal_variances(covars):
    return np.maximum(raw_diagonal_variances(covars), MIN_COVAR)


def covariance_diagnostics(covars):
    values = raw_diagonal_variances(covars)
    feature_minima = np.min(values, axis=0)
    feature_minimum_states = np.argmin(values, axis=0)
    minimum = float(np.min(values))
    return {
        "minimum_diagonal_variance": minimum,
        "minimum_variance_feature_indices": [
            int(i) for i in np.flatnonzero(np.isclose(values, minimum, rtol=0, atol=0).any(axis=0))
        ],
        "feature_minimum_diagonal_variances": feature_minima.tolist(),
        "feature_minimum_variance_state_indices": feature_minimum_states.astype(int).tolist(),
        "features_below_covariance_floor": [
            int(i) for i in np.flatnonzero(feature_minima < MIN_COVAR)
        ],
        "diagonal_variances_by_state_feature": values.tolist(),
    }


def covariance_floor_status(covars):
    values = raw_diagonal_variances(covars)
    minimum = float(np.min(values))
    valid = bool(np.all(np.isfinite(values)) and minimum >= MIN_COVAR)
    return valid, minimum


def nonconstant_feature_indices(observations):
    """Training-only structural check; retain every nonconstant feature, however sparse."""
    observations = np.asarray(observations, dtype=float)
    variances = np.var(observations, axis=0)
    return np.flatnonzero(variances > 0.0)


def observation_feature_diagnostics(observations):
    observations = np.asarray(observations, dtype=float)
    result = []
    for feature in range(observations.shape[1]):
        column = observations[:, feature]
        result.append({
            "feature_index": feature,
            "mean": float(np.mean(column)),
            "variance": float(np.var(column)),
            "standard_deviation": float(np.std(column)),
            "zero_fraction": float(np.mean(column == 0.0)),
            "distinct_value_count": int(np.unique(column).size),
            "minimum": float(np.min(column)),
            "maximum": float(np.max(column)),
        })
    return result


def transition_diagnostics(transmat):
    transmat = np.asarray(transmat, dtype=float)
    safe = np.where(transmat > 0.0, transmat, 1.0)
    entropy = -np.sum(np.where(transmat > 0.0, transmat * np.log(safe), 0.0), axis=1)
    return {
        "row_sums": transmat.sum(axis=1).tolist(),
        "row_entropy_nats": entropy.tolist(),
        "nonzero_transitions_per_row": np.count_nonzero(transmat, axis=1).astype(int).tolist(),
    }


def gaussian_log_emissions(observations, means, variances):
    observations = np.asarray(observations, dtype=float)
    means = np.asarray(means, dtype=float)
    variances = np.maximum(np.asarray(variances, dtype=float), MIN_COVAR)
    delta = observations[:, None, :] - means[None, :, :]
    return -0.5 * (
        observations.shape[1] * np.log(2.0 * np.pi)
        + np.log(variances).sum(axis=1)[None, :]
        + (delta * delta / variances[None, :, :]).sum(axis=2)
    )


def predictive_priors(startprob, transmat, means, variances, observations):
    """P(Z_t | observations strictly before t), updated only after tick t."""
    startprob = np.asarray(startprob, dtype=float)
    transmat = np.asarray(transmat, dtype=float)
    log_emissions = gaussian_log_emissions(observations, means, variances)
    priors = np.empty_like(log_emissions)
    log_trans = np.log(np.maximum(transmat, 1e-300))
    alpha = None
    for tick in range(len(observations)):
        if alpha is None:
            prior = startprob.copy()
        else:
            prior = np.exp(logsumexp(alpha[:, None] + log_trans, axis=0))
            prior /= prior.sum()
        priors[tick] = prior
        alpha = np.log(np.maximum(prior, 1e-300)) + log_emissions[tick]
        alpha -= logsumexp(alpha)
    return priors


def scaled_variance_prior(observations, n_states, strength):
    """Inverse-gamma shrinkage prior centered on training-only feature variances."""
    observations = np.asarray(observations, dtype=float)
    if not np.isfinite(strength) or strength <= 0:
        raise ValueError("prior strength must be finite and positive")
    feature_variances = np.var(observations, axis=0)
    if np.any(~np.isfinite(feature_variances)) or np.any(feature_variances <= 0):
        raise ValueError("scaled prior requires nonconstant training features")
    prior = np.tile(float(strength) * feature_variances, (n_states, 1))
    return prior, 1.0 + float(strength), feature_variances


def restart_selection_key(likelihood, seed):
    """Stable ordering for restarts whose training likelihoods are numerically tied."""
    stable_likelihood = float(format(
        float(likelihood), f".{RESTART_SELECTION_SIGNIFICANT_DIGITS}g"
    ))
    return stable_likelihood, -int(seed)


def select_best_restart(converged_restarts):
    """Choose highest rounded training likelihood, then the lowest seed."""
    if not converged_restarts:
        raise ValueError("at least one converged restart is required")
    return max(converged_restarts, key=lambda item: restart_selection_key(item[0], item[1]))


def fit_candidate(observations, n_states, covars_prior=COVARS_PRIOR,
                  covars_weight=COVARS_WEIGHT, covariance_prior_metadata=None):
    """Fit fixed-K restarts; no K or regime count is selected."""
    attempts, converged = [], []
    for seed in FIT_SEEDS:
        model = GaussianHMM(
            n_components=n_states, covariance_type="diag", n_iter=MAX_ITER,
            tol=TOL, min_covar=MIN_COVAR, covars_prior=covars_prior,
            covars_weight=covars_weight, random_state=seed,
            implementation="log", verbose=False,
        )
        try:
            model.fit(observations)
            likelihood = float(model.score(observations))
            ok = bool(model.monitor_.converged)
            covariance_ok, minimum_variance = covariance_floor_status(model.covars_)
            covariance_detail = covariance_diagnostics(model.covars_)
            state_order = canonical_state_order(model.means_)
            canonical_transition = model.transmat_[np.ix_(state_order, state_order)]
            posterior_occupancy = model.predict_proba(observations).sum(axis=0)[state_order]
            attempts.append({
                "seed": seed, "converged": ok,
                "iterations": int(model.monitor_.iter),
                "training_log_likelihood": likelihood,
                "minimum_diagonal_variance": minimum_variance,
                "covariance_floor_respected": covariance_ok,
                "covariance_diagnostics": covariance_detail,
                "canonical_state_means": model.means_[state_order].tolist(),
                "canonical_transition_matrix": canonical_transition.tolist(),
                "canonical_initial_probabilities": model.startprob_[state_order].tolist(),
                "transition_diagnostics": transition_diagnostics(canonical_transition),
                "posterior_state_occupancy": posterior_occupancy.tolist(),
                "posterior_state_occupancy_fraction": (posterior_occupancy / len(observations)).tolist(),
            })
            if ok and covariance_ok and np.isfinite(likelihood):
                converged.append((likelihood, seed, model))
        except (ValueError, FloatingPointError, np.linalg.LinAlgError) as exc:
            attempts.append({"seed": seed, "converged": False,
                             "error": f"{type(exc).__name__}: {exc}"})
    if not converged:
        floor_rejected = any(
            item.get("converged") and not item.get("covariance_floor_respected", False)
            for item in attempts
        )
        return None, {
            "status": "no_valid_fit",
            "failure_reason": (
                "all converged restarts violated the 1e-3 diagonal covariance floor"
                if floor_rejected else "no converged restart"
            ),
            "attempts": attempts,
        }
    likelihood, selected_seed, model = select_best_restart(converged)
    dimension = observations.shape[1]
    parameter_count = (n_states - 1) + n_states * (n_states - 1) + 2 * n_states * dimension
    bic = -2.0 * likelihood + parameter_count * np.log(len(observations))
    order = canonical_state_order(model.means_)
    inverse = np.empty(n_states, dtype=int)
    inverse[order] = np.arange(n_states)
    info = {
        "status": "converged", "selected_restart_seed": selected_seed,
        "attempts": attempts, "training_log_likelihood": likelihood,
        "minimum_diagonal_variance": covariance_floor_status(model.covars_)[1],
        "covariance_floor": MIN_COVAR,
        "covariance_prior": covariance_prior_metadata or {
            "kind": "hmmlearn_0.3.3_default_inverse_gamma",
            "covars_prior": COVARS_PRIOR, "covars_weight": COVARS_WEIGHT,
        },
        "bic_diagnostic_only": float(bic), "parameter_count": int(parameter_count),
        "canonical_state_order_raw_indices": order.tolist(),
        "canonical_state_means": model.means_[order].tolist(),
        "canonical_state_diagonal_variances": raw_diagonal_variances(model.covars_)[order].tolist(),
        "canonical_state_posterior_occupancy": model.predict_proba(observations).sum(axis=0)[order].tolist(),
        "canonical_transition_matrix": model.transmat_[np.ix_(order, order)].tolist(),
        "canonical_initial_probabilities": model.startprob_[order].tolist(),
    }
    return (model, (order, inverse)), info


def occurrence_probability(regime_prior, trials, successes, previous_event):
    """Beta(1,1) posterior mean; never manufacture a prior-only estimate."""
    active = np.asarray(regime_prior) > REGIME_MASS_EPSILON
    counts = np.asarray(trials)[:, previous_event]
    wins = np.asarray(successes)[:, previous_event]
    if not np.any(active) or np.any(counts[active] == 0):
        return None
    conditional = (wins[active] + 1.0) / (counts[active] + 2.0)
    return float(np.dot(regime_prior[active], conditional) / regime_prior[active].sum())


def mark_support_sufficient(count):
    return int(count) >= MARK_MINIMUM


def score_model_fold(model_bundle, train_observations, test_observations,
                     train_end, test_end, scale_count, event_values):
    model, (order, inverse) = model_bundle
    train_path = inverse[model.predict(train_observations)]
    variances = diagonal_variances(model.covars_)[order]
    means = model.means_[order]
    startprob = model.startprob_[order]
    transmat = model.transmat_[np.ix_(order, order)]
    all_obs = np.vstack((train_observations, test_observations))
    test_priors = predictive_priors(
        startprob, transmat, means, variances, all_obs
    )[train_end:test_end]
    predictions, scale_support = {}, []
    for scale in range(scale_count):
        interval = 1 if scale == 0 else 2**scale
        phase = 0 if scale == 0 else 2**(scale - 1) - 1
        ticks = list(range(phase, test_end, interval))
        train_ticks = [t for t in ticks if t < train_end]
        test_ticks = [t for t in ticks if train_end <= t < test_end]
        k = model.n_components
        trials, successes = np.zeros((k, 2), dtype=int), np.zeros((k, 2), dtype=int)
        mark_counts = np.zeros(k, dtype=int)
        for tick in train_ticks:
            if event_values.get((scale, tick), 0.0) > 0:
                mark_counts[train_path[tick]] += 1
        for previous_tick, tick in zip(train_ticks[:-1], train_ticks[1:]):
            regime = train_path[tick]
            previous = int(event_values.get((scale, previous_tick), 0.0) > 0)
            current = int(event_values.get((scale, tick), 0.0) > 0)
            trials[regime, previous] += 1
            successes[regime, previous] += current
        previous_event = int(event_values.get((scale, train_ticks[-1]), 0.0) > 0) if train_ticks else 0
        supported = 0
        for tick in test_ticks:
            prior = test_priors[tick - train_end]
            probability = occurrence_probability(prior, trials, successes, previous_event)
            mark_eligible = bool(np.all(mark_counts[prior > REGIME_MASS_EPSILON] >= MARK_MINIMUM))
            predictions[(scale, tick)] = {
                "probability": probability, "mark_eligible": mark_eligible,
                "observed_event": int(event_values.get((scale, tick), 0.0) > 0),
            }
            supported += int(probability is not None)
            previous_event = int(event_values.get((scale, tick), 0.0) > 0)
        scale_support.append({
            "scale": scale, "training_positive_marks_by_canonical_regime": mark_counts.tolist(),
            "regimes_meeting_independent_gamma_minimum": [
                int(i) for i, count in enumerate(mark_counts) if mark_support_sufficient(count)
            ],
            "independent_gamma_minimum": MARK_MINIMUM,
            "test_opportunities": len(test_ticks),
            "test_occurrence_predictions_supported": supported,
        })
    return {"predictions": predictions, "scale_support": scale_support}


def summarize_comparison(fold, train_end, test_end, results, event_values,
                         representations=("A", "B", "C")):
    common_event, common_mark = None, None
    for name in representations:
        pred = results[name]["predictions"]
        for key, prediction in pred.items():
            expected_event = int(event_values.get(key, 0.0) > 0)
            if int(prediction["observed_event"]) != expected_event:
                raise ValueError(
                    f"{name} has an inconsistent observed event label at target {key}"
                )
        event_keys = {key for key, val in pred.items() if val["probability"] is not None}
        mark_keys = {key for key, val in pred.items()
                     if val["mark_eligible"] and event_values.get(key, 0.0) > 0}
        common_event = event_keys if common_event is None else common_event & event_keys
        common_mark = mark_keys if common_mark is None else common_mark & mark_keys
    common_event, common_mark = sorted(common_event or set()), sorted(common_mark or set())
    comparisons = {}
    for name in representations:
        scores = []
        for key in common_event:
            p = float(np.clip(results[name]["predictions"][key]["probability"], 1e-12, 1 - 1e-12))
            y = results[name]["predictions"][key]["observed_event"]
            scores.append(float(np.log(p if y else 1 - p)))
        comparisons[name] = {
            "common_occurrence_target_count": len(scores),
            "common_occurrence_log_score_sum": float(np.sum(scores)) if scores else None,
            "common_occurrence_mean_log_score": float(np.mean(scores)) if scores else None,
            "common_positive_mark_targets_with_30_event_support": len(common_mark),
            "positive_mark_log_score": None,
            "positive_mark_note": "No mark distribution was fitted or selected; support counts only.",
        }
    total = sum(
        1 for s in range(len(results[representations[0]]["scale_support"]))
        for t in range(0 if s == 0 else 2**(s - 1) - 1, test_end, 1 if s == 0 else 2**s)
        if train_end <= t < test_end
    )
    summary = {
        "fold": fold, "fit_range": [0, train_end], "predictive_range": [train_end, test_end],
        "target_opportunities_all_scales": total,
        "representations_compared": list(representations),
        "common_occurrence_targets_across_representations": len(common_event),
        "common_positive_mark_targets_with_30_event_support_across_representations": len(common_mark),
        "comparisons": comparisons, "selection": "none; no representation or K is ranked or selected",
    }
    if tuple(representations) == ("A", "B", "C"):
        summary["common_occurrence_targets_across_A_B_C"] = len(common_event)
        summary["common_positive_mark_targets_with_30_event_support_across_A_B_C"] = len(common_mark)
    return summary


def run_experiment(trace_csv):
    versions = assert_pinned_dependencies()
    signal = read_prefix(trace_csv)
    events = extract_events(signal)
    observations, components, event_values = build_representations(signal, events)
    scale_count, folds = components.shape[1], []
    for fold_number, (train_end, test_end) in enumerate(FOLDS, start=1):
        fold_models = {name: {} for name in ("A", "B", "C")}
        scored_internal = {}
        for representation in ("A", "B", "C"):
            for k in K_VALUES:
                train_obs = observations[representation][:train_end]
                test_obs = observations[representation][train_end:test_end]
                fitted, fit_info = fit_candidate(train_obs, k)
                fit_info["training_observation_features"] = observation_feature_diagnostics(train_obs)
                entry = {"fit": fit_info, "status": "unscored"}
                if fitted is not None:
                    scored = score_model_fold(
                        fitted, train_obs, test_obs, train_end, test_end,
                        scale_count, event_values,
                    )
                    entry.update({"status": "scored", "scale_support": scored["scale_support"]})
                    scored_internal[(representation, k)] = scored
                fold_models[representation][str(k)] = entry
        # Explicit diagnostic variant: remove only features that are exactly
        # constant in this fold's training observations. Keep all nonconstant
        # channels, even if their zero fraction is extremely high. Original B
        # and all scale-specific prediction targets remain unchanged.
        b_train = observations["B"][:train_end]
        retained_b_features = nonconstant_feature_indices(b_train)
        structural_zero_sensitivity = {
            "variant": "B_excluding_training_constant_features_only",
            "retained_feature_indices": retained_b_features.astype(int).tolist(),
            "excluded_constant_feature_indices": [
                int(i) for i in range(b_train.shape[1]) if i not in set(retained_b_features.tolist())
            ],
            "prediction_targets_unchanged": True,
            "fits_by_k": {},
        }
        reduced_scored_by_k = {}
        if len(retained_b_features):
            for k in K_VALUES:
                reduced_train = b_train[:, retained_b_features]
                reduced_fit, reduced_info = fit_candidate(reduced_train, k)
                reduced_info["training_observation_features"] = observation_feature_diagnostics(
                    reduced_train
                )
                reduced_info["retained_original_feature_indices"] = retained_b_features.astype(int).tolist()
                if reduced_fit is not None:
                    reduced_test = observations["B"][train_end:test_end][:, retained_b_features]
                    reduced_scored = score_model_fold(
                        reduced_fit, reduced_train, reduced_test, train_end, test_end,
                        scale_count, event_values,
                    )
                    reduced_scored_by_k[k] = reduced_scored
                    structural_zero_sensitivity["fits_by_k"][str(k)] = {
                        "status": "scored", "fit": reduced_info,
                        "scale_support": reduced_scored["scale_support"],
                    }
                else:
                    structural_zero_sensitivity["fits_by_k"][str(k)] = {
                        "status": "no_valid_fit", "fit": reduced_info,
                    }
        else:
            structural_zero_sensitivity["status"] = "no_nonconstant_training_features"

        # Predeclared inverse-gamma prior sensitivity for multi-state fits.
        # The prior mean is each feature's variance in the current fold's
        # training prefix; nu is a fixed pseudo-count strength. Apply the same
        # rule to A, C, and the explicit structural-zero-excluded B variant.
        prior_observations = {
            "A": observations["A"][:train_end],
            "C": observations["C"][:train_end],
            "B_without_constant": b_train[:, retained_b_features],
        }
        prior_sensitivity = {
            "prior_rule": "covars_prior = nu * per_feature_training_variance; covars_weight = 1 + nu",
            "strengths": list(COVARIANCE_PRIOR_STRENGTHS),
            "k_values": list(COVARIANCE_PRIOR_K_VALUES),
            "training_feature_variances": {
                name: np.var(values, axis=0).tolist()
                for name, values in prior_observations.items()
            },
            "fits": {},
            "common_target_comparisons": {},
        }
        prior_scored = {}
        for nu in COVARIANCE_PRIOR_STRENGTHS:
            prior_sensitivity["fits"][str(nu)] = {}
            for name, train_obs in prior_observations.items():
                prior_sensitivity["fits"][str(nu)][name] = {}
                prior, prior_weight, feature_variances = scaled_variance_prior(
                    train_obs, 1, nu
                )
                for k in COVARIANCE_PRIOR_K_VALUES:
                    # Rebuild the prior for this fixed K (same per-feature target).
                    prior, prior_weight, feature_variances = scaled_variance_prior(
                        train_obs, k, nu
                    )
                    metadata = {
                        "kind": "training_variance_scaled_inverse_gamma_sensitivity",
                        "strength_nu": nu,
                        "covars_weight": prior_weight,
                        "prior_mean_by_feature": feature_variances.tolist(),
                    }
                    fitted_prior, fit_prior_info = fit_candidate(
                        train_obs, k, covars_prior=prior,
                        covars_weight=prior_weight,
                        covariance_prior_metadata=metadata,
                    )
                    if fitted_prior is None:
                        prior_sensitivity["fits"][str(nu)][name][str(k)] = {
                            "status": "no_valid_fit", "fit": fit_prior_info,
                        }
                    else:
                        if name == "B_without_constant":
                            test_obs = observations["B"][train_end:test_end][:, retained_b_features]
                        else:
                            test_obs = observations[name][train_end:test_end]
                        prior_score = score_model_fold(
                            fitted_prior, train_obs, test_obs, train_end, test_end,
                            scale_count, event_values,
                        )
                        prior_scored[(nu, name, k)] = prior_score
                        prior_sensitivity["fits"][str(nu)][name][str(k)] = {
                            "status": "scored", "fit": fit_prior_info,
                            "scale_support": prior_score["scale_support"],
                        }
            for k in COVARIANCE_PRIOR_K_VALUES:
                names = ("A", "C", "B_without_constant")
                available = {
                    name: prior_scored[(nu, name, k)]
                    for name in names if (nu, name, k) in prior_scored
                }
                if len(available) == len(names):
                    prior_summary = summarize_comparison(
                        fold_number, train_end, test_end, available, event_values,
                        representations=names,
                    )
                    prior_summary["status"] = "scored_on_common_training_targets"
                else:
                    prior_summary = {
                        "status": "incomplete",
                        "fit_status_by_representation": {
                            name: prior_sensitivity["fits"][str(nu)][name][str(k)]["status"]
                            for name in names
                        },
                    }
                prior_sensitivity["common_target_comparisons"][str(nu) + ":" + str(k)] = prior_summary

        comparisons_by_k = {}
        for k in K_VALUES:
            available = {name: scored_internal[(name, k)] for name in ("A", "B", "C")
                         if (name, k) in scored_internal}
            if len(available) == 3:
                summary = summarize_comparison(fold_number, train_end, test_end, available, event_values)
            else:
                summary = {"fold": fold_number, "fit_range": [0, train_end],
                           "predictive_range": [train_end, test_end], "status": "incomplete",
                           "reason": "At least one A/B/C HMM had no valid fit after convergence and covariance-floor checks."}
            comparisons_by_k[str(k)] = summary
            for name in ("A", "B", "C"):
                entry = fold_models[name][str(k)]
                if entry["status"] == "scored":
                    entry["common_target_comparison"] = (
                        summary["comparisons"][name] if "comparisons" in summary else None
                    )

        # Compare the fixed K=1 candidate only when A, C, and the explicitly
        # reduced B diagnostic all have valid fits. This is still training-only,
        # uses identical scale/tick targets, and never ranks/selects a model.
        if (1 in reduced_scored_by_k
                and ("A", 1) in scored_internal
                and ("C", 1) in scored_internal):
            variant_results = {
                "A": scored_internal[("A", 1)],
                "C": scored_internal[("C", 1)],
                "B_without_constant": reduced_scored_by_k[1],
            }
            variant_comparison = summarize_comparison(
                fold_number, train_end, test_end, variant_results, event_values,
                representations=("A", "C", "B_without_constant"),
            )
            variant_comparison["status"] = "scored_on_common_training_targets"
        else:
            variant_comparison = {
                "status": "incomplete",
                "reason": "A, C, and structural-zero-excluded B do not all have a valid K=1 fit.",
            }
        structural_zero_sensitivity["common_target_comparison_k1"] = variant_comparison

        folds.append({
            "fold": fold_number,
            "fit_range": [0, train_end],
            "predictive_range": [train_end, test_end],
            "models": fold_models,
            "structural_zero_feature_sensitivity": structural_zero_sensitivity,
            "covariance_prior_sensitivity": prior_sensitivity,
            "common_target_comparisons_by_k": comparisons_by_k,
        })
    config = {
        "training_prefix": [0, 2048], "folds": [list(x) for x in FOLDS],
        "representations": ["A", "B", "C"],
        "additional_diagnostic_only_variant": "B excluding exactly constant training features; no prediction targets removed",
        "k_values_all_reported_no_selection": list(K_VALUES),
        "fit_seeds": list(FIT_SEEDS),
        "restart_likelihood_tie_break_significant_digits": RESTART_SELECTION_SIGNIFICANT_DIGITS,
        "covariance_type": "diag", "max_iter": MAX_ITER,
        "tolerance": TOL, "minimum_diagonal_covariance": MIN_COVAR,
        "hmmlearn_covars_prior": COVARS_PRIOR, "hmmlearn_covars_weight": COVARS_WEIGHT,
        "covariance_prior_sensitivity_strengths": list(COVARIANCE_PRIOR_STRENGTHS),
        "covariance_prior_sensitivity_k_values": list(COVARIANCE_PRIOR_K_VALUES),
        "report_float_significant_digits": REPORT_FLOAT_SIGNIFICANT_DIGITS,
        "minimum_independent_gamma_events_per_scale_regime": MARK_MINIMUM,
        "regime_mass_epsilon": REGIME_MASS_EPSILON,
    }
    config_sha = hashlib.sha256(json.dumps(config, sort_keys=True, separators=(",", ":")).encode()).hexdigest()
    return {
        "experiment": "generative-model-v1-independent-hmm-a-b-c-training-only",
        "status": "research_only_no_model_or_regime_selected",
        "dataset": {
            "identifier": "westermo-v1.0-right-observation-point",
            "source_archive_sha256": "8a1f49eb49747f3046eda8063b104834f41f0df30d4fc2357aea5609826a4445",
            "derived_trace_sha256": "6429e3d6a84efdc13edc14e44b2de2e6cbd31452e1958116c271745069b3fbb7",
            "training_prefix_sha256": TRAINING_PREFIX_SHA256,
            "training_range": [0, 2048], "validation_range": [3267, 4356],
            "test_range": [4356, 5445], "validation_test_rows_read": False,
        },
        "provenance": {
            "python": platform.python_version(), "dependencies": versions,
            "experiment_script_sha256": hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),
            "configuration_sha256": config_sha,
            "report_float_significant_digits": REPORT_FLOAT_SIGNIFICANT_DIGITS,
            "restart_likelihood_tie_break_significant_digits": RESTART_SELECTION_SIGNIFICANT_DIGITS, "input_csv_path_label": Path(trace_csv).name,
        },
        "configuration": config,
        "decomposition": {
            "algorithm": "frozen training-prefix port from compare_gamma_lognormal.py",
            "positive_events": len(events),
            "positive_marks_by_scale": [
                int(sum(1 for s, _, _ in events if s == scale)) for scale in range(scale_count)
            ],
            "component_matrix_shape": list(components.shape), "causal_fold_decomposition": False,
        },
        "folds": folds,
        "decision": {
            "selected_representation": None, "selected_regime_count": None,
            "selected_mark_family": None, "selected_pooling": None,
            "independent_gamma_minimum_events_per_scale_regime": MARK_MINIMUM,
            "production_fitter_implemented": False, "stochastic_generator_implemented": False,
            "generated_qpn_condition_implemented": False,
            "notes": [
                "A/B/C use identical training-only event and positive-mark targets.",
                "Occurrence scores use only the common supported target intersection across A/B/C for each fixed K and fold.",
                "Positive mark support is reported under the unchanged 30-event per-scale/regime rule; no Gamma or lognormal likelihood is fitted.",
                "No K or representation is selected; BIC is a within-representation diagnostic only.",
                "Regime priors at tick t condition only on observations through t-1.",
                "The decomposition is frozen over [0,2048), so folds are not strict causal decomposition replays.",
                "hmmlearn min_covar is used for covariance initialization; the post-EM 1e-3 covariance check is an explicit experiment-level rejection guard.",
                "Training-only per-feature dispersion, zero fractions, distinct values, per-state variances, and posterior occupancies are recorded to diagnose B collapse.",
                "A separate B sensitivity variant excludes exactly constant training observation features; all nonconstant features and all original prediction targets are retained. Its fixed-K=1 score is a non-discriminating one-state baseline.",
                "A predeclared inverse-gamma covariance-prior strength grid (nu=1,10,100) is applied identically to A, C, and structural-zero-excluded B for K=2,3; every setting is reported and none is selected.",
                "Every HMM restart records canonical transition matrices, row entropy, nonzero transitions, and posterior state occupancy to expose transient or weakly occupied states.",
                "No validation/test observations, artificial events, or padded observed training values are used.",
                "Unfit support is not converted to inactive_by_policy.",
            ],
        },
    }


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("trace_csv", type=Path)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    report = normalize_report_precision(run_experiment(args.trace_csv))
    rendered = json.dumps(report, indent=2, sort_keys=True, allow_nan=False) + "\n"
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(rendered, encoding="utf-8")
    print(rendered)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
