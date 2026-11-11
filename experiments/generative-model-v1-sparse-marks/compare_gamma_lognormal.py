#!/usr/bin/env python3
"""Training-prefix-only comparison of shared-trend Gamma and lognormal marks.

Input: trace.csv from the checksum-verified Westermo v1 extraction artifact.
Only rows [0, 2048) are read. No padding, synthetic events, validation/test
observations, production fitter, or generator are used.
"""
import argparse
import csv
import hashlib
import io
import json
from pathlib import Path

import numpy as np
from scipy.optimize import brentq, minimize
from scipy.special import gammainc, gammaln, logsumexp, ndtr

TRAINING_PREFIX_SHA256 = "017e38220cc7590653c630455732c90c2c4c80e148a105e1dabe02e133ebe032"
FOLDS = ((1024, 1280), (1280, 1536), (1536, 1792), (1792, 2048))
PENALTY = 1.0
QUANTILES = (0.90, 0.95, 0.99)


def read_prefix(path):
    # Unbuffered, line-bounded input: only the header and 2,048 training rows
    # are read or parsed. Validation/test rows are never consumed.
    lines = []
    with Path(path).open("rb", buffering=0) as stream:
        lines.append(stream.readline())
        for _ in range(2048):
            row = stream.readline()
            if not row:
                raise ValueError("training prefix ended before 2,048 rows")
            lines.append(row)
    raw_prefix = b"".join(lines)
    if hashlib.sha256(raw_prefix).hexdigest() != TRAINING_PREFIX_SHA256:
        raise ValueError("training-prefix SHA-256 does not match the verified artifact")
    reader = csv.DictReader(io.StringIO(raw_prefix.decode("utf-8")))
    if reader.fieldnames != ["sample_index", "value"]:
        raise ValueError("expected sample_index,value columns")
    values = []
    for expected, row in enumerate(reader):
        if int(row["sample_index"]) != expected:
            raise ValueError("missing or misaligned training-prefix sample")
        value = float(row["value"])
        if not np.isfinite(value) or value < 0:
            raise ValueError("training values must be finite and non-negative")
        values.append(value)
    if len(values) != 2048:
        raise ValueError("expected exactly 2,048 training-prefix rows")
    return np.asarray(values, dtype=float)


def optimize_intervals(x, fraction=0.004):
    x = np.asarray(x, dtype=float).copy()
    n = len(x)
    if n < 3:
        return x
    cutoff = n * fraction
    if cutoff > 1:
        b = (x > 0).astype(int)
        runs = np.zeros(n + 1, dtype=int)
        for i in range(n - 1, -1, -1):
            runs[i] = b[i] * (runs[i + 1] + b[i])
        runs = runs[:n]
        for i in range(1, n):
            runs[i] = 0 if runs[i] == 0 else max(runs[i], runs[i - 1])
        x = (runs > cutoff) * x
    for i in range(1, n - 1):
        low, high = min(x[i - 1], x[i + 1]), max(x[i - 1], x[i + 1])
        if x[i] < low:
            x[i] = low
        elif x[i] > low:
            x[i] = high
    return x


def extract_events(signal):
    n = len(signal)
    max_scale = int(round(np.log2(n)))
    coef = np.zeros((max_scale + 1, n))
    remainder = signal.copy()
    for scale in range(max_scale, 0, -1):
        width = 2**scale
        candidates = []
        for start in range(0, n, width):
            vals, counts = np.unique(remainder[start:start + width], return_counts=True)
            singletons = vals[counts == 1]
            candidates.append(float(singletons.max()) if len(singletons) else 0.0)
        candidates = optimize_intervals(candidates)
        for i, mark in enumerate(candidates):
            start = i * width
            if mark <= 0:
                continue
            hits = np.flatnonzero(remainder[start:start + width] == mark)
            if len(hits):
                remainder[start + hits[0]] = 0.0
            tick = start + width // 2 - 1
            remainder[tick] = max(0.0, remainder[tick] - mark)
            coef[scale, start:start + width] = mark
    coef[0] = optimize_intervals(remainder)
    events = [(0, t, float(v)) for t, v in enumerate(coef[0]) if v > 0]
    for scale in range(1, max_scale + 1):
        width = 2**scale
        for tick in range(width // 2 - 1, n, width):
            if coef[scale, tick] > 0:
                events.append((scale, tick, float(coef[scale, tick])))
    return events


def fit_hmm(y, k):
    """Deterministic diagonal-Gaussian HMM EM; explicitly exploratory."""
    n = len(y)
    means = np.quantile(y, np.linspace(0, 1, k + 2)[1:-1])
    variances = np.full(k, max(float(np.var(y)), 1e-3))
    trans = np.full((k, k), 0.1 / max(k - 1, 1))
    np.fill_diagonal(trans, 0.9 if k > 1 else 1.0)
    trans /= trans.sum(axis=1, keepdims=True)
    initial = np.full(k, 1 / k)
    previous = -np.inf
    for _ in range(200):
        emit = np.column_stack([
            -0.5 * (np.log(2 * np.pi * variances[j]) + (y - means[j])**2 / variances[j])
            for j in range(k)
        ])
        log_a = np.log(np.maximum(trans, 1e-300))
        forward = np.empty((n, k))
        forward[0] = np.log(initial) + emit[0]
        for t in range(1, n):
            forward[t] = emit[t] + logsumexp(forward[t - 1][:, None] + log_a, axis=0)
        likelihood = float(logsumexp(forward[-1]))
        backward = np.zeros((n, k))
        for t in range(n - 2, -1, -1):
            backward[t] = logsumexp(
                log_a + emit[t + 1][None, :] + backward[t + 1][None, :], axis=1
            )
        post_log = forward + backward
        post_log -= logsumexp(post_log, axis=1, keepdims=True)
        post = np.exp(post_log)
        xi = np.zeros((k, k))
        for t in range(n - 1):
            pair = forward[t][:, None] + log_a + emit[t + 1][None, :] + backward[t + 1][None, :]
            pair -= logsumexp(pair)
            xi += np.exp(pair)
        initial = post[0]
        trans = xi / np.maximum(post[:-1].sum(axis=0)[:, None], 1e-12)
        trans = np.maximum(trans, 1e-8)
        trans /= trans.sum(axis=1, keepdims=True)
        weight = post.sum(axis=0)
        means = (post * y[:, None]).sum(axis=0) / np.maximum(weight, 1e-12)
        variances = (post * (y[:, None] - means)**2).sum(axis=0) / np.maximum(weight, 1e-12)
        variances = np.maximum(variances, 1e-3)
        if abs(likelihood - previous) < 1e-6 * (1 + abs(likelihood)):
            break
        previous = likelihood

    emit = np.column_stack([
        -0.5 * (np.log(2 * np.pi * variances[j]) + (y - means[j])**2 / variances[j])
        for j in range(k)
    ])
    log_a = np.log(np.maximum(trans, 1e-300))
    delta = np.empty((n, k))
    back = np.zeros((n, k), dtype=int)
    delta[0] = np.log(np.maximum(initial, 1e-300)) + emit[0]
    for t in range(1, n):
        candidate = delta[t - 1][:, None] + log_a
        back[t] = np.argmax(candidate, axis=0)
        delta[t] = np.max(candidate, axis=0) + emit[t]
    path = np.empty(n, dtype=int)
    path[-1] = np.argmax(delta[-1])
    for t in range(n - 2, -1, -1):
        path[t] = back[t + 1, path[t + 1]]

    filtered = np.empty((n, k))
    filtered[0] = np.log(np.maximum(initial, 1e-300)) + emit[0]
    filtered[0] -= logsumexp(filtered[0])
    for t in range(1, n):
        filtered[t] = emit[t] + logsumexp(filtered[t - 1][:, None] + log_a, axis=0)
        filtered[t] -= logsumexp(filtered[t])
    order = np.argsort(means)
    inverse = np.empty(k, dtype=int)
    inverse[order] = np.arange(k)
    bic = -2 * likelihood + (k*k + 2*k - 1) * np.log(n)
    return {
        "means": means[order], "variances": variances[order],
        "transition": trans[np.ix_(order, order)], "path": inverse[path],
        "filtered": filtered[:, order], "bic": float(bic),
    }


def design(scales, regimes, k):
    x = np.zeros((len(scales), k + 1))
    x[:, 0] = 1.0
    x[:, 1] = np.asarray(scales) - 3
    for j in range(1, k):
        x[:, j + 1] = np.asarray(regimes) == j
    return x


def fit_marks(events, hmm):
    k = len(hmm["means"])
    scales = np.asarray([e[0] for e in events], dtype=int)
    ticks = np.asarray([e[1] for e in events], dtype=int)
    marks = np.asarray([e[2] for e in events], dtype=float)
    matrix = design(scales, hmm["path"][ticks], k)

    def gamma_nll(p):
        shape = np.exp(p[0])
        mean = np.exp(np.clip(matrix @ p[1:], -40, 40))
        logpdf = ((shape - 1) * np.log(marks) - marks * shape / mean
                  - gammaln(shape) - shape * np.log(mean / shape))
        return -float(logpdf.sum()) + PENALTY * float(np.sum(p[3:]**2))

    def lognormal_nll(p):
        sigma = np.exp(p[-1])
        log_mean = matrix @ p[:-1]
        location = log_mean - 0.5 * sigma**2
        log_marks = np.log(marks)
        logpdf = (-log_marks - p[-1] - 0.5 * np.log(2 * np.pi)
                  - 0.5 * ((log_marks - location) / sigma)**2)
        return -float(logpdf.sum()) + PENALTY * float(np.sum(p[2:-1]**2))

    mean = float(np.mean(marks))
    shape0 = np.clip(mean**2 / max(float(np.var(marks)), 1e-9), 0.05, 100)
    g0 = np.r_[np.log(shape0), np.log(mean), 0.5, np.zeros(k - 1)]
    l0 = np.r_[np.mean(np.log(marks)), 0.5, np.zeros(k - 1),
               np.log(max(float(np.std(np.log(marks))), 0.1))]
    opts = {"maxiter": 5000, "ftol": 1e-12}
    g = minimize(gamma_nll, g0, method="L-BFGS-B",
                 bounds=[(-4, 6), (-20, 20), (-5, 5)] + [(-10, 10)]*(k - 1), options=opts)
    l = minimize(lognormal_nll, l0, method="L-BFGS-B",
                 bounds=[(-20, 20), (-5, 5)] + [(-10, 10)]*(k - 1) + [(-5, 5)], options=opts)
    if not g.success or not l.success:
        raise RuntimeError("mark-family optimizer failed to converge")
    return g.x, l.x


def score(test_events, hmm, signal, start, end, gamma_p, lognormal_p):
    k = len(hmm["means"])
    posterior = hmm["filtered"][-1].copy()
    priors = {}
    for tick in range(start, end):
        prior = posterior @ hmm["transition"]
        priors[tick] = prior
        emission = np.asarray([
            -0.5 * (np.log(2 * np.pi * hmm["variances"][j])
                    + (np.log1p(signal[tick]) - hmm["means"][j])**2 / hmm["variances"][j])
            for j in range(k)
        ])
        lp = np.log(np.maximum(prior, 1e-300)) + emission
        posterior = np.exp(lp - logsumexp(lp))

    gs, ls = [], []
    hits = {"gamma": {q: 0 for q in QUANTILES}, "lognormal": {q: 0 for q in QUANTILES}}
    for scale, tick, mark in test_events:
        w = priors[tick]
        x = design(np.full(k, scale), np.arange(k), k)
        shape = np.exp(gamma_p[0])
        mean_g = np.exp(x @ gamma_p[1:])
        theta = mean_g / shape
        lg = ((shape - 1) * np.log(mark) - mark / theta
              - gammaln(shape) - shape * np.log(theta))
        gs.append(float(logsumexp(np.log(np.maximum(w, 1e-300)) + lg)))

        sigma = np.exp(lognormal_p[-1])
        log_mean = x @ lognormal_p[:-1]
        location = log_mean - 0.5 * sigma**2
        log_mark = np.log(mark)
        ll = (-log_mark - np.log(sigma) - 0.5 * np.log(2 * np.pi)
              - 0.5 * ((log_mark - location) / sigma)**2)
        ls.append(float(logsumexp(np.log(np.maximum(w, 1e-300)) + ll)))

        for q in QUANTILES:
            qg = brentq(lambda y: float(np.dot(w, gammainc(shape, y / theta))) - q,
                        1e-12, max(1e7, mark * 1e4 + 1e5))
            ql = brentq(lambda y: float(np.dot(w, ndtr((np.log(y) - location) / sigma))) - q,
                        1e-12, max(1e7, mark * 1e4 + 1e5))
            hits["gamma"][q] += int(mark <= qg)
            hits["lognormal"][q] += int(mark <= ql)
    n = len(test_events)
    return {
        "count": n, "gamma": float(np.mean(gs)), "lognormal": float(np.mean(ls)),
        "coverage": {family: {str(q): hits[family][q] / n for q in QUANTILES}
                     for family in hits},
        "coverage_hits": hits,
    }


def run(path):
    signal = read_prefix(path)
    events = extract_events(signal)
    counts = [sum(e[0] == scale for e in events) for scale in range(12)]
    expected = [0, 193, 287, 224, 128, 64, 32, 16, 8, 4, 2, 1]
    if counts != expected:
        raise ValueError(f"emission conformance failed: {counts}")

    folds = []
    total_g = total_l = total_n = 0.0
    pooled_hits = {"gamma": {q: 0 for q in QUANTILES},
                   "lognormal": {q: 0 for q in QUANTILES}}
    for number, (train_end, test_end) in enumerate(FOLDS, 1):
        candidates = [fit_hmm(np.log1p(signal[:train_end]), k) for k in (1, 2, 3)]
        hmm = min(candidates, key=lambda item: item["bic"])
        train = [e for e in events if e[1] < train_end]
        test = [e for e in events if train_end <= e[1] < test_end]
        gamma_p, lognormal_p = fit_marks(train, hmm)
        result = score(test, hmm, signal, train_end, test_end, gamma_p, lognormal_p)
        result.update({
            "fold": number, "training_range": [0, train_end],
            "scoring_range": [train_end, test_end], "selected_regimes": len(hmm["means"]),
            "candidate_bic": {str(k): v["bic"] for k, v in zip((1, 2, 3), candidates)},
            "gamma_shape": float(np.exp(gamma_p[0])),
            "gamma_scale_slope": float(gamma_p[2]),
            "lognormal_sigma": float(np.exp(lognormal_p[-1])),
            "lognormal_log_mean_slope": float(lognormal_p[1]),
            "regime_offset_penalty": PENALTY,
        })
        folds.append(result)
        total_g += result["gamma"] * result["count"]
        total_l += result["lognormal"] * result["count"]
        total_n += result["count"]
        for family in pooled_hits:
            for q in QUANTILES:
                pooled_hits[family][q] += result["coverage_hits"][family][q]

    return {
        "experiment": "westermo-training-prefix-gamma-vs-lognormal-marks",
        "status": "exploratory_training_only_not_model_selection",
        "training_prefix_sha256": TRAINING_PREFIX_SHA256,
        "training_prefix": [0, 2048],
        "validation_test_rows_read": False,
        "synthetic_or_artificial_events": False,
        "positive_marks_by_scale_0_to_11": counts,
        "independent_gamma_minimum_events_per_scale_regime": 30,
        "folds": folds,
        "pooled": {
            "positive_marks_scored": total_n,
            "gamma_mean_log_score": total_g / total_n,
            "lognormal_mean_log_score": total_l / total_n,
            "lognormal_minus_gamma": (total_l - total_g) / total_n,
            "quantile_coverage": {
                family: {str(q): pooled_hits[family][q] / total_n for q in QUANTILES}
                for family in pooled_hits
            },
        },
        "limitations": [
            "The HMM is an independent exploratory reimplementation, not a canonical validated fitter.",
            "The frozen-prefix decomposition is computed once; folds are not end-to-end causal replays. "
            "Fold-specific HMM and mark parameters use only events before each fold origin.",
            "Scores are conditional on positive events and omit event-occurrence likelihood.",
            "Fold 4 upper-tail coverage is poor for both families; sparse upper scales cannot support "
            "a reliable extreme-quantile claim.",
            "No regime candidate or mark family is selected for production.",
        ],
    }


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("trace_csv", type=Path)
    parser.add_argument("--output", type=Path)
    args = parser.parse_args()
    report = run(args.trace_csv)
    rendered = json.dumps(report, indent=2, sort_keys=True) + "\n"
    if args.output:
        args.output.write_text(rendered, encoding="utf-8")
    print(rendered)
