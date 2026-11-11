#!/usr/bin/env python3
"""Training-prefix-only support audit for scale/regime event Markov chains.

Reports binary opportunity/transition counts only. It does not fit occurrence
probabilities, select a regime model, or read validation/test observations.
"""
import argparse
import json
from pathlib import Path

import numpy as np

from compare_gamma_lognormal import (
    FOLDS,
    TRAINING_PREFIX_SHA256,
    extract_events,
    fit_hmm,
    read_prefix,
)


def audit(trace_csv: Path) -> dict:
    signal = read_prefix(trace_csv)
    events = extract_events(signal)
    event_ticks = {(scale, tick) for scale, tick, _ in events}
    max_scale = int(round(np.log2(len(signal))))

    occupancy = []
    for scale in range(max_scale + 1):
        interval = 1 if scale == 0 else 2**scale
        phase = 0 if scale == 0 else 2**(scale - 1) - 1
        ticks = range(phase, len(signal), interval)
        opportunities = len(ticks)
        positives = sum((scale, tick) in event_ticks for tick in ticks)
        occupancy.append({
            "scale": scale,
            "opportunities": opportunities,
            "positive_events": positives,
            "zero_events": opportunities - positives,
            "occupancy": positives / opportunities if opportunities else None,
            "variation": (
                "all_zero" if positives == 0 else
                "all_one" if positives == opportunities else "variable"
            ),
        })

    folds = []
    for fold_number, (train_end, _) in enumerate(FOLDS, start=1):
        candidates = [
            fit_hmm(np.log1p(signal[:train_end]), k) for k in (1, 2, 3)
        ]
        hmm = min(candidates, key=lambda item: item["bic"])
        path = hmm["path"]
        regime_count = len(hmm["means"])
        scale_rows = []

        # Scale 0–3 are the only scales with variable event occupancy (plus
        # scale 0's all-zero sequence). Scales 4+ are summarized above.
        for scale in range(min(4, max_scale + 1)):
            interval = 1 if scale == 0 else 2**scale
            phase = 0 if scale == 0 else 2**(scale - 1) - 1
            ticks = list(range(phase, train_end, interval))
            scale_events = {(s, t) for s, t in event_ticks if s == scale}
            regimes = []

            for regime in range(regime_count):
                assigned = [tick for tick in ticks if path[tick] == regime]
                positive = sum((scale, tick) in scale_events for tick in assigned)
                n00 = n01 = n10 = n11 = 0

                # Group each adjacent opportunity pair by the regime at the
                # current emission tick. The first opportunity has no pair.
                for previous_tick, tick in zip(ticks[:-1], ticks[1:]):
                    if path[tick] != regime:
                        continue
                    previous_event = int((scale, previous_tick) in scale_events)
                    current_event = int((scale, tick) in scale_events)
                    if previous_event == 0 and current_event == 0:
                        n00 += 1
                    elif previous_event == 0 and current_event == 1:
                        n01 += 1
                    elif previous_event == 1 and current_event == 0:
                        n10 += 1
                    else:
                        n11 += 1

                regimes.append({
                    "regime": regime,
                    "opportunities": len(assigned),
                    "positive_events": positive,
                    "zero_events": len(assigned) - positive,
                    "n00": n00,
                    "n01": n01,
                    "n10": n10,
                    "n11": n11,
                    "transition_count": n00 + n01 + n10 + n11,
                    "previous_zero_support": n00 + n01,
                    "previous_one_support": n10 + n11,
                })
            scale_rows.append({"scale": scale, "regimes": regimes})

        folds.append({
            "fold": fold_number,
            "fit_range": [0, train_end],
            "exploratory_selected_regimes_by_bic": regime_count,
            "candidate_bic": {
                str(k): float(item["bic"])
                for k, item in zip((1, 2, 3), candidates)
            },
            "scale_regime_transition_counts": scale_rows,
        })

    return {
        "experiment": "westermo-training-prefix-scale-regime-occurrence-support-audit",
        "status": "descriptive_training_only_not_model_selection",
        "training_prefix": [0, len(signal)],
        "training_prefix_sha256": TRAINING_PREFIX_SHA256,
        "validation_test_rows_read": False,
        "synthetic_or_artificial_events": False,
        "independent_gamma_minimum_events_per_scale_regime": 30,
        "positive_marks_by_scale": occupancy,
        "folds": folds,
        "limitations": [
            "The diagonal-Gaussian HMM is an independent exploratory implementation, not a canonical validated fitter.",
            "Viterbi states are used only for descriptive training transition support, not predictive scores or regime-selection evidence.",
            "The deterministic decomposition is frozen over the complete training prefix; fold ranges are not end-to-end causal decomposition replays.",
            "No occurrence probabilities, shrinkage strength, regime, or mark family is selected.",
            "A constant all-zero or all-one opportunity sequence does not identify a two-transition Markov model and is not a policy decision.",
        ],
    }


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("trace_csv", type=Path)
    parser.add_argument("--output", type=Path)
    args = parser.parse_args()
    report = audit(args.trace_csv)
    rendered = json.dumps(report, indent=2, sort_keys=True) + "\n"
    if args.output:
        args.output.write_text(rendered, encoding="utf-8")
    print(rendered)
