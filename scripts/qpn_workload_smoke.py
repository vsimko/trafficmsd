#!/usr/bin/env python3
"""Exercise the QPN schedule builder and LINE trace bridge on a tiny synthetic trace."""

from __future__ import annotations

import csv
import json
import subprocess
import sys
import tempfile
from pathlib import Path

from line_qpn_trace_evaluation import run_experiment

ROOT = Path(__file__).resolve().parents[1]
BUILDER = ROOT / "scripts" / "build_qpn_workload_schedules.py"
CONFIG = ROOT / "experiments" / "qpn-workload-evaluation-v1" / "config.json"
MODEL = ROOT / "experiments" / "qpn-workload-evaluation-v1" / "model-spec.json"
CONTROL = ROOT / "experiments" / "qpn-workload-evaluation-v1" / "analytical-control.json"
CONTROL_RUNNER = ROOT / "scripts" / "line_qpn_analytical_control.py"


def write_signal(path: Path, values: list[int]) -> None:
    with path.open("w", encoding="utf-8", newline="") as stream:
        writer = csv.writer(stream)
        writer.writerow(["sample_index", "value"])
        writer.writerows(enumerate(values))


def main() -> None:
    # Both conditions contain four jobs over twelve seconds. The measured
    # schedule deliberately contains a two-job batch, which becomes a zero
    # interarrival interval in the exact trace replay.
    measured = [0, 2_000_000, 0, 1_000_000, 0, 0, 1_000_000, 0, 0, 0, 0, 0]
    replay = [0, 1_000_000, 0, 0, 2_000_000, 0, 0, 1_000_000, 0, 0, 0, 0]

    with tempfile.TemporaryDirectory(prefix="trafficmsd-qpn-smoke-") as temp:
        root = Path(temp)
        source_csv = root / "source-signal.csv"
        replay_csv = root / "reconstructed-signal.csv"
        source_manifest = root / "source-manifest.json"
        schedule_dir = root / "schedules"
        write_signal(source_csv, measured)
        write_signal(replay_csv, replay)
        source_manifest.write_text(
            json.dumps({"dataset_id": "synthetic-qpn-bridge-smoke-v1"}) + "\n",
            encoding="utf-8",
        )

        subprocess.run(
            [
                sys.executable,
                str(BUILDER),
                "--source-csv", str(source_csv),
                "--replay-csv", str(replay_csv),
                "--source-manifest", str(source_manifest),
                "--config", str(CONFIG),
                "--model-spec", str(MODEL),
                "--output-dir", str(schedule_dir),
            ],
            check=True,
        )

        for condition in ("measured", "replay"):
            run_experiment(
                schedule_dir / f"{condition}-experiment.json",
                schedule_dir / f"{condition}-arrival-schedule.json",
                MODEL,
                root / f"result-{condition}",
            )

        subprocess.run(
            [
                sys.executable,
                str(CONTROL_RUNNER),
                "--arrival-rate", "0.25",
                "--model-spec", str(MODEL),
                "--config", str(CONTROL),
                "--output", str(root / "analytical-control.json"),
            ],
            check=True,
        )

    print("QPN measured/replay trace bridge and M/M/1 analytical control: passed")


if __name__ == "__main__":
    main()
