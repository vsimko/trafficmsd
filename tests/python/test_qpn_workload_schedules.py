from __future__ import annotations

import csv
import tempfile
import unittest
from decimal import Decimal
from pathlib import Path

from scripts.build_qpn_workload_schedules import (
    build_schedule,
    read_signal_csv,
    validate_document,
)


class QpnWorkloadScheduleTests(unittest.TestCase):
    def test_cumulative_quantization_carries_remainder_across_bins(self):
        schedule, summary = build_schedule(
            [Decimal("6"), Decimal("6"), Decimal("0"), Decimal("9")],
            schedule_id="test-cumulative",
            time_unit="s",
            bin_width=Decimal("1"),
            bytes_per_job=Decimal("10"),
        )
        self.assertEqual([row["job_count"] for row in schedule["batches"]], [0, 1, 0, 1])
        self.assertEqual([row["time"] for row in schedule["batches"]], [0.0, 1.0, 2.0, 3.0])
        self.assertEqual(summary["total_bytes"], 21.0)
        self.assertEqual(summary["total_jobs"], 2)
        self.assertEqual(summary["final_remainder_bytes"], 1.0)

    def test_empty_bins_are_retained(self):
        schedule, summary = build_schedule(
            [Decimal("0"), Decimal("0"), Decimal("10")],
            schedule_id="test-empty-bins",
            time_unit="s",
            bin_width=Decimal("0.5"),
            bytes_per_job=Decimal("10"),
        )
        self.assertEqual(
            [(row["source_bin"], row["time"], row["job_count"]) for row in schedule["batches"]],
            [(0, 0.0, 0), (1, 0.5, 0), (2, 1.0, 1)],
        )
        self.assertEqual(summary["bin_count"], 3)
        self.assertEqual(summary["first_arrival_bin"], 2)
        self.assertEqual(summary["last_arrival_bin"], 2)
        self.assertEqual(summary["active_horizon"], 0.0)
        self.assertEqual(summary["trailing_idle_bins"], 0)
        self.assertAlmostEqual(summary["simulation_horizon"], 1.5)
        validate_document("schedule", schedule)

    def test_active_horizon_is_between_first_and_last_nonempty_bins(self):
        schedule, summary = build_schedule(
            [Decimal("0"), Decimal("10"), Decimal("0"), Decimal("10"), Decimal("0")],
            schedule_id="test-leading-and-trailing-idle",
            time_unit="s",
            bin_width=Decimal("0.5"),
            bytes_per_job=Decimal("10"),
        )
        self.assertEqual(summary["first_arrival_bin"], 1)
        self.assertEqual(summary["last_arrival_bin"], 3)
        self.assertEqual(summary["active_horizon"], 1.0)
        self.assertEqual(summary["horizon"], 2.5)
        self.assertEqual(summary["trailing_idle_bins"], 1)
        self.assertAlmostEqual(summary["simulation_horizon"], 1.75)
        self.assertEqual(
            [row["job_count"] for row in schedule["batches"]],
            [0, 1, 0, 1, 0],
        )

    def test_simulation_horizon_stops_before_next_cycle_after_batched_arrivals(self):
        schedule, summary = build_schedule(
            [Decimal("30")],
            schedule_id="test-simultaneous-batch",
            time_unit="s",
            bin_width=Decimal("1"),
            bytes_per_job=Decimal("10"),
        )
        self.assertEqual(summary["total_jobs"], 3)
        self.assertEqual(summary["active_horizon"], 0.0)
        # The native LDES sampler floors each zero interval to 1e-9:
        # three arrivals occur at 1e-9, 2e-9, and 3e-9; the next
        # cyclic arrival would be at 4e-9.
        self.assertAlmostEqual(summary["simulation_horizon"], 3.5e-9, places=15)

    def test_signal_csv_requires_contiguous_indices_and_finite_nonnegative_values(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            path = Path(temp_dir) / "signal.csv"
            path.write_text("sample_index,value\n0,1.25\n1,0\n", encoding="utf-8")
            values, raw = read_signal_csv(path)
            self.assertEqual(values, [Decimal("1.25"), Decimal("0")])
            self.assertTrue(raw)

            path.write_text("sample_index,value\n0,1\n2,2\n", encoding="utf-8")
            with self.assertRaisesRegex(ValueError, "contiguous"):
                read_signal_csv(path)

            path.write_text("sample_index,value\n0,-1\n", encoding="utf-8")
            with self.assertRaisesRegex(ValueError, "non-negative"):
                read_signal_csv(path)

            path.write_text("sample_index,value\n0,nan\n", encoding="utf-8")
            with self.assertRaisesRegex(ValueError, "finite"):
                read_signal_csv(path)

    def test_trailing_empty_bins_are_retained_outside_active_horizon(self):
        schedule, summary = build_schedule(
            [Decimal("10"), Decimal("0"), Decimal("0")],
            schedule_id="test-trailing-idle",
            time_unit="s",
            bin_width=Decimal("0.5"),
            bytes_per_job=Decimal("10"),
        )
        self.assertEqual(summary["horizon"], 1.5)
        self.assertEqual(summary["active_horizon"], 0.0)
        self.assertEqual(summary["trailing_idle_bins"], 2)
        self.assertAlmostEqual(summary["simulation_horizon"], 1.5e-9, places=15)
        self.assertEqual([row["job_count"] for row in schedule["batches"]], [1, 0, 0])

    def test_schedule_rejects_nonpositive_mapping(self):
        with self.assertRaisesRegex(ValueError, "bytes_per_job"):
            build_schedule(
                [Decimal("1")],
                schedule_id="invalid",
                time_unit="s",
                bin_width=Decimal("1"),
                bytes_per_job=Decimal("0"),
            )


if __name__ == "__main__":
    unittest.main()
