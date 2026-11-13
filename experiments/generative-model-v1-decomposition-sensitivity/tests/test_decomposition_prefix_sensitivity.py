import sys
import tempfile
import unittest
from pathlib import Path

import numpy as np

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
import audit_decomposition_prefix_sensitivity as audit


class CompleteWindowExtractionTests(unittest.TestCase):
    def test_prefix_shorter_than_window_has_no_event_at_that_scale(self):
        events = audit.extract_events(np.arange(1.0, 8.0))
        self.assertFalse(any(scale == 3 for scale, _, _ in events))

    def test_exact_window_boundary_processes_window_once(self):
        events = audit.extract_events(np.arange(1.0, 9.0))
        scale_three = [(tick, mark) for scale, tick, mark in events if scale == 3]
        self.assertEqual(len(scale_three), 1)
        self.assertEqual(scale_three[0][0], 3)

    def test_complete_window_and_incomplete_tail_process_only_complete_window(self):
        events = audit.extract_events(np.arange(1.0, 13.0))
        scale_three = [(tick, mark) for scale, tick, mark in events if scale == 3]
        self.assertEqual(len(scale_three), 1)
        self.assertEqual(scale_three[0][0], 3)

    def test_emission_ticks_keep_global_center_left_alignment(self):
        events = audit.extract_events(np.arange(1.0, 17.0))
        ticks = sorted(tick for scale, tick, _ in events if scale == 3)
        self.assertEqual(ticks, [3, 11])

    def test_scale_zero_covers_real_samples_without_padding(self):
        events = audit.extract_events(np.ones(10))
        scale_zero_ticks = sorted(tick for scale, tick, _ in events if scale == 0)
        self.assertEqual(scale_zero_ticks, list(range(10)))
        self.assertTrue(all(tick < 10 for _, tick, _ in events))

    def test_eligible_reference_map_excludes_incomplete_window(self):
        events = [(3, 3, 8.0), (3, 11, 9.0), (0, 11, 1.0)]
        observed = audit.eligible_event_map(events, 12)
        self.assertEqual(observed, {(3, 3): 8.0, (0, 11): 1.0})


class PrefixReaderBoundaryTests(unittest.TestCase):
    def test_reader_stops_after_header_and_exact_training_prefix(self):
        rows = ["sample_index,value\n"]
        rows.extend(f"{index},{index % 7}\n" for index in range(2048))
        raw = "".join(rows).encode("utf-8")
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "trace.csv"
            path.write_bytes(raw + b"this trailing row is intentionally malformed\n")
            original = audit.TRAINING_PREFIX_SHA256
            try:
                audit.TRAINING_PREFIX_SHA256 = audit.sha256_bytes(raw)
                values = audit.read_training_prefix(path)
            finally:
                audit.TRAINING_PREFIX_SHA256 = original
        self.assertEqual(len(values), 2048)
        self.assertEqual(values[-1], float(2047 % 7))


if __name__ == "__main__":
    unittest.main()
