from __future__ import annotations

import csv
import json
import tempfile
import unittest
from pathlib import Path

from scripts.compare_streaming_coefficients import analyze


class StreamingCoefficientComparisonTests(unittest.TestCase):
    def test_compares_emission_coefficients_and_checks_reconstruction(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            model_path = root / "offline-model.json"
            matrix_path = root / "streaming-coefficients.csv"
            offline_signal_path = root / "offline-signal.csv"
            streaming_signal_path = root / "streaming-signal.csv"
            output_dir = root / "report"

            components = [
                {
                    "scale_index": 0,
                    "window_start_sample": index,
                    "window_length_samples": 1,
                    "emission_sample_index": index,
                    "amplitude": 1.0 if index == 0 else 0.0,
                }
                for index in range(8)
            ]
            components.extend([
                {
                    "scale_index": 2,
                    "window_start_sample": 0,
                    "window_length_samples": 4,
                    "emission_sample_index": 1,
                    "amplitude": 5.0,
                },
                {
                    "scale_index": 3,
                    "window_start_sample": 0,
                    "window_length_samples": 8,
                    "emission_sample_index": 3,
                    "amplitude": 2.0,
                },
            ])
            model_path.write_text(json.dumps({
                "model_id": "synthetic",
                "coverage": {
                    "observed_sample_count": 8,
                    "decomposition_sample_count": 8,
                },
                "components": components,
            }), encoding="utf-8")

            rows = []
            for scale in range(4):
                for sample in range(8):
                    if scale == 0:
                        value = 1.0 if sample == 0 else 0.0
                    elif scale == 2 and sample < 4:
                        value = 5.0
                    elif scale == 3:
                        value = 2.0
                    else:
                        value = 0.0
                    rows.append((scale, sample, value))
            with matrix_path.open("w", newline="", encoding="utf-8") as handle:
                writer = csv.writer(handle)
                writer.writerow(["scale_index", "sample_index", "value"])
                writer.writerows(rows)

            reconstructed = [1.0, 5.0, 0.0, 2.0, 0.0, 0.0, 0.0, 0.0]
            for path in [offline_signal_path, streaming_signal_path]:
                with path.open("w", newline="", encoding="utf-8") as handle:
                    writer = csv.writer(handle)
                    writer.writerow(["sample_index", "value"])
                    writer.writerows(enumerate(reconstructed))

            result = analyze(
                model_path,
                matrix_path,
                offline_signal_path,
                streaming_signal_path,
                output_dir,
            )
            self.assertEqual(result["aggregate"]["offline_positive_candidates"], 3)
            self.assertEqual(result["aggregate"]["streaming_positive_candidates"], 3)
            self.assertEqual(
                result["per_scale"][2]["observed"]["max_absolute_error"], 0.0
            )
            self.assertTrue((output_dir / "coefficient-comparison.json").exists())
            self.assertTrue((output_dir / "coefficient-comparison.md").exists())

    def test_rejects_missing_streaming_matrix_cells(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            matrix_path = root / "matrix.csv"
            matrix_path.write_text(
                "scale_index,sample_index,value\n0,0,0\n",
                encoding="utf-8",
            )
            from scripts.compare_streaming_coefficients import load_streaming_candidates

            with self.assertRaisesRegex(ValueError, "expected"):
                load_streaming_candidates(matrix_path, padded_count=8, max_scale=3)


if __name__ == "__main__":
    unittest.main()
