import importlib.util
import unittest
from decimal import Decimal
from pathlib import Path

SCRIPT = Path(__file__).resolve().parents[2] / "scripts" / "datasets" / "extract_westermo.py"
SPEC = importlib.util.spec_from_file_location("extract_westermo", SCRIPT)
MODULE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(MODULE)


class AggregatePacketsTests(unittest.TestCase):
    def test_left_closed_right_open_boundaries(self):
        signal, origin, last = MODULE.aggregate_packets([
            (Decimal("11.0"), 30),
            (Decimal("10.999"), 20),
            (Decimal("10.0"), 100),
        ], Decimal("1"))
        self.assertEqual(signal, [120, 30])
        self.assertEqual(origin, Decimal("10.0"))
        self.assertEqual(last, Decimal("11.0"))

    def test_empty_bins_are_zero(self):
        signal, _, _ = MODULE.aggregate_packets([
            (Decimal("4.2"), 12),
            (Decimal("6.2"), 7),
        ], Decimal("1"))
        self.assertEqual(signal, [12, 0, 7])

    def test_fractional_bin_width(self):
        signal, _, _ = MODULE.aggregate_packets([
            (Decimal("1.0"), 10),
            (Decimal("1.49"), 20),
            (Decimal("1.5"), 30),
        ], Decimal("0.5"))
        self.assertEqual(signal, [30, 30])

    def test_input_order_does_not_change_output(self):
        rows = [(Decimal("2.0"), 3), (Decimal("0.0"), 1), (Decimal("1.0"), 2)]
        first = MODULE.aggregate_packets(rows, Decimal("1"))
        second = MODULE.aggregate_packets(list(reversed(rows)), Decimal("1"))
        self.assertEqual(first, second)

    def test_capinfos_prefers_exact_interface_count_over_rounded_summary(self):
        report = (
            "Number of packets:   35 k\\n"
            "Interface #0 info:\\n"
            "                     Number of packets = 35067\\n"
        )
        import re
        matches = re.findall(r"Number of packets\\s*[:=]\\s*([\\d,]+)", report)
        self.assertEqual(int(matches[-1].replace(",", "")), 35067)

    def test_rejects_non_positive_width_and_empty_input(self):
        with self.assertRaises(ValueError):
            MODULE.aggregate_packets([(Decimal("0"), 1)], Decimal("0"))
        with self.assertRaises(ValueError):
            MODULE.aggregate_packets([], Decimal("1"))


if __name__ == "__main__":
    unittest.main()
