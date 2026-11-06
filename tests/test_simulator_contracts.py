"""Regression tests for the simulator-neutral contract v1 validators."""

import copy
import importlib.util
import math
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
SPEC = importlib.util.spec_from_file_location(
    "validate_simulator_contracts", ROOT / "scripts" / "validate_simulator_contracts.py"
)
contracts = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(contracts)


class ContractValidationTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        folder = ROOT / "experiments" / "simulator-contract-v1"
        cls.schedule = contracts.load_json(folder / "arrival-schedule.json")
        cls.experiment = contracts.load_json(folder / "experiment.json")
        cls.capabilities = contracts.load_json(folder / "capabilities.json")
        cls.result = contracts.load_json(folder / "result.json")

    def test_all_fixtures_validate(self):
        contracts.validate_fixtures()

    def test_canonical_serialization_is_stable(self):
        first = contracts.canonical_json({"z": 1, "a": [2, 3]})
        second = contracts.canonical_json({"a": [2, 3], "z": 1})
        self.assertEqual(first, second)
        self.assertEqual(contracts.sha256_json({"z": 1, "a": [2, 3]}),
                         contracts.sha256_json({"a": [2, 3], "z": 1}))

    def test_non_finite_values_are_rejected(self):
        with self.assertRaises(ValueError):
            contracts.canonical_json({"value": math.inf})
        with self.assertRaisesRegex(ValueError, "overflows finite range"):
            contracts._parse_finite_float("1e999")

    def test_empty_bins_are_preserved_and_indexed(self):
        self.assertEqual(self.schedule["batches"][1]["job_count"], 0)
        broken = copy.deepcopy(self.schedule)
        broken["batches"][1]["source_bin"] = 2
        with self.assertRaisesRegex(ValueError, "contiguous"):
            contracts.validate_schedule_semantics(broken)

    def test_invalid_bin_time_is_rejected(self):
        broken = copy.deepcopy(self.schedule)
        broken["batches"][2]["time"] = 2.5
        with self.assertRaisesRegex(ValueError, "bin_start"):
            contracts.validate_schedule_semantics(broken)

    def test_remainder_must_be_less_than_job_size(self):
        broken = copy.deepcopy(self.schedule)
        broken["workload_mapping"]["final_remainder_bytes"] = 1000
        with self.assertRaisesRegex(ValueError, "remainder"):
            contracts.validate_schedule_semantics(broken)

    def test_stochastic_schedule_requires_seed(self):
        broken = copy.deepcopy(self.schedule)
        broken["generation"]["mode"] = "stochastic"
        with self.assertRaisesRegex(ValueError, "explicit seed"):
            contracts.validate_schedule_semantics(broken)

    def test_replication_count_must_match_seed_list(self):
        broken = copy.deepcopy(self.experiment)
        broken["replications"]["count"] = 3
        with self.assertRaisesRegex(ValueError, "number of explicit seeds"):
            contracts.validate_experiment_semantics(broken, self.capabilities)

    def test_backend_rejects_unsupported_constructs(self):
        broken = copy.deepcopy(self.experiment)
        broken["capabilities"]["constructs"].append("inhibitor_arc")
        with self.assertRaisesRegex(ValueError, "capability mismatch"):
            contracts.validate_experiment_semantics(broken, self.capabilities)

    def test_fixed_warmup_requires_duration(self):
        broken = copy.deepcopy(self.experiment)
        broken["warmup"] = {"policy": "fixed_time"}
        with self.assertRaisesRegex(ValueError, "positive duration"):
            contracts.validate_experiment_semantics(broken, self.capabilities)

    def test_required_metric_cannot_be_silently_missing(self):
        broken = copy.deepcopy(self.result)
        del broken["metrics"]["throughput"]
        with self.assertRaisesRegex(ValueError, "omits required metrics"):
            contracts.validate_result_semantics(self.experiment, broken)

    def test_unavailable_required_metric_fails_result(self):
        broken = copy.deepcopy(self.result)
        broken["validation"] = "passed"
        with self.assertRaisesRegex(ValueError, "validation=failed"):
            contracts.validate_result_semantics(self.experiment, broken)


if __name__ == "__main__":
    unittest.main()
