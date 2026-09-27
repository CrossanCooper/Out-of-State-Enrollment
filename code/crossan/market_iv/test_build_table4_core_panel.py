#!/usr/bin/env python3
#=====================================================================
## Author: Codex, for Crossan Cooper
## Last Modified: 2026-09-27
## file use: Test compact Table 4 panel selection and validation.
## inputs: Synthetic data and the current verified audit panel.
## outputs: unittest results only.
#=====================================================================
import unittest
import os
from pathlib import Path

import numpy as np
import pandas as pd

from build_table4_core_panel import (
    CORE_COLUMNS,
    GROUPS,
    build_core_panel,
    default_paths,
    dictionary_frame,
    peer_group_frame,
)


AUDIT_PANEL = Path(
    os.environ.get("TABLE4_EXPANDED_PANEL_PATH", default_paths()[0])
).expanduser().resolve()


class CorePanelTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.audit = pd.read_csv(AUDIT_PANEL)

    def test_current_panel_has_exact_public_schema_and_sample(self):
        core = build_core_panel(self.audit)
        self.assertEqual(tuple(core.columns), CORE_COLUMNS)
        self.assertEqual(core.shape, (648, 13))
        self.assertFalse(core.isna().any().any())
        self.assertEqual(core[["state_abbr", "grad_y"]].drop_duplicates().shape[0], 648)

    def test_values_are_selected_without_transformation(self):
        core = build_core_panel(self.audit)
        reference = self.audit.set_index(["state_abbr", "grad_y"])
        selected = core.set_index(["state_abbr", "grad_y"])
        for column in set(CORE_COLUMNS) - {"state_abbr", "grad_y"}:
            left = selected[column].sort_index()
            right = reference[column].sort_index()
            if pd.api.types.is_numeric_dtype(left):
                np.testing.assert_allclose(left, right, rtol=0, atol=0)
            else:
                pd.testing.assert_series_equal(left, right, check_names=False)

    def test_dictionary_covers_every_public_column_once(self):
        core = build_core_panel(self.audit)
        dictionary = dictionary_frame(core)
        self.assertEqual(dictionary.column.tolist(), list(CORE_COLUMNS))
        self.assertFalse(dictionary.duplicated("column").any())
        self.assertTrue(dictionary.missing_rows.eq(0).all())

    def test_peer_groups_are_exact_sizes_and_nested(self):
        groups = peer_group_frame()
        self.assertEqual(groups.groupby("control").size().to_dict(),
                         {"h_core3": 3, "h_core6": 6, "h_core10": 10, "h_core15": 15})
        previous = set()
        for control in ("h_core3", "h_core6", "h_core10", "h_core15"):
            current = set(groups.loc[groups.control.eq(control), "unitid"])
            self.assertTrue(previous.issubset(current))
            self.assertEqual(current, set(GROUPS[control]))
            previous = current

    def test_missing_required_column_fails_loudly(self):
        with self.assertRaisesRegex(ValueError, "missing required columns"):
            build_core_panel(self.audit.drop(columns="z_lso"))

    def test_key_and_timing_corruption_fail_loudly(self):
        duplicated = pd.concat([self.audit, self.audit.iloc[[0]]], ignore_index=True)
        with self.assertRaisesRegex(ValueError, "648-row"):
            build_core_panel(duplicated)
        bad_timing = self.audit.copy()
        bad_timing.loc[0, "entry_year"] += 1
        with self.assertRaisesRegex(ValueError, "entry_year"):
            build_core_panel(bad_timing)


if __name__ == "__main__":
    unittest.main()
