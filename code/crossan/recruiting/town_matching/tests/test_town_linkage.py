import sys
import unittest
from pathlib import Path


CODE_DIR = Path(__file__).resolve().parents[1]
if str(CODE_DIR) not in sys.path:
    sys.path.insert(0, str(CODE_DIR))

from town_linkage import (
    TownObservation,
    build_match_rows,
    build_side_records,
    build_town_year_panel,
    canonical_town_rows,
    clean_student_town,
    normalize_state,
    normalize_town,
)


class TownLinkageTests(unittest.TestCase):
    def test_state_and_town_normalization(self) -> None:
        self.assertEqual(normalize_state("California"), "CA")
        self.assertEqual(normalize_state("A L"), "AL")
        self.assertEqual(normalize_town("St. Louis"), "SAINT LOUIS")
        self.assertEqual(normalize_town("Ft Payne"), "FORT PAYNE")

    def test_clean_student_town_keeps_alexander_city(self) -> None:
        self.assertEqual(clean_student_town("Alex City"), "Alexander City")
        self.assertEqual(clean_student_town("Alexander City"), "Alexander City")
        self.assertEqual(clean_student_town("Vestavia Hill"), "Vestavia Hills")

    def test_side_records_materialize_raw_to_processed_crosswalk(self) -> None:
        raw_records, canonical_records = build_side_records(
            [
                TownObservation("St. Louis", "Missouri"),
                TownObservation("Saint Louis", "MO"),
                TownObservation("Ft Payne", "Alabama"),
            ],
            town_field_name="city",
            state_field_name="state",
            side_label="recruiting",
        )
        saint_louis = [
            row for row in raw_records if row["processed_town"] == "SAINT LOUIS"
        ]
        self.assertEqual(len(saint_louis), 2)
        self.assertEqual({row["processed_state"] for row in saint_louis}, {"MO"})

        canonical_rows = canonical_town_rows(canonical_records, "recruiting")
        saint_louis_group = next(row for row in canonical_rows if row["processed_town"] == "SAINT LOUIS")
        self.assertEqual(saint_louis_group["processed_group_count"], "2")
        self.assertEqual(saint_louis_group["raw_variant_count"], "2")

    def test_exact_match_is_state_aware(self) -> None:
        recruiting_raw, _ = build_side_records(
            [TownObservation("Springfield", "Missouri")],
            town_field_name="city",
            state_field_name="state",
            side_label="recruiting",
        )
        _, student_canonical = build_side_records(
            [
                TownObservation("Springfield", "MO"),
                TownObservation("Springfield", "IL"),
            ],
            town_field_name="Origin Town",
            state_field_name="Origin State",
            side_label="student",
        )
        rows = build_match_rows(recruiting_raw, student_canonical, target_prefix="student")
        self.assertEqual(rows[0]["match_type"], "exact_processed")
        self.assertEqual(rows[0]["student_matched_processed_state"], "MO")

    def test_town_year_panel_uses_matched_student_town_key(self) -> None:
        recruiting_observations = [TownObservation("St Louis", "MO", year=2017)]
        student_observations = [
            TownObservation("Saint Louis", "MO", year=2017, honors=True, bachelor_degree=True)
        ]
        recruiting_raw, _ = build_side_records(
            recruiting_observations,
            town_field_name="city",
            state_field_name="state",
            side_label="recruiting",
        )
        _, student_canonical = build_side_records(
            student_observations,
            town_field_name="Origin Town",
            state_field_name="Origin State",
            side_label="student",
        )
        matches = build_match_rows(recruiting_raw, student_canonical, target_prefix="student")
        panel = build_town_year_panel(recruiting_observations, student_observations, matches)

        self.assertEqual(len(panel), 1)
        self.assertEqual(panel[0]["processed_town"], "SAINT LOUIS")
        self.assertEqual(panel[0]["recruiting_visit_count"], "1")
        self.assertEqual(panel[0]["linked_recruiting_visit_count"], "1")
        self.assertEqual(panel[0]["student_record_count"], "1")
        self.assertEqual(panel[0]["student_honors_record_count"], "1")


if __name__ == "__main__":
    unittest.main()
