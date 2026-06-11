import sys
import unittest
from pathlib import Path


CODE_DIR = Path(__file__).resolve().parents[1]
if str(CODE_DIR) not in sys.path:
    sys.path.insert(0, str(CODE_DIR))

from build_high_school_denominators import (  # noqa: E402
    aggregate_private_pss_rows,
    aggregate_public_characteristics_rows,
    build_panel_matched_denominator_rows,
    build_denominator_rows,
    latest_source_year_at_or_before,
    parse_nonnegative_int,
)


class HighSchoolDenominatorTests(unittest.TestCase):
    def test_parse_nonnegative_int_treats_nces_negative_codes_as_missing(self) -> None:
        self.assertEqual(parse_nonnegative_int("12.0"), 12)
        self.assertIsNone(parse_nonnegative_int("-1"))
        self.assertIsNone(parse_nonnegative_int("-2"))
        self.assertIsNone(parse_nonnegative_int("-9"))
        self.assertIsNone(parse_nonnegative_int(""))

    def test_public_rows_aggregate_grade12_by_normalized_town_state(self) -> None:
        rows = [
            {"LCITY": "St. Louis", "STABR": "MO", "G12": "25"},
            {"LCITY": "Saint Louis", "STABR": "MO", "G12": "30"},
            {"LCITY": "Saint Louis", "STABR": "MO", "G12": "-1"},
        ]
        aggregate = aggregate_public_characteristics_rows(rows, school_year=2019)
        value = aggregate[("SAINT LOUIS", "MO", 2019)]

        self.assertEqual(value.grade12_enrollment, 55)
        self.assertEqual(value.grade12_school_count, 2)
        self.assertEqual(value.skipped_missing_grade12, 1)

    def test_private_rows_fall_back_to_mailing_city_when_physical_city_is_blank(self) -> None:
        rows = [
            {
                "PL_CIT": "",
                "PL_STABB": "",
                "PCITY": "Vestavia Hills",
                "PSTABB": "AL",
                "P300": "14",
            }
        ]
        aggregate = aggregate_private_pss_rows(rows, survey_school_year=2019)
        value = aggregate[("VESTAVIA HILLS", "AL", 2019)]

        self.assertEqual(value.grade12_enrollment, 14)
        self.assertEqual(value.grade12_school_count, 1)

    def test_latest_private_source_year_uses_no_future_data(self) -> None:
        self.assertEqual(latest_source_year_at_or_before(2018, {2015, 2017, 2019}), 2017)
        self.assertEqual(latest_source_year_at_or_before(2019, {2015, 2017, 2019}), 2019)
        self.assertIsNone(latest_source_year_at_or_before(2014, {2015, 2017, 2019}))

    def test_denominator_rows_require_available_public_source_for_total(self) -> None:
        public = aggregate_public_characteristics_rows(
            [{"LCITY": "Tuscaloosa", "STABR": "AL", "G12": "100"}],
            school_year=2017,
        )
        private = aggregate_private_pss_rows(
            [{"PCITY": "Tuscaloosa", "PSTABB": "AL", "P300": "20"}],
            survey_school_year=2017,
        )
        rows = build_denominator_rows(
            public_aggregates=public,
            private_aggregates=private,
            school_years=(2016, 2017),
            public_years_available={2017},
            private_source_years={2017},
        )
        row_2016 = next(row for row in rows if row["school_year"] == "2016")
        row_2017 = next(row for row in rows if row["school_year"] == "2017")

        self.assertEqual(row_2016["denominator_available"], "0")
        self.assertEqual(row_2016["total_grade12_enrollment"], "")
        self.assertEqual(row_2017["denominator_available"], "1")
        self.assertEqual(row_2017["total_grade12_enrollment"], "120")

    def test_panel_matched_rows_use_same_state_fuzzy_for_clear_typos(self) -> None:
        public = aggregate_public_characteristics_rows(
            [
                {"LCITY": "Birmingham", "STABR": "AL", "G12": "100"},
                {"LCITY": "Montgomery", "STABR": "AL", "G12": "80"},
            ],
            school_year=2017,
        )
        private = aggregate_private_pss_rows([], survey_school_year=2017)
        denominator_rows = build_denominator_rows(
            public_aggregates=public,
            private_aggregates=private,
            school_years=(2017,),
            public_years_available={2017},
            private_source_years={2017},
        )
        panel_towns = [
            {
                "processed_town": "BIRMIGHAM",
                "processed_state": "AL",
                "compact_town": "BIRMIGHAM",
                "representative_raw_town": "Birmigham",
                "representative_raw_state": "AL",
                "processed_group_count": "1",
                "raw_variant_count": "1",
            }
        ]
        matched_rows, review_rows = build_panel_matched_denominator_rows(
            denominator_rows,
            panel_towns,
            school_years=(2017,),
        )

        self.assertEqual(matched_rows[0]["processed_town"], "BIRMIGHAM")
        self.assertEqual(matched_rows[0]["denominator_source_town"], "BIRMINGHAM")
        self.assertEqual(matched_rows[0]["denominator_match_type"], "fuzzy_clear_best")
        self.assertEqual(matched_rows[0]["denominator_available"], "1")
        self.assertEqual(matched_rows[0]["total_grade12_enrollment"], "100")
        self.assertEqual(len(review_rows), 1)

    def test_panel_matched_rows_use_aliases_for_state_prefix_artifacts(self) -> None:
        public = aggregate_public_characteristics_rows(
            [{"LCITY": "Gulf Shores", "STABR": "AL", "G12": "90"}],
            school_year=2017,
        )
        denominator_rows = build_denominator_rows(
            public_aggregates=public,
            private_aggregates={},
            school_years=(2017,),
            public_years_available={2017},
            private_source_years={2017},
        )
        panel_towns = [
            {
                "processed_town": "ALABAMA GULF SHORES",
                "processed_state": "AL",
                "compact_town": "ALABAMAGULFSHORES",
                "representative_raw_town": "Alabama Gulf Shores",
                "representative_raw_state": "Alabama",
                "processed_group_count": "1",
                "raw_variant_count": "1",
            }
        ]
        matched_rows, _review_rows = build_panel_matched_denominator_rows(
            denominator_rows,
            panel_towns,
            school_years=(2017,),
        )

        self.assertEqual(matched_rows[0]["denominator_source_town"], "GULF SHORES")
        self.assertEqual(matched_rows[0]["denominator_match_type"], "alias_exact_processed")
        self.assertEqual(matched_rows[0]["denominator_match_alias"], "GULF SHORES")
        self.assertEqual(matched_rows[0]["denominator_available"], "1")

    def test_panel_matched_rows_use_high_gap_fuzzy_for_near_typo(self) -> None:
        public = aggregate_public_characteristics_rows(
            [
                {"LCITY": "Attalla", "STABR": "AL", "G12": "70"},
                {"LCITY": "Selma", "STABR": "AL", "G12": "50"},
            ],
            school_year=2017,
        )
        denominator_rows = build_denominator_rows(
            public_aggregates=public,
            private_aggregates={},
            school_years=(2017,),
            public_years_available={2017},
            private_source_years={2017},
        )
        panel_towns = [
            {
                "processed_town": "ATTALA",
                "processed_state": "AL",
                "compact_town": "ATTALA",
                "representative_raw_town": "Attala",
                "representative_raw_state": "AL",
                "processed_group_count": "1",
                "raw_variant_count": "1",
            }
        ]
        matched_rows, _review_rows = build_panel_matched_denominator_rows(
            denominator_rows,
            panel_towns,
            school_years=(2017,),
        )

        self.assertEqual(matched_rows[0]["denominator_source_town"], "ATTALLA")
        self.assertEqual(matched_rows[0]["denominator_match_type"], "fuzzy_high_gap")
        self.assertEqual(matched_rows[0]["denominator_available"], "1")

    def test_panel_matched_rows_do_not_auto_match_conflicting_directions(self) -> None:
        public = aggregate_public_characteristics_rows(
            [{"LCITY": "West Islip", "STABR": "NY", "G12": "100"}],
            school_year=2017,
        )
        denominator_rows = build_denominator_rows(
            public_aggregates=public,
            private_aggregates={},
            school_years=(2017,),
            public_years_available={2017},
            private_source_years={2017},
        )
        panel_towns = [
            {
                "processed_town": "EAST ISLIP",
                "processed_state": "NY",
                "compact_town": "EASTISLIP",
                "representative_raw_town": "East Islip",
                "representative_raw_state": "NY",
                "processed_group_count": "1",
                "raw_variant_count": "1",
            }
        ]
        matched_rows, _review_rows = build_panel_matched_denominator_rows(
            denominator_rows,
            panel_towns,
            school_years=(2017,),
        )

        self.assertEqual(matched_rows[0]["denominator_proper_match_indicator"], "0")
        self.assertEqual(matched_rows[0]["denominator_available"], "0")


if __name__ == "__main__":
    unittest.main()
