import sys
import unittest
from pathlib import Path


CODE_DIR = Path(__file__).resolve().parents[1]
if str(CODE_DIR) not in sys.path:
    sys.path.insert(0, str(CODE_DIR))

from build_visit_graduate_panel import build_visit_graduate_panel, school_year_from_date


class VisitGraduatePanelTests(unittest.TestCase):
    def test_school_year_from_date_uses_august_start(self) -> None:
        self.assertEqual(school_year_from_date("2016-08-01"), 2016)
        self.assertEqual(school_year_from_date("2017-07-31"), 2016)
        self.assertEqual(school_year_from_date("2017-08-01"), 2017)

    def test_panel_links_visits_t_to_graduates_t_plus_h(self) -> None:
        recruiting_rows = [
            {
                "city": "St Louis",
                "state": "Missouri",
                "state_abbrev": "MO",
                "extracted_date": "2016-09-15",
                "extracted_EventName": "Example High School",
            },
            {
                "city": "St Louis",
                "state": "Missouri",
                "state_abbrev": "MO",
                "extracted_date": "2017-02-15",
                "extracted_EventName": "Example High School",
            },
        ]
        student_rows = [
            {
                "Origin Town": "Saint Louis",
                "Origin State": "MO",
                "Year": "2021",
                "Degree": "Bachelor's Degree",
                "Honors": "Yes",
            }
        ]
        match_rows = [
            {
                "processed_town": "SAINT LOUIS",
                "processed_state": "MO",
                "student_matched_processed_town": "SAINT LOUIS",
                "student_matched_processed_state": "MO",
                "match_type": "exact_processed",
                "proper_match_indicator": "1",
                "match_score": "1.000",
            }
        ]
        panel = build_visit_graduate_panel(recruiting_rows, student_rows, match_rows)
        row = next(
            item
            for item in panel
            if item["processed_town"] == "SAINT LOUIS" and item["recruiting_school_year"] == "2016"
        )

        self.assertEqual(row["outcome_grad_year"], "2021")
        self.assertEqual(row["visits_t"], "2")
        self.assertEqual(row["any_visit_t"], "1")
        self.assertEqual(row["graduates_t_plus_h"], "1")
        self.assertEqual(row["bachelor_graduates_t_plus_h"], "1")
        self.assertEqual(row["honors_graduates_t_plus_h"], "1")
        self.assertEqual(row["preferred_regression_sample"], "1")

    def test_panel_adds_pre_period_graduate_levels_and_trend(self) -> None:
        recruiting_rows = [
            {
                "city": "Mobile",
                "state": "Alabama",
                "state_abbrev": "AL",
                "extracted_date": "2018-09-15",
                "extracted_EventName": "Mobile High School",
            }
        ]
        student_rows = [
            {
                "Origin Town": "Mobile",
                "Origin State": "AL",
                "Year": "2015",
                "Degree": "Bachelor's Degree",
                "Honors": "No",
            },
            {
                "Origin Town": "Mobile",
                "Origin State": "AL",
                "Year": "2017",
                "Degree": "Bachelor's Degree",
                "Honors": "No",
            },
            {
                "Origin Town": "Mobile",
                "Origin State": "AL",
                "Year": "2017",
                "Degree": "Bachelor's Degree",
                "Honors": "No",
            },
            {
                "Origin Town": "Mobile",
                "Origin State": "AL",
                "Year": "2023",
                "Degree": "Bachelor's Degree",
                "Honors": "No",
            },
        ]
        match_rows = [
            {
                "processed_town": "MOBILE",
                "processed_state": "AL",
                "student_matched_processed_town": "MOBILE",
                "student_matched_processed_state": "AL",
                "match_type": "exact_processed",
                "proper_match_indicator": "1",
                "match_score": "1.000",
            }
        ]
        panel = build_visit_graduate_panel(recruiting_rows, student_rows, match_rows)
        row = next(item for item in panel if item["processed_town"] == "MOBILE")

        self.assertEqual(row["recruiting_school_year"], "2018")
        self.assertEqual(row["pre_graduates_t_minus_1"], "2")
        self.assertEqual(row["pre_graduates_t_minus_2"], "0")
        self.assertEqual(row["pre_graduates_t_minus_3"], "1")
        self.assertEqual(row["pre_graduates_t_minus_4"], "0")
        self.assertEqual(row["pre_graduates_t_minus_5"], "0")
        self.assertEqual(row["pre_graduates_mean_3"], "1.0000000000")
        self.assertEqual(row["pre_graduates_mean_5"], "0.6000000000")
        self.assertEqual(row["pre_graduates_trend_3"], "1")
        self.assertEqual(row["pre_graduates_trend_5"], "2")
        self.assertEqual(row["fixed_baseline_grad_year"], "2015")
        self.assertEqual(row["fixed_baseline_graduates"], "1")
        self.assertEqual(row["graduates_per_fixed_baseline_plus_one_t_plus_h"], "0.5000000000")
        self.assertEqual(row["pre_graduates_growth_since_fixed_baseline"], "0.5000000000")

    def test_review_needed_visits_are_not_in_preferred_treatment(self) -> None:
        recruiting_rows = [
            {
                "city": "Ambiguous",
                "state": "AL",
                "state_abbrev": "AL",
                "extracted_date": "2019-10-01",
                "extracted_EventName": "Ambiguous High School",
            }
        ]
        student_rows = [
            {
                "Origin Town": "Ambiguous",
                "Origin State": "AL",
                "Year": "2024",
                "Degree": "Bachelor's Degree",
                "Honors": "No",
            }
        ]
        match_rows = [
            {
                "processed_town": "AMBIGUOUS",
                "processed_state": "AL",
                "student_matched_processed_town": "OTHER",
                "student_matched_processed_state": "AL",
                "match_type": "fuzzy_review",
                "proper_match_indicator": "0",
                "match_score": "0.700",
            }
        ]
        panel = build_visit_graduate_panel(recruiting_rows, student_rows, match_rows)
        row = next(item for item in panel if item["processed_town"] == "AMBIGUOUS")

        self.assertEqual(row["visits_t"], "0")
        self.assertEqual(row["any_visit_t"], "0")
        self.assertEqual(row["review_needed_visits_t"], "1")
        self.assertEqual(row["graduates_t_plus_h"], "1")

    def test_same_state_fuzzy_sensitivity_adds_review_match_to_candidate_town(self) -> None:
        recruiting_rows = [
            {
                "city": "Fredericksbrg",
                "state": "Texas",
                "state_abbrev": "TX",
                "extracted_date": "2019-10-01",
                "extracted_EventName": "Fredericksburg High School",
            }
        ]
        student_rows = [
            {
                "Origin Town": "Fredericksburg",
                "Origin State": "TX",
                "Year": "2024",
                "Degree": "Bachelor's Degree",
                "Honors": "No",
            }
        ]
        match_rows = [
            {
                "processed_town": "FREDERICKSBRG",
                "processed_state": "TX",
                "student_matched_processed_town": "FREDERICKSBURG",
                "student_matched_processed_state": "TX",
                "match_type": "fuzzy_review",
                "proper_match_indicator": "0",
                "match_score": "0.963",
            }
        ]
        panel = build_visit_graduate_panel(recruiting_rows, student_rows, match_rows)
        candidate_row = next(item for item in panel if item["processed_town"] == "FREDERICKSBURG")
        source_row = next(item for item in panel if item["processed_town"] == "FREDERICKSBRG")

        self.assertEqual(candidate_row["visits_t"], "0")
        self.assertEqual(candidate_row["any_visit_t"], "0")
        self.assertEqual(candidate_row["same_state_fuzzy_review_visits_t"], "1")
        self.assertEqual(candidate_row["visits_t_with_same_state_fuzzy"], "1")
        self.assertEqual(candidate_row["any_visit_t_with_same_state_fuzzy"], "1")
        self.assertEqual(candidate_row["graduates_t_plus_h"], "1")
        self.assertEqual(source_row["review_needed_visits_t"], "1")

    def test_panel_merges_online_denominators_and_builds_shares(self) -> None:
        recruiting_rows = [
            {
                "city": "Tuscaloosa",
                "state": "Alabama",
                "state_abbrev": "AL",
                "extracted_date": "2019-09-01",
                "extracted_EventName": "Tuscaloosa High School",
            }
        ]
        student_rows = [
            {
                "Origin Town": "Tuscaloosa",
                "Origin State": "AL",
                "Year": "2017",
                "Degree": "Bachelor's Degree",
                "Honors": "No",
            },
            {
                "Origin Town": "Tuscaloosa",
                "Origin State": "AL",
                "Year": "2024",
                "Degree": "Bachelor's Degree",
                "Honors": "No",
            }
        ]
        match_rows = [
            {
                "processed_town": "TUSCALOOSA",
                "processed_state": "AL",
                "student_matched_processed_town": "TUSCALOOSA",
                "student_matched_processed_state": "AL",
                "match_type": "exact_processed",
                "proper_match_indicator": "1",
                "match_score": "1.000",
            }
        ]
        denominator_rows = [
            {
                "processed_town": "TUSCALOOSA",
                "processed_state": "AL",
                "school_year": "2017",
                "public_grade12_enrollment": "180",
                "private_grade12_enrollment": "20",
                "total_grade12_enrollment": "200",
                "public_grade12_school_count": "2",
                "private_grade12_school_count": "1",
                "private_pss_source_school_year": "2017",
                "private_pss_source_label": "2017-18",
                "denominator_available": "1",
            },
            {
                "processed_town": "TUSCALOOSA",
                "processed_state": "AL",
                "school_year": "2019",
                "public_grade12_enrollment": "90",
                "private_grade12_enrollment": "10",
                "total_grade12_enrollment": "100",
                "public_grade12_school_count": "1",
                "private_grade12_school_count": "1",
                "private_pss_source_school_year": "2019",
                "private_pss_source_label": "2019-20",
                "denominator_available": "1",
            }
        ]
        panel = build_visit_graduate_panel(
            recruiting_rows,
            student_rows,
            match_rows,
            denominator_rows=denominator_rows,
        )
        row = next(item for item in panel if item["processed_town"] == "TUSCALOOSA")

        self.assertEqual(row["denominator_regression_sample"], "1")
        self.assertEqual(row["total_grade12_enrollment_t"], "100")
        self.assertEqual(row["graduates_share_t_plus_h"], "0.0100000000")
        self.assertEqual(row["fixed_hs_denominator_regression_sample"], "1")
        self.assertEqual(row["fixed_hs_denominator_school_year"], "2017")
        self.assertEqual(row["fixed_hs_grade12_enrollment"], "200")
        self.assertEqual(row["graduates_share_fixed_hs_t_plus_h"], "0.0050000000")
        self.assertEqual(row["pre_graduates_t_minus_2_share_fixed_hs"], "0.0050000000")
        self.assertEqual(row["pre_graduates_t_minus_5_share_fixed_hs"], "0.0000000000")

    def test_panel_adds_leave_year_out_predicted_graduates(self) -> None:
        recruiting_rows = [
            {
                "city": "Town A",
                "state": "AL",
                "state_abbrev": "AL",
                "extracted_date": "2019-09-01",
                "extracted_EventName": "Town A High School",
            }
        ]
        student_rows = [
            {"Origin Town": "Town A", "Origin State": "AL", "Year": "2022", "Degree": "BA", "Honors": ""},
            {"Origin Town": "Town B", "Origin State": "AL", "Year": "2022", "Degree": "BA", "Honors": ""},
            {"Origin Town": "Town B", "Origin State": "AL", "Year": "2022", "Degree": "BA", "Honors": ""},
            {"Origin Town": "Town B", "Origin State": "AL", "Year": "2022", "Degree": "BA", "Honors": ""},
            {"Origin Town": "Town A", "Origin State": "AL", "Year": "2024", "Degree": "BA", "Honors": ""},
            {"Origin Town": "Town A", "Origin State": "AL", "Year": "2024", "Degree": "BA", "Honors": ""},
            {"Origin Town": "Town B", "Origin State": "AL", "Year": "2024", "Degree": "BA", "Honors": ""},
        ]
        match_rows = [
            {
                "processed_town": "TOWN A",
                "processed_state": "AL",
                "student_matched_processed_town": "TOWN A",
                "student_matched_processed_state": "AL",
                "match_type": "exact_processed",
                "proper_match_indicator": "1",
                "match_score": "1.000",
            }
        ]
        denominator_rows = [
            {
                "processed_town": "TOWN A",
                "processed_state": "AL",
                "school_year": "2017",
                "public_grade12_enrollment": "100",
                "private_grade12_enrollment": "0",
                "total_grade12_enrollment": "100",
                "public_grade12_school_count": "1",
                "private_grade12_school_count": "0",
                "private_pss_source_school_year": "2017",
                "private_pss_source_label": "2017-18",
                "denominator_available": "1",
            }
        ]
        panel = build_visit_graduate_panel(
            recruiting_rows,
            student_rows,
            match_rows,
            denominator_rows=denominator_rows,
        )
        row = next(
            item
            for item in panel
            if item["processed_town"] == "TOWN A" and item["recruiting_school_year"] == "2019"
        )

        self.assertEqual(row["graduates_t_plus_h"], "2")
        self.assertEqual(row["predicted_graduates_t_plus_h"], "0.7500000000")
        self.assertEqual(row["predicted_graduates_share_fixed_hs_t_plus_h"], "0.0075000000")


if __name__ == "__main__":
    unittest.main()
