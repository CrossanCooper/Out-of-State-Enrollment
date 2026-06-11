#!/usr/bin/env python3
#=====================================================================
## Author: Crossan Cooper
## Last Modified: 06-08-2026
#
## file use: Build a town-level panel linking admissions visits in school
## year t to UA graduate counts in graduation year t+h.
#
## inputs:
## 1. data/full_ua_recruiting_scrape.csv -- admissions visit records
## 2. data/all_alabama_data.csv -- UA commencement student-origin records
## 3. town_matching/output/ua_recruiting_to_student_town_matches.csv -- town linkage crosswalk
## 4. town_matching/output/ua_high_school_denominators_online_panel_matched.csv -- optional grade-12 denominators
#
## outputs:
## 1. town_matching/output/ua_visit_t_graduates_t_plus_5_panel.csv -- regression panel
## 2. town_matching/output/ua_visit_t_graduates_t_plus_5_summary.md -- panel summary
#=====================================================================
"""Build a town-level panel linking recruiting visits in t to graduates in t+h."""

from __future__ import annotations

import argparse
import csv
from collections import Counter, defaultdict
from dataclasses import dataclass
from pathlib import Path

from town_linkage import (
    DEFAULT_OUTPUT_DIR,
    DEFAULT_RECRUITING_PATH,
    DEFAULT_STUDENT_PATH,
    clean_student_state,
    clean_student_town,
    compact_town,
    normalize_state,
    normalize_town,
    run_linkage,
    squash_spaces,
)


#=====================================================================
# 1 - Paths and panel settings
#=====================================================================

ROOT = Path(__file__).resolve().parents[1]
DEFAULT_MATCH_PATH = DEFAULT_OUTPUT_DIR / "ua_recruiting_to_student_town_matches.csv"
DEFAULT_PANEL_PATH = DEFAULT_OUTPUT_DIR / "ua_visit_t_graduates_t_plus_5_panel.csv"
DEFAULT_SUMMARY_PATH = DEFAULT_OUTPUT_DIR / "ua_visit_t_graduates_t_plus_5_summary.md"
DEFAULT_DENOMINATOR_PATH = DEFAULT_OUTPUT_DIR / "ua_high_school_denominators_online_panel_matched.csv"


#=====================================================================
# 2 - Data structures and IO helpers
#=====================================================================

@dataclass(frozen=True)
class RecruitingMatch:
    """A processed recruiting town's linkage to the student-origin town universe."""

    panel_town: str
    panel_state: str
    candidate_town: str
    candidate_state: str
    match_type: str
    proper_match: bool
    match_score: float
    same_state_fuzzy_candidate: bool


def school_year_from_date(date_text: str, start_month: int = 8) -> int:
    """Convert an ISO date to an academic-year label.

    With the default August start, dates from August-December 2016 are school
    year 2016, and dates from January-July 2017 are also school year 2016.
    """
    year_text, month_text, _day_text = date_text.split("-")
    year = int(year_text)
    month = int(month_text)
    return year if month >= start_month else year - 1


def school_year_label(school_year: int) -> str:
    """Format a school-year label for output tables."""
    return f"{school_year}-{str(school_year + 1)[-2:]}"


def parse_match_score(value: str) -> float:
    """Parse match scores, placing blanks below any valid score."""
    try:
        return float(value)
    except ValueError:
        return -1.0


def read_csv_rows(path: Path) -> list[dict[str, str]]:
    """Read CSV rows as dictionaries."""
    with path.open(newline="", encoding="utf-8") as handle:
        return list(csv.DictReader(handle))


def write_csv(path: Path, rows: list[dict[str, str]]) -> None:
    """Write dictionaries to CSV."""
    path.parent.mkdir(parents=True, exist_ok=True)
    if not rows:
        path.write_text("")
        return
    with path.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=list(rows[0].keys()))
        writer.writeheader()
        writer.writerows(rows)


def read_denominator_map(path: Path | None) -> dict[tuple[str, str, int], dict[str, str]]:
    """Read optional online high-school denominators keyed by town/state/year."""
    if path is None or not path.exists():
        return {}
    denominator_map = {}
    for row in read_csv_rows(path):
        year_text = squash_spaces(row.get("school_year", ""))
        if not year_text.isdigit():
            continue
        key = (row["processed_town"], row["processed_state"], int(year_text))
        denominator_map[key] = row
    return denominator_map


def parse_positive_float(value: str) -> float | None:
    """Parse positive denominators for share calculations."""
    try:
        number = float(value)
    except ValueError:
        return None
    return number if number > 0 else None


def format_share(numerator: float, denominator_text: str) -> str:
    """Format a count divided by a positive denominator."""
    denominator = parse_positive_float(denominator_text)
    if denominator is None:
        return ""
    return f"{numerator / denominator:.10f}"


def format_ratio(numerator: float, denominator: float) -> str:
    """Format a ratio for denominators that are positive by construction."""
    return f"{numerator / denominator:.10f}"


def format_float(value: float) -> str:
    """Format a floating-point panel variable without excess precision."""
    return f"{value:.10f}"


def choose_match(existing: RecruitingMatch | None, candidate: RecruitingMatch) -> RecruitingMatch:
    """Choose the most reliable linkage when raw variants share a processed key."""
    if existing is None:
        return candidate
    existing_rank = (int(existing.proper_match), existing.match_score)
    candidate_rank = (int(candidate.proper_match), candidate.match_score)
    return candidate if candidate_rank > existing_rank else existing


#=====================================================================
# 3 - Visit, match, and graduate panel construction
#=====================================================================

def build_recruiting_match_map(
    match_rows: list[dict[str, str]],
    same_state_fuzzy_threshold: float = 0.90,
) -> dict[tuple[str, str], RecruitingMatch]:
    """Map processed recruiting towns to preferred student-origin panel towns."""
    match_map: dict[tuple[str, str], RecruitingMatch] = {}
    for row in match_rows:
        source_key = (row["processed_town"], row["processed_state"])
        proper_match = row["proper_match_indicator"] == "1"
        match_score = parse_match_score(row.get("match_score", ""))
        candidate_town = row.get("student_matched_processed_town", "")
        candidate_state = row.get("student_matched_processed_state", "")
        if proper_match:
            panel_town = candidate_town
            panel_state = candidate_state
        else:
            panel_town = row["processed_town"]
            panel_state = row["processed_state"]
        same_state_fuzzy_candidate = (
            not proper_match
            and row["match_type"].startswith("fuzzy")
            and bool(candidate_town)
            and candidate_state == row["processed_state"]
            and match_score >= same_state_fuzzy_threshold
        )
        candidate = RecruitingMatch(
            panel_town=panel_town,
            panel_state=panel_state,
            candidate_town=candidate_town,
            candidate_state=candidate_state,
            match_type=row["match_type"],
            proper_match=proper_match,
            match_score=match_score,
            same_state_fuzzy_candidate=same_state_fuzzy_candidate,
        )
        match_map[source_key] = choose_match(match_map.get(source_key), candidate)
    return match_map


def is_honors_row(value: str) -> bool:
    """Return True for affirmative honors flags in student-origin data."""
    return squash_spaces(value).lower() in {"yes", "y", "true", "1"}


def is_bachelor_degree(value: str) -> bool:
    """Return True for bachelor's degree rows."""
    return "bachelor" in squash_spaces(value).lower()


def town_state_id(town: str, state: str) -> str:
    """Create a stable town/state identifier."""
    return f"{state}_{compact_town(town)}"


def aggregate_recruiting_visits(
    recruiting_rows: list[dict[str, str]],
    match_map: dict[tuple[str, str], RecruitingMatch],
    start_month: int,
) -> tuple[
    set[int],
    set[tuple[str, str]],
    Counter[tuple[str, str, int]],
    Counter[tuple[str, str, int]],
    Counter[tuple[str, str, int]],
    Counter[tuple[str, str, int]],
    Counter[tuple[str, str, int]],
    Counter[tuple[str, str, int]],
    defaultdict[tuple[str, str, int], set[str]],
]:
    """Aggregate recruiting visits by linked town and school year."""
    school_years: set[int] = set()
    visit_towns: set[tuple[str, str]] = set()
    preferred_visits: Counter[tuple[str, str, int]] = Counter()
    exact_visits: Counter[tuple[str, str, int]] = Counter()
    fuzzy_clear_visits: Counter[tuple[str, str, int]] = Counter()
    same_state_fuzzy_review_visits: Counter[tuple[str, str, int]] = Counter()
    review_visits: Counter[tuple[str, str, int]] = Counter()
    all_visit_rows: Counter[tuple[str, str, int]] = Counter()
    unique_events: defaultdict[tuple[str, str, int], set[str]] = defaultdict(set)

    for row in recruiting_rows:
        raw_town = squash_spaces(row.get("city", ""))
        raw_state = squash_spaces(row.get("state_abbrev", "")) or squash_spaces(row.get("state", ""))
        date_text = squash_spaces(row.get("extracted_date", ""))
        if not raw_town or not raw_state or not date_text:
            continue

        school_year = school_year_from_date(date_text, start_month=start_month)
        source_key = (normalize_town(raw_town), normalize_state(raw_state))
        match = match_map.get(
            source_key,
            RecruitingMatch(
                panel_town=source_key[0],
                panel_state=source_key[1],
                candidate_town="",
                candidate_state="",
                match_type="missing_match_row",
                proper_match=False,
                match_score=-1.0,
                same_state_fuzzy_candidate=False,
            ),
        )
        panel_key = (match.panel_town, match.panel_state, school_year)

        school_years.add(school_year)
        visit_towns.add((match.panel_town, match.panel_state))
        all_visit_rows[panel_key] += 1
        unique_events[panel_key].add(squash_spaces(row.get("extracted_EventName", "")))

        if match.proper_match:
            preferred_visits[panel_key] += 1
            if match.match_type in {"exact_processed", "exact_compact"}:
                exact_visits[panel_key] += 1
            elif match.match_type == "fuzzy_clear_best":
                fuzzy_clear_visits[panel_key] += 1
        else:
            review_visits[panel_key] += 1
            if match.same_state_fuzzy_candidate:
                candidate_key = (match.candidate_town, match.candidate_state, school_year)
                same_state_fuzzy_review_visits[candidate_key] += 1

    return (
        school_years,
        visit_towns,
        preferred_visits,
        exact_visits,
        fuzzy_clear_visits,
        same_state_fuzzy_review_visits,
        review_visits,
        all_visit_rows,
        unique_events,
    )


def aggregate_student_outcomes(
    student_rows: list[dict[str, str]],
) -> tuple[
    set[int],
    set[tuple[str, str]],
    Counter[tuple[str, str, int]],
    Counter[tuple[str, str, int]],
    Counter[tuple[str, str, int]],
]:
    """Aggregate student-origin rows by town/state and graduation year."""
    student_years: set[int] = set()
    student_towns: set[tuple[str, str]] = set()
    graduate_counts: Counter[tuple[str, str, int]] = Counter()
    bachelor_counts: Counter[tuple[str, str, int]] = Counter()
    honors_counts: Counter[tuple[str, str, int]] = Counter()

    for row in student_rows:
        year_text = squash_spaces(row.get("Year", ""))
        if not year_text.isdigit():
            continue
        year = int(year_text)
        raw_town = row.get("Origin Town", row.get("OriginTown", ""))
        raw_state = row.get("Origin State", row.get("OriginState", ""))
        town = normalize_town(clean_student_town(raw_town))
        state = normalize_state(clean_student_state(raw_state))
        if not town or not state:
            continue
        key = (town, state, year)
        graduate_counts[key] += 1
        bachelor_counts[key] += int(is_bachelor_degree(row.get("Degree", "")))
        honors_counts[key] += int(is_honors_row(row.get("Honors", "")))
        student_years.add(year)
        student_towns.add((town, state))

    return student_years, student_towns, graduate_counts, bachelor_counts, honors_counts


def build_visit_graduate_panel(
    recruiting_rows: list[dict[str, str]],
    student_rows: list[dict[str, str]],
    match_rows: list[dict[str, str]],
    denominator_rows: list[dict[str, str]] | None = None,
    horizon: int = 5,
    start_month: int = 8,
    same_state_fuzzy_threshold: float = 0.90,
    baseline_grad_year: int = 2015,
    fixed_hs_denominator_year: int = 2017,
) -> list[dict[str, str]]:
    """Build a town x school-year panel for visits in t and graduates in t+h."""
    match_map = build_recruiting_match_map(match_rows, same_state_fuzzy_threshold)
    (
        school_years,
        visit_towns,
        preferred_visits,
        exact_visits,
        fuzzy_clear_visits,
        same_state_fuzzy_review_visits,
        review_visits,
        all_visit_rows,
        unique_events,
    ) = aggregate_recruiting_visits(recruiting_rows, match_map, start_month)
    (
        student_years,
        student_towns,
        graduate_counts,
        bachelor_counts,
        honors_counts,
    ) = aggregate_student_outcomes(student_rows)
    total_graduates_by_year: Counter[int] = Counter()
    town_total_graduates: Counter[tuple[str, str]] = Counter()
    total_graduates_all_years = 0
    for (town, state, year), count in graduate_counts.items():
        total_graduates_by_year[year] += count
        town_total_graduates[(town, state)] += count
        total_graduates_all_years += count
    denominator_map = {}
    if denominator_rows is not None:
        for row in denominator_rows:
            year_text = squash_spaces(row.get("school_year", ""))
            if year_text.isdigit():
                denominator_map[(row["processed_town"], row["processed_state"], int(year_text))] = row

    towns = sorted(student_towns | visit_towns)
    rows = []
    for town, state in towns:
        in_student_universe = (town, state) in student_towns
        for school_year in sorted(school_years):
            outcome_year = school_year + horizon
            key = (town, state, school_year)
            outcome_key = (town, state, outcome_year)
            outcome_observed = outcome_year in student_years
            denominator = denominator_map.get((town, state, school_year), {})
            total_denominator = denominator.get("total_grade12_enrollment", "")
            denominator_available = denominator.get("denominator_available", "0") == "1"
            fixed_hs_denominator = denominator_map.get((town, state, fixed_hs_denominator_year), {})
            fixed_hs_denominator_text = fixed_hs_denominator.get("total_grade12_enrollment", "")
            fixed_hs_denominator_available = (
                fixed_hs_denominator.get("denominator_available", "0") == "1"
                and parse_positive_float(fixed_hs_denominator_text) is not None
            )
            graduates = graduate_counts[outcome_key]
            outcome_year_total_graduates = total_graduates_by_year[outcome_year]
            town_leave_year_graduates = town_total_graduates[(town, state)] - graduates
            all_leave_year_graduates = total_graduates_all_years - outcome_year_total_graduates
            predicted_graduates = 0.0
            if all_leave_year_graduates > 0:
                predicted_graduates = (
                    town_leave_year_graduates / all_leave_year_graduates
                ) * outcome_year_total_graduates
            bachelor_graduates = bachelor_counts[outcome_key]
            honors_graduates = honors_counts[outcome_key]
            pre_grad_1 = graduate_counts[(town, state, school_year - 1)]
            pre_grad_2 = graduate_counts[(town, state, school_year - 2)]
            pre_grad_3 = graduate_counts[(town, state, school_year - 3)]
            pre_grad_4 = graduate_counts[(town, state, school_year - 4)]
            pre_grad_5 = graduate_counts[(town, state, school_year - 5)]
            pre_bachelor_1 = bachelor_counts[(town, state, school_year - 1)]
            pre_bachelor_2 = bachelor_counts[(town, state, school_year - 2)]
            pre_bachelor_3 = bachelor_counts[(town, state, school_year - 3)]
            pre_bachelor_4 = bachelor_counts[(town, state, school_year - 4)]
            pre_bachelor_5 = bachelor_counts[(town, state, school_year - 5)]
            pre_grad_mean_3 = (pre_grad_1 + pre_grad_2 + pre_grad_3) / 3
            pre_grad_mean_5 = (pre_grad_1 + pre_grad_2 + pre_grad_3 + pre_grad_4 + pre_grad_5) / 5
            pre_bachelor_mean_3 = (pre_bachelor_1 + pre_bachelor_2 + pre_bachelor_3) / 3
            pre_bachelor_mean_5 = (
                pre_bachelor_1 + pre_bachelor_2 + pre_bachelor_3 + pre_bachelor_4 + pre_bachelor_5
            ) / 5
            baseline_graduates = graduate_counts[(town, state, baseline_grad_year)]
            baseline_bachelor_graduates = bachelor_counts[(town, state, baseline_grad_year)]
            baseline_denominator = baseline_graduates + 1
            baseline_bachelor_denominator = baseline_bachelor_graduates + 1
            pre_growth_since_baseline = (pre_grad_1 - baseline_graduates) / baseline_denominator
            pre_bachelor_growth_since_baseline = (
                (pre_bachelor_1 - baseline_bachelor_graduates) / baseline_bachelor_denominator
            )
            visit_count = preferred_visits[key]
            broad_visit_count = preferred_visits[key] + same_state_fuzzy_review_visits[key]
            rows.append(
                {
                    "town_state_id": town_state_id(town, state),
                    "processed_town": town,
                    "processed_state": state,
                    "recruiting_school_year": str(school_year),
                    "recruiting_school_year_label": school_year_label(school_year),
                    "outcome_grad_year": str(outcome_year),
                    "outcome_grad_year_observed": str(int(outcome_observed)),
                    "in_student_origin_town_universe": str(int(in_student_universe)),
                    "preferred_regression_sample": str(int(outcome_observed and in_student_universe)),
                    "denominator_regression_sample": str(
                        int(outcome_observed and in_student_universe and denominator_available)
                    ),
                    "fixed_hs_denominator_regression_sample": str(
                        int(outcome_observed and in_student_universe and fixed_hs_denominator_available)
                    ),
                    "visits_t": str(visit_count),
                    "any_visit_t": str(int(visit_count > 0)),
                    "exact_match_visits_t": str(exact_visits[key]),
                    "fuzzy_clear_match_visits_t": str(fuzzy_clear_visits[key]),
                    "same_state_fuzzy_review_visits_t": str(same_state_fuzzy_review_visits[key]),
                    "visits_t_with_same_state_fuzzy": str(broad_visit_count),
                    "any_visit_t_with_same_state_fuzzy": str(int(broad_visit_count > 0)),
                    "review_needed_visits_t": str(review_visits[key]),
                    "all_raw_visit_rows_t": str(all_visit_rows[key]),
                    "unique_recruiting_event_names_t": str(
                        len({event for event in unique_events[key] if event})
                    ),
                    "graduates_t_plus_h": str(graduates),
                    "predicted_graduates_t_plus_h": format_float(predicted_graduates),
                    "bachelor_graduates_t_plus_h": str(bachelor_graduates),
                    "honors_graduates_t_plus_h": str(honors_graduates),
                    "pre_graduates_t_minus_1": str(pre_grad_1),
                    "pre_graduates_t_minus_2": str(pre_grad_2),
                    "pre_graduates_t_minus_3": str(pre_grad_3),
                    "pre_graduates_t_minus_4": str(pre_grad_4),
                    "pre_graduates_t_minus_5": str(pre_grad_5),
                    "pre_graduates_mean_3": format_float(pre_grad_mean_3),
                    "pre_graduates_mean_5": format_float(pre_grad_mean_5),
                    "pre_graduates_trend_3": str(pre_grad_1 - pre_grad_3),
                    "pre_graduates_trend_5": str(pre_grad_1 - pre_grad_5),
                    "fixed_baseline_grad_year": str(baseline_grad_year),
                    "fixed_baseline_graduates": str(baseline_graduates),
                    "graduates_per_fixed_baseline_plus_one_t_plus_h": format_ratio(
                        graduates, baseline_denominator
                    ),
                    "pre_graduates_growth_since_fixed_baseline": format_float(
                        pre_growth_since_baseline
                    ),
                    "pre_bachelor_graduates_t_minus_1": str(pre_bachelor_1),
                    "pre_bachelor_graduates_t_minus_2": str(pre_bachelor_2),
                    "pre_bachelor_graduates_t_minus_3": str(pre_bachelor_3),
                    "pre_bachelor_graduates_t_minus_4": str(pre_bachelor_4),
                    "pre_bachelor_graduates_t_minus_5": str(pre_bachelor_5),
                    "pre_bachelor_graduates_mean_3": format_float(pre_bachelor_mean_3),
                    "pre_bachelor_graduates_mean_5": format_float(pre_bachelor_mean_5),
                    "pre_bachelor_graduates_trend_3": str(pre_bachelor_1 - pre_bachelor_3),
                    "pre_bachelor_graduates_trend_5": str(pre_bachelor_1 - pre_bachelor_5),
                    "fixed_baseline_bachelor_graduates": str(baseline_bachelor_graduates),
                    "bachelor_graduates_per_fixed_baseline_plus_one_t_plus_h": format_ratio(
                        bachelor_graduates, baseline_bachelor_denominator
                    ),
                    "pre_bachelor_graduates_growth_since_fixed_baseline": format_float(
                        pre_bachelor_growth_since_baseline
                    ),
                    "denominator_available_t": str(int(denominator_available)),
                    "public_grade12_enrollment_t": denominator.get("public_grade12_enrollment", ""),
                    "private_grade12_enrollment_t": denominator.get("private_grade12_enrollment", ""),
                    "total_grade12_enrollment_t": total_denominator,
                    "public_grade12_school_count_t": denominator.get("public_grade12_school_count", ""),
                    "private_grade12_school_count_t": denominator.get("private_grade12_school_count", ""),
                    "private_pss_source_school_year_t": denominator.get(
                        "private_pss_source_school_year", ""
                    ),
                    "private_pss_source_label_t": denominator.get("private_pss_source_label", ""),
                    "graduates_share_t_plus_h": format_share(graduates, total_denominator),
                    "bachelor_graduates_share_t_plus_h": format_share(
                        bachelor_graduates, total_denominator
                    ),
                    "honors_graduates_share_t_plus_h": format_share(honors_graduates, total_denominator),
                    "fixed_hs_denominator_school_year": str(fixed_hs_denominator_year),
                    "fixed_hs_denominator_school_year_label": school_year_label(
                        fixed_hs_denominator_year
                    ),
                    "fixed_hs_grade12_enrollment": fixed_hs_denominator_text,
                    "fixed_hs_denominator_available": str(int(fixed_hs_denominator_available)),
                    "graduates_share_fixed_hs_t_plus_h": format_share(
                        graduates, fixed_hs_denominator_text
                    ),
                    "predicted_graduates_share_fixed_hs_t_plus_h": format_share(
                        predicted_graduates, fixed_hs_denominator_text
                    ),
                    "bachelor_graduates_share_fixed_hs_t_plus_h": format_share(
                        bachelor_graduates, fixed_hs_denominator_text
                    ),
                    "pre_graduates_t_minus_1_share_fixed_hs": format_share(
                        pre_grad_1, fixed_hs_denominator_text
                    ),
                    "pre_graduates_t_minus_2_share_fixed_hs": format_share(
                        pre_grad_2, fixed_hs_denominator_text
                    ),
                    "pre_graduates_t_minus_3_share_fixed_hs": format_share(
                        pre_grad_3, fixed_hs_denominator_text
                    ),
                    "pre_graduates_t_minus_4_share_fixed_hs": format_share(
                        pre_grad_4, fixed_hs_denominator_text
                    ),
                    "pre_graduates_t_minus_5_share_fixed_hs": format_share(
                        pre_grad_5, fixed_hs_denominator_text
                    ),
                    "pre_graduates_trend_3_share_fixed_hs": format_share(
                        pre_grad_1 - pre_grad_3, fixed_hs_denominator_text
                    ),
                    "pre_graduates_trend_5_share_fixed_hs": format_share(
                        pre_grad_1 - pre_grad_5, fixed_hs_denominator_text
                    ),
                }
            )
    return rows


def write_summary(
    path: Path,
    panel_rows: list[dict[str, str]],
    horizon: int,
    start_month: int,
    same_state_fuzzy_threshold: float,
    baseline_grad_year: int,
    fixed_hs_denominator_year: int,
) -> None:
    """Write a compact markdown summary of the regression panel."""
    path.parent.mkdir(parents=True, exist_ok=True)
    total_rows = len(panel_rows)
    sample_rows = sum(row["preferred_regression_sample"] == "1" for row in panel_rows)
    denominator_sample_rows = sum(row.get("denominator_regression_sample") == "1" for row in panel_rows)
    fixed_hs_denominator_sample_rows = sum(
        row.get("fixed_hs_denominator_regression_sample") == "1" for row in panel_rows
    )
    treated_rows = sum(int(row["visits_t"]) > 0 and row["preferred_regression_sample"] == "1" for row in panel_rows)
    review_visits = sum(int(row["review_needed_visits_t"]) for row in panel_rows)
    preferred_visits = sum(int(row["visits_t"]) for row in panel_rows)
    same_state_fuzzy_review_visits = sum(
        int(row["same_state_fuzzy_review_visits_t"]) for row in panel_rows
    )
    broad_visits = sum(int(row["visits_t_with_same_state_fuzzy"]) for row in panel_rows)
    school_years = sorted({row["recruiting_school_year_label"] for row in panel_rows})
    observed_outcome_years = sorted(
        {row["outcome_grad_year"] for row in panel_rows if row["outcome_grad_year_observed"] == "1"}
    )

    lines = [
        "# UA visits-to-graduates panel",
        "",
        f"- Horizon: t+{horizon}",
        f"- School-year start month: {start_month}",
        f"- Fixed baseline graduation year for normalized outcomes: {baseline_grad_year}",
        f"- Fixed high-school denominator year for share outcomes: {school_year_label(fixed_hs_denominator_year)}",
        f"- Same-state fuzzy town sensitivity threshold: {same_state_fuzzy_threshold:.2f}",
        f"- Panel rows: {total_rows}",
        f"- Preferred regression-sample rows: {sample_rows}",
        f"- Denominator regression-sample rows: {denominator_sample_rows}",
        f"- Fixed high-school denominator regression-sample rows: {fixed_hs_denominator_sample_rows}",
        f"- Preferred sample rows with at least one linked visit: {treated_rows}",
        f"- Linked visits used in `visits_t`: {preferred_visits}",
        f"- Additional same-state fuzzy review visits in sensitivity treatment: {same_state_fuzzy_review_visits}",
        f"- Visits used in `visits_t_with_same_state_fuzzy`: {broad_visits}",
        f"- Review-needed or unmatched visits kept out of `visits_t`: {review_visits}",
        f"- Recruiting school years: {', '.join(school_years)}",
        f"- Observed outcome graduation years: {', '.join(observed_outcome_years)}",
        "",
        "## Regression Use",
        "",
        "Use `preferred_regression_sample == 1` for the baseline town-by-school-year panel.",
        "`visits_t` is the preferred treatment count, and `graduates_t_plus_h` is the main outcome.",
        "Use `denominator_regression_sample == 1` for share, weighted-share, and exposure models.",
        "`visits_t_with_same_state_fuzzy` adds review-needed fuzzy town matches that have exact state matches and clear town similarity above the threshold.",
        "Include town and school-year fixed effects in the baseline specification.",
        "",
        "Rows with `review_needed_visits_t > 0` identify towns where some visit rows need manual town-linkage review.",
    ]
    path.write_text("\n".join(lines) + "\n")


#=====================================================================
# 4 - Command-line workflow
#=====================================================================

def parse_args() -> argparse.Namespace:
    """Parse command-line arguments."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--recruiting-path", type=Path, default=DEFAULT_RECRUITING_PATH)
    parser.add_argument("--student-path", type=Path, default=DEFAULT_STUDENT_PATH)
    parser.add_argument("--match-path", type=Path, default=DEFAULT_MATCH_PATH)
    parser.add_argument("--denominator-path", type=Path, default=DEFAULT_DENOMINATOR_PATH)
    parser.add_argument("--output", type=Path, default=DEFAULT_PANEL_PATH)
    parser.add_argument("--summary", type=Path, default=DEFAULT_SUMMARY_PATH)
    parser.add_argument("--horizon", type=int, default=5)
    parser.add_argument("--school-year-start-month", type=int, default=8)
    parser.add_argument("--baseline-grad-year", type=int, default=2015)
    parser.add_argument("--fixed-hs-denominator-year", type=int, default=2017)
    parser.add_argument("--same-state-fuzzy-threshold", type=float, default=0.90)
    parser.add_argument(
        "--refresh-matches",
        action="store_true",
        help="Regenerate town-matching outputs before building the panel.",
    )
    return parser.parse_args()


def main() -> None:
    """CLI entry point."""
    args = parse_args()
    if args.refresh_matches or not args.match_path.exists():
        run_linkage(
            recruiting_path=args.recruiting_path,
            student_path=args.student_path,
            output_dir=args.match_path.parent,
            legacy_names=False,
        )
    panel_rows = build_visit_graduate_panel(
        recruiting_rows=read_csv_rows(args.recruiting_path),
        student_rows=read_csv_rows(args.student_path),
        match_rows=read_csv_rows(args.match_path),
        denominator_rows=read_csv_rows(args.denominator_path) if args.denominator_path.exists() else None,
        horizon=args.horizon,
        start_month=args.school_year_start_month,
        same_state_fuzzy_threshold=args.same_state_fuzzy_threshold,
        baseline_grad_year=args.baseline_grad_year,
        fixed_hs_denominator_year=args.fixed_hs_denominator_year,
    )
    write_csv(args.output, panel_rows)
    write_summary(
        args.summary,
        panel_rows,
        args.horizon,
        args.school_year_start_month,
        args.same_state_fuzzy_threshold,
        args.baseline_grad_year,
        args.fixed_hs_denominator_year,
    )
    print(f"Wrote {len(panel_rows)} panel rows to {args.output}")
    print(f"Wrote summary to {args.summary}")


if __name__ == "__main__":
    main()
