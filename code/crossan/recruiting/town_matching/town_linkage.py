#!/usr/bin/env python3
#=====================================================================
## Author: Crossan Cooper
## Last Modified: 06-08-2026
#
## file use: Link UA admissions recruiting locations to UA student-origin
## towns and build audit crosswalks plus a town-year aggregate panel.
#
## inputs:
## 1. data/full_ua_recruiting_scrape.csv -- admissions visit records
## 2. data/all_alabama_data.csv -- UA commencement student-origin records
#
## outputs:
## 1. town_matching/output/ua_recruiting_town_processing_crosswalk.csv -- recruiting town normalization audit
## 2. town_matching/output/ua_student_town_processing_crosswalk.csv -- student-origin town normalization audit
## 3. town_matching/output/ua_recruiting_to_student_town_matches.csv -- recruiting-to-student town links
## 4. town_matching/output/ua_student_to_recruiting_town_matches.csv -- student-to-recruiting town links
## 5. town_matching/output/ua_recruiting_student_town_year_panel.csv -- linked town-year aggregate panel
## 6. town_matching/output/ua_town_matching_summary.md -- linkage summary
#=====================================================================
"""Link UA high-school recruiting locations to student-origin towns.

The matching unit is town/state, not individual students. The script produces
bidirectional town crosswalks, review files for ambiguous matches, and a
town-year panel that places recruiting visit counts beside student-origin
record counts.
"""

from __future__ import annotations

import argparse
import csv
import os
import difflib
import re
import unicodedata
from collections import Counter, defaultdict
from dataclasses import dataclass
from pathlib import Path


#=====================================================================
# 1 - Paths and constants
#=====================================================================

CODE_ROOT = Path(__file__).resolve().parents[1]
DEFAULT_DATA_ROOT = Path(os.environ.get(
    "ADMISSIONS_PROJECT_DATA_ROOT",
    "/Users/crossancooper/Dropbox/Professional/active-projects/admissions_project",
)).expanduser()
DEFAULT_OUTPUT_DIR = Path(os.environ.get(
    "ADMISSIONS_PROJECT_TOWN_MATCHING_OUTPUT_DIR",
    str(DEFAULT_DATA_ROOT / "town_matching" / "output"),
)).expanduser()
DEFAULT_RECRUITING_PATH = DEFAULT_DATA_ROOT / "data" / "full_ua_recruiting_scrape.csv"
DEFAULT_STUDENT_PATH = DEFAULT_DATA_ROOT / "data" / "all_alabama_data.csv"

STATE_NAME_TO_ABBR = {
    "ALABAMA": "AL",
    "ALASKA": "AK",
    "ARIZONA": "AZ",
    "ARKANSAS": "AR",
    "CALIFORNIA": "CA",
    "COLORADO": "CO",
    "CONNECTICUT": "CT",
    "DELAWARE": "DE",
    "DISTRICT OF COLUMBIA": "DC",
    "FLORIDA": "FL",
    "GEORGIA": "GA",
    "HAWAII": "HI",
    "IDAHO": "ID",
    "ILLINOIS": "IL",
    "INDIANA": "IN",
    "IOWA": "IA",
    "KANSAS": "KS",
    "KENTUCKY": "KY",
    "LOUISIANA": "LA",
    "MAINE": "ME",
    "MARYLAND": "MD",
    "MASSACHUSETTS": "MA",
    "MICHIGAN": "MI",
    "MINNESOTA": "MN",
    "MISSISSIPPI": "MS",
    "MISSOURI": "MO",
    "MONTANA": "MT",
    "NEBRASKA": "NE",
    "NEVADA": "NV",
    "NEW HAMPSHIRE": "NH",
    "NEW JERSEY": "NJ",
    "NEW MEXICO": "NM",
    "NEW YORK": "NY",
    "NORTH CAROLINA": "NC",
    "NORTH DAKOTA": "ND",
    "OHIO": "OH",
    "OKLAHOMA": "OK",
    "OREGON": "OR",
    "PENNSYLVANIA": "PA",
    "RHODE ISLAND": "RI",
    "SOUTH CAROLINA": "SC",
    "SOUTH DAKOTA": "SD",
    "TENNESSEE": "TN",
    "TEXAS": "TX",
    "UTAH": "UT",
    "VERMONT": "VT",
    "VIRGINIA": "VA",
    "WASHINGTON": "WA",
    "WEST VIRGINIA": "WV",
    "WISCONSIN": "WI",
    "WYOMING": "WY",
}


#=====================================================================
# 2 - Data structures and town/state normalization helpers
#=====================================================================

@dataclass(frozen=True)
class TownObservation:
    """One source row carrying a town/state and optional year metadata."""

    raw_town: str
    raw_state: str
    year: int | None = None
    honors: bool = False
    bachelor_degree: bool = False


@dataclass(frozen=True)
class LinkageOutputs:
    """Paths written by a linkage run."""

    recruiting_town_crosswalk: Path
    student_town_crosswalk: Path
    recruiting_processed_towns: Path
    student_processed_towns: Path
    recruiting_to_student: Path
    recruiting_to_student_review: Path
    student_to_recruiting: Path
    student_to_recruiting_review: Path
    town_year_panel: Path
    summary: Path


def ascii_text(value: str) -> str:
    """Return an ASCII version of a town or state string for stable matching."""
    return unicodedata.normalize("NFKD", value or "").encode("ascii", "ignore").decode("ascii")


def squash_spaces(value: str) -> str:
    """Collapse repeated whitespace after trimming leading/trailing spaces."""
    return re.sub(r"\s+", " ", value or "").strip()


def collapse_spelled_letters(value: str) -> str:
    """Join OCR-style values like ``A L`` into ``AL``."""
    tokens = value.split(" ")
    if len(tokens) >= 2 and all(len(token) == 1 and token.isalpha() for token in tokens):
        return "".join(tokens)
    return value


def normalize_state(state_value: str) -> str:
    """Normalize state names and abbreviations to two-letter abbreviations."""
    state = collapse_spelled_letters(squash_spaces(ascii_text(state_value))).upper()
    state = re.sub(r"[.,]", "", state)
    return STATE_NAME_TO_ABBR.get(state, state)


def normalize_town(town_value: str) -> str:
    """Normalize towns while preserving enough detail for review files."""
    town = collapse_spelled_letters(squash_spaces(ascii_text(town_value))).upper()
    town = town.replace("&", " AND ")
    town = re.sub(r"\bFT\.?\b", "FORT", town)
    town = re.sub(r"\bMT\.?\b", "MOUNT", town)
    town = re.sub(r"\bST\.?\b", "SAINT", town)
    town = re.sub(r"\bTWP\.?\b", "TOWNSHIP", town)
    town = re.sub(r"[^\w\s]", " ", town)
    town = re.sub(r"_", " ", town)
    return squash_spaces(town)


def compact_town(value: str) -> str:
    """Remove punctuation and spaces for exact compact-key comparisons."""
    return "".join(ch for ch in ascii_text(value) if ch.isalnum()).upper()


def parse_year(value: str) -> int | None:
    """Parse a four-digit year from a string if present."""
    value = squash_spaces(value)
    return int(value) if re.fullmatch(r"\d{4}", value) else None


def clean_student_town(raw_town: str) -> str:
    """Clean OCR and manual-entry artifacts in UA student-origin towns."""
    town = squash_spaces(raw_town)
    town = re.sub(r",.*", "", town)
    town = re.sub(r"(?<=[a-z])(?=[A-Z])", " ", town)

    manual_map = {
        "A LB ER TV IL LE": "Albertville",
        "A LL EN": "Allen",
        "A RL IN GT ON": "Arlington",
        "ABBBEVILLE": "Abbeville",
        "AGOURA": "Agoura Hills",
        "ALEX CITY": "Alexander City",
        "ALEXANDER CITY": "Alexander City",
        "VESTAVIA": "Vestavia Hills",
        "VESTAVIA HILL": "Vestavia Hills",
        "VESTAVIA HILLS": "Vestavia Hills",
    }
    return manual_map.get(squash_spaces(town).upper(), town).strip()


def clean_student_state(raw_state: str) -> str:
    """Clean UA student-origin state values before normalization."""
    return re.sub(r"[.,]", "", squash_spaces(raw_state))


def town_similarity(left: str, right: str) -> float:
    """Score two already-normalized town names."""
    left_ascii = ascii_text(left)
    right_ascii = ascii_text(right)
    left_compact = compact_town(left_ascii)
    right_compact = compact_town(right_ascii)
    processed_ratio = difflib.SequenceMatcher(None, left_ascii, right_ascii).ratio()
    compact_ratio = difflib.SequenceMatcher(None, left_compact, right_compact).ratio()
    containment = 1.0 if left_compact in right_compact or right_compact in left_compact else 0.0
    return max(processed_ratio, compact_ratio, 0.92 * compact_ratio + 0.08 * containment)


def choose_representative_raw(rows: list[dict[str, str]]) -> dict[str, str]:
    """Choose a readable raw town/state variant for a processed town group."""
    return sorted(
        rows,
        key=lambda row: (
            -int(row["source_row_count"]),
            len(row["raw_town"]),
            row["raw_town"],
            row["raw_state"],
        ),
    )[0]


#=====================================================================
# 3 - Crosswalk and matching construction
#=====================================================================

def build_side_records(
    observations: list[TownObservation],
    town_field_name: str,
    state_field_name: str,
    side_label: str,
) -> tuple[list[dict[str, str]], dict[tuple[str, str], dict[str, str]]]:
    """Build raw and canonical town/state records for one side of the match."""
    raw_counts = Counter((obs.raw_town, obs.raw_state) for obs in observations)
    raw_records = []
    grouped_by_processed: dict[tuple[str, str], list[dict[str, str]]] = defaultdict(list)

    for raw_town, raw_state in sorted(raw_counts):
        processed_town = normalize_town(raw_town)
        processed_state = normalize_state(raw_state)
        if not processed_town or not processed_state:
            continue
        record = {
            "side_label": side_label,
            "raw_town": raw_town,
            "raw_state": raw_state,
            "processed_town": processed_town,
            "processed_state": processed_state,
            "compact_town": compact_town(processed_town),
            "source_row_count": str(raw_counts[(raw_town, raw_state)]),
            "town_field_name": town_field_name,
            "state_field_name": state_field_name,
        }
        raw_records.append(record)
        grouped_by_processed[(processed_town, processed_state)].append(record)

    canonical_records = {}
    for processed_key, group_rows in grouped_by_processed.items():
        representative = choose_representative_raw(group_rows)
        canonical_records[processed_key] = {
            "representative_raw_town": representative["raw_town"],
            "representative_raw_state": representative["raw_state"],
            "processed_town": processed_key[0],
            "processed_state": processed_key[1],
            "compact_town": compact_town(processed_key[0]),
            "processed_group_count": str(sum(int(row["source_row_count"]) for row in group_rows)),
            "raw_variant_count": str(len(group_rows)),
        }
    return raw_records, canonical_records


def canonical_town_rows(
    canonical_records: dict[tuple[str, str], dict[str, str]],
    side_label: str,
) -> list[dict[str, str]]:
    """Convert canonical town dictionaries to stable, auditable rows."""
    rows = []
    for record in canonical_records.values():
        rows.append(
            {
                "side_label": side_label,
                "processed_town": record["processed_town"],
                "processed_state": record["processed_state"],
                "compact_town": record["compact_town"],
                "representative_raw_town": record["representative_raw_town"],
                "representative_raw_state": record["representative_raw_state"],
                "processed_group_count": record["processed_group_count"],
                "raw_variant_count": record["raw_variant_count"],
            }
        )
    rows.sort(
        key=lambda row: (
            row["processed_state"],
            row["processed_town"],
            row["representative_raw_town"],
            row["representative_raw_state"],
        )
    )
    return rows


def _empty_match(target_prefix: str) -> dict[str, str]:
    return {
        f"{target_prefix}_matched_raw_town": "",
        f"{target_prefix}_matched_raw_state": "",
        f"{target_prefix}_matched_processed_town": "",
        f"{target_prefix}_matched_processed_state": "",
        f"{target_prefix}_matched_group_row_count": "",
        f"{target_prefix}_matched_raw_variant_count": "",
        f"{target_prefix}_state_candidate_count": "0",
        "match_score": "",
        "match_type": "",
        "proper_match_indicator": "0",
    }


def _filled_match(base: dict[str, str], candidate: dict[str, str], target_prefix: str) -> dict[str, str]:
    return {
        **base,
        f"{target_prefix}_matched_raw_town": candidate["representative_raw_town"],
        f"{target_prefix}_matched_raw_state": candidate["representative_raw_state"],
        f"{target_prefix}_matched_processed_town": candidate["processed_town"],
        f"{target_prefix}_matched_processed_state": candidate["processed_state"],
        f"{target_prefix}_matched_group_row_count": candidate["processed_group_count"],
        f"{target_prefix}_matched_raw_variant_count": candidate["raw_variant_count"],
    }


def match_one_record(
    source_record: dict[str, str],
    target_by_processed: dict[tuple[str, str], dict[str, str]],
    target_by_compact: dict[tuple[str, str], list[dict[str, str]]],
    target_by_state: dict[str, list[dict[str, str]]],
    target_prefix: str,
) -> dict[str, str]:
    """Match one source town/state to a target town/state dictionary."""
    processed_key = (source_record["processed_town"], source_record["processed_state"])
    compact_key = (source_record["compact_town"], source_record["processed_state"])
    base = _empty_match(target_prefix)

    state_candidates = target_by_state.get(source_record["processed_state"], [])
    base[f"{target_prefix}_state_candidate_count"] = str(len(state_candidates))
    if not state_candidates:
        return {**base, "match_type": "no_same_state_candidate"}

    exact_processed = target_by_processed.get(processed_key)
    if exact_processed is not None:
        return {
            **_filled_match(base, exact_processed, target_prefix),
            "match_score": "1.000",
            "match_type": "exact_processed",
            "proper_match_indicator": "1",
        }

    compact_matches = target_by_compact.get(compact_key, [])
    if len(compact_matches) == 1:
        return {
            **_filled_match(base, compact_matches[0], target_prefix),
            "match_score": "1.000",
            "match_type": "exact_compact",
            "proper_match_indicator": "1",
        }

    scored_candidates = [
        (town_similarity(source_record["processed_town"], candidate["processed_town"]), candidate)
        for candidate in state_candidates
    ]
    scored_candidates.sort(
        key=lambda item: (
            item[0],
            int(item[1]["processed_group_count"]),
            item[1]["processed_town"],
            item[1]["representative_raw_town"],
        ),
        reverse=True,
    )

    best_score, best_candidate = scored_candidates[0]
    best_output = {
        **_filled_match(base, best_candidate, target_prefix),
        "match_score": f"{best_score:.3f}",
    }

    if len(scored_candidates) == 1:
        if best_score >= 0.94:
            return {**best_output, "match_type": "fuzzy_single_high_confidence", "proper_match_indicator": "1"}
        return {**best_output, "match_type": "fuzzy_single_review"}

    second_score, _ = scored_candidates[1]
    if best_score >= 0.95 and (best_score - second_score) >= 0.04:
        return {**best_output, "match_type": "fuzzy_clear_best", "proper_match_indicator": "1"}
    return {**best_output, "match_type": "fuzzy_review"}


def build_match_rows(
    source_raw_records: list[dict[str, str]],
    target_canonical_records: dict[tuple[str, str], dict[str, str]],
    target_prefix: str,
) -> list[dict[str, str]]:
    """Build match rows from source raw towns to target canonical towns."""
    target_by_processed = dict(target_canonical_records)
    target_by_compact: dict[tuple[str, str], list[dict[str, str]]] = defaultdict(list)
    target_by_state: dict[str, list[dict[str, str]]] = defaultdict(list)

    for candidate in target_canonical_records.values():
        target_by_compact[(candidate["compact_town"], candidate["processed_state"])].append(candidate)
        target_by_state[candidate["processed_state"]].append(candidate)

    rows = []
    for source_record in source_raw_records:
        match = match_one_record(
            source_record=source_record,
            target_by_processed=target_by_processed,
            target_by_compact=target_by_compact,
            target_by_state=target_by_state,
            target_prefix=target_prefix,
        )
        rows.append({**source_record, **match})
    return rows


def build_review_rows(rows: list[dict[str, str]]) -> list[dict[str, str]]:
    """Return non-automatic matches ordered for manual review."""
    priority = {
        "fuzzy_single_review": 1,
        "fuzzy_review": 1,
        "no_same_state_candidate": 3,
    }
    review_rows = [row for row in rows if row["proper_match_indicator"] == "0"]
    review_rows.sort(
        key=lambda row: (
            priority.get(row["match_type"], 99),
            -float(row["match_score"] or -1.0),
            int(row[next(key for key in row if key.endswith("_state_candidate_count"))]),
            row["processed_state"],
            row["processed_town"],
        )
    )
    return review_rows


def load_recruiting_observations(path: Path) -> list[TownObservation]:
    """Load town/year observations from the high-school recruiting scrape."""
    observations = []
    with path.open(newline="", encoding="utf-8") as handle:
        reader = csv.DictReader(handle)
        for row in reader:
            town = squash_spaces(row.get("city", ""))
            state = squash_spaces(row.get("state", "")) or squash_spaces(row.get("state_abbrev", ""))
            if town and state:
                observations.append(
                    TownObservation(
                        raw_town=town,
                        raw_state=state,
                        year=parse_year(row.get("extracted_year", "")),
                    )
                )
    return observations


def load_student_observations(path: Path) -> list[TownObservation]:
    """Load town/year observations from UA student-origin records."""
    observations = []
    with path.open(newline="", encoding="utf-8") as handle:
        reader = csv.DictReader(handle)
        for row in reader:
            raw_town = row.get("Origin Town", row.get("OriginTown", ""))
            raw_state = row.get("Origin State", row.get("OriginState", ""))
            town = clean_student_town(raw_town)
            state = clean_student_state(raw_state)
            if not town or not state:
                continue
            degree = ascii_text(row.get("Degree", "")).lower()
            honors_value = squash_spaces(row.get("Honors", "")).lower()
            observations.append(
                TownObservation(
                    raw_town=town,
                    raw_state=state,
                    year=parse_year(row.get("Year", "")),
                    honors=honors_value in {"yes", "y", "true", "1"},
                    bachelor_degree="bachelor" in degree,
                )
            )
    return observations


def panel_town_key(processed_town: str, processed_state: str) -> str:
    """Create a stable town identifier for panel outputs."""
    return f"{processed_state}_{compact_town(processed_town)}"


def build_recruiting_to_student_key_map(
    recruiting_to_student_rows: list[dict[str, str]],
) -> dict[tuple[str, str], tuple[str, str, str, str]]:
    """Map recruiting processed towns to panel town keys.

    The tuple value is ``(panel_town, panel_state, match_type, proper_flag)``.
    Automatic matches inherit the matched student town; review-needed rows keep
    the recruiting town so they remain visible in the panel.
    """
    key_map = {}
    for row in recruiting_to_student_rows:
        source_key = (row["processed_town"], row["processed_state"])
        if row["proper_match_indicator"] == "1":
            key_map[source_key] = (
                row["student_matched_processed_town"],
                row["student_matched_processed_state"],
                row["match_type"],
                "1",
            )
        else:
            key_map[source_key] = (
                row["processed_town"],
                row["processed_state"],
                row["match_type"],
                "0",
            )
    return key_map


def build_town_year_panel(
    recruiting_observations: list[TownObservation],
    student_observations: list[TownObservation],
    recruiting_to_student_rows: list[dict[str, str]],
) -> list[dict[str, str]]:
    """Aggregate recruiting visits and student records to town/year cells."""
    key_map = build_recruiting_to_student_key_map(recruiting_to_student_rows)
    recruiting_counts: Counter[tuple[str, str, int | None]] = Counter()
    linked_recruiting_counts: Counter[tuple[str, str, int | None]] = Counter()
    match_types_by_town: dict[tuple[str, str], set[str]] = defaultdict(set)
    student_counts: Counter[tuple[str, str, int | None]] = Counter()
    student_honors_counts: Counter[tuple[str, str, int | None]] = Counter()
    student_bachelor_counts: Counter[tuple[str, str, int | None]] = Counter()

    for obs in recruiting_observations:
        source_key = (normalize_town(obs.raw_town), normalize_state(obs.raw_state))
        panel_town, panel_state, match_type, proper = key_map.get(
            source_key,
            (source_key[0], source_key[1], "not_in_match_file", "0"),
        )
        panel_key = (panel_town, panel_state, obs.year)
        recruiting_counts[panel_key] += 1
        if proper == "1":
            linked_recruiting_counts[panel_key] += 1
        match_types_by_town[(panel_town, panel_state)].add(match_type)

    for obs in student_observations:
        panel_town = normalize_town(obs.raw_town)
        panel_state = normalize_state(obs.raw_state)
        panel_key = (panel_town, panel_state, obs.year)
        student_counts[panel_key] += 1
        student_honors_counts[panel_key] += int(obs.honors)
        student_bachelor_counts[panel_key] += int(obs.bachelor_degree)

    all_keys = set(recruiting_counts) | set(student_counts)
    rows = []
    for town, state, year in sorted(all_keys, key=lambda key: (key[1], key[0], key[2] or 0)):
        town_key = (town, state)
        rows.append(
            {
                "town_state_id": panel_town_key(town, state),
                "processed_town": town,
                "processed_state": state,
                "year": "" if year is None else str(year),
                "recruiting_visit_count": str(recruiting_counts[(town, state, year)]),
                "linked_recruiting_visit_count": str(linked_recruiting_counts[(town, state, year)]),
                "student_record_count": str(student_counts[(town, state, year)]),
                "student_bachelor_record_count": str(student_bachelor_counts[(town, state, year)]),
                "student_honors_record_count": str(student_honors_counts[(town, state, year)]),
                "recruiting_match_types": " | ".join(sorted(match_types_by_town.get(town_key, set()))),
            }
        )
    return rows


def write_csv(path: Path, rows: list[dict[str, str]]) -> None:
    """Write rows to CSV, creating parent directories as needed."""
    path.parent.mkdir(parents=True, exist_ok=True)
    if not rows:
        path.write_text("")
        return
    with path.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=list(rows[0].keys()))
        writer.writeheader()
        writer.writerows(rows)


def match_type_counts(rows: list[dict[str, str]]) -> Counter[str]:
    """Count match rows by match type."""
    return Counter(row["match_type"] for row in rows)


def write_summary(
    output_path: Path,
    recruiting_to_student: list[dict[str, str]],
    student_to_recruiting: list[dict[str, str]],
    panel_rows: list[dict[str, str]],
) -> None:
    """Write a markdown summary for coauthor review."""
    output_path.parent.mkdir(parents=True, exist_ok=True)
    recruiting_matched = sum(row["proper_match_indicator"] == "1" for row in recruiting_to_student)
    student_matched = sum(row["proper_match_indicator"] == "1" for row in student_to_recruiting)
    lines = [
        "# UA town matching summary",
        "",
        f"- Recruiting town variants: {len(recruiting_to_student)}",
        f"- Recruiting variants automatically linked to a student-origin town: {recruiting_matched}",
        f"- Student-origin town variants: {len(student_to_recruiting)}",
        f"- Student-origin variants automatically linked to a recruiting town: {student_matched}",
        f"- Town-year panel rows: {len(panel_rows)}",
        "",
        "## Recruiting-to-student match types",
        "",
    ]
    for match_type, count in sorted(match_type_counts(recruiting_to_student).items()):
        lines.append(f"- {match_type}: {count}")
    lines.extend(["", "## Student-to-recruiting match types", ""])
    for match_type, count in sorted(match_type_counts(student_to_recruiting).items()):
        lines.append(f"- {match_type}: {count}")
    lines.extend(
        [
            "",
            "## Notes",
            "",
            "- Review-needed files contain fuzzy or same-state-missing rows; these should be hand-checked before using ambiguous towns in preferred specifications.",
            "- `student_record_count` is based on commencement/student-origin rows in `data/all_alabama_data.csv`; it is a town-level enrollment-origin proxy, not an individual high-school-to-student match.",
        ]
    )
    output_path.write_text("\n".join(lines) + "\n")


def output_paths(output_dir: Path, legacy_names: bool = False) -> LinkageOutputs:
    """Return the output paths for a town-linkage run."""
    if legacy_names:
        return LinkageOutputs(
            recruiting_town_crosswalk=output_dir / "ua_recruiting_town_processing_crosswalk.csv",
            student_town_crosswalk=output_dir / "ua_graduate_town_processing_crosswalk.csv",
            recruiting_processed_towns=output_dir / "ua_recruiting_processed_town_universe.csv",
            student_processed_towns=output_dir / "ua_graduate_processed_town_universe.csv",
            recruiting_to_student=output_dir / "ua_recruiting_to_graduate_town_matches.csv",
            recruiting_to_student_review=output_dir / "ua_recruiting_to_graduate_town_matches_review_needed.csv",
            student_to_recruiting=output_dir / "ua_graduate_to_recruiting_town_matches.csv",
            student_to_recruiting_review=output_dir / "ua_graduate_to_recruiting_town_matches_review_needed.csv",
            town_year_panel=output_dir / "ua_recruiting_student_town_year_panel.csv",
            summary=output_dir / "ua_town_matching_summary.md",
        )
    return LinkageOutputs(
        recruiting_town_crosswalk=output_dir / "ua_recruiting_town_processing_crosswalk.csv",
        student_town_crosswalk=output_dir / "ua_student_town_processing_crosswalk.csv",
        recruiting_processed_towns=output_dir / "ua_recruiting_processed_town_universe.csv",
        student_processed_towns=output_dir / "ua_student_processed_town_universe.csv",
        recruiting_to_student=output_dir / "ua_recruiting_to_student_town_matches.csv",
        recruiting_to_student_review=output_dir / "ua_recruiting_to_student_town_matches_review_needed.csv",
        student_to_recruiting=output_dir / "ua_student_to_recruiting_town_matches.csv",
        student_to_recruiting_review=output_dir / "ua_student_to_recruiting_town_matches_review_needed.csv",
        town_year_panel=output_dir / "ua_recruiting_student_town_year_panel.csv",
        summary=output_dir / "ua_town_matching_summary.md",
    )


def run_linkage(
    recruiting_path: Path = DEFAULT_RECRUITING_PATH,
    student_path: Path = DEFAULT_STUDENT_PATH,
    output_dir: Path = DEFAULT_OUTPUT_DIR,
    legacy_names: bool = False,
) -> LinkageOutputs:
    """Run the full town-linkage workflow and write outputs."""
    recruiting_observations = load_recruiting_observations(recruiting_path)
    student_observations = load_student_observations(student_path)

    recruiting_raw, recruiting_canonical = build_side_records(
        observations=recruiting_observations,
        town_field_name="city",
        state_field_name="state",
        side_label="recruiting",
    )
    student_raw, student_canonical = build_side_records(
        observations=student_observations,
        town_field_name="Origin Town",
        state_field_name="Origin State",
        side_label="student",
    )

    recruiting_to_student = build_match_rows(
        source_raw_records=recruiting_raw,
        target_canonical_records=student_canonical,
        target_prefix="student",
    )
    student_to_recruiting = build_match_rows(
        source_raw_records=student_raw,
        target_canonical_records=recruiting_canonical,
        target_prefix="recruiting",
    )
    panel_rows = build_town_year_panel(
        recruiting_observations=recruiting_observations,
        student_observations=student_observations,
        recruiting_to_student_rows=recruiting_to_student,
    )

    paths = output_paths(output_dir, legacy_names=legacy_names)
    write_csv(paths.recruiting_town_crosswalk, recruiting_raw)
    write_csv(paths.student_town_crosswalk, student_raw)
    write_csv(paths.recruiting_processed_towns, canonical_town_rows(recruiting_canonical, "recruiting"))
    write_csv(paths.student_processed_towns, canonical_town_rows(student_canonical, "student"))
    write_csv(paths.recruiting_to_student, recruiting_to_student)
    write_csv(paths.recruiting_to_student_review, build_review_rows(recruiting_to_student))
    write_csv(paths.student_to_recruiting, student_to_recruiting)
    write_csv(paths.student_to_recruiting_review, build_review_rows(student_to_recruiting))
    write_csv(paths.town_year_panel, panel_rows)
    write_summary(paths.summary, recruiting_to_student, student_to_recruiting, panel_rows)
    return paths


#=====================================================================
# 4 - Command-line workflow
#=====================================================================

def parse_args() -> argparse.Namespace:
    """Parse command-line arguments."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--recruiting-path", type=Path, default=DEFAULT_RECRUITING_PATH)
    parser.add_argument("--student-path", type=Path, default=DEFAULT_STUDENT_PATH)
    parser.add_argument("--output-dir", type=Path, default=DEFAULT_OUTPUT_DIR)
    parser.add_argument(
        "--legacy-names",
        action="store_true",
        help="Write legacy graduate-named files for backward compatibility.",
    )
    return parser.parse_args()


def main() -> None:
    """CLI entry point."""
    args = parse_args()
    paths = run_linkage(
        recruiting_path=args.recruiting_path,
        student_path=args.student_path,
        output_dir=args.output_dir,
        legacy_names=args.legacy_names,
    )
    print(f"Wrote recruiting town processing crosswalk to {paths.recruiting_town_crosswalk}")
    print(f"Wrote student town processing crosswalk to {paths.student_town_crosswalk}")
    print(f"Wrote recruiting processed town universe to {paths.recruiting_processed_towns}")
    print(f"Wrote student processed town universe to {paths.student_processed_towns}")
    print(f"Wrote recruiting-to-student matches to {paths.recruiting_to_student}")
    print(f"Wrote student-to-recruiting matches to {paths.student_to_recruiting}")
    print(f"Wrote town-year panel to {paths.town_year_panel}")
    print(f"Wrote summary to {paths.summary}")


if __name__ == "__main__":
    main()
