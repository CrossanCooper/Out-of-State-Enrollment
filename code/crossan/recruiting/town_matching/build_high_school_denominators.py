#!/usr/bin/env python3
#=====================================================================
## Author: Crossan Cooper
## Last Modified: 06-08-2026
#
## file use: Build town-level fixed high-school grade-12 enrollment
## denominators from online NCES public and private school sources.
#
## inputs:
## 1. NCES EDGE Public School Characteristics CSVs -- public grade-12 counts
## 2. NCES Private School Survey public-use CSV ZIPs -- private grade-12 counts
## 3. town_matching/output/ua_student_processed_town_universe.csv -- UA panel town keys
## 4. town_matching/output/ua_recruiting_processed_town_universe.csv -- visited town keys
#
## outputs:
## 1. town_matching/output/ua_high_school_denominators_online.csv -- source town/year denominators
## 2. town_matching/output/ua_high_school_denominators_online_panel_matched.csv -- panel-keyed denominators
## 3. town_matching/output/ua_high_school_denominators_online_match_review.csv -- denominator match audit
## 4. town_matching/output/ua_high_school_denominators_online_summary.md -- denominator summary
#=====================================================================
"""Build town-level high school enrollment denominators from online NCES data.

The output is keyed to the same normalized town/state identifiers used by the
UA recruiting panel.  Public school grade-12 enrollment is pulled from NCES
EDGE public-school characteristics files.  Private school grade-12 enrollment
is pulled from NCES Private School Survey public-use files and carried forward
from the latest available PSS collection at or before the target school year.
"""

from __future__ import annotations

import argparse
import csv
import io
import json
import os
import tempfile
import urllib.parse
import urllib.request
import zipfile
from collections import Counter, defaultdict
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable

from town_linkage import (
    DEFAULT_OUTPUT_DIR,
    STATE_NAME_TO_ABBR,
    clean_student_town,
    compact_town,
    normalize_state,
    normalize_town,
    town_similarity,
)
from build_visit_graduate_panel import school_year_label


#=====================================================================
# 1 - Paths, source URLs, and calibrated matching settings
#=====================================================================

CODE_ROOT = Path(__file__).resolve().parents[1]
DEFAULT_DATA_ROOT = Path(os.environ.get(
    "ADMISSIONS_PROJECT_DATA_ROOT",
    "/Users/crossancooper/Dropbox/Professional/active-projects/admissions_project",
)).expanduser()
DEFAULT_RAW_DIR = Path(os.environ.get(
    "ADMISSIONS_PROJECT_TOWN_MATCHING_RAW_DIR",
    str(DEFAULT_DATA_ROOT / "town_matching" / "data" / "raw_online"),
)).expanduser()
DEFAULT_OUTPUT = DEFAULT_OUTPUT_DIR / "ua_high_school_denominators_online.csv"
DEFAULT_PANEL_MATCHED_OUTPUT = DEFAULT_OUTPUT_DIR / "ua_high_school_denominators_online_panel_matched.csv"
DEFAULT_MATCH_REVIEW = DEFAULT_OUTPUT_DIR / "ua_high_school_denominators_online_match_review.csv"
DEFAULT_SUMMARY = DEFAULT_OUTPUT_DIR / "ua_high_school_denominators_online_summary.md"
DEFAULT_TOWN_UNIVERSE_PATHS = (
    DEFAULT_OUTPUT_DIR / "ua_student_processed_town_universe.csv",
    DEFAULT_OUTPUT_DIR / "ua_recruiting_processed_town_universe.csv",
)

ARCGIS_SEARCH_URL = "https://www.arcgis.com/sharing/rest/search"
ARCGIS_DOWNLOAD_URL = "https://data-nces.opendata.arcgis.com/api/download/v1/items/{item_id}/csv?layers=0"

PUBLIC_CHARACTERISTICS_TITLES = {
    2017: "Public School Characteristics 2017-18",
    2018: "Public School Characteristics 2018-19",
    2019: "Public School Characteristics 2019-20",
    2020: "Public School Characteristics 2020-21",
}

PSS_CSV_URLS = {
    2015: "https://nces.ed.gov/surveys/pss/zip/pss1516_pu_csv.zip",
    2017: "https://nces.ed.gov/surveys/pss/zip/pss1718_pu_csv.zip",
    2019: "https://nces.ed.gov/surveys/pss/zip/pss1920_pu_csv.zip",
    2021: "https://nces.ed.gov/surveys/pss/zip/pss2122_pu_csv.zip",
}

DEFAULT_PUBLIC_SCHOOL_YEARS = (2017, 2018)
DEFAULT_SCHOOL_YEARS = (2016, 2017, 2018)
MISSING_NUMERIC_CODES = {-1, -2, -9}
DIRECTION_TOKENS = {"EAST", "WEST", "NORTH", "SOUTH"}
STATE_ABBR_TO_NAME = {abbr: name for name, abbr in STATE_NAME_TO_ABBR.items()}
TOWN_ABBREVIATION_ALIASES = {
    "ARLNGTN": "ARLINGTON",
    "BCH": "BEACH",
    "BLFS": "BLUFFS",
    "BRK": "BROOK",
    "FLS": "FALLS",
    "GRV": "GROVE",
    "GRVS": "GROVES",
    "HLS": "HILLS",
    "PK": "PARK",
    "PRT": "PORT",
    "PT": "PORT",
    "WASHINGTN": "WASHINGTON",
}
MANUAL_TOWN_ALIASES = {
    "HGHLNDS RANCH": "HIGHLANDS RANCH",
    "KNG OF PRUSSA": "KING OF PRUSSIA",
}


#=====================================================================
# 2 - Data structures and generic helpers
#=====================================================================

@dataclass
class EnrollmentAggregate:
    """Town-level enrollment and school counts for one data source/year."""

    grade12_enrollment: int = 0
    grade12_school_count: int = 0
    skipped_missing_grade12: int = 0


def parse_nonnegative_int(value: str | int | float | None) -> int | None:
    """Parse NCES counts, treating negative NCES sentinel codes as missing."""
    if value is None:
        return None
    value_text = str(value).strip()
    if not value_text:
        return None
    try:
        number = int(float(value_text.replace(",", "")))
    except ValueError:
        return None
    if number in MISSING_NUMERIC_CODES or number < 0:
        return None
    return number


def first_present(row: dict[str, str], field_names: Iterable[str]) -> str:
    """Return the first nonblank value among candidate column names."""
    for field_name in field_names:
        value = (row.get(field_name) or "").strip()
        if value:
            return value
    return ""


def nces_urlretrieve(url: str, output_path: Path, timeout: int, force: bool = False) -> None:
    """Download a remote NCES/ArcGIS file into the raw-online cache."""
    if output_path.exists() and not force:
        return
    output_path.parent.mkdir(parents=True, exist_ok=True)
    request = urllib.request.Request(url, headers={"User-Agent": "Mozilla/5.0"})
    with urllib.request.urlopen(request, timeout=timeout) as response:
        with tempfile.NamedTemporaryFile(delete=False, dir=str(output_path.parent)) as temp_handle:
            temp_path = Path(temp_handle.name)
            while True:
                chunk = response.read(1024 * 1024)
                if not chunk:
                    break
                temp_handle.write(chunk)
    temp_path.replace(output_path)


def read_json_url(url: str, timeout: int) -> dict:
    """Fetch a JSON endpoint with a browser-like user agent."""
    request = urllib.request.Request(url, headers={"User-Agent": "Mozilla/5.0"})
    with urllib.request.urlopen(request, timeout=timeout) as response:
        return json.loads(response.read().decode("utf-8"))


def find_public_characteristics_item(title: str, raw_dir: Path, timeout: int) -> dict:
    """Resolve NCES EDGE public-school characteristics ArcGIS metadata."""
    query = {
        "q": f'title:"{title}" owner:OpenDataMgr_NCES',
        "f": "json",
        "num": "10",
    }
    search_url = f"{ARCGIS_SEARCH_URL}?{urllib.parse.urlencode(query)}"
    payload = read_json_url(search_url, timeout=timeout)
    raw_dir.mkdir(parents=True, exist_ok=True)
    (raw_dir / f"arcgis_search_{title.lower().replace(' ', '_')}.json").write_text(
        json.dumps(payload, indent=2),
        encoding="utf-8",
    )
    matches = [item for item in payload.get("results", []) if item.get("title") == title]
    if not matches:
        raise RuntimeError(f"No ArcGIS item found for {title!r}")
    return matches[0]


def find_public_characteristics_item_id(title: str, raw_dir: Path, timeout: int) -> str:
    """Resolve an NCES EDGE public-school characteristics ArcGIS item id."""
    return find_public_characteristics_item(title, raw_dir=raw_dir, timeout=timeout)["id"]


def download_public_characteristics_query_csv(
    service_layer_url: str,
    output_path: Path,
    timeout: int,
    page_size: int = 2000,
) -> None:
    """Download selected public-school fields from an ArcGIS query endpoint.

    ArcGIS Hub CSV downloads occasionally return transient HTTP 500 errors for
    large layers.  The feature-service query endpoint is slower but lets us
    page through only the city, state, and grade-12 fields needed here.
    """
    if not service_layer_url:
        raise RuntimeError("No service URL available for ArcGIS query fallback")

    fieldnames = ["LCITY", "STABR", "G12"]
    output_path.parent.mkdir(parents=True, exist_ok=True)
    temp_path: Path | None = None
    try:
        with tempfile.NamedTemporaryFile(
            "w",
            delete=False,
            dir=str(output_path.parent),
            encoding="utf-8",
            newline="",
        ) as temp_handle:
            temp_path = Path(temp_handle.name)
            writer = csv.DictWriter(temp_handle, fieldnames=fieldnames)
            writer.writeheader()
            offset = 0
            while True:
                query = {
                    "where": "1=1",
                    "outFields": ",".join(fieldnames),
                    "returnGeometry": "false",
                    "f": "json",
                    "resultOffset": str(offset),
                    "resultRecordCount": str(page_size),
                    "orderByFields": "OBJECTID",
                }
                query_url = f"{service_layer_url}/query?{urllib.parse.urlencode(query)}"
                payload = read_json_url(query_url, timeout=timeout)
                if "error" in payload:
                    raise RuntimeError(f"ArcGIS query failed: {payload['error']}")

                features = payload.get("features", [])
                if not features:
                    break

                for feature in features:
                    attributes = feature.get("attributes", {})
                    writer.writerow({field: attributes.get(field, "") for field in fieldnames})

                offset += len(features)
                if len(features) < page_size:
                    break
    except Exception:
        if temp_path is not None and temp_path.exists():
            temp_path.unlink()
        raise
    temp_path.replace(output_path)


def download_public_characteristics_csv(
    school_year: int,
    raw_dir: Path,
    timeout: int,
    force: bool = False,
) -> Path:
    """Download an NCES EDGE public-school characteristics CSV for one year."""
    csv_path = raw_dir / f"public_school_characteristics_{school_year}_{school_year + 1}.csv"
    if csv_path.exists() and not force:
        return csv_path
    title = PUBLIC_CHARACTERISTICS_TITLES[school_year]
    item = find_public_characteristics_item(title, raw_dir=raw_dir, timeout=timeout)
    if not csv_path.exists() or force:
        try:
            nces_urlretrieve(
                ARCGIS_DOWNLOAD_URL.format(item_id=item["id"]),
                csv_path,
                timeout=timeout,
                force=force,
            )
        except Exception:
            download_public_characteristics_query_csv(
                item.get("url", ""),
                csv_path,
                timeout=timeout,
            )
    return csv_path


#=====================================================================
# 3 - NCES aggregation and panel-town matching
#=====================================================================

def aggregate_public_characteristics_rows(
    rows: Iterable[dict[str, str]],
    school_year: int,
) -> dict[tuple[str, str, int], EnrollmentAggregate]:
    """Aggregate public school grade-12 enrollment by normalized town/state."""
    aggregates: dict[tuple[str, str, int], EnrollmentAggregate] = defaultdict(EnrollmentAggregate)
    for row in rows:
        town = normalize_town(first_present(row, ("LCITY", "CITY")))
        state = normalize_state(first_present(row, ("STABR", "LSTATE", "STATE")))
        if not town or not state:
            continue
        key = (town, state, school_year)
        grade12 = parse_nonnegative_int(row.get("G12"))
        if grade12 is None:
            aggregates[key].skipped_missing_grade12 += 1
            continue
        if grade12 > 0:
            aggregates[key].grade12_enrollment += grade12
            aggregates[key].grade12_school_count += 1
    return dict(aggregates)


def aggregate_public_characteristics_csv(
    csv_path: Path,
    school_year: int,
) -> dict[tuple[str, str, int], EnrollmentAggregate]:
    """Read and aggregate one downloaded public-school characteristics CSV."""
    with csv_path.open(newline="", encoding="utf-8-sig") as handle:
        return aggregate_public_characteristics_rows(csv.DictReader(handle), school_year)


def download_pss_zip(survey_school_year: int, raw_dir: Path, timeout: int, force: bool = False) -> Path:
    """Download one NCES Private School Survey public-use CSV ZIP."""
    url = PSS_CSV_URLS[survey_school_year]
    zip_path = raw_dir / f"pss_{survey_school_year}_{survey_school_year + 1}_pu_csv.zip"
    nces_urlretrieve(url, zip_path, timeout=timeout, force=force)
    return zip_path


def pss_csv_member_name(zip_path: Path) -> str:
    """Select the public-use CSV member inside a PSS ZIP archive."""
    with zipfile.ZipFile(zip_path) as archive:
        csv_members = [
            name
            for name in archive.namelist()
            if name.lower().endswith(".csv") and "__macosx" not in name.lower()
        ]
    if not csv_members:
        raise RuntimeError(f"No CSV member found in {zip_path}")
    return sorted(csv_members, key=lambda name: ("pss" not in name.lower(), len(name), name))[0]


def read_pss_csv_rows(zip_path: Path) -> list[dict[str, str]]:
    """Read PSS public-use rows from a downloaded ZIP archive."""
    member_name = pss_csv_member_name(zip_path)
    with zipfile.ZipFile(zip_path) as archive:
        with archive.open(member_name) as raw_handle:
            # Older NCES PSS public-use CSVs contain Latin-1/Windows characters.
            text_handle = io.TextIOWrapper(raw_handle, encoding="latin-1", newline="")
            return list(csv.DictReader(text_handle))


def aggregate_private_pss_rows(
    rows: Iterable[dict[str, str]],
    survey_school_year: int,
) -> dict[tuple[str, str, int], EnrollmentAggregate]:
    """Aggregate private school grade-12 enrollment by normalized town/state."""
    aggregates: dict[tuple[str, str, int], EnrollmentAggregate] = defaultdict(EnrollmentAggregate)
    for row in rows:
        # PL_* is the physical location, but PSS often stores usable location in
        # mailing fields.  Use physical location when present and fall back to mailing.
        town = normalize_town(first_present(row, ("PL_CIT", "PCITY")))
        state = normalize_state(first_present(row, ("PL_STABB", "PSTABB")))
        if not town or not state:
            continue
        key = (town, state, survey_school_year)
        grade12 = parse_nonnegative_int(row.get("P300"))
        if grade12 is None:
            aggregates[key].skipped_missing_grade12 += 1
            continue
        if grade12 > 0:
            aggregates[key].grade12_enrollment += grade12
            aggregates[key].grade12_school_count += 1
    return dict(aggregates)


def aggregate_private_pss_zip(
    zip_path: Path,
    survey_school_year: int,
) -> dict[tuple[str, str, int], EnrollmentAggregate]:
    """Read and aggregate one downloaded PSS public-use ZIP."""
    return aggregate_private_pss_rows(read_pss_csv_rows(zip_path), survey_school_year)


def merge_aggregate_maps(
    maps: Iterable[dict[tuple[str, str, int], EnrollmentAggregate]],
) -> dict[tuple[str, str, int], EnrollmentAggregate]:
    """Merge same-source aggregate dictionaries across source years."""
    merged: dict[tuple[str, str, int], EnrollmentAggregate] = defaultdict(EnrollmentAggregate)
    for aggregate_map in maps:
        for key, source_value in aggregate_map.items():
            target = merged[key]
            target.grade12_enrollment += source_value.grade12_enrollment
            target.grade12_school_count += source_value.grade12_school_count
            target.skipped_missing_grade12 += source_value.skipped_missing_grade12
    return dict(merged)


def latest_source_year_at_or_before(target_year: int, source_years: Iterable[int]) -> int | None:
    """Choose the latest biennial PSS source available before a school year."""
    eligible = [source_year for source_year in source_years if source_year <= target_year]
    return max(eligible) if eligible else None


def build_denominator_rows(
    public_aggregates: dict[tuple[str, str, int], EnrollmentAggregate],
    private_aggregates: dict[tuple[str, str, int], EnrollmentAggregate],
    school_years: Iterable[int],
    public_years_available: set[int],
    private_source_years: set[int],
) -> list[dict[str, str]]:
    """Combine public and private aggregates into a school-year denominator panel."""
    towns = {
        (town, state)
        for town, state, _year in list(public_aggregates.keys()) + list(private_aggregates.keys())
    }
    rows: list[dict[str, str]] = []
    for town, state in sorted(towns):
        for school_year in sorted(school_years):
            public = public_aggregates.get((town, state, school_year), EnrollmentAggregate())
            private_source_year = latest_source_year_at_or_before(school_year, private_source_years)
            private = (
                private_aggregates.get((town, state, private_source_year), EnrollmentAggregate())
                if private_source_year is not None
                else EnrollmentAggregate()
            )
            public_available = school_year in public_years_available
            denominator_available = public_available and private_source_year is not None
            total = public.grade12_enrollment + private.grade12_enrollment
            rows.append(
                {
                    "processed_town": town,
                    "processed_state": state,
                    "school_year": str(school_year),
                    "school_year_label": school_year_label(school_year),
                    "public_grade12_enrollment": str(public.grade12_enrollment) if public_available else "",
                    "private_grade12_enrollment": str(private.grade12_enrollment),
                    "total_grade12_enrollment": str(total) if denominator_available else "",
                    "public_grade12_school_count": str(public.grade12_school_count) if public_available else "",
                    "private_grade12_school_count": str(private.grade12_school_count),
                    "private_pss_source_school_year": (
                        str(private_source_year) if private_source_year is not None else ""
                    ),
                    "private_pss_source_label": (
                        school_year_label(private_source_year) if private_source_year is not None else ""
                    ),
                    "denominator_available": str(int(denominator_available and total > 0)),
                    "public_source": (
                        "NCES EDGE Public School Characteristics" if public_available else "not_available"
                    ),
                    "private_source": "NCES PSS public-use CSV carried forward",
                }
            )
    return rows


def write_csv(path: Path, rows: list[dict[str, str]]) -> None:
    """Write dictionary rows to CSV."""
    path.parent.mkdir(parents=True, exist_ok=True)
    if not rows:
        path.write_text("", encoding="utf-8")
        return
    with path.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=list(rows[0].keys()))
        writer.writeheader()
        writer.writerows(rows)


def read_csv_rows(path: Path) -> list[dict[str, str]]:
    """Read a CSV file into dictionaries, returning an empty list if absent."""
    if not path.exists():
        return []
    with path.open(newline="", encoding="utf-8-sig") as handle:
        return list(csv.DictReader(handle))


def read_town_universe(paths: Iterable[Path]) -> list[dict[str, str]]:
    """Read the student/recruiting town universe used by the panel."""
    grouped: dict[tuple[str, str], dict[str, str]] = {}
    counts: Counter[tuple[str, str]] = Counter()
    for path in paths:
        for row in read_csv_rows(path):
            town = normalize_town(row.get("processed_town", ""))
            state = normalize_state(row.get("processed_state", ""))
            if not town or not state:
                continue
            key = (town, state)
            counts[key] += parse_nonnegative_int(row.get("processed_group_count")) or 0
            grouped.setdefault(
                key,
                {
                    "processed_town": town,
                    "processed_state": state,
                    "compact_town": compact_town(town),
                    "representative_raw_town": row.get("representative_raw_town", town),
                    "representative_raw_state": row.get("representative_raw_state", state),
                    "raw_variant_count": row.get("raw_variant_count", "1"),
                },
            )

    records = []
    for key, row in grouped.items():
        records.append(
            {
                **row,
                "processed_group_count": str(counts[key] or 1),
            }
        )
    records.sort(key=lambda row: (row["processed_state"], row["processed_town"]))
    return records


def denominator_source_records(rows: list[dict[str, str]]) -> list[dict[str, str]]:
    """Build source-town records from denominator rows with positive totals."""
    available_by_key: Counter[tuple[str, str]] = Counter()
    for row in rows:
        if row.get("denominator_available") != "1":
            continue
        key = (row["processed_town"], row["processed_state"])
        available_by_key[key] += parse_nonnegative_int(row.get("total_grade12_enrollment")) or 0

    records = []
    for (town, state), total in sorted(available_by_key.items()):
        records.append(
            {
                "processed_town": town,
                "processed_state": state,
                "compact_town": compact_town(town),
                "representative_raw_town": town,
                "representative_raw_state": state,
                "processed_group_count": str(total),
                "raw_variant_count": "1",
            }
        )
    return records


def index_source_records(
    records: list[dict[str, str]],
) -> tuple[
    dict[tuple[str, str], dict[str, str]],
    dict[tuple[str, str], list[dict[str, str]]],
    dict[str, list[dict[str, str]]],
]:
    """Index denominator source records for exact, compact, and fuzzy matching."""
    by_processed = {(row["processed_town"], row["processed_state"]): row for row in records}
    by_compact: dict[tuple[str, str], list[dict[str, str]]] = defaultdict(list)
    by_state: dict[str, list[dict[str, str]]] = defaultdict(list)
    for row in records:
        by_compact[(row["compact_town"], row["processed_state"])].append(row)
        by_state[row["processed_state"]].append(row)
    return by_processed, dict(by_compact), dict(by_state)


def has_conflicting_direction(left: str, right: str) -> bool:
    """Avoid automatic fuzzy matches that swap directional prefixes."""
    left_tokens = left.split()
    right_tokens = right.split()
    if not left_tokens or not right_tokens:
        return False
    left_direction = left_tokens[0] if left_tokens[0] in DIRECTION_TOKENS else ""
    right_direction = right_tokens[0] if right_tokens[0] in DIRECTION_TOKENS else ""
    return bool(left_direction and right_direction and left_direction != right_direction)


def town_alias_candidates(town: str, state: str) -> list[str]:
    """Generate conservative aliases for OCR and prefix artifacts in panel towns."""
    aliases: list[str] = []

    def add_alias(value: str) -> None:
        alias = normalize_town(value)
        if alias and alias != town and alias not in aliases:
            aliases.append(alias)

    state_name = STATE_ABBR_TO_NAME.get(state, "")
    for prefix in (state_name, state, "US", "USA", "UNITED STATES"):
        if prefix and town.startswith(f"{prefix} "):
            add_alias(town[len(prefix) :])

    add_alias(clean_student_town(town))

    tokens = town.split()
    expanded_tokens = [TOWN_ABBREVIATION_ALIASES.get(token, token) for token in tokens]
    if expanded_tokens != tokens:
        add_alias(" ".join(expanded_tokens))

    manual_alias = MANUAL_TOWN_ALIASES.get(town)
    if manual_alias:
        add_alias(manual_alias)

    return aliases


def format_match(
    base: dict[str, str],
    source: dict[str, str],
    match_type: str,
    score: str = "1.000",
    alias: str = "",
    second_score: str = "",
) -> dict[str, str]:
    """Format a matched source-town record with common metadata fields."""
    return {
        **base,
        "denominator_source_town": source["processed_town"],
        "denominator_source_state": source["processed_state"],
        "denominator_match_type": match_type,
        "denominator_match_score": score,
        "denominator_proper_match_indicator": "1",
        "denominator_match_alias": alias,
        "denominator_second_best_score": second_score,
    }


def match_panel_town_to_denominator(
    panel_record: dict[str, str],
    by_processed: dict[tuple[str, str], dict[str, str]],
    by_compact: dict[tuple[str, str], list[dict[str, str]]],
    by_state: dict[str, list[dict[str, str]]],
) -> dict[str, str]:
    """Match one panel town to an NCES denominator source town."""
    town = panel_record["processed_town"]
    state = panel_record["processed_state"]
    key = (town, state)
    compact_key = (panel_record["compact_town"], state)
    state_candidates = by_state.get(state, [])

    base = {
        "denominator_source_town": "",
        "denominator_source_state": "",
        "denominator_match_type": "no_same_state_candidate" if not state_candidates else "unmatched",
        "denominator_match_score": "",
        "denominator_proper_match_indicator": "0",
        "denominator_match_alias": "",
        "denominator_state_candidate_count": str(len(state_candidates)),
        "denominator_second_best_score": "",
    }
    if not state_candidates:
        return base

    if key in by_processed:
        return format_match(base, by_processed[key], "exact_processed")

    for alias in town_alias_candidates(town, state):
        alias_key = (alias, state)
        if alias_key in by_processed:
            return format_match(base, by_processed[alias_key], "alias_exact_processed", alias=alias)

    compact_matches = by_compact.get(compact_key, [])
    if len(compact_matches) == 1:
        return format_match(base, compact_matches[0], "exact_compact")

    for alias in town_alias_candidates(town, state):
        alias_compact_matches = by_compact.get((compact_town(alias), state), [])
        if len(alias_compact_matches) == 1:
            return format_match(
                base,
                alias_compact_matches[0],
                "alias_exact_compact",
                alias=alias,
            )

    scored = sorted(
        ((town_similarity(town, candidate["processed_town"]), candidate) for candidate in state_candidates),
        key=lambda item: (item[0], item[1]["processed_group_count"], item[1]["processed_town"]),
        reverse=True,
    )
    best_score, best_source = scored[0]
    second_score = scored[1][0] if len(scored) > 1 else -1.0
    match_type = "fuzzy_review"
    proper = "0"
    if not has_conflicting_direction(town, best_source["processed_town"]):
        if len(scored) == 1 and best_score >= 0.94:
            match_type = "fuzzy_single_high_confidence"
            proper = "1"
        elif best_score >= 0.93 and (best_score - second_score) >= 0.04:
            match_type = "fuzzy_clear_best"
            proper = "1"
        elif best_score >= 0.92 and (best_score - second_score) >= 0.20:
            match_type = "fuzzy_high_gap"
            proper = "1"

    return {
        **base,
        "denominator_source_town": best_source["processed_town"],
        "denominator_source_state": best_source["processed_state"],
        "denominator_match_type": match_type,
        "denominator_match_score": f"{best_score:.3f}",
        "denominator_proper_match_indicator": proper,
        "denominator_second_best_score": "" if second_score < 0 else f"{second_score:.3f}",
    }


def empty_denominator_row(town: str, state: str, school_year: int, match: dict[str, str]) -> dict[str, str]:
    """Create a panel-keyed denominator row when no source town is matched."""
    return {
        "processed_town": town,
        "processed_state": state,
        "school_year": str(school_year),
        "school_year_label": school_year_label(school_year),
        "public_grade12_enrollment": "",
        "private_grade12_enrollment": "",
        "total_grade12_enrollment": "",
        "public_grade12_school_count": "",
        "private_grade12_school_count": "",
        "private_pss_source_school_year": "",
        "private_pss_source_label": "",
        "denominator_available": "0",
        "public_source": "not_matched_to_nces_school_location_town",
        "private_source": "not_matched_to_nces_school_location_town",
        **match,
    }


def build_panel_matched_denominator_rows(
    denominator_rows: list[dict[str, str]],
    panel_town_records: list[dict[str, str]],
    school_years: Iterable[int],
) -> tuple[list[dict[str, str]], list[dict[str, str]]]:
    """Key denominator rows to panel towns using exact and same-state fuzzy matching."""
    source_records = denominator_source_records(denominator_rows)
    by_processed, by_compact, by_state = index_source_records(source_records)
    source_by_key_year = {
        (row["processed_town"], row["processed_state"], int(row["school_year"])): row
        for row in denominator_rows
    }

    matched_rows = []
    review_rows = []
    for panel_record in panel_town_records:
        panel_town = panel_record["processed_town"]
        panel_state = panel_record["processed_state"]
        match = match_panel_town_to_denominator(panel_record, by_processed, by_compact, by_state)
        if match["denominator_match_type"] not in {"exact_processed", "exact_compact"}:
            review_rows.append({**panel_record, **match})

        for school_year in sorted(school_years):
            if match["denominator_proper_match_indicator"] != "1":
                matched_rows.append(empty_denominator_row(panel_town, panel_state, school_year, match))
                continue
            source = source_by_key_year.get(
                (match["denominator_source_town"], match["denominator_source_state"], school_year)
            )
            if source is None:
                matched_rows.append(empty_denominator_row(panel_town, panel_state, school_year, match))
                continue
            matched_rows.append(
                {
                    **source,
                    "processed_town": panel_town,
                    "processed_state": panel_state,
                    **match,
                }
            )
    return matched_rows, review_rows


def write_summary(
    path: Path,
    rows: list[dict[str, str]],
    public_years_available: set[int],
    private_source_years: set[int],
    matched_rows: list[dict[str, str]] | None = None,
    review_rows: list[dict[str, str]] | None = None,
) -> None:
    """Write a compact source and coverage summary."""
    available_rows = [row for row in rows if row["denominator_available"] == "1"]
    total_denominator = sum(int(row["total_grade12_enrollment"] or 0) for row in available_rows)
    lines = [
        "# Online high school denominator summary",
        "",
        "- Public source: NCES EDGE Public School Characteristics CSV exports.",
        "- Private source: NCES Private School Survey public-use CSV ZIP files.",
        "- Private PSS rule: latest PSS survey year at or before the recruiting school year.",
        "- Default public coverage excludes 2019-20 and later COVID-era school years for now.",
        f"- Public school years available: {', '.join(map(str, sorted(public_years_available)))}",
        f"- Private PSS source years available: {', '.join(map(str, sorted(private_source_years)))}",
        f"- Town-school-year rows: {len(rows)}",
        f"- Rows with complete public+private denominators: {len(available_rows)}",
        f"- Sum of complete grade-12 denominators: {total_denominator}",
    ]
    if matched_rows is not None:
        matched_available_rows = [row for row in matched_rows if row["denominator_available"] == "1"]
        matched_towns = {
            (row["processed_town"], row["processed_state"])
            for row in matched_rows
        }
        matched_available_towns_2017 = {
            (row["processed_town"], row["processed_state"])
            for row in matched_rows
            if row["school_year"] == "2017" and row["denominator_available"] == "1"
        }
        match_type_counts = Counter(row["denominator_match_type"] for row in matched_rows[:: len(set(row["school_year"] for row in matched_rows)) or 1])
        review_count = len(review_rows or [])
        lines.extend(
            [
                "",
                "## Panel-town matched denominator file",
                "",
                "- Matching unit: panel town/state to NCES school-location town/state, exact first and then conservative same-state fuzzy matching.",
                "- This is not a school-district boundary match; it assigns school-location denominators to panel town names.",
                f"- Panel towns written: {len(matched_towns)}",
                f"- Panel towns with positive fixed 2017-18 denominator: {len(matched_available_towns_2017)}",
                f"- Panel town-school-year rows with complete denominators: {len(matched_available_rows)}",
                f"- Panel town match rows with non-exact or unmatched denominator links: {review_count}",
                "- Match types: "
                + ", ".join(f"{key}={value}" for key, value in sorted(match_type_counts.items())),
            ]
        )
    lines.extend(
        [
            "",
            "Rows with `denominator_available == 0` should be excluded from denominator-based regressions.",
        ]
    )
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text("\n".join(lines) + "\n", encoding="utf-8")


#=====================================================================
# 4 - Command-line workflow
#=====================================================================

def parse_year_list(value: str) -> tuple[int, ...]:
    """Parse comma-separated years from the command line."""
    return tuple(int(item.strip()) for item in value.split(",") if item.strip())


def parse_args() -> argparse.Namespace:
    """Parse command-line arguments."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--raw-dir", type=Path, default=DEFAULT_RAW_DIR)
    parser.add_argument("--output", type=Path, default=DEFAULT_OUTPUT)
    parser.add_argument("--panel-matched-output", type=Path, default=DEFAULT_PANEL_MATCHED_OUTPUT)
    parser.add_argument("--match-review", type=Path, default=DEFAULT_MATCH_REVIEW)
    parser.add_argument("--summary", type=Path, default=DEFAULT_SUMMARY)
    parser.add_argument(
        "--town-universe-path",
        action="append",
        type=Path,
        default=list(DEFAULT_TOWN_UNIVERSE_PATHS),
        help="Panel town-universe CSV. May be passed multiple times.",
    )
    parser.add_argument("--skip-panel-match", action="store_true")
    parser.add_argument("--school-years", default=",".join(map(str, DEFAULT_SCHOOL_YEARS)))
    parser.add_argument("--public-years", default=",".join(map(str, DEFAULT_PUBLIC_SCHOOL_YEARS)))
    parser.add_argument("--private-source-years", default="2015,2017")
    parser.add_argument("--timeout", type=int, default=180)
    parser.add_argument("--force-download", action="store_true")
    return parser.parse_args()


def main() -> None:
    """CLI entry point."""
    args = parse_args()
    school_years = parse_year_list(args.school_years)
    public_years = parse_year_list(args.public_years)
    private_source_years = parse_year_list(args.private_source_years)

    public_maps = []
    for school_year in public_years:
        csv_path = download_public_characteristics_csv(
            school_year,
            raw_dir=args.raw_dir,
            timeout=args.timeout,
            force=args.force_download,
        )
        public_maps.append(aggregate_public_characteristics_csv(csv_path, school_year))

    private_maps = []
    for source_year in private_source_years:
        zip_path = download_pss_zip(
            source_year,
            raw_dir=args.raw_dir,
            timeout=args.timeout,
            force=args.force_download,
        )
        private_maps.append(aggregate_private_pss_zip(zip_path, source_year))

    denominator_rows = build_denominator_rows(
        public_aggregates=merge_aggregate_maps(public_maps),
        private_aggregates=merge_aggregate_maps(private_maps),
        school_years=school_years,
        public_years_available=set(public_years),
        private_source_years=set(private_source_years),
    )
    write_csv(args.output, denominator_rows)
    matched_rows = None
    review_rows = None
    if not args.skip_panel_match:
        panel_town_records = read_town_universe(args.town_universe_path)
        if panel_town_records:
            matched_rows, review_rows = build_panel_matched_denominator_rows(
                denominator_rows,
                panel_town_records,
                school_years,
            )
            write_csv(args.panel_matched_output, matched_rows)
            write_csv(args.match_review, review_rows)
            print(f"Wrote {len(matched_rows)} panel-matched denominator rows to {args.panel_matched_output}")
            print(f"Wrote {len(review_rows)} denominator match-review rows to {args.match_review}")
    write_summary(
        args.summary,
        denominator_rows,
        set(public_years),
        set(private_source_years),
        matched_rows=matched_rows,
        review_rows=review_rows,
    )
    print(f"Wrote {len(denominator_rows)} denominator rows to {args.output}")
    print(f"Wrote summary to {args.summary}")


if __name__ == "__main__":
    main()
