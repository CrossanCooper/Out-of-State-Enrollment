#!/usr/bin/env python3
#=====================================================================
## Author: Crossan Cooper
## Last Modified: 06-08-2026
#
## file use: Build town-level ACS covariates keyed to the UA recruiting
## panel's processed town/state identifiers.
#
## inputs:
## 1. Census ACS 2017 5-year API variables -- population, race, poverty, income
## 2. town_matching/output/ua_visit_t_graduates_t_plus_5_panel.csv -- target town universe
#
## outputs:
## 1. town_matching/output/ua_acs_town_covariates_2017.csv -- matched ACS covariates
## 2. town_matching/output/ua_acs_town_covariates_2017_match_review.csv -- match audit
## 3. town_matching/output/ua_acs_town_covariates_2017_summary.md -- ACS build summary
#=====================================================================
"""Build town-level ACS covariates for the UA recruiting town panel.

The output is keyed to the normalized ``processed_town``/``processed_state``
identifiers used throughout ``town_matching``.  ACS data are pulled from the
official Census 2017 ACS 5-year place-level files.  We use named Census API
variables by default so race, poverty, and income measures are keyed by
variable name rather than table row order.  Replicate-estimate CSV files remain
available as a fallback/debug source.
"""

from __future__ import annotations

import argparse
import csv
import gzip
import json
import os
import shutil
import tempfile
import time
import urllib.parse
import urllib.request
from urllib.error import HTTPError, URLError
from collections import defaultdict
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable

from town_linkage import (
    DEFAULT_OUTPUT_DIR,
    normalize_state,
    normalize_town,
    town_similarity,
)


#=====================================================================
# 1 - Paths, source URLs, and ACS variables
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
DEFAULT_PANEL = DEFAULT_OUTPUT_DIR / "ua_visit_t_graduates_t_plus_5_panel.csv"
DEFAULT_OUTPUT = DEFAULT_OUTPUT_DIR / "ua_acs_town_covariates_2017.csv"
DEFAULT_REVIEW = DEFAULT_OUTPUT_DIR / "ua_acs_town_covariates_2017_match_review.csv"
DEFAULT_SUMMARY = DEFAULT_OUTPUT_DIR / "ua_acs_town_covariates_2017_summary.md"

ACS_REPLICATE_URL = (
    "https://www2.census.gov/programs-surveys/acs/replicate_estimates/"
    "{year}/data/5-year/160/{table_id}.csv.gz"
)
ACS_API_URL = "https://api.census.gov/data/{year}/acs/acs5"

ACS_YEAR = 2017
ACS_TABLE_IDS = ("B01003", "B02001", "B17001", "B19013")
ACS_API_VARIABLES = (
    "B01003_001E",
    "B02001_001E",
    "B02001_003E",
    "B17001_001E",
    "B17001_002E",
    "B19013_001E",
)
ACS_VARIABLE_LABELS = {
    "acs_total_population": "Total population (B01003)",
    "acs_black_share": "Black-alone population share (B02001_003 / B02001_001)",
    "acs_poverty_share": "Share below poverty among population with poverty status determined (B17001_002 / B17001_001)",
    "acs_median_household_income": "Median household income in 2017 inflation-adjusted dollars (B19013_001)",
}

STATE_ABBR_TO_FIPS = {
    "AL": "01",
    "AK": "02",
    "AZ": "04",
    "AR": "05",
    "CA": "06",
    "CO": "08",
    "CT": "09",
    "DE": "10",
    "DC": "11",
    "FL": "12",
    "GA": "13",
    "HI": "15",
    "ID": "16",
    "IL": "17",
    "IN": "18",
    "IA": "19",
    "KS": "20",
    "KY": "21",
    "LA": "22",
    "ME": "23",
    "MD": "24",
    "MA": "25",
    "MI": "26",
    "MN": "27",
    "MS": "28",
    "MO": "29",
    "MT": "30",
    "NE": "31",
    "NV": "32",
    "NH": "33",
    "NJ": "34",
    "NM": "35",
    "NY": "36",
    "NC": "37",
    "ND": "38",
    "OH": "39",
    "OK": "40",
    "OR": "41",
    "PA": "42",
    "RI": "44",
    "SC": "45",
    "SD": "46",
    "TN": "47",
    "TX": "48",
    "UT": "49",
    "VT": "50",
    "VA": "51",
    "WA": "53",
    "WV": "54",
    "WI": "55",
    "WY": "56",
}

STATE_FIPS_TO_ABBR = {value: key for key, value in STATE_ABBR_TO_FIPS.items()}
ACS_MISSING_CODES = {
    -999999999,
    -888888888,
    -666666666,
    -555555555,
    -333333333,
    -222222222,
}


#=====================================================================
# 2 - Data structures and argument parsing
#=====================================================================

@dataclass(frozen=True)
class TownKey:
    """A normalized town/state key from the UA regression panel."""

    processed_town: str
    processed_state: str


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--panel", type=Path, default=DEFAULT_PANEL)
    parser.add_argument("--raw-dir", type=Path, default=DEFAULT_RAW_DIR)
    parser.add_argument("--output", type=Path, default=DEFAULT_OUTPUT)
    parser.add_argument("--review", type=Path, default=DEFAULT_REVIEW)
    parser.add_argument("--summary", type=Path, default=DEFAULT_SUMMARY)
    parser.add_argument("--year", type=int, default=ACS_YEAR)
    parser.add_argument(
        "--source",
        choices=("api", "replicate"),
        default="api",
        help="Use named-variable Census API pulls by default; keep replicate files as a fallback/debug option.",
    )
    parser.add_argument("--timeout", type=int, default=60)
    parser.add_argument("--download-retries", type=int, default=4)
    parser.add_argument(
        "--api-key",
        default=None,
        help="Optional Census API key. Defaults to CENSUS_API_KEY from the environment or ~/.Renviron.",
    )
    parser.add_argument("--force-download", action="store_true")
    parser.add_argument("--fuzzy-threshold", type=float, default=0.94)
    parser.add_argument("--fuzzy-margin", type=float, default=0.03)
    return parser.parse_args()


def parse_number(value: str | None) -> float | None:
    """Parse Census estimates, treating ACS sentinel values as missing."""
    if value is None:
        return None
    value = str(value).strip().replace(",", "")
    if not value or value in {"-", "(X)", "N"}:
        return None
    try:
        number = float(value)
    except ValueError:
        return None
    if int(number) in ACS_MISSING_CODES:
        return None
    return number


def safe_ratio(numerator: float | None, denominator: float | None) -> float | None:
    """Return numerator/denominator only when both values are usable."""
    if numerator is None or denominator is None or denominator <= 0:
        return None
    return numerator / denominator


def strip_place_suffix(place_name: str) -> str:
    """Convert an ACS place name into the local normalized town key.

    ACS place names include legal suffixes such as ``city``, ``town``, ``CDP``,
    and ``village``.  The recruiting/student town keys generally omit those
    suffixes, so we remove only a final legal suffix after normalization.
    """
    base = place_name.split(",", 1)[0]
    normalized = normalize_town(base)
    suffix_patterns = (
        "UNIFIED GOVERNMENT BALANCE",
        "CONSOLIDATED GOVERNMENT BALANCE",
        "URBAN COUNTY",
        "METRO TOWNSHIP",
        "MUNICIPALITY",
        "PLANTATION",
        "BOROUGH",
        "VILLAGE",
        "CITY",
        "TOWN",
        "CDP",
    )
    for suffix in suffix_patterns:
        token = f" {suffix}"
        if normalized.endswith(token):
            normalized = normalized[: -len(token)].strip()
            break
    return normalized


def acs_file_path(raw_dir: Path, year: int, table_id: str) -> Path:
    return raw_dir / f"acs_{year}_5yr_place_{table_id}.csv.gz"


def acs_api_file_path(raw_dir: Path, year: int) -> Path:
    return raw_dir / f"acs_{year}_5yr_place_api_core_covariates.json"


def read_census_api_key(explicit_key: str | None = None) -> str | None:
    """Read a Census API key without requiring it on the shell command line."""
    if explicit_key:
        return explicit_key.strip() or None
    env_key = os.environ.get("CENSUS_API_KEY", "").strip()
    if env_key:
        return env_key

    renviron_path = Path.home() / ".Renviron"
    if not renviron_path.exists():
        return None
    for line in renviron_path.read_text(encoding="utf-8").splitlines():
        stripped = line.strip()
        if not stripped or stripped.startswith("#") or "=" not in stripped:
            continue
        name, value = stripped.split("=", 1)
        if name.strip() == "CENSUS_API_KEY":
            return value.strip().strip('"').strip("'") or None
    return None


def has_valid_api_payload(path: Path) -> bool:
    """Return true when a cached API payload looks like Census JSON data."""
    try:
        payload = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError):
        return False
    return isinstance(payload, list) and len(payload) > 1 and isinstance(payload[0], list)


def fetch_acs_api_state(
    year: int,
    state_fips: str,
    timeout: int,
    retries: int,
    api_key: str | None,
) -> list[list[str]]:
    """Fetch named ACS variables for all places in one state."""
    params = {
        "get": ",".join(("NAME", *ACS_API_VARIABLES)),
        "for": "place:*",
        "in": f"state:{state_fips}",
    }
    if api_key:
        params["key"] = api_key
    url = f"{ACS_API_URL.format(year=year)}?{urllib.parse.urlencode(params, safe=',:*')}"
    last_error: Exception | None = None
    for attempt in range(1, retries + 1):
        print(f"Downloading ACS {year} named place variables for state {state_fips} (attempt {attempt}/{retries})")
        request = urllib.request.Request(url, headers={"User-Agent": "Mozilla/5.0"})
        try:
            with urllib.request.urlopen(request, timeout=timeout) as response:
                text = response.read().decode("utf-8")
            payload = json.loads(text)
            if isinstance(payload, list) and payload and isinstance(payload[0], list):
                return payload
            last_error = RuntimeError(f"Census API returned an unexpected payload for state {state_fips}")
        except (HTTPError, TimeoutError, URLError, json.JSONDecodeError) as error:
            last_error = error
        if attempt < retries:
            time.sleep(2 * attempt)
    raise RuntimeError(f"Failed to download ACS named variables for state {state_fips}") from last_error


def download_acs_api(
    year: int,
    raw_dir: Path,
    timeout: int,
    force: bool,
    retries: int,
    api_key: str | None,
) -> Path:
    """Download named ACS variables for all Census places into the raw cache."""
    output_path = acs_api_file_path(raw_dir, year)
    if output_path.exists() and not force:
        if has_valid_api_payload(output_path):
            return output_path
        output_path.unlink()
    output_path.parent.mkdir(parents=True, exist_ok=True)

    combined_payload: list[list[str]] | None = None
    for state_fips in sorted(STATE_FIPS_TO_ABBR):
        state_payload = fetch_acs_api_state(
            year=year,
            state_fips=state_fips,
            timeout=timeout,
            retries=retries,
            api_key=api_key,
        )
        if combined_payload is None:
            combined_payload = state_payload
        else:
            combined_payload.extend(state_payload[1:])
    if combined_payload is None:
        raise RuntimeError("No ACS API data were downloaded")

    with tempfile.NamedTemporaryFile("w", delete=False, dir=str(output_path.parent), encoding="utf-8") as temp_handle:
        temp_path = Path(temp_handle.name)
        json.dump(combined_payload, temp_handle)
        temp_handle.write("\n")
    temp_path.replace(output_path)
    return output_path


#=====================================================================
# 3 - ACS source parsing and town matching
#=====================================================================

def download_acs_table(
    table_id: str,
    year: int,
    raw_dir: Path,
    timeout: int,
    force: bool,
    retries: int,
) -> Path:
    """Download one ACS table file into the raw-online cache."""
    output_path = acs_file_path(raw_dir, year, table_id)
    if output_path.exists() and not force:
        return output_path
    output_path.parent.mkdir(parents=True, exist_ok=True)
    url = ACS_REPLICATE_URL.format(year=year, table_id=table_id)
    last_error: Exception | None = None
    for attempt in range(1, retries + 1):
        print(f"Downloading ACS {year} {table_id} (attempt {attempt}/{retries})")
        request = urllib.request.Request(url, headers={"User-Agent": "Mozilla/5.0"})
        temp_path: Path | None = None
        try:
            with urllib.request.urlopen(request, timeout=timeout) as response:
                with tempfile.NamedTemporaryFile(delete=False, dir=str(output_path.parent)) as temp_handle:
                    temp_path = Path(temp_handle.name)
                    shutil.copyfileobj(response, temp_handle)
            temp_path.replace(output_path)
            return output_path
        except (HTTPError, TimeoutError, URLError) as error:
            last_error = error
            if temp_path is not None and temp_path.exists():
                temp_path.unlink()
            if attempt < retries:
                time.sleep(2 * attempt)
        except Exception:
            if temp_path is not None and temp_path.exists():
                temp_path.unlink()
            raise
    raise RuntimeError(f"Failed to download ACS table {table_id} after {retries} attempts") from last_error
    return output_path


def read_town_keys(panel_path: Path) -> list[TownKey]:
    """Read unique processed town/state keys from the regression panel."""
    keys = set()
    with panel_path.open(newline="", encoding="utf-8-sig") as handle:
        for row in csv.DictReader(handle):
            town = normalize_town(row.get("processed_town", ""))
            state = normalize_state(row.get("processed_state", ""))
            if town and state in STATE_ABBR_TO_FIPS:
                keys.add(TownKey(town, state))
    return sorted(keys, key=lambda key: (key.processed_state, key.processed_town))


def iter_acs_rows(path: Path) -> Iterable[dict[str, str]]:
    """Yield data rows from one gzip-compressed ACS table file."""
    with gzip.open(path, "rt", encoding="latin-1", newline="") as handle:
        reader = csv.DictReader(handle)
        for row in reader:
            if row.get("GEOID"):
                yield row


def collect_acs_estimates(paths_by_table: dict[str, Path]) -> dict[str, dict[str, float | str]]:
    """Collect required ACS estimates by GEOID across table files."""
    rows_by_geoid: dict[str, dict[str, float | str]] = defaultdict(dict)
    estimates_by_table: dict[str, dict[str, dict[int, float | None]]] = defaultdict(dict)

    for table_id, path in paths_by_table.items():
        for row in iter_acs_rows(path):
            geoid = row["GEOID"]
            rows_by_geoid[geoid]["acs_geoid"] = geoid
            rows_by_geoid[geoid]["acs_place_name"] = row.get("NAME", "")
            order = int(float(row.get("ORDER") or 0))
            estimates_by_table[table_id].setdefault(geoid, {})[order] = parse_number(row.get("estimate"))

    for geoid, output_row in rows_by_geoid.items():
        b01003 = estimates_by_table["B01003"].get(geoid, {})
        b02001 = estimates_by_table["B02001"].get(geoid, {})
        b17001 = estimates_by_table["B17001"].get(geoid, {})
        b19013 = estimates_by_table["B19013"].get(geoid, {})

        total_population = b01003.get(1)
        race_total_population = b02001.get(1)
        black_alone_population = b02001.get(3)
        poverty_universe_population = b17001.get(1)
        poverty_population = b17001.get(2)
        median_household_income = b19013.get(1)

        output_row.update(
            acs_total_population=total_population,
            acs_race_total_population=race_total_population,
            acs_black_alone_population=black_alone_population,
            acs_black_share=safe_ratio(black_alone_population, race_total_population),
            acs_poverty_universe_population=poverty_universe_population,
            acs_poverty_population=poverty_population,
            acs_poverty_share=safe_ratio(poverty_population, poverty_universe_population),
            acs_median_household_income=median_household_income,
        )
    return dict(rows_by_geoid)


def collect_acs_api_estimates(path: Path) -> dict[str, dict[str, float | str]]:
    """Collect ACS estimates from the named-variable Census API JSON payload."""
    payload = json.loads(path.read_text(encoding="utf-8"))
    if not isinstance(payload, list) or not payload:
        raise ValueError(f"ACS API payload has no rows: {path}")
    headers = payload[0]
    if not isinstance(headers, list):
        raise ValueError(f"ACS API payload has an invalid header row: {path}")

    rows_by_geoid: dict[str, dict[str, float | str]] = {}
    for values in payload[1:]:
        row = dict(zip(headers, values))
        state_fips = str(row.get("state", ""))
        place_fips = str(row.get("place", ""))
        if not state_fips or not place_fips:
            continue
        geoid = f"16000US{state_fips}{place_fips}"
        race_total_population = parse_number(row.get("B02001_001E"))
        black_alone_population = parse_number(row.get("B02001_003E"))
        poverty_universe_population = parse_number(row.get("B17001_001E"))
        poverty_population = parse_number(row.get("B17001_002E"))
        rows_by_geoid[geoid] = {
            "acs_geoid": geoid,
            "acs_place_name": str(row.get("NAME", "")),
            "acs_total_population": parse_number(row.get("B01003_001E")),
            "acs_race_total_population": race_total_population,
            "acs_black_alone_population": black_alone_population,
            "acs_black_share": safe_ratio(black_alone_population, race_total_population),
            "acs_poverty_universe_population": poverty_universe_population,
            "acs_poverty_population": poverty_population,
            "acs_poverty_share": safe_ratio(poverty_population, poverty_universe_population),
            "acs_median_household_income": parse_number(row.get("B19013_001E")),
        }
    return rows_by_geoid


def state_from_geoid(geoid: str) -> str:
    """Return a state abbreviation from an ACS place GEOID."""
    fips = geoid.removeprefix("16000US")[:2]
    return STATE_FIPS_TO_ABBR.get(fips, "")


def prepare_place_rows(acs_by_geoid: dict[str, dict[str, float | str]]) -> list[dict[str, float | str]]:
    """Add normalized matching fields to ACS place rows."""
    place_rows = []
    for row in acs_by_geoid.values():
        geoid = str(row.get("acs_geoid", ""))
        state = state_from_geoid(geoid)
        if not state:
            continue
        place_row = dict(row)
        place_row["acs_processed_town"] = strip_place_suffix(str(row.get("acs_place_name", "")))
        place_row["processed_state"] = state
        place_rows.append(place_row)
    return place_rows


def choose_exact_place(candidates: list[dict[str, float | str]]) -> dict[str, float | str]:
    """Choose a deterministic ACS place when duplicate normalized names exist."""
    return sorted(
        candidates,
        key=lambda row: (
            -(float(row.get("acs_total_population") or 0)),
            str(row.get("acs_place_name") or ""),
            str(row.get("acs_geoid") or ""),
        ),
    )[0]


def match_places(
    town_keys: list[TownKey],
    place_rows: list[dict[str, float | str]],
    fuzzy_threshold: float,
    fuzzy_margin: float,
) -> tuple[list[dict[str, float | str]], list[dict[str, float | str]]]:
    """Match UA town keys to ACS places with exact then conservative fuzzy rules."""
    by_state_town: dict[tuple[str, str], list[dict[str, float | str]]] = defaultdict(list)
    by_state: dict[str, list[dict[str, float | str]]] = defaultdict(list)
    for place in place_rows:
        key = (str(place["processed_state"]), str(place["acs_processed_town"]))
        by_state_town[key].append(place)
        by_state[str(place["processed_state"])].append(place)

    matched_rows: list[dict[str, float | str]] = []
    review_rows: list[dict[str, float | str]] = []

    for town_key in town_keys:
        base = {
            "processed_town": town_key.processed_town,
            "processed_state": town_key.processed_state,
        }
        exact_candidates = by_state_town.get((town_key.processed_state, town_key.processed_town), [])
        if exact_candidates:
            place = choose_exact_place(exact_candidates)
            matched_rows.append({
                **base,
                **place,
                "acs_match_method": "exact",
                "acs_match_score": 1.0,
                "acs_second_best_score": "",
            })
            if len(exact_candidates) > 1:
                review_rows.append({
                    **base,
                    "review_reason": "duplicate_exact_acs_places",
                    "candidate_count": len(exact_candidates),
                    "best_acs_place_name": place.get("acs_place_name", ""),
                    "best_acs_geoid": place.get("acs_geoid", ""),
                    "best_score": 1.0,
                    "second_best_score": "",
                })
            continue

        candidates = []
        for place in by_state.get(town_key.processed_state, []):
            score = town_similarity(town_key.processed_town, str(place["acs_processed_town"]))
            candidates.append((score, place))
        candidates.sort(key=lambda item: (-item[0], str(item[1].get("acs_place_name") or "")))
        best_score, best_place = candidates[0] if candidates else (0.0, {})
        second_score = candidates[1][0] if len(candidates) > 1 else 0.0
        fuzzy_ok = best_score >= fuzzy_threshold and (best_score - second_score) >= fuzzy_margin
        if fuzzy_ok:
            matched_rows.append({
                **base,
                **best_place,
                "acs_match_method": "fuzzy",
                "acs_match_score": best_score,
                "acs_second_best_score": second_score,
            })
        else:
            matched_rows.append({
                **base,
                "acs_geoid": "",
                "acs_place_name": "",
                "acs_processed_town": "",
                "acs_match_method": "unmatched",
                "acs_match_score": best_score if candidates else "",
                "acs_second_best_score": second_score if candidates else "",
            })
        if not fuzzy_ok:
            review_rows.append({
                **base,
                "review_reason": "unmatched_or_low_confidence_fuzzy",
                "candidate_count": len(candidates),
                "best_acs_place_name": best_place.get("acs_place_name", ""),
                "best_acs_geoid": best_place.get("acs_geoid", ""),
                "best_score": best_score if candidates else "",
                "second_best_score": second_score if candidates else "",
            })

    return matched_rows, review_rows


OUTPUT_FIELDS = (
    "processed_town",
    "processed_state",
    "acs_geoid",
    "acs_place_name",
    "acs_processed_town",
    "acs_match_method",
    "acs_match_score",
    "acs_second_best_score",
    "acs_total_population",
    "acs_race_total_population",
    "acs_black_alone_population",
    "acs_black_share",
    "acs_poverty_universe_population",
    "acs_poverty_population",
    "acs_poverty_share",
    "acs_median_household_income",
)

REVIEW_FIELDS = (
    "processed_town",
    "processed_state",
    "review_reason",
    "candidate_count",
    "best_acs_place_name",
    "best_acs_geoid",
    "best_score",
    "second_best_score",
)


def write_csv(path: Path, rows: list[dict[str, float | str]], fieldnames: tuple[str, ...]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=fieldnames, extrasaction="ignore")
        writer.writeheader()
        writer.writerows(rows)


def format_number(value: float) -> str:
    if abs(value - round(value)) < 1e-9:
        return f"{int(round(value)):,}"
    return f"{value:,.3f}"


def write_summary(
    path: Path,
    year: int,
    town_keys: list[TownKey],
    place_rows: list[dict[str, float | str]],
    matched_rows: list[dict[str, float | str]],
    review_rows: list[dict[str, float | str]],
    output_path: Path,
    review_path: Path,
) -> None:
    matched_count = sum(row.get("acs_match_method") in {"exact", "fuzzy"} for row in matched_rows)
    exact_count = sum(row.get("acs_match_method") == "exact" for row in matched_rows)
    fuzzy_count = sum(row.get("acs_match_method") == "fuzzy" for row in matched_rows)
    unmatched_count = sum(row.get("acs_match_method") == "unmatched" for row in matched_rows)
    states = sorted({key.processed_state for key in town_keys})
    lines = [
        f"# ACS town covariates ({year} ACS 5-year)",
        "",
        f"Source: Census ACS {year} 5-year place-level named API variables or replicate-estimate CSV files.",
        f"Output: `{output_path}`",
        f"Review file: `{review_path}`",
        "",
        "## Match summary",
        "",
        f"- UA town/state keys: {len(town_keys):,}",
        f"- States represented: {len(states):,}",
        f"- ACS place rows loaded: {len(place_rows):,}",
        f"- Matched town keys: {matched_count:,} ({matched_count / len(town_keys):.1%})",
        f"- Exact matches: {exact_count:,}",
        f"- High-confidence fuzzy matches: {fuzzy_count:,}",
        f"- Unmatched or low-confidence fuzzy: {unmatched_count:,}",
        f"- Review rows: {len(review_rows):,}",
        "",
        "## Variables",
        "",
        *[f"- `{variable}`: {label}" for variable, label in ACS_VARIABLE_LABELS.items()],
        "",
        "The existing NCES denominator file remains the source for fixed grade-12 size,",
        "private grade-12 share, and public/private high-school counts.",
        "",
        "ACS variables are place-level table estimates. Matching is exact on state first",
        "and then exact or high-confidence fuzzy on processed town name within state.",
    ]
    if matched_rows:
        populations = [
            float(row["acs_total_population"])
            for row in matched_rows
            if row.get("acs_match_method") in {"exact", "fuzzy"} and row.get("acs_total_population") not in {"", None}
        ]
        if populations:
            lines.extend([
                "",
                "## Matched-place population diagnostics",
                "",
                f"- Min matched ACS population: {format_number(min(populations))}",
                f"- Median matched ACS population: {format_number(sorted(populations)[len(populations) // 2])}",
                f"- Max matched ACS population: {format_number(max(populations))}",
            ])
        for variable in ACS_VARIABLE_LABELS:
            values = [
                float(row[variable])
                for row in matched_rows
                if row.get("acs_match_method") in {"exact", "fuzzy"} and row.get(variable) not in {"", None}
            ]
            if values:
                lines.extend([
                    "",
                    f"## `{variable}` diagnostics",
                    "",
                    f"- Nonmissing matched town keys: {len(values):,}",
                    f"- Mean: {format_number(sum(values) / len(values))}",
                    f"- Median: {format_number(sorted(values)[len(values) // 2])}",
                ])
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text("\n".join(lines) + "\n", encoding="utf-8")


#=====================================================================
# 4 - Command-line workflow
#=====================================================================

def main() -> None:
    args = parse_args()
    town_keys = read_town_keys(args.panel)
    if args.source == "api":
        api_key = read_census_api_key(args.api_key)
        api_path = download_acs_api(
            year=args.year,
            raw_dir=args.raw_dir,
            timeout=args.timeout,
            force=args.force_download,
            retries=args.download_retries,
            api_key=api_key,
        )
        acs_estimates = collect_acs_api_estimates(api_path)
    else:
        paths_by_table = {
            table_id: download_acs_table(
                table_id,
                year=args.year,
                raw_dir=args.raw_dir,
                timeout=args.timeout,
                force=args.force_download,
                retries=args.download_retries,
            )
            for table_id in ACS_TABLE_IDS
        }
        acs_estimates = collect_acs_estimates(paths_by_table)
    place_rows = prepare_place_rows(acs_estimates)
    matched_rows, review_rows = match_places(
        town_keys,
        place_rows,
        fuzzy_threshold=args.fuzzy_threshold,
        fuzzy_margin=args.fuzzy_margin,
    )
    write_csv(args.output, matched_rows, OUTPUT_FIELDS)
    write_csv(args.review, review_rows, REVIEW_FIELDS)
    write_summary(
        args.summary,
        year=args.year,
        town_keys=town_keys,
        place_rows=place_rows,
        matched_rows=matched_rows,
        review_rows=review_rows,
        output_path=args.output,
        review_path=args.review,
    )
    print(f"Wrote ACS town covariates to {args.output}")
    print(f"Wrote ACS match review file to {args.review}")
    print(f"Wrote ACS summary to {args.summary}")


if __name__ == "__main__":
    main()
