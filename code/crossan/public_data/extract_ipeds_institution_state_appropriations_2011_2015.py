#!/usr/bin/env python3
#=====================================================================
## Created by: Codex for Crossan Cooper
## Last Modified: 6-9-26
##
## build institution-level 2011-2015 state appropriations measures
## from the local IPEDS DuckDB for the public Chetty sample
##
## inputs:
## 1. admissions_project/data/CollegeAdmissions_Data.csv
## 2. admissions_project/data/appropriations_data.csv
## 3. pgp-ipeds/ipeds-database/ipeds.duckdb
##
## outputs:
## 1. output/tables/chetty_ipeds_institution_crosswalk.csv
## 2. output/tables/chetty_ipeds_institution_state_appropriations_2011_2015.csv
##
## notes:
## - preserves the narrow IPEDS state-appropriations measure
## - computes a state-support measure equal to state appropriations
##   + state operating grants and contracts
## - also computes a broader operating-support measure equal to
##   state appropriations + state operating grants + local
##   appropriations + local operating grants
#=====================================================================

#=====================================================================
# 0 - imports and paths
#=====================================================================

from __future__ import annotations

import csv
import math
import os
from collections import defaultdict
from pathlib import Path

import duckdb


WINDOW_START = 2011
WINDOW_END = 2015

MANUAL_UNITID_OVERRIDES = {
    "Texas A&M University": 228723,
    "University Of Florida": 134130,
    "University Of Pittsburgh System": 215293,
}


SCRIPT_DIR = Path(__file__).resolve().parent
PROJECT_ROOT = Path(os.environ.get("ADMISSIONS_PROJECT_ROOT", SCRIPT_DIR.parent)).resolve()
SOURCE_ROOT = Path(
    os.environ.get(
        "ADMISSIONS_SOURCE_ROOT",
        "/Users/crossancooper/Dropbox/Professional/active-projects/admissions_project",
    )
).resolve()
OUTPUT_ROOT = Path(
    os.environ.get("ADMISSIONS_OUTPUT_ROOT", str(PROJECT_ROOT / "output"))
).resolve()

CHETTY_PATH = SOURCE_ROOT / "data" / "CollegeAdmissions_Data.csv"
APPROPRIATIONS_PATH = SOURCE_ROOT / "data" / "appropriations_data.csv"
DB_PATH = Path(
    os.environ.get(
        "ADMISSIONS_IPEDS_DUCKDB",
        str(PROJECT_ROOT / "pgp-ipeds" / "ipeds-database" / "ipeds.duckdb"),
    )
).resolve()
OUTPUT_DIR = OUTPUT_ROOT / "tables"

CROSSWALK_OUTPUT_PATH = OUTPUT_DIR / "chetty_ipeds_institution_crosswalk.csv"
MEASURES_OUTPUT_PATH = (
    OUTPUT_DIR / "chetty_ipeds_institution_state_appropriations_2011_2015.csv"
)


#=====================================================================
# 1 - helper functions
#=====================================================================

def mean_or_none(values: list[float]) -> float | None:
    if not values:
        return None
    return sum(values) / len(values)


def read_chetty_public_institutions() -> list[dict[str, int | str]]:
    institutions: dict[tuple[str, int], dict[str, int | str]] = {}

    with CHETTY_PATH.open(newline="", encoding="utf-8-sig") as handle:
        reader = csv.DictReader(handle)
        for row in reader:
            if row.get("public") != "Public":
                continue

            name = row["name"].strip()
            super_opeid = int(row["super_opeid"])
            key = (name, super_opeid)

            institutions[key] = {
                "chetty_name": name,
                "super_opeid": super_opeid,
                "opeid_guess": super_opeid * 100,
            }

    return sorted(
        institutions.values(),
        key=lambda row: str(row["chetty_name"]),
    )


def read_inflation_lookup() -> dict[int, dict[str, float]]:
    heca_by_year: dict[int, set[float]] = defaultdict(set)
    cpi_by_year: dict[int, set[float]] = defaultdict(set)

    with APPROPRIATIONS_PATH.open(newline="", encoding="utf-8-sig") as handle:
        reader = csv.DictReader(handle)
        for row in reader:
            fy_raw = row.get("FY", "").strip()
            if not fy_raw.isdigit():
                continue

            fy = int(fy_raw)
            if fy < WINDOW_START or fy > WINDOW_END:
                continue

            heca_by_year[fy].add(float(row["HECA (Inflation) Adjustment"]))
            cpi_by_year[fy].add(float(row["CPI (Inflation) Adjustment"]))

    inflation_lookup: dict[int, dict[str, float]] = {}

    for year in range(WINDOW_START, WINDOW_END + 1):
        if len(heca_by_year[year]) != 1 or len(cpi_by_year[year]) != 1:
            raise ValueError(f"Could not identify unique inflation factors for {year}.")

        inflation_lookup[year] = {
            "heca": next(iter(heca_by_year[year])),
            "cpi": next(iter(cpi_by_year[year])),
        }

    return inflation_lookup


def resolve_crosswalk(
    con: duckdb.DuckDBPyConnection,
    institutions: list[dict[str, int | str]],
) -> list[dict[str, int | str]]:
    hd_rows = con.execute(
        """
        SELECT
            unitid,
            institution_name,
            state,
            control,
            CAST(opeid AS BIGINT) AS opeid
        FROM hd
        WHERE year = 2015 AND control = 1
        """
    ).fetchall()

    by_unitid = {int(row[0]): row for row in hd_rows}
    by_opeid: dict[int, list[tuple[int, str, str, int, int]]] = defaultdict(list)

    for row in hd_rows:
        if row[4] is not None:
            by_opeid[int(row[4])].append(row)

    crosswalk: list[dict[str, int | str]] = []

    for institution in institutions:
        chetty_name = str(institution["chetty_name"])
        super_opeid = int(institution["super_opeid"])
        opeid_guess = int(institution["opeid_guess"])

        match_row = None
        match_type = None

        if chetty_name in MANUAL_UNITID_OVERRIDES:
            unitid = MANUAL_UNITID_OVERRIDES[chetty_name]
            match_row = by_unitid.get(unitid)
            match_type = "manual_unitid"

        if match_row is None:
            candidates = by_opeid.get(opeid_guess, [])

            filtered_candidates = [
                row
                for row in candidates
                if "System Office" not in row[1] and "Online" not in row[1]
            ]

            if len(filtered_candidates) == 1:
                match_row = filtered_candidates[0]
                match_type = "opeid_match"
            elif len(candidates) == 1:
                match_row = candidates[0]
                match_type = "opeid_match_unfiltered"

        if match_row is None:
            raise ValueError(f"Could not match Chetty institution: {chetty_name}")

        crosswalk.append(
            {
                "chetty_name": chetty_name,
                "super_opeid": super_opeid,
                "opeid_guess": opeid_guess,
                "unitid": int(match_row[0]),
                "ipeds_institution_name": str(match_row[1]),
                "state_abbr": str(match_row[2]),
                "control": int(match_row[3]),
                "opeid": int(match_row[4]),
                "match_type": match_type,
            }
        )

    return sorted(crosswalk, key=lambda row: str(row["chetty_name"]))


def build_counts_lookup(
    con: duckdb.DuckDBPyConnection,
    table_name: str,
    unitids: list[int],
) -> dict[int, int]:
    if not unitids:
        return {}

    unitid_clause = ", ".join(str(unitid) for unitid in sorted(set(unitids)))

    query = f"""
        SELECT unitid, COUNT(*) AS n_years
        FROM {table_name}
        WHERE unitid IN ({unitid_clause})
          AND year BETWEEN {WINDOW_START} AND {WINDOW_END}
        GROUP BY unitid
    """

    rows = con.execute(query).fetchall()
    return {int(row[0]): int(row[1]) for row in rows}


def build_institution_measures(
    con: duckdb.DuckDBPyConnection,
    crosswalk: list[dict[str, int | str]],
    inflation_lookup: dict[int, dict[str, float]],
) -> tuple[list[dict[str, int | str]], list[dict[str, int | str | float]]]:
    unitids = [int(row["unitid"]) for row in crosswalk]
    unitid_clause = ", ".join(str(unitid) for unitid in sorted(set(unitids)))

    f1a_counts = build_counts_lookup(con, "f1a", unitids)
    f2_counts = build_counts_lookup(con, "f2", unitids)
    efia_counts = build_counts_lookup(con, "efia", unitids)

    finance_rows = con.execute(
        f"""
        SELECT
            finance.unitid,
            finance.year,
            finance.finance_source,
            finance.state_appropriations,
            finance.state_support,
            finance.broad_operating_support,
            CAST(e.efteug AS DOUBLE) AS efteug,
            CAST(e.eftegd AS DOUBLE) AS eftegd
        FROM (
            SELECT
                unitid,
                year,
                'f1a' AS finance_source,
                CAST(f1b11 AS DOUBLE) AS state_appropriations,
                CAST(
                    COALESCE(f1b03, 0) +
                    COALESCE(f1b11, 0) AS DOUBLE
                ) AS state_support,
                CAST(
                    COALESCE(f1b03, 0) +
                    COALESCE(f1b04a, 0) +
                    COALESCE(f1b11, 0) +
                    COALESCE(f1b12, 0) AS DOUBLE
                ) AS broad_operating_support
            FROM f1a
            WHERE unitid IN ({unitid_clause})
              AND year BETWEEN {WINDOW_START} AND {WINDOW_END}

            UNION ALL

            SELECT
                unitid,
                year,
                'f2' AS finance_source,
                CAST(f2d03 AS DOUBLE) AS state_appropriations,
                CAST(
                    COALESCE(f2d03, 0) +
                    COALESCE(f2d06, 0) AS DOUBLE
                ) AS state_support,
                CAST(
                    COALESCE(f2d03, 0) +
                    COALESCE(f2d04, 0) +
                    COALESCE(f2d06, 0) +
                    COALESCE(f2d07, 0) AS DOUBLE
                ) AS broad_operating_support
            FROM f2
            WHERE unitid IN ({unitid_clause})
              AND year BETWEEN {WINDOW_START} AND {WINDOW_END}
        ) finance
        LEFT JOIN efia e
          ON finance.unitid = e.unitid AND finance.year = e.year
        ORDER BY finance.unitid, finance.year, finance.finance_source
        """
    ).fetchall()

    rows_by_unitid: dict[
        int,
        list[
            tuple[
                int,
                str,
                float | None,
                float | None,
                float | None,
                float | None,
                float | None,
            ]
        ],
    ] = defaultdict(list)

    for row in finance_rows:
        rows_by_unitid[int(row[0])].append(
            (int(row[1]), str(row[2]), row[3], row[4], row[5], row[6], row[7])
        )

    measures: list[dict[str, int | str | float]] = []

    for row in crosswalk:
        unitid = int(row["unitid"])
        yearly_rows = rows_by_unitid.get(unitid, [])

        usable_nominal = []
        usable_heca = []
        usable_nominal_per_student = []
        usable_heca_per_student = []
        usable_state_support_nominal = []
        usable_state_support_heca = []
        usable_state_support_nominal_per_student = []
        usable_state_support_heca_per_student = []
        usable_broad_nominal = []
        usable_broad_heca = []
        usable_broad_nominal_per_student = []
        usable_broad_heca_per_student = []
        usable_fte = []

        finance_source_used = None

        for (
            year,
            finance_source,
            state_appropriations,
            state_support,
            broad_operating_support,
            efteug,
            eftegd,
        ) in yearly_rows:
            fte_components = [value for value in (efteug, eftegd) if value is not None]
            fte_total = sum(fte_components) if fte_components else None

            if (
                broad_operating_support is None
                or fte_total is None
                or fte_total <= 0
                or year not in inflation_lookup
            ):
                continue

            heca_factor = inflation_lookup[year]["heca"]
            state_support_heca = (
                state_support / heca_factor if state_support is not None else None
            )
            broad_operating_support_heca = broad_operating_support / heca_factor

            if state_appropriations is not None:
                state_appropriations_heca = state_appropriations / heca_factor

                usable_nominal.append(state_appropriations)
                usable_heca.append(state_appropriations_heca)
                usable_nominal_per_student.append(state_appropriations / fte_total)
                usable_heca_per_student.append(state_appropriations_heca / fte_total)

            if state_support is not None:
                usable_state_support_nominal.append(state_support)
                usable_state_support_heca.append(state_support_heca)
                usable_state_support_nominal_per_student.append(state_support / fte_total)
                usable_state_support_heca_per_student.append(
                    state_support_heca / fte_total
                )

            usable_broad_nominal.append(broad_operating_support)
            usable_broad_heca.append(broad_operating_support_heca)
            usable_broad_nominal_per_student.append(broad_operating_support / fte_total)
            usable_broad_heca_per_student.append(broad_operating_support_heca / fte_total)
            usable_fte.append(fte_total)
            finance_source_used = finance_source

        avg_heca_per_student = mean_or_none(usable_heca_per_student)
        avg_state_support_heca_per_student = mean_or_none(
            usable_state_support_heca_per_student
        )
        avg_broad_heca_per_student = mean_or_none(usable_broad_heca_per_student)

        finance_panel = "f1a"
        if f1a_counts.get(unitid, 0) == 0 and f2_counts.get(unitid, 0) > 0:
            finance_panel = "f2_only"
        if f1a_counts.get(unitid, 0) == 0 and f2_counts.get(unitid, 0) == 0:
            finance_panel = "no_finance_rows"

        measures.append(
            {
                "chetty_name": row["chetty_name"],
                "super_opeid": row["super_opeid"],
                "opeid_guess": row["opeid_guess"],
                "unitid": unitid,
                "ipeds_institution_name": row["ipeds_institution_name"],
                "state_abbr": row["state_abbr"],
                "match_type": row["match_type"],
                "finance_panel": finance_panel,
                "finance_source_used": finance_source_used,
                "f1a_years_2011_2015": f1a_counts.get(unitid, 0),
                "f2_years_2011_2015": f2_counts.get(unitid, 0),
                "efia_years_2011_2015": efia_counts.get(unitid, 0),
                "n_usable_years": len(usable_heca_per_student),
                "n_usable_state_support_years": len(usable_state_support_heca_per_student),
                "n_usable_broad_support_years": len(usable_broad_heca_per_student),
                "avg_state_appropriations_nominal": mean_or_none(usable_nominal),
                "avg_state_appropriations_heca": mean_or_none(usable_heca),
                "avg_fte": mean_or_none(usable_fte),
                "avg_appropriations_per_student_nominal": mean_or_none(
                    usable_nominal_per_student
                ),
                "avg_appropriations_per_student_heca": avg_heca_per_student,
                "avgAppPerStudentThousands": (
                    avg_heca_per_student / 1000 if avg_heca_per_student else None
                ),
                "LogAppPerStudent": (
                    math.log(avg_heca_per_student)
                    if avg_heca_per_student is not None and avg_heca_per_student > 0
                    else None
                ),
                "avg_state_support_nominal": mean_or_none(usable_state_support_nominal),
                "avg_state_support_heca": mean_or_none(usable_state_support_heca),
                "avg_state_support_per_student_nominal": mean_or_none(
                    usable_state_support_nominal_per_student
                ),
                "avg_state_support_per_student_heca": mean_or_none(
                    usable_state_support_heca_per_student
                ),
                "avgStateSupportPerStudentThousands": (
                    avg_state_support_heca_per_student / 1000
                    if avg_state_support_heca_per_student is not None
                    else None
                ),
                "LogStateSupportPerStudent": (
                    math.log(avg_state_support_heca_per_student)
                    if avg_state_support_heca_per_student is not None
                    and avg_state_support_heca_per_student > 0
                    else None
                ),
                "avg_broad_operating_support_nominal": mean_or_none(usable_broad_nominal),
                "avg_broad_operating_support_heca": mean_or_none(usable_broad_heca),
                "avg_broad_operating_support_per_student_nominal": mean_or_none(
                    usable_broad_nominal_per_student
                ),
                "avg_broad_operating_support_per_student_heca": mean_or_none(
                    usable_broad_heca_per_student
                ),
                "avgBroadSupportPerStudentThousands": (
                    avg_broad_heca_per_student / 1000
                    if avg_broad_heca_per_student is not None
                    else None
                ),
                "LogBroadSupportPerStudent": (
                    math.log(avg_broad_heca_per_student)
                    if avg_broad_heca_per_student is not None
                    and avg_broad_heca_per_student > 0
                    else None
                ),
                "usable_state_appropriations_2011_2015": int(
                    len(usable_heca_per_student) > 0
                ),
                "usable_state_support_2011_2015": int(
                    len(usable_state_support_heca_per_student) > 0
                ),
                "usable_broad_support_2011_2015": int(
                    len(usable_broad_heca_per_student) > 0
                ),
            }
        )

    crosswalk_with_coverage: list[dict[str, int | str]] = []
    measures_by_unitid = {int(row["unitid"]): row for row in measures}

    for row in crosswalk:
        measure_row = measures_by_unitid[int(row["unitid"])]
        crosswalk_with_coverage.append(
            {
                **row,
                "finance_panel": measure_row["finance_panel"],
                "finance_source_used": measure_row["finance_source_used"],
                "f1a_years_2011_2015": measure_row["f1a_years_2011_2015"],
                "f2_years_2011_2015": measure_row["f2_years_2011_2015"],
                "efia_years_2011_2015": measure_row["efia_years_2011_2015"],
                "n_usable_years": measure_row["n_usable_years"],
                "n_usable_state_support_years": measure_row[
                    "n_usable_state_support_years"
                ],
                "n_usable_broad_support_years": measure_row[
                    "n_usable_broad_support_years"
                ],
                "usable_state_appropriations_2011_2015": measure_row[
                    "usable_state_appropriations_2011_2015"
                ],
                "usable_state_support_2011_2015": measure_row[
                    "usable_state_support_2011_2015"
                ],
                "usable_broad_support_2011_2015": measure_row[
                    "usable_broad_support_2011_2015"
                ],
            }
        )

    return crosswalk_with_coverage, sorted(
        measures,
        key=lambda measure: (str(measure["state_abbr"]), str(measure["chetty_name"])),
    )


def write_csv(path: Path, rows: list[dict[str, int | str | float]]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)

    if not rows:
        raise ValueError(f"No rows to write for {path}")

    fieldnames = list(rows[0].keys())
    with path.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(rows)


#=====================================================================
# 2 - main
#=====================================================================

def main() -> None:
    if not CHETTY_PATH.exists():
        raise FileNotFoundError(f"Could not find CollegeAdmissions_Data.csv at {CHETTY_PATH}")

    if not APPROPRIATIONS_PATH.exists():
        raise FileNotFoundError(
            f"Could not find appropriations_data.csv at {APPROPRIATIONS_PATH}"
        )

    if not DB_PATH.exists():
        raise FileNotFoundError(f"Could not find ipeds.duckdb at {DB_PATH}")

    institutions = read_chetty_public_institutions()
    inflation_lookup = read_inflation_lookup()

    con = duckdb.connect(str(DB_PATH), read_only=True)
    try:
        crosswalk = resolve_crosswalk(con, institutions)
        crosswalk_with_coverage, measures = build_institution_measures(
            con,
            crosswalk,
            inflation_lookup,
        )
    finally:
        con.close()

    write_csv(CROSSWALK_OUTPUT_PATH, crosswalk_with_coverage)
    write_csv(MEASURES_OUTPUT_PATH, measures)

    usable_count = sum(
        int(row["usable_state_appropriations_2011_2015"]) for row in crosswalk_with_coverage
    )

    print(
        f"Matched {len(crosswalk_with_coverage)} public Chetty institutions; "
        f"{usable_count} have usable 2011-2015 institution-level state appropriations."
    )


if __name__ == "__main__":
    main()
