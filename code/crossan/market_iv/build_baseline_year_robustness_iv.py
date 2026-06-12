#!/usr/bin/env python3
#=====================================================================
## Author: Crossan Cooper
## Last Modified: 06-12-2026
#
## file use: Builds alternative-baseline leave-state-out market IV panels
## for robustness checks of the linked-only location-spillover model. This
## script only constructs IV data; regressions are estimated in R with fixest.
#
## workflow position:
## 1. Run after the preferred market-IV build when preparing robustness checks.
## 2. Run before estimate_baseline_year_robustness.R.
#
## inputs:
## 1. ipeds.duckdb -- IPEDS EF_C residence records.
## 2. flagship_oos_share_crosswalk_expanded.csv -- public flagship unitid crosswalk.
#
## outputs:
## 1. baseline_year_robustness_z_panel.csv -- alternative-baseline IV panel.
## 2. baseline_year_ipeds_coverage.csv -- early-year IPEDS coverage summary.
#=====================================================================
"""Build leave-state-out market IVs under alternative baseline years.

The main market-IV builder uses the 2000 IPEDS residence file for the baseline
share. This robustness script checks whether adjacent early years can be used in
the same construction. The shared IPEDS ``ef_c`` table stores origin-state codes
in ``line`` rather than ``efcstate`` in 2000--2001, so this script explicitly
uses ``coalesce(efcstate, line)`` when reading residence flows.

Outputs are written under the main admissions project ``data/market_iv`` folder
and do not include memo PDF or TeX artifacts.
"""

from __future__ import annotations

import os
from pathlib import Path

import duckdb
import pandas as pd


#=====================================================================
# 1 - Define project paths and inputs
#=====================================================================

### i. Resolve project roots from environment variables when supplied.
SCRIPT_PATH = Path(__file__).resolve()
ACTIVE_PROJECTS_ROOT = Path(
    os.environ.get(
        "ACTIVE_PROJECTS_ROOT",
        "/Users/crossancooper/Dropbox/Professional/active-projects",
    )
)
ADMISSIONS_PROJECT_ROOT = Path(
    os.environ.get("ADMISSIONS_PROJECT_ROOT", ACTIVE_PROJECTS_ROOT / "admissions_project")
)
ADMISSIONS_PROJECT_CODEX_ROOT = Path(
    os.environ.get(
        "ADMISSIONS_PROJECT_CODEX_ROOT",
        ACTIVE_PROJECTS_ROOT / "admissions_project_personal" / "admissions-project-codex",
    )
)
SHARED_IPEDS_ROOT = Path(
    os.environ.get("SHARED_IPEDS_ROOT", ADMISSIONS_PROJECT_ROOT / "data" / "pgp-ipeds")
)
OUTPUT_ROOT = Path(
    os.environ.get("MARKET_IV_OUTPUT_ROOT", ADMISSIONS_PROJECT_ROOT / "data" / "market_iv")
)
OUT_DIR = OUTPUT_ROOT / "baseline_year_robustness"

DB_PATH = Path(
    os.environ.get(
        "IPEDS_DUCKDB_PATH",
        SHARED_IPEDS_ROOT / "ipeds-database" / "ipeds.duckdb",
    )
)
FLAGSHIP_CROSSWALK_PATH = (
    Path(
        os.environ.get(
            "FLAGSHIP_CROSSWALK_PATH",
            ADMISSIONS_PROJECT_CODEX_ROOT
            / "output"
            / "tables"
            / "flagship_oos_share_crosswalk_expanded.csv",
        )
    )
)


#=====================================================================
# 2 - Define externally calibrated constants and baseline windows
#=====================================================================

UA_UNITID = 100751
UA_STATE = "AL"
ENTRY_START_YEAR = 2000
ENTRY_END_YEAR = 2020

FIPS_TO_STATE = {
    1: "AL",
    2: "AK",
    4: "AZ",
    5: "AR",
    6: "CA",
    8: "CO",
    9: "CT",
    10: "DE",
    11: "DC",
    12: "FL",
    13: "GA",
    15: "HI",
    16: "ID",
    17: "IL",
    18: "IN",
    19: "IA",
    20: "KS",
    21: "KY",
    22: "LA",
    23: "ME",
    24: "MD",
    25: "MA",
    26: "MI",
    27: "MN",
    28: "MS",
    29: "MO",
    30: "MT",
    31: "NE",
    32: "NV",
    33: "NH",
    34: "NJ",
    35: "NM",
    36: "NY",
    37: "NC",
    38: "ND",
    39: "OH",
    40: "OK",
    41: "OR",
    42: "PA",
    44: "RI",
    45: "SC",
    46: "SD",
    47: "TN",
    48: "TX",
    49: "UT",
    50: "VT",
    51: "VA",
    53: "WA",
    54: "WV",
    55: "WI",
    56: "WY",
}
STATE_TO_FIPS = {state: fips for fips, state in FIPS_TO_STATE.items()}
DOMESTIC_FIPS = sorted(FIPS_TO_STATE)
### i. Alabama is excluded because outcomes are destination states outside Alabama.
OOS_STATES = [state for state in sorted(STATE_TO_FIPS) if state != UA_STATE]

### i. Single years are core checks; pooled years test noisy residence files.
# Single-year baselines are the core comparison. The pooled early-year options
# are useful for checking whether the 2000/2001 results are driven by a noisy
# single residence file.
BASELINES = {
    "2000": (2000,),
    "2001": (2001,),
    "2002": (2002,),
    "2000_2001": (2000, 2001),
    "2000_2002": (2000, 2001, 2002),
}


#=====================================================================
# 3 - Read IPEDS residence data
#=====================================================================

def domestic_tuple() -> str:
    """Return a SQL tuple of domestic FIPS codes."""

    return "(" + ",".join(str(x) for x in DOMESTIC_FIPS) + ")"


def read_ipeds_residence() -> pd.DataFrame:
    """Read flagship residence flows with a 2000--2001 state-code fallback."""

    if not DB_PATH.exists():
        raise FileNotFoundError(DB_PATH)
    if not FLAGSHIP_CROSSWALK_PATH.exists():
        raise FileNotFoundError(FLAGSHIP_CROSSWALK_PATH)

    crosswalk = pd.read_csv(FLAGSHIP_CROSSWALK_PATH)
    con = duckdb.connect(str(DB_PATH), read_only=True)
    con.register("flagship_crosswalk", crosswalk)
    ## (a) Coalesce recovers 2000--2001 state codes, which IPEDS stores in line.
    query = f"""
        WITH residence AS (
            SELECT
                f.unitid,
                f.input_name,
                f.matched_institution_name,
                f.state AS institution_state,
                CAST(f.fips_state AS INTEGER) AS institution_fips,
                CAST(c.year AS INTEGER) AS entry_year,
                CAST(COALESCE(c.efcstate, c.line) AS INTEGER) AS origin_fips,
                CAST(c.efres01 AS DOUBLE) AS first_time_count
            FROM flagship_crosswalk f
            INNER JOIN ef_c c
                ON c.unitid = f.unitid
            WHERE c.year BETWEEN {ENTRY_START_YEAR} AND {ENTRY_END_YEAR}
        )
        SELECT *
        FROM residence
        WHERE origin_fips IN {domestic_tuple()}
    """
    flows = con.execute(query).df()
    ### i. Restricting to domestic FIPS rows above makes this map one-to-one.
    flows["origin_state"] = flows["origin_fips"].map(FIPS_TO_STATE)
    return flows


#=====================================================================
# 4 - Construct alternative-baseline instruments
#=====================================================================

def state_grid(years: range) -> pd.DataFrame:
    """Return a complete non-Alabama state-by-entry-year grid."""

    return pd.MultiIndex.from_product(
        [OOS_STATES, list(years)], names=["origin_state", "entry_year"]
    ).to_frame(index=False)


def build_ua_panel(flows: pd.DataFrame) -> pd.DataFrame:
    """Build UA origin counts and leave-state-out OOS totals."""

    ua = flows.loc[flows["unitid"] == UA_UNITID].copy()
    ### i. Collapse UA residence rows to one count for each origin-by-entry year.
    counts = (
        ua.groupby(["entry_year", "origin_state"], as_index=False)["first_time_count"]
        .sum()
        .rename(columns={"first_time_count": "ua_state_count"})
    )

    ### ii. A balanced grid keeps zero-flow states in all baseline-year variants.
    panel = state_grid(range(ENTRY_START_YEAR, ENTRY_END_YEAR + 1))
    panel = panel.merge(counts, on=["origin_state", "entry_year"], how="left")
    panel["ua_state_count"] = panel["ua_state_count"].fillna(0.0)

    ### iii. Total UA nonresident enrollment is the domestic total net of Alabama.
    totals = (
        ua.groupby("entry_year", as_index=False)
        .apply(
            lambda df: pd.Series(
                {
                    "ua_domestic_total": df["first_time_count"].sum(),
                    "ua_alabama_count": df.loc[
                        df["origin_state"].eq(UA_STATE), "first_time_count"
                    ].sum(),
                }
            ),
            include_groups=False,
        )
        .reset_index(drop=True)
    )
    totals["ua_oos_total"] = totals["ua_domestic_total"] - totals["ua_alabama_count"]
    panel = panel.merge(totals, on="entry_year", how="left")
    ### iv. Leave-state-out total removes origin j from UA's aggregate OOS shift.
    panel["ua_oos_total_lso"] = panel["ua_oos_total"] - panel["ua_state_count"]
    panel["grad_year"] = panel["entry_year"] + 4
    return panel


def peer_share(flows: pd.DataFrame, years: tuple[int, ...]) -> pd.DataFrame:
    """Compute pooled peer-flagship OOS flow shares for a baseline year set."""

    pre = flows.loc[flows["entry_year"].isin(years)].copy()
    ### i. The denominator is all non-Alabama-origin OOS flows to non-Alabama peers.
    peer = pre.loc[
        (pre["unitid"] != UA_UNITID)
        & (pre["institution_state"] != UA_STATE)
        & (pre["origin_state"] != UA_STATE)
        & (pre["origin_fips"] != pre["institution_fips"])
    ].copy()
    counts = (
        peer.groupby("origin_state", as_index=False)["first_time_count"]
        .sum()
        .rename(columns={"first_time_count": "peer_oos_count"})
    )
    ### ii. States with no peer-market flow stay in the panel with share zero.
    out = pd.DataFrame({"origin_state": OOS_STATES})
    out = out.merge(counts, on="origin_state", how="left")
    out["peer_oos_count"] = out["peer_oos_count"].fillna(0.0)
    total = out["peer_oos_count"].sum()
    if total <= 0:
        raise ValueError(f"No peer-flow total for baseline {years}")
    out["peer_share"] = out["peer_oos_count"] / total
    return out


def build_instruments(flows: pd.DataFrame) -> pd.DataFrame:
    """Build long panel of leave-state-out IVs for each baseline definition."""

    ua_panel = build_ua_panel(flows)
    rows = []
    for label, years in BASELINES.items():
        ### i. Recompute both the peer share and UA baseline total for each window.
        shares = peer_share(flows, years)
        baseline_lso = (
            ua_panel.loc[ua_panel["entry_year"].isin(years)]
            .groupby("origin_state", as_index=False)["ua_oos_total_lso"]
            .mean()
            .rename(columns={"ua_oos_total_lso": "baseline_ua_oos_total_lso"})
        )
        this = ua_panel.merge(shares, on="origin_state", how="left").merge(
            baseline_lso, on="origin_state", how="left"
        )
        this["baseline_label"] = label
        this["baseline_years"] = ",".join(str(year) for year in years)
        ### ii. The shift is UA nonresident growth net of origin state's own flow.
        this["ua_oos_growth_lso"] = (
            this["ua_oos_total_lso"] - this["baseline_ua_oos_total_lso"]
        )
        ### iii. The IV is fixed baseline peer share times leave-state-out growth.
        this["z_lso"] = this["peer_share"] * this["ua_oos_growth_lso"]
        rows.append(
            this[
                [
                    "baseline_label",
                    "baseline_years",
                    "origin_state",
                    "entry_year",
                    "grad_year",
                    "peer_oos_count",
                    "peer_share",
                    "baseline_ua_oos_total_lso",
                    "ua_oos_total_lso",
                    "ua_oos_growth_lso",
                    "z_lso",
                ]
            ]
        )
    return pd.concat(rows, ignore_index=True)


#=====================================================================
# 5 - Summarize early-year IPEDS coverage
#=====================================================================

def build_coverage_summary(flows: pd.DataFrame) -> pd.DataFrame:
    """Summarize usable residence coverage around the candidate baselines."""

    ### i. Coverage uses the exact peer-flow filters used in the IV denominator.
    peer_rows = flows.loc[
        (flows["unitid"] != UA_UNITID)
        & (flows["institution_state"] != UA_STATE)
        & (flows["origin_state"] != UA_STATE)
        & (flows["origin_fips"] != flows["institution_fips"])
        & (flows["entry_year"].between(2000, 2004))
    ].copy()
    ua_rows = flows.loc[
        (flows["unitid"] == UA_UNITID) & (flows["entry_year"].between(2000, 2004))
    ].copy()

    ### ii. Institution count and flow totals diagnose thin early IPEDS files.
    peer_summary = (
        peer_rows.groupby("entry_year", as_index=False)
        .agg(
            peer_non_al_institutions=("unitid", "nunique"),
            peer_non_al_oos_from_non_al=("first_time_count", "sum"),
        )
        .rename(columns={"entry_year": "year"})
    )
    ua_summary = (
        ua_rows.groupby("entry_year", as_index=False)
        .apply(
            lambda df: pd.Series(
                {
                    "ua_domestic_total": df["first_time_count"].sum(),
                    "ua_alabama_count": df.loc[
                        df["origin_state"].eq(UA_STATE), "first_time_count"
                    ].sum(),
                    "ua_oos_total": df.loc[
                        ~df["origin_state"].eq(UA_STATE), "first_time_count"
                    ].sum(),
                }
            ),
            include_groups=False,
        )
        .reset_index(drop=True)
        .rename(columns={"entry_year": "year"})
    )
    return peer_summary.merge(ua_summary, on="year", how="outer").sort_values("year")


#=====================================================================
# 6 - Execute robustness panel build
#=====================================================================

def main() -> None:
    """Build alternative-baseline IV panels and coverage summaries."""

    OUT_DIR.mkdir(parents=True, exist_ok=True)
    ### i. The robustness build writes only CSV outputs consumed by the R script.
    flows = read_ipeds_residence()
    z_panel = build_instruments(flows)
    coverage = build_coverage_summary(flows)

    z_panel.to_csv(OUT_DIR / "baseline_year_robustness_z_panel.csv", index=False)
    coverage.to_csv(OUT_DIR / "baseline_year_ipeds_coverage.csv", index=False)
    print("Wrote", OUT_DIR / "baseline_year_robustness_z_panel.csv")
    print("Wrote", OUT_DIR / "baseline_year_ipeds_coverage.csv")
    print(coverage.to_string(index=False))


if __name__ == "__main__":
    main()
