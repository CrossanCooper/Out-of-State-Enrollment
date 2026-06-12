#!/usr/bin/env python3
#=====================================================================
## Author: Crossan Cooper
## Last Modified: 06-12-2026
#
## file use: Builds the cleaned state-by-cohort data panels and 2000
## peer-flagship market-share instrument used by the UA location-spillover
## design. Regression diagnostics and figures are produced in the companion
## R script estimate_market_iv_diagnostics.R.
#
## workflow position:
## 1. Run this first. Its market-IV panel is the shared input for the
##    diagnostics script and the linked-only spillover estimator.
#
## inputs:
## 1. ipeds.duckdb -- IPEDS EF_C residence records.
## 2. flagship_oos_share_crosswalk_expanded.csv -- public flagship unitid crosswalk.
## 3. ua_students_parsed.csv -- parsed UA commencement records.
#
## outputs:
## 1. data/market_iv_panel.csv -- state-by-entering-cohort IV panel.
## 2. data/ua_ipeds_origin_entry_panel.csv -- UA IPEDS origin panel.
## 3. data/ua_commencement_origin_panel.csv -- parsed commencement origin panel.
## 4. tables/market_exposure_by_state.csv -- baseline market exposures.
#=====================================================================
"""Build production market-exposure IV inputs for UA nonresident enrollment.

The script reads IPEDS residence data from the shared main-project
``data/pgp-ipeds`` folder and reads the existing flagship crosswalk from this
working repo. It writes only data products used by later R scripts. Regression
diagnostics, Markdown summaries, and PNG figures are intentionally handled in R
so those steps use ``fixest::feols()`` rather than custom Python regressions.

The core comparison is between a baseline-feeder exposure and a new-market
exposure. The latter is based on pre-period flows from each origin state to
other public flagships, excluding UA and other Alabama institutions in the
preferred version, so that it measures latent demand for nonresident flagship
attendance rather than pre-existing attachment to UA.
"""

from __future__ import annotations

import os
import re
from pathlib import Path

import duckdb
import pandas as pd


#=====================================================================
# 1 - Define project paths and runtime configuration
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
DATA_DIR = OUTPUT_ROOT / "data"
TABLE_DIR = OUTPUT_ROOT / "tables"


#=====================================================================
# 2 - Define inputs and externally calibrated constants
#=====================================================================

### i. Input locations can be overridden for other machines or build systems.
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
UA_COMMENCEMENT_PATH = (
    Path(
        os.environ.get(
            "UA_COMMENCEMENT_PATH",
            ADMISSIONS_PROJECT_CODEX_ROOT
            / "ua_scraping"
            / "data"
            / "generated_files"
            / "ua_students_parsed.csv",
        )
    )
)

UA_UNITID = 100751
UA_STATE = "AL"
PRE_YEARS = (2000,)
ENTRY_START_YEAR = 2002
ENTRY_END_YEAR = 2020
IPEDS_READ_START_YEAR = min(ENTRY_START_YEAR, min(PRE_YEARS))
IPEDS_PANEL_START_YEAR = IPEDS_READ_START_YEAR
GRAD_START_YEAR = ENTRY_START_YEAR + 4
GRAD_END_YEAR = ENTRY_END_YEAR + 4

### ii. FIPS mapping follows IPEDS EF_C state residence coding.
# FIPS state codes used by IPEDS EF_C for the 50 states and DC. Territories,
# foreign students, unknown state, and total rows are excluded from the design
# panel because the spillover model is about domestic destination states.
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
OOS_STATES = [state for state in sorted(STATE_TO_FIPS) if state != UA_STATE]


#=====================================================================
# 3 - Validate inputs and define shared helpers
#=====================================================================

def ensure_output_dirs() -> None:
    """Create all output directories used by the script."""

    for path in (OUTPUT_ROOT, DATA_DIR, TABLE_DIR):
        path.mkdir(parents=True, exist_ok=True)


def require_inputs() -> None:
    """Fail early with explicit paths if required inputs are unavailable."""

    for path in (DB_PATH, FLAGSHIP_CROSSWALK_PATH, UA_COMMENCEMENT_PATH):
        if not path.exists():
            raise FileNotFoundError(f"Missing required input: {path}")


def domestic_state_grid(year_col: str, years: range) -> pd.DataFrame:
    """Return a complete state-by-year panel excluding Alabama."""

    return pd.MultiIndex.from_product(
        [OOS_STATES, list(years)],
        names=["origin_state", year_col],
    ).to_frame(index=False)


#=====================================================================
# 4 - Read IPEDS residence data and construct baseline exposures
#=====================================================================

def load_flagship_crosswalk() -> pd.DataFrame:
    """Load the existing expanded flagship crosswalk and validate UA is present."""

    crosswalk = pd.read_csv(FLAGSHIP_CROSSWALK_PATH)
    required = {
        "input_name",
        "matched_institution_name",
        "unitid",
        "state",
        "fips_state",
        "expanded_group",
    }
    missing = required - set(crosswalk.columns)
    if missing:
        raise ValueError(f"Flagship crosswalk missing columns: {sorted(missing)}")

    if UA_UNITID not in set(crosswalk["unitid"]):
        raise ValueError("Expanded flagship crosswalk does not include UA unitid 100751.")

    return crosswalk


def read_ipeds_residence(con: duckdb.DuckDBPyConnection, crosswalk: pd.DataFrame) -> pd.DataFrame:
    """Read state-origin first-time student counts for expanded flagships.

    In the local IPEDS EF_C table, 2000--2001 store the state code in ``line``
    while later years store it in ``efcstate``. The coalesced origin code lets a
    2000 baseline use the same FIPS mapping as the later residence files.
    """

    con.register("flagship_crosswalk", crosswalk)
    domestic_tuple = "(" + ",".join(str(x) for x in DOMESTIC_FIPS) + ")"
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
            WHERE c.year BETWEEN {IPEDS_READ_START_YEAR} AND {ENTRY_END_YEAR}
        )
        SELECT *
        FROM residence
        WHERE origin_fips IN {domestic_tuple}
    """
    flows = con.execute(query).df()
    flows["origin_state"] = flows["origin_fips"].map(FIPS_TO_STATE)
    return flows


def build_ua_ipeds_panel(flows: pd.DataFrame) -> pd.DataFrame:
    """Build the UA entering-cohort origin-share panel from IPEDS residence data."""

    ua = flows.loc[flows["unitid"] == UA_UNITID].copy()
    ### i. Collapse any duplicate EF_C rows to one UA origin count per entry year.
    counts = (
        ua.groupby(["entry_year", "origin_state"], as_index=False)["first_time_count"]
        .sum()
        .rename(columns={"first_time_count": "ua_ipeds_state_count"})
    )

    ### ii. Use a complete state-year grid so missing origin flows are zeros.
    grid = domestic_state_grid("entry_year", range(IPEDS_PANEL_START_YEAR, ENTRY_END_YEAR + 1))
    panel = grid.merge(counts, on=["entry_year", "origin_state"], how="left")
    panel["ua_ipeds_state_count"] = panel["ua_ipeds_state_count"].fillna(0.0)

    ### iii. Compute UA domestic, Alabama, and nonresident totals by entry cohort.
    totals = (
        ua.groupby("entry_year", as_index=False)
        .agg(
            ua_ipeds_domestic_total=("first_time_count", "sum"),
            ua_ipeds_alabama_count=(
                "first_time_count",
                lambda x: float(
                    ua.loc[x.index, "origin_state"].eq(UA_STATE).mul(x).sum()
                ),
            ),
        )
    )
    totals["ua_ipeds_oos_total"] = (
        totals["ua_ipeds_domestic_total"] - totals["ua_ipeds_alabama_count"]
    )
    totals["ua_ipeds_oos_share_domestic"] = (
        totals["ua_ipeds_oos_total"] / totals["ua_ipeds_domestic_total"]
    )

    panel = panel.merge(totals, on="entry_year", how="left")
    ### iv. Store both all-domestic and out-of-state origin shares for diagnostics.
    panel["ua_ipeds_state_share_all_domestic"] = (
        panel["ua_ipeds_state_count"] / panel["ua_ipeds_domestic_total"]
    )
    panel["ua_ipeds_state_share_oos"] = (
        panel["ua_ipeds_state_count"] / panel["ua_ipeds_oos_total"]
    )
    panel["grad_year"] = panel["entry_year"] + 4
    panel["ua_ipeds_state_share_all_domestic_pct"] = (
        100 * panel["ua_ipeds_state_share_all_domestic"]
    )
    panel["ua_ipeds_state_share_oos_pct"] = 100 * panel["ua_ipeds_state_share_oos"]
    panel["ua_ipeds_oos_share_domestic_pct"] = (
        100 * panel["ua_ipeds_oos_share_domestic"]
    )
    return panel


def build_market_exposures(flows: pd.DataFrame) -> pd.DataFrame:
    """Compute pre-period UA baseline and peer-flagship market exposures."""

    pre = flows.loc[flows["entry_year"].isin(PRE_YEARS)].copy()

    ### i. UA's 2000 nonresident origin shares are kept for comparison only.
    ua_pre = pre.loc[
        (pre["unitid"] == UA_UNITID) & (pre["origin_state"] != UA_STATE)
    ]
    ua_state_pre = (
        ua_pre.groupby("origin_state", as_index=False)["first_time_count"]
        .sum()
        .rename(columns={"first_time_count": "ua_pre_oos_count"})
    )
    ua_pre_total = float(ua_state_pre["ua_pre_oos_count"].sum())
    ua_state_pre["ua_baseline_share_pre"] = ua_state_pre["ua_pre_oos_count"] / ua_pre_total

    ### ii. Peer-market exposure uses origin-state flows to other public flagships.
    peer_pre_all = pre.loc[
        (pre["unitid"] != UA_UNITID)
        & (pre["origin_state"] != UA_STATE)
        & (pre["origin_fips"] != pre["institution_fips"])
    ].copy()
    ### iii. The preferred exposure excludes Alabama institutions from the peer market.
    peer_pre_non_al = peer_pre_all.loc[peer_pre_all["institution_state"] != UA_STATE].copy()

    def peer_exposure(df: pd.DataFrame, count_name: str, share_name: str) -> pd.DataFrame:
        """Convert pooled peer flows into origin-state shares of the peer market."""

        state_counts = (
            df.groupby("origin_state", as_index=False)["first_time_count"]
            .sum()
            .rename(columns={"first_time_count": count_name})
        )
        total = float(state_counts[count_name].sum())
        state_counts[share_name] = state_counts[count_name] / total
        return state_counts

    peer_all = peer_exposure(
        peer_pre_all,
        "peer_flagship_pre_oos_count_all_nonua",
        "peer_flagship_flow_share_pre_all_nonua",
    )
    peer_non_al = peer_exposure(
        peer_pre_non_al,
        "peer_flagship_pre_oos_count_non_alabama",
        "peer_flagship_flow_share_pre_non_alabama",
    )

    exposures = pd.DataFrame({"origin_state": OOS_STATES})
    exposures = exposures.merge(ua_state_pre, on="origin_state", how="left")
    exposures = exposures.merge(peer_all, on="origin_state", how="left")
    exposures = exposures.merge(peer_non_al, on="origin_state", how="left")
    count_cols = [col for col in exposures.columns if col.endswith("_count")]
    share_cols = [col for col in exposures.columns if col.endswith("_share_pre")]
    ### iv. States without positive 2000 flows remain in the balanced panel at zero.
    exposures[count_cols + share_cols] = exposures[count_cols + share_cols].fillna(0.0)

    ### v. Underpenetration measures are diagnostic gaps between peer and UA shares.
    exposures["underpenetrated_gap_all_nonua"] = (
        exposures["peer_flagship_flow_share_pre_all_nonua"]
        - exposures["ua_baseline_share_pre"]
    )
    exposures["underpenetrated_gap_non_alabama"] = (
        exposures["peer_flagship_flow_share_pre_non_alabama"]
        - exposures["ua_baseline_share_pre"]
    )
    exposures["underpenetrated_positive_all_nonua"] = exposures[
        "underpenetrated_gap_all_nonua"
    ].clip(lower=0.0)
    exposures["underpenetrated_positive_non_alabama"] = exposures[
        "underpenetrated_gap_non_alabama"
    ].clip(lower=0.0)

    return exposures


#=====================================================================
# 5 - Build commencement and shift-share instrument panels
#=====================================================================

def parse_hometown_state(hometown: object) -> str | None:
    """Extract a two-letter domestic state abbreviation from a commencement hometown."""

    if not isinstance(hometown, str):
        return None
    value = hometown.strip()
    ### i. Parsed commencement hometowns typically end as "City, ST".
    match = re.search(r",\s*([A-Z]{2})\s*$", value)
    if not match:
        return None
    state = match.group(1)
    return state if state in STATE_TO_FIPS else None


def build_commencement_panel() -> pd.DataFrame:
    """Build a graduation-year origin-share panel from UA commencement records."""

    usecols = ["grad_year", "degree", "hometown"]
    raw = pd.read_csv(UA_COMMENCEMENT_PATH, usecols=usecols)
    ### i. Restrict to undergraduate cohorts that line up with 2002--2020 entry years.
    raw = raw.loc[
        raw["grad_year"].between(GRAD_START_YEAR, GRAD_END_YEAR)
        & raw["degree"].astype(str).str.contains("bachelor", case=False, na=False)
    ].copy()
    all_bachelors = (
        raw.groupby("grad_year", as_index=False)
        .size()
        .rename(columns={"size": "commencement_all_bachelors_total"})
    )
    ### ii. Only hometowns with a parseable domestic state enter origin counts.
    raw["origin_state"] = raw["hometown"].map(parse_hometown_state)
    domestic = raw.loc[raw["origin_state"].notna()].copy()

    ### iii. Collapse parsed records to state-by-graduation-year counts.
    counts = (
        domestic.groupby(["grad_year", "origin_state"], as_index=False)
        .size()
        .rename(columns={"size": "commencement_state_count"})
    )

    grid = domestic_state_grid("grad_year", range(GRAD_START_YEAR, GRAD_END_YEAR + 1))
    panel = grid.merge(counts, on=["grad_year", "origin_state"], how="left")
    panel["commencement_state_count"] = panel["commencement_state_count"].fillna(0.0)

    ### iv. Parse coverage is reported so changes in hometown parsing are visible.
    totals = (
        domestic.groupby("grad_year", as_index=False)
        .agg(
            commencement_domestic_total=("origin_state", "size"),
            commencement_alabama_count=(
                "origin_state",
                lambda x: float((x == UA_STATE).sum()),
            ),
        )
    )
    totals["commencement_oos_total"] = (
        totals["commencement_domestic_total"] - totals["commencement_alabama_count"]
    )
    totals["commencement_oos_share_domestic"] = (
        totals["commencement_oos_total"] / totals["commencement_domestic_total"]
    )
    totals = totals.merge(all_bachelors, on="grad_year", how="left")
    totals["commencement_state_parse_coverage"] = (
        totals["commencement_domestic_total"] / totals["commencement_all_bachelors_total"]
    )

    panel = panel.merge(totals, on="grad_year", how="left")
    panel["commencement_state_share_all_domestic"] = (
        panel["commencement_state_count"] / panel["commencement_domestic_total"]
    )
    panel["commencement_state_share_oos"] = (
        panel["commencement_state_count"] / panel["commencement_oos_total"]
    )
    panel["entry_year"] = panel["grad_year"] - 4
    panel["commencement_state_share_all_domestic_pct"] = (
        100 * panel["commencement_state_share_all_domestic"]
    )
    panel["commencement_state_share_oos_pct"] = (
        100 * panel["commencement_state_share_oos"]
    )
    panel["commencement_oos_share_domestic_pct"] = (
        100 * panel["commencement_oos_share_domestic"]
    )
    return panel


def add_instruments(ua_panel: pd.DataFrame, exposures: pd.DataFrame) -> pd.DataFrame:
    """Interact pre-period exposures with UA aggregate nonresident expansion."""

    panel = ua_panel.merge(exposures, on="origin_state", how="left")

    ### i. Leave-state-out growth removes each origin's own UA inflow from the shift.
    pre_totals = (
        panel.loc[panel["entry_year"].isin(PRE_YEARS)]
        .groupby("origin_state", as_index=False)
        .agg(
            pre_leave_state_out_oos_total=(
                "ua_ipeds_oos_total",
                lambda x: float(x.mean()),
            ),
            pre_leave_state_out_state_count=(
                "ua_ipeds_state_count",
                lambda x: float(x.mean()),
            ),
        )
    )
    pre_totals["pre_leave_state_out_oos_total"] = (
        pre_totals["pre_leave_state_out_oos_total"]
        - pre_totals["pre_leave_state_out_state_count"]
    )

    ### ii. Aggregate growth variants are retained as diagnostics alongside LSO growth.
    aggregate_pre = (
        panel.loc[panel["entry_year"].isin(PRE_YEARS)]
        .drop_duplicates("entry_year")["ua_ipeds_oos_total"]
        .mean()
    )
    aggregate_share_pre = (
        panel.loc[panel["entry_year"].isin(PRE_YEARS)]
        .drop_duplicates("entry_year")["ua_ipeds_oos_share_domestic"]
        .mean()
    )

    panel = panel.merge(
        pre_totals[["origin_state", "pre_leave_state_out_oos_total"]],
        on="origin_state",
        how="left",
    )
    panel["ua_oos_count_growth"] = panel["ua_ipeds_oos_total"] - aggregate_pre
    panel["ua_oos_share_growth"] = (
        panel["ua_ipeds_oos_share_domestic"] - aggregate_share_pre
    )
    panel["ua_oos_count_growth_leave_state_out"] = (
        panel["ua_ipeds_oos_total"]
        - panel["ua_ipeds_state_count"]
        - panel["pre_leave_state_out_oos_total"]
    )

    ### iii. Each IV equals a fixed 2000 exposure times UA nonresident growth.
    exposure_cols = {
        "ua_baseline_share_pre": "z_ua_baseline",
        "peer_flagship_flow_share_pre_all_nonua": "z_peer_all_nonua",
        "peer_flagship_flow_share_pre_non_alabama": "z_peer_non_alabama",
        "underpenetrated_gap_non_alabama": "z_underpenetrated_signed_non_alabama",
        "underpenetrated_positive_non_alabama": "z_underpenetrated_positive_non_alabama",
    }
    for exposure_col, prefix in exposure_cols.items():
        panel[f"{prefix}_count_growth"] = (
            panel[exposure_col] * panel["ua_oos_count_growth"]
        )
        panel[f"{prefix}_count_growth_lso"] = (
            panel[exposure_col] * panel["ua_oos_count_growth_leave_state_out"]
        )
        panel[f"{prefix}_share_growth"] = (
            panel[exposure_col] * panel["ua_oos_share_growth"]
        )

    return panel


def merge_commencement(panel: pd.DataFrame, commencement: pd.DataFrame) -> pd.DataFrame:
    """Attach graduation-cohort origin shares to the instrument panel."""

    keep_cols = [
        "origin_state",
        "entry_year",
        "grad_year",
        "commencement_state_count",
        "commencement_all_bachelors_total",
        "commencement_state_parse_coverage",
        "commencement_domestic_total",
        "commencement_oos_total",
        "commencement_state_share_all_domestic_pct",
        "commencement_state_share_oos_pct",
        "commencement_oos_share_domestic_pct",
    ]
    return panel.merge(
        commencement[keep_cols],
        on=["origin_state", "entry_year", "grad_year"],
        how="left",
    )


#=====================================================================
# 6 - Execute production data build
#=====================================================================

def main() -> None:
    """Run the full market-IV build and write production data products."""

    ensure_output_dirs()
    require_inputs()

    ### i. Read source data once, then pass in-memory panels through pure builders.
    crosswalk = load_flagship_crosswalk()
    with duckdb.connect(str(DB_PATH), read_only=True) as con:
        flows = read_ipeds_residence(con, crosswalk)

    ua_ipeds = build_ua_ipeds_panel(flows)
    exposures = build_market_exposures(flows)
    commencement = build_commencement_panel()
    panel = add_instruments(ua_ipeds, exposures)
    panel = merge_commencement(panel, commencement)

    ### ii. Data outputs feed the estimation scripts.
    ua_ipeds.to_csv(DATA_DIR / "ua_ipeds_origin_entry_panel.csv", index=False)
    commencement.to_csv(DATA_DIR / "ua_commencement_origin_panel.csv", index=False)
    panel.to_csv(DATA_DIR / "market_iv_panel.csv", index=False)
    ### iii. Baseline exposure output feeds the R diagnostic and plotting script.
    exposures.to_csv(TABLE_DIR / "market_exposure_by_state.csv", index=False)

    print(f"Wrote market-IV panel outputs under {DATA_DIR}")
    print(f"Wrote market exposure table: {TABLE_DIR / 'market_exposure_by_state.csv'}")
    print("Run estimate_market_iv_diagnostics.R next for diagnostics and figures.")


if __name__ == "__main__":
    main()
