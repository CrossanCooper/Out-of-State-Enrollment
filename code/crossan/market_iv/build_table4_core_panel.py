#!/usr/bin/env python3
#=====================================================================
## Author: Codex, for Crossan Cooper
## Last Modified: 2026-09-27
##
## file use: Create a compact, coauthor-facing Table 4 estimation panel from
## the wider audit panel. Preserve production variable names so existing R
## formulas work without translation.
#
## inputs:
## 1. panel_expanded_peer_groups.csv -- verified 648-row audit panel
#
## outputs:
## 1. table4_peer_controls_core.csv -- 13-column regression extract
## 2. table4_peer_controls_core_dictionary.csv -- machine-readable definitions
## 3. table4_peer_controls_core_README.md -- sample and variable documentation
## 4. table4_peer_controls_core_peer_groups.csv -- exact peer membership
#=====================================================================
from __future__ import annotations

import argparse
import os
from pathlib import Path

import numpy as np
import pandas as pd


#=====================================================================
# 1 - Stable public schema
#=====================================================================

# Keep this order stable: identifiers, outcome/treatment/instrument, economic
# controls, then mutually exclusive peer-share controls.
CORE_COLUMNS = (
    "d_state",
    "state_abbr",
    "grad_y",
    "entry_year",
    "diff_log_d_share",
    "o_share_model",
    "z_lso",
    "ur",
    "net_rate",
    "h_core3",
    "h_core6",
    "h_core10",
    "h_core15",
)

DICTIONARY = {
    "d_state": ("State j: destination for Alabama-origin graduates and origin for UA OOS entrants.", "State name", "Destination fixed effect / cluster"),
    "state_abbr": ("Two-letter abbreviation for d_state.", "State code", "Merge key"),
    "grad_y": ("UA graduation cohort c.", "Calendar year", "Cohort fixed effect"),
    "entry_year": ("Entering cohort t, defined as grad_y - 4.", "Calendar year", "Timing key for IPEDS and peer data"),
    "diff_log_d_share": ("Log destination share minus log Alabama share; equivalently log(N_dest_obs/N_home_obs).", "Natural-log odds", "Outcome"),
    "o_share_model": ("State j's share of UA domestic first-time entrants from IPEDS in entry_year.", "Percent; 1 = one percentage point", "Endogenous regressor"),
    "z_lso": ("Original leave-origin-j-out market-exposure instrument.", "Exposure-weighted entrant-count change", "Excluded instrument"),
    "ur": ("Destination-state annual unemployment rate in grad_y.", "Percent", "Economic control"),
    "net_rate": ("Destination-state net migration: 100 × (arrivals - departures) / lagged population.", "Percent of lagged population", "Economic control"),
    "h_core3": ("Change since 2000 in state j's share of pooled domestic entrants at the fixed 3-school peer group.", "Percentage points", "Alternative peer control"),
    "h_core6": ("Change since 2000 in state j's share of pooled domestic entrants at the fixed 6-school peer group.", "Percentage points", "Alternative peer control"),
    "h_core10": ("Change since 2000 in state j's share of pooled domestic entrants at the fixed 10-school peer group.", "Percentage points", "Alternative peer control"),
    "h_core15": ("Change since 2000 in state j's share of pooled domestic entrants at the fixed 15-school peer group.", "Percentage points", "Alternative peer control"),
}

# Unit IDs and names make the shared regression file interpretable without
# requiring the wider audit directory. Groups are nested by construction.
PEERS = {
    106397: ("University of Arkansas", "AR"),
    157085: ("University of Kentucky", "KY"),
    217882: ("Clemson University", "SC"),
    209551: ("University of Oregon", "OR"),
    234076: ("University of Virginia-Main Campus", "VA"),
    129020: ("University of Connecticut", "CT"),
    215293: ("University of Pittsburgh-Pittsburgh Campus", "PA"),
    166629: ("University of Massachusetts-Amherst", "MA"),
    181464: ("University of Nebraska-Lincoln", "NE"),
    209542: ("Oregon State University", "OR"),
    230764: ("University of Utah", "UT"),
    155317: ("University of Kansas", "KS"),
    176080: ("Mississippi State University", "MS"),
    188030: ("New Mexico State University-Main Campus", "NM"),
    207388: ("Oklahoma State University-Main Campus", "OK"),
}
CORE_THREE = (106397, 157085, 217882)
ADDITIONS = (209551, 234076, 129020, 215293, 166629, 181464, 209542,
             230764, 155317, 176080, 188030, 207388)
GROUPS = {
    "h_core3": CORE_THREE,
    "h_core6": CORE_THREE + ADDITIONS[:3],
    "h_core10": CORE_THREE + ADDITIONS[:7],
    "h_core15": CORE_THREE + ADDITIONS,
}


#=====================================================================
# 2 - Validation and compact-panel construction
#=====================================================================

def build_core_panel(source: pd.DataFrame) -> pd.DataFrame:
    """Select and validate the public regression schema.

    This function deliberately performs no estimation and does not reconstruct
    upstream variables. It fails if the verified audit panel changes in a way
    that would silently alter the shared regression extract.
    """
    missing = set(CORE_COLUMNS) - set(source.columns)
    if missing:
        raise ValueError(f"Input panel is missing required columns: {sorted(missing)}")

    core = source.loc[:, CORE_COLUMNS].copy()
    core = core.sort_values(["grad_y", "state_abbr"], kind="stable").reset_index(drop=True)

    if len(core) != 648:
        raise ValueError(f"Expected the exact 648-row Table 4 sample; found {len(core)} rows.")
    if core.duplicated(["state_abbr", "grad_y"]).any():
        raise ValueError("State-by-graduation-cohort keys must be unique.")
    if core.isna().any().any():
        columns = core.columns[core.isna().any()].tolist()
        raise ValueError(f"Core regression variables contain missing values: {columns}")
    if not np.array_equal(core["entry_year"].to_numpy(), core["grad_y"].to_numpy() - 4):
        raise ValueError("entry_year must equal grad_y - 4 on every row.")
    if core["state_abbr"].nunique() != 50:
        raise ValueError("Expected 50 non-Alabama destination/origin states.")
    expected_cohorts = set(range(2006, 2024)) - {2020}
    if set(core["grad_y"]) != expected_cohorts:
        raise ValueError("Graduation cohorts must be 2006–2023 excluding 2020.")
    if (core["o_share_model"] < 0).any():
        raise ValueError("IPEDS origin shares cannot be negative.")
    return core


def dictionary_frame(core: pd.DataFrame) -> pd.DataFrame:
    """Return one documented row for each public column in file order."""
    rows = []
    for position, column in enumerate(CORE_COLUMNS, start=1):
        definition, units, role = DICTIONARY[column]
        rows.append({
            "position": position,
            "column": column,
            "definition": definition,
            "units": units,
            "role": role,
            "missing_rows": int(core[column].isna().sum()),
        })
    return pd.DataFrame(rows)


def peer_group_frame() -> pd.DataFrame:
    """Return exact long-form membership for each alternative control."""
    rows = []
    previous = set()
    for control, members in GROUPS.items():
        if previous - set(members):
            raise ValueError("Peer groups must be nested.")
        for order, unitid in enumerate(members, start=1):
            institution, state = PEERS[unitid]
            rows.append({
                "control": control,
                "group_size": len(members),
                "order_in_group": order,
                "unitid": unitid,
                "institution": institution,
                "institution_state": state,
                "requested_core": unitid in CORE_THREE,
            })
        previous = set(members)
    return pd.DataFrame(rows)


def readme_text(core: pd.DataFrame, input_path: Path, output_path: Path) -> str:
    """Document sample, specification, peer groups, and portable rebuild use."""
    return f"""# Compact Table 4 peer-control panel

Copy this directory to `data/market_iv/peer_controls` in the shared admissions-project folder.

`{output_path.name}` is the coauthor-facing regression extract for the Table 4 peer-share robustness exercise. It contains {len(core):,} state-by-graduation-cohort observations and {len(CORE_COLUMNS)} variables. Every core variable is complete.

Each row is a non-Alabama state `j` and UA graduation cohort `c`. The sample includes positive observed destination flows in 2006–2023, excluding 2020. `entry_year = grad_y - 4` maps graduates to the entering cohort used for UA and peer origin shares.

The reproduced Table 4 column (4) specification uses:

- outcome: `diff_log_d_share`
- endogenous regressor: `o_share_model`
- excluded instrument: `z_lso`
- controls: `ur`, `net_rate`
- fixed effects: `d_state`, `grad_y`
- peer robustness: include **one** of `h_core3`, `h_core6`, `h_core10`, or `h_core15`
- inference: cluster by `d_state` in the main specification

The `h_core*` variables are percentage-point changes since 2000 in origin state `j`'s share of pooled domestic enrollment at a fixed peer group. A peer located in `j` is excluded from that state's numerator and denominator. These controls are alternatives; they should not be entered jointly.

The groups retain Arkansas, Kentucky, and Clemson as the core. The larger groups add fully reporting institutions in order of their baseline match to UA; UA and UNC Chapel Hill are excluded. Exact membership is in `{output_path.stem}_peer_groups.csv`.

This compact file intentionally omits raw numerators, denominators, aliases, lag/lead variables, older growth controls, interpolation diagnostics, and AME inputs. The `audit` subdirectory retains the wider panel used to make this extract and the state-year peer-share inputs used for validation.

## Directory contents

- `{output_path.name}`: compact estimation panel
- `{output_path.stem}_dictionary.csv`: variable definitions, units, and roles
- `{output_path.stem}_peer_groups.csv`: exact membership of each nested peer group
- `audit/panel_expanded_peer_groups.csv`: verified wide panel consumed by the generator
- `audit/expanded_peer_groups_state_year_shares.csv`: pooled state-year peer shares and their underlying counts
- `README.md`: this documentation

## Rebuild

The generator accepts explicit paths, so it can be committed to the project Git repository while the input and output live in a shared data directory:

```bash
python build_table4_core_panel.py \\
  --input /path/to/admissions_project/data/market_iv/peer_controls/audit/panel_expanded_peer_groups.csv \\
  --output /path/to/admissions_project/data/market_iv/peer_controls/table4_peer_controls_core.csv \\
  --readme /path/to/admissions_project/data/market_iv/peer_controls/README.md
```

The shared root can be set with `ADMISSIONS_PROJECT_ROOT` or `TABLE4_PEER_CONTROLS_ROOT`. Individual paths can be set with `TABLE4_EXPANDED_PANEL_PATH` and `TABLE4_CORE_PANEL_PATH`. If `--dictionary`, `--readme`, and `--groups` are omitted, those files are written beside the output CSV.

Input filename used for this build: `{input_path.name}`. This script creates the compact extract from a verified audit panel; it does not rebuild the upstream Table 4 outcome, instrument, or peer shares from raw data.
"""


#=====================================================================
# 3 - Portable command-line interface
#=====================================================================

def default_paths() -> tuple[Path, Path]:
    """Resolve the shared-data layout, with environment overrides.

    The audit panel is retained in an ``audit`` subdirectory because it is an
    intermediate verification file.  The compact panel is written one level
    above it so coauthors can find the estimation input immediately.
    """
    active_projects_root = Path.home() / "Dropbox/Professional/active-projects"
    project_root = Path(
        os.environ.get(
            "ADMISSIONS_PROJECT_ROOT",
            active_projects_root / "admissions_project",
        )
    )
    folder = Path(
        os.environ.get(
            "TABLE4_PEER_CONTROLS_ROOT",
            project_root / "data/market_iv/peer_controls",
        )
    )
    source = Path(
        os.environ.get(
            "TABLE4_EXPANDED_PANEL_PATH",
            folder / "audit/panel_expanded_peer_groups.csv",
        )
    )
    output = Path(
        os.environ.get(
            "TABLE4_CORE_PANEL_PATH",
            folder / "table4_peer_controls_core.csv",
        )
    )
    return source, output


def parse_args() -> argparse.Namespace:
    source, output = default_paths()
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--input", type=Path, default=source, help="Wide verified audit-panel CSV.")
    parser.add_argument("--output", type=Path, default=output, help="Compact output CSV.")
    parser.add_argument("--dictionary", type=Path, help="Dictionary CSV; defaults beside --output.")
    parser.add_argument("--readme", type=Path, help="README Markdown; defaults beside --output.")
    parser.add_argument("--groups", type=Path, help="Peer-membership CSV; defaults beside --output.")
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    input_path = args.input.expanduser().resolve()
    output_path = args.output.expanduser().resolve()
    dictionary_path = (args.dictionary or output_path.with_name(output_path.stem + "_dictionary.csv")).expanduser().resolve()
    readme_path = (args.readme or output_path.with_name(output_path.stem + "_README.md")).expanduser().resolve()
    groups_path = (args.groups or output_path.with_name(output_path.stem + "_peer_groups.csv")).expanduser().resolve()

    if input_path == output_path:
        raise ValueError("Input and output paths must differ.")
    if not input_path.exists():
        raise FileNotFoundError(f"Input panel not found: {input_path}")

    core = build_core_panel(pd.read_csv(input_path))
    for path in (output_path, dictionary_path, readme_path, groups_path):
        path.parent.mkdir(parents=True, exist_ok=True)
    core.to_csv(output_path, index=False)
    dictionary_frame(core).to_csv(dictionary_path, index=False)
    peer_group_frame().to_csv(groups_path, index=False)
    readme_path.write_text(readme_text(core, input_path, output_path))
    print(f"Wrote {len(core):,} rows × {len(core.columns)} columns to {output_path}")


if __name__ == "__main__":
    main()
