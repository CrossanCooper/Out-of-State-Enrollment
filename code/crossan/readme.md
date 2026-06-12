# Crossan Code Overview

Last updated: 2026-06-11

This folder contains Crossan-authored code for the admissions project. Ryan's
main analysis scripts remain in the root `code/` folder. The data setup is not
changed by this reorganization: scripts still read from and write to the
original project data directory:

```text
/Users/crossancooper/Dropbox/Professional/active-projects/admissions_project
```

Shared paths are defined in `_shared/project_paths.R`. That file keeps legacy
data locations stable while letting Crossan code live under `code/crossan/`.

## Directory Map

```text
code/crossan/
  _shared/        Shared path configuration file (define file paths)
  commencement/   UA commencement cleaning, linkage, and descriptives
  public_data/    Public data pulls and exploratory public-data figures
  recruiting/     Admissions visit cleaning and town-level visit effects
  revelio/        Revelio education/profile/job-spell extraction helpers
  market_iv/      Code for evaluating market access shift-share IV
  wisconsin/      Wisconsin FOIA graduate-processing helper
```

## Main Order Of Operations

### 1. Revelio Preparation

The Revelio scripts create or inspect intermediate Revelio files used later in
the commencement linkage and Ryan's destination analysis.

```text
revelio/revelio_ed_data_cleaning_v1.R
```

Finds UA/Tuscaloosa student records in Revelio education data.

Core market-IV workflow outputs:

```text
revelio_data/bama_students.csv
revelio_data/tuscaloosa_students.csv
revelio_data/tuscaloosa_student_id_list.csv
```

```text
revelio/revelio_data_linkage_v1.R
```

Links UA Revelio education records to Revelio profile/job-spell files.

Important inputs:

```text
revelio_data/job_spells_data_ua.csv
revelio_data/revelio_tuscaloosa_students.csv
revelio_data/revelio_ua_student_names.csv
```

Important output:

```text
revelio_data/linked_revelio_data.csv
```

```text
revelio/work_location_v1.R
```

Produces exploratory work-location and out-migration descriptives. This is not
the main source of Ryan's analysis files.

### 2. Commencement And Commencement-Revelio Linkage

```text
commencement/commencement_revelio_link_v1.R
```

Processes UA commencement files from `generated_data/`, cleans degree/origin
fields, links commencement records to Revelio profiles, and writes the core
student-level files.

Important outputs:

```text
data/all_alabama_data.csv
data/cleaned_ua_revelio.csv
data/linked_commencement_revelio_profile_data.csv
```

These are central handoff files for Ryan's main analysis.

```text
commencement/ua_descriptives_v1.R
```

Uses the cleaned commencement file and recruiting inputs to produce exploratory
major, honors, recruiting, and post-graduation descriptives. The preferred
town-level admissions-visit regressions now live in `recruiting/town_matching/`.

Important inputs:

```text
data/all_alabama_data.csv
data/recruiting_data_ozan.csv
data/full_ua_recruiting_scrape.csv
data/bama_highschool_2011_2024.csv
data/linked_commencement_revelio_profile_data.csv
revelio_data/first_spell_join.rds
```

Important outputs:

```text
figures/descriptive-figs/
figures/recruiting_iv.png
figures/recruiting_iv_changes.png
```

### 3. Public Data

```text
public_data/ipeds_data.R
```

Builds an exploratory IPEDS flagship panel and enrollment-share figures.

Important output:

```text
data/flagship_panel.csv
```

```text
public_data/town_zcta_link.R
```

Builds a town-ZCTA bridge and ACS town-level income/covariate file.

Important outputs:

```text
data/hud_city_zip_crosswalk.csv
data/acs_2018_2022_town_estimates.csv
```

`data/acs_2018_2022_town_estimates.csv` is used in Ryan's `analysis_main.R`
when attaching town-level ACS income measures to commencement-origin records.

```text
public_data/chetty_analysis.R
```

Produces exploratory Chetty/Deming/Friedman public-data figures. These are
motivating/descriptive outputs rather than core data handoffs to Ryan's current
main analysis scripts.

### 4. Market IV For Location Spillovers

```text
market_iv/build_market_iv.py
```

Builds the state-by-cohort IPEDS/commencement panels and the 2000
peer-flagship market-share instrument for the linked-only location-spillover
count model. The preferred instrument uses each origin state's 2000 share of
non-Alabama-origin out-of-state first-time students at non-Alabama public
flagships, interacted with leave-state-out UA nonresident enrollment growth.
This script only constructs data products; regression diagnostics are estimated
in R.

Important outputs:

```text
data/market_iv/data/market_iv_panel.csv
data/market_iv/data/ua_ipeds_origin_entry_panel.csv
data/market_iv/data/ua_commencement_origin_panel.csv
data/market_iv/tables/market_exposure_by_state.csv
data/market_iv/tables/first_stage_diagnostics.csv
data/market_iv/tables/state_growth_diagnostics.csv
data/market_iv/market_iv_build_summary.md
figures/market-iv/ipeds_growth_vs_exposure.png
figures/market-iv/reduced_form_scatterplots.png
```

```text
market_iv/estimate_market_iv_diagnostics.R
```

Reads the market-IV panels, estimates the first-stage-style diagnostic
regressions with `fixest::feols()`, writes the diagnostic tables, and writes the
market-IV figures under `figures/market-iv/`. The reduced-form scatterplot is
saved as both `ipeds_growth_vs_exposure.png` and
`reduced_form_scatterplots.png`.

```text
market_iv/linked_only_spillover_ols_iv.R
```

Estimates the linked-only OLS and leave-state-out IV versions of the
state-by-cohort inflow/outflow count model with `fixest::feols()`. The script
uses `pacman::p_load()` for package loading and is run directly with `Rscript`;
there is no Makefile workflow in this folder.

Important outputs:

```text
data/market_iv/linked_only/linked_only_balanced_panel_with_iv.csv
data/market_iv/linked_only/linked_only_ols_iv_summary.csv
data/market_iv/linked_only/linked_only_ols_iv_summary.md
```

```text
market_iv/build_baseline_year_robustness_iv.py
market_iv/estimate_baseline_year_robustness.R
```

Builds and estimates alternative-baseline versions of the same leave-state-out
market IV, holding the linked-only estimating panel fixed.

Important outputs:

```text
data/market_iv/baseline_year_robustness/baseline_year_ipeds_coverage.csv
data/market_iv/baseline_year_robustness/baseline_year_robustness_z_panel.csv
data/market_iv/baseline_year_robustness/baseline_year_robustness_estimates.csv
data/market_iv/baseline_year_robustness/baseline_year_robustness_compact.csv
data/market_iv/baseline_year_robustness/baseline_year_robustness.md
```

Run order from `code/crossan/market_iv/`:

```bash
/Users/crossancooper/Dropbox/Professional/active-projects/admissions_project/data/pgp-ipeds/ipeds-database/.venv/bin/python build_market_iv.py
Rscript estimate_market_iv_diagnostics.R
Rscript linked_only_spillover_ols_iv.R
/Users/crossancooper/Dropbox/Professional/active-projects/admissions_project/data/pgp-ipeds/ipeds-database/.venv/bin/python build_baseline_year_robustness_iv.py
Rscript estimate_baseline_year_robustness.R
```

The default IPEDS DuckDB input is the shared main-project copy:

```text
/Users/crossancooper/Dropbox/Professional/active-projects/admissions_project/data/pgp-ipeds/ipeds-database/ipeds.duckdb
```

The market-IV scripts write CSV and Markdown outputs under
`/Users/crossancooper/Dropbox/Professional/active-projects/admissions_project/data/market_iv`.
Figures are written separately under
`/Users/crossancooper/Dropbox/Professional/active-projects/admissions_project/figures/market-iv`.
The code path intentionally does not produce memo PDF or TeX files.

### 5. Recruiting Visit Cleaning

```text
recruiting/ozan_recruiting_cleaning_v1.R
```

Explores and chunks zipped recruiting HTML/source files. The current script
writes intermediate RDS chunks, including to external/local storage paths.

```text
recruiting/ozan_hmtl_cleaning_v1.R
```

Extracts structured recruiting visit records from the HTML chunks.

Important output:

```text
data/full_ua_recruiting_scrape.csv
```

This file is the key admissions-visit input for the town-level recruiting
pipeline.

### 6. Recruiting Town-Level Visit Effects

```text
recruiting/run_recruiting_town_pipeline.R
```

RStudio-first wrapper for the current admissions-officer-visit workflow. Before
clicking Source, edit the options at the top:

```r
refresh_online <- FALSE
full_robustness <- TRUE
horizons <- "5,6"
```

The wrapper runs the town-matching scripts in this order:

```text
1. town_matching/town_linkage.py
2. town_matching/build_high_school_denominators.py
3. town_matching/build_visit_graduate_panel.py             # t+5 panel
4. town_matching/build_acs_town_covariates.py
5. town_matching/build_visit_graduate_panel.py             # t+6 panel
6. town_matching/run_visit_graduate_robustness.R           # t+5 town-FE coefficient grid
7. town_matching/run_visit_graduate_robustness.R           # t+6 town-FE coefficient grid
8. town_matching/run_visit_graduate_no_town_fe.R
9. town_matching/build_recruiting_ppml_main_results.R      # main PPML tables
10. town_matching/run_recruiting_visit_driver_analysis.R
11. town_matching/run_town_fe_identification_diagnostics.R
12. town_matching/run_visit_graduate_town_trend_checks.R
13. town_matching/plot_recruiting_paper_figures.R
14. town_matching/plot_visit_count_distribution.R
```

With `full_robustness <- TRUE`, the wrapper estimates both the t+5 and t+6
town-fixed-effect coefficient grids. The PPML rows from those grids are the
main results and are extracted into `town_matching/output/main_results/`.
The raw grid files still live under `town_matching/output/robustness/` because
that was the historical folder name for the broader specification grid.

Main outputs are written under:

```text
town_matching/output/
```

Key admissions-visit effect outputs:

```text
town_matching/output/ua_recruiting_to_student_town_matches.csv
town_matching/output/ua_high_school_denominators_online_panel_matched.csv
town_matching/output/ua_acs_town_covariates_2017.csv
town_matching/output/ua_visit_t_graduates_t_plus_5_panel.csv
town_matching/output/robustness/ua_visit_t_graduates_t_plus_6_panel.csv
town_matching/output/main_results/recruiting_ppml_town_fe_coefficients.csv
town_matching/output/main_results/recruiting_ppml_no_town_fe_coefficients.csv
town_matching/output/main_results/recruiting_ppml_town_fe.tex
town_matching/output/main_results/recruiting_ppml_no_town_fe.tex
town_matching/output/robustness/ua_visit_t_graduates_t_plus_5_robustness_coefficients.csv
town_matching/output/robustness/ua_visit_t_graduates_t_plus_6_robustness_coefficients.csv
town_matching/output/no_town_fe/ua_visit_graduate_no_town_fe_coefficients.csv
town_matching/output/robustness/ua_visit_t_graduates_t_plus_3_6_town_trend_coefficients.csv
town_matching/output/visit_drivers/recruiting_visit_driver_coefficients.csv
```

The two `.tex` files in `town_matching/output/main_results/` are the central
PPML tables for the admissions-officer-visit analysis. The coefficient CSVs in
the same folder are the filtered, paper-facing PPML rows used to build those
tables.

Paper-style visit-effect figures are written under:

```text
output/docs/figures/
```

## Files Useful For Ryan's Main Analysis

Ryan's scripts do not call Crossan scripts directly, but they do use several
data files generated by the Crossan workflow or by Ryan scripts that depend on
Crossan outputs.

| Data file | Generated by | Used by Ryan scripts | Notes |
|---|---|---|---|
| `data/all_alabama_data.csv` | `commencement/commencement_revelio_link_v1.R` | `analysis_main.R`, `analysis_msa.R` | Core cleaned UA commencement-origin file. Keep name/location stable. |
| `data/linked_commencement_revelio_profile_data.csv` | `commencement/commencement_revelio_link_v1.R` | `analysis_main.R`, `analysis_msa.R`, `build_budget_shock.R` | Commencement records linked to Revelio profiles. Keep name/location stable. |
| `data/cleaned_ua_revelio.csv` | `commencement/commencement_revelio_link_v1.R` | `build_destinations.R` | Ryan uses this to build destination/job-spell files. |
| `revelio_data/first_spell_join.rds` | Ryan's `build_destinations.R`, using `data/cleaned_ua_revelio.csv` | `analysis_main.R`, `analysis_msa.R`, `build_budget_shock.R` | Not directly written by Crossan code in this folder, but depends on Crossan-cleaned Revelio data. |
| `data/acs_2018_2022_town_estimates.csv` | `public_data/town_zcta_link.R` | `analysis_main.R` | Town-level ACS income/covariates attached to commencement-origin towns. |
| `data/full_ua_recruiting_scrape.csv` | `recruiting/ozan_hmtl_cleaning_v1.R` | Recruiting pipeline | Main admissions visit input; not currently a Ryan main-analysis input. |
| `town_matching/output/*` | `recruiting/run_recruiting_town_pipeline.R` | Recruiting/admissions-visit tables | Current admissions officer visit panels, coefficient tables, diagnostics, and figures. |
| `data/market_iv/data/market_iv_panel.csv` | `market_iv/build_market_iv.py` | Market-IV spillover checks | State-by-entry-cohort market-IV panel. Figures from this workflow live under `figures/market-iv/`, not under `data/market_iv/`. |
| `data/market_iv/tables/first_stage_diagnostics.csv` | `market_iv/estimate_market_iv_diagnostics.R` | Market-IV first-stage checks | Diagnostic regressions estimated with `fixest::feols()` and origin-state clustered standard errors. |
| `data/market_iv/linked_only/linked_only_ols_iv_summary.csv` | `market_iv/linked_only_spillover_ols_iv.R` | Market-IV spillover checks | Linked-only OLS, first-stage, and IV estimates from `fixest::feols()` with destination-state clustered standard errors. |
| `data/market_iv/baseline_year_robustness/*` | `market_iv/build_baseline_year_robustness_iv.py`, `market_iv/estimate_baseline_year_robustness.R` | Market-IV robustness checks | Alternative-baseline IV panels and estimates. |

## Practical Notes

- Do not change the names or locations of the handoff files above unless Ryan's
  scripts are updated at the same time.
- The root `code/` folder is Ryan's active analysis area. Crossan code should
  stay under `code/crossan/`.
- `market_iv/` scripts are run directly with the shared IPEDS Python
  environment and `Rscript`; do not use a Makefile for this workflow.
- Market-IV regressions should be estimated in R with `fixest::feols()`.
- Market-IV figures should be written to `figures/market-iv/`, while market-IV
  CSV and Markdown outputs should remain under `data/market_iv/`.
- The recruiting wrapper is designed for RStudio. It still calls Python and R
  sub-scripts internally via `system2()`, so RStudio needs access to `python3`
  and `Rscript`.
- Rerunning scripts that write handoff files can overwrite Ryan's inputs. Treat
  those runs as intentional data refreshes.
