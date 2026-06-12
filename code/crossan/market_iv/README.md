# Market IV

Production code for the market-exposure instrument used in the UA
location-spillover count model.

## Run

From this directory:

```bash
/Users/crossancooper/Dropbox/Professional/active-projects/admissions_project/data/pgp-ipeds/ipeds-database/.venv/bin/python build_market_iv.py
Rscript estimate_market_iv_diagnostics.R
Rscript linked_only_spillover_ols_iv.R
/Users/crossancooper/Dropbox/Professional/active-projects/admissions_project/data/pgp-ipeds/ipeds-database/.venv/bin/python build_baseline_year_robustness_iv.py
Rscript estimate_baseline_year_robustness.R
```

Run the scripts in this order when rebuilding the full market-IV workflow:

| Step | Script | What it does | Main downstream use |
|---|---|---|---|
| 1 | `build_market_iv.py` | Builds the preferred 2000 peer-flagship market-share instrument and the state-by-entry-cohort IPEDS/commencement panels. | Required before all preferred diagnostic and linked-only estimation scripts. |
| 2 | `estimate_market_iv_diagnostics.R` | Uses the Step 1 panel to estimate first-stage diagnostic regressions with `fixest::feols()`, writes state-growth diagnostics, and redraws the reduced-form scatterplot. | Diagnostic output only; it is not required before estimating the linked-only spillover model. |
| 3 | `linked_only_spillover_ols_iv.R` | Builds the linked-only inflow/outflow count panel from Revelio and linked commencement files, merges the Step 1 preferred IV, and estimates the five OLS/first-stage/IV specifications with `fixest::feols()`. | Required before the alternative-baseline robustness estimates because it writes the fixed linked-only estimating panel. |
| 4 | `build_baseline_year_robustness_iv.py` | Builds alternative-baseline versions of the same leave-state-out market IV from early IPEDS years. | Required before Step 5. |
| 5 | `estimate_baseline_year_robustness.R` | Merges the Step 4 alternative IVs onto the Step 3 linked-only panel and re-estimates the IV model across baseline-year choices. | Produces the baseline-year robustness table and Markdown summary. |

Step 4 can technically be run before Step 3 because it only uses IPEDS and the
flagship crosswalk. Step 5 requires both Step 3 and Step 4 outputs.

## Inputs

The scripts read main-project Revelio and linked commencement inputs from:

```text
/Users/crossancooper/Dropbox/Professional/active-projects/admissions_project
```

They read IPEDS from the shared main-project data folder by default:

```text
/Users/crossancooper/Dropbox/Professional/active-projects/admissions_project/data/pgp-ipeds
```

They read the flagship crosswalk and parsed commencement input from the Codex
working repo by default:

```text
/Users/crossancooper/Dropbox/Professional/active-projects/admissions_project_personal/admissions-project-codex
```

These roots can be overridden with:

- `ADMISSIONS_PROJECT_ROOT`
- `ADMISSIONS_PROJECT_CODEX_ROOT`
- `SHARED_IPEDS_ROOT`
- `MARKET_IV_OUTPUT_ROOT`
- `MARKET_IV_FIGURE_ROOT`
- `IPEDS_DUCKDB_PATH`
- `FLAGSHIP_CROSSWALK_PATH`
- `UA_COMMENCEMENT_PATH`

## Outputs

CSV and Markdown outputs are written to:

```text
/Users/crossancooper/Dropbox/Professional/active-projects/admissions_project/data/market_iv
```

Figures are written to:

```text
/Users/crossancooper/Dropbox/Professional/active-projects/admissions_project/figures/market-iv
```

The reduced-form scatterplot is written under both of these names:

```text
ipeds_growth_vs_exposure.png
reduced_form_scatterplots.png
```

The production output contains CSV, Markdown, and PNG files only. Memo PDF and
TeX files are intentionally not produced by this code path.

The R scripts use `pacman::p_load()` and `fixest::feols()` for regressions. The
Python scripts only construct IPEDS/commencement panels and market-IV variables.
