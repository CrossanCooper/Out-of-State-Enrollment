#=====================================================================
## Author: Crossan Cooper
## Last Modified: 06-08-2026
#
## file use: Estimate the original baseline town-level regressions linking
## UA recruiting visits in school year t to UA graduate counts in t+5.
#
## inputs:
## 1. town_matching/output/ua_visit_t_graduates_t_plus_5_panel.csv -- t+5 visit-to-graduate panel
#
## outputs:
## 1. town_matching/output/ua_visit_t_graduates_t_plus_5_regression_coefficients.csv -- coefficient table
## 2. town_matching/output/ua_visit_t_graduates_t_plus_5_regression_summary.txt -- regression summary
#=====================================================================

#=====================================================================
# 1 - Packages and paths
#=====================================================================

# default list of packages and cleaning command
rm(list=ls())
if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table,fixest)

default_data_root <- "/Users/crossancooper/Dropbox/Professional/active-projects/admissions_project"
repo_root <- normalizePath(
  Sys.getenv("ADMISSIONS_PROJECT_DATA_ROOT", default_data_root),
  mustWork = FALSE
)
panel_path <- file.path(
  repo_root,
  "town_matching",
  "output",
  "ua_visit_t_graduates_t_plus_5_panel.csv"
)
output_dir <- file.path(repo_root, "town_matching", "output")
coef_path <- file.path(output_dir, "ua_visit_t_graduates_t_plus_5_regression_coefficients.csv")
summary_path <- file.path(output_dir, "ua_visit_t_graduates_t_plus_5_regression_summary.txt")

#=====================================================================
# 2 - Load panel and define analysis samples
#=====================================================================

panel <- fread(panel_path)

numeric_cols <- c(
  "recruiting_school_year",
  "outcome_grad_year",
  "outcome_grad_year_observed",
  "in_student_origin_town_universe",
  "preferred_regression_sample",
  "denominator_regression_sample",
  "fixed_hs_denominator_regression_sample",
  "visits_t",
  "any_visit_t",
  "exact_match_visits_t",
  "fuzzy_clear_match_visits_t",
  "same_state_fuzzy_review_visits_t",
  "visits_t_with_same_state_fuzzy",
  "any_visit_t_with_same_state_fuzzy",
  "review_needed_visits_t",
  "all_raw_visit_rows_t",
  "unique_recruiting_event_names_t",
  "graduates_t_plus_h",
  "predicted_graduates_t_plus_h",
  "bachelor_graduates_t_plus_h",
  "honors_graduates_t_plus_h",
  "pre_graduates_t_minus_1",
  "pre_graduates_t_minus_2",
  "pre_graduates_t_minus_3",
  "pre_graduates_t_minus_4",
  "pre_graduates_t_minus_5",
  "pre_graduates_mean_3",
  "pre_graduates_mean_5",
  "pre_graduates_trend_3",
  "pre_graduates_trend_5",
  "fixed_baseline_grad_year",
  "fixed_baseline_graduates",
  "graduates_per_fixed_baseline_plus_one_t_plus_h",
  "pre_graduates_growth_since_fixed_baseline",
  "pre_bachelor_graduates_t_minus_1",
  "pre_bachelor_graduates_t_minus_2",
  "pre_bachelor_graduates_t_minus_3",
  "pre_bachelor_graduates_t_minus_4",
  "pre_bachelor_graduates_t_minus_5",
  "pre_bachelor_graduates_mean_3",
  "pre_bachelor_graduates_mean_5",
  "pre_bachelor_graduates_trend_3",
  "pre_bachelor_graduates_trend_5",
  "fixed_baseline_bachelor_graduates",
  "bachelor_graduates_per_fixed_baseline_plus_one_t_plus_h",
  "pre_bachelor_graduates_growth_since_fixed_baseline",
  "denominator_available_t",
  "public_grade12_enrollment_t",
  "private_grade12_enrollment_t",
  "total_grade12_enrollment_t",
  "public_grade12_school_count_t",
  "private_grade12_school_count_t",
  "private_pss_source_school_year_t",
  "graduates_share_t_plus_h",
  "bachelor_graduates_share_t_plus_h",
  "honors_graduates_share_t_plus_h",
  "fixed_hs_denominator_school_year",
  "fixed_hs_grade12_enrollment",
  "fixed_hs_denominator_available",
  "graduates_share_fixed_hs_t_plus_h",
  "predicted_graduates_share_fixed_hs_t_plus_h",
  "bachelor_graduates_share_fixed_hs_t_plus_h",
  "pre_graduates_t_minus_1_share_fixed_hs",
  "pre_graduates_t_minus_2_share_fixed_hs",
  "pre_graduates_t_minus_3_share_fixed_hs",
  "pre_graduates_t_minus_4_share_fixed_hs",
  "pre_graduates_t_minus_5_share_fixed_hs",
  "pre_graduates_trend_3_share_fixed_hs",
  "pre_graduates_trend_5_share_fixed_hs"
)
existing_numeric_cols <- intersect(numeric_cols, names(panel))
panel[, (existing_numeric_cols) := lapply(.SD, as.numeric), .SDcols = existing_numeric_cols]

analysis_panel <- panel[preferred_regression_sample == 1]
analysis_panel[, recruiting_school_year := factor(recruiting_school_year)]
analysis_panel_in_state <- analysis_panel[processed_state == "AL"]
analysis_panel_out_state <- analysis_panel[processed_state != "AL"]
denom_panel <- analysis_panel[
  denominator_regression_sample == 1 &
    !is.na(total_grade12_enrollment_t) &
    total_grade12_enrollment_t > 0
]
fixed_hs_panel <- analysis_panel[
  fixed_hs_denominator_regression_sample == 1 &
    !is.na(fixed_hs_grade12_enrollment) &
    fixed_hs_grade12_enrollment > 0
]
fixed_hs_panel_in_state <- fixed_hs_panel[processed_state == "AL"]
fixed_hs_panel_out_state <- fixed_hs_panel[processed_state != "AL"]

#=====================================================================
# 3 - Estimate baseline, denominator, placebo, and split-sample models
#=====================================================================

# Baseline: town FE absorb persistent town size/preferences, and school-year FE
# absorb year-specific shocks in recruiting intensity and graduation counts.
safe_fit <- function(expr) {
  tryCatch(
    eval.parent(substitute(expr)),
    error = function(error) error
  )
}

model_specs <- list()
model_specs$graduates_ols <- list(
  model = safe_fit(
    feols(
      graduates_t_plus_h ~ visits_t | town_state_id + recruiting_school_year,
      cluster = ~town_state_id,
      data = analysis_panel
    )
  ),
  term = "visits_t",
  sample = "preferred"
)
model_specs$graduates_any_visit_ols <- list(
  model = safe_fit(
    feols(
      graduates_t_plus_h ~ any_visit_t | town_state_id + recruiting_school_year,
      cluster = ~town_state_id,
      data = analysis_panel
    )
  ),
  term = "any_visit_t",
  sample = "preferred"
)
model_specs$graduates_ols_same_state_fuzzy <- list(
  model = safe_fit(
    feols(
      graduates_t_plus_h ~ visits_t_with_same_state_fuzzy | town_state_id + recruiting_school_year,
      cluster = ~town_state_id,
      data = analysis_panel
    )
  ),
  term = "visits_t_with_same_state_fuzzy",
  sample = "preferred"
)
model_specs$graduates_any_visit_same_state_fuzzy <- list(
  model = safe_fit(
    feols(
      graduates_t_plus_h ~ any_visit_t_with_same_state_fuzzy |
        town_state_id + recruiting_school_year,
      cluster = ~town_state_id,
      data = analysis_panel
    )
  ),
  term = "any_visit_t_with_same_state_fuzzy",
  sample = "preferred"
)
model_specs$bachelors_ols <- list(
  model = safe_fit(
    feols(
      bachelor_graduates_t_plus_h ~ visits_t | town_state_id + recruiting_school_year,
      cluster = ~town_state_id,
      data = analysis_panel
    )
  ),
  term = "visits_t",
  sample = "preferred"
)
model_specs$bachelors_any_visit_ols <- list(
  model = safe_fit(
    feols(
      bachelor_graduates_t_plus_h ~ any_visit_t | town_state_id + recruiting_school_year,
      cluster = ~town_state_id,
      data = analysis_panel
    )
  ),
  term = "any_visit_t",
  sample = "preferred"
)
model_specs$graduates_ols_lag_controls <- list(
  model = safe_fit(
    feols(
      graduates_t_plus_h ~ visits_t +
        pre_graduates_t_minus_1 + pre_graduates_t_minus_2 + pre_graduates_t_minus_3 +
        pre_graduates_t_minus_4 + pre_graduates_t_minus_5 |
        town_state_id + recruiting_school_year,
      cluster = ~town_state_id,
      data = analysis_panel
    )
  ),
  term = "visits_t",
  sample = "preferred_lag_controls"
)
model_specs$graduates_any_visit_ols_lag_controls <- list(
  model = safe_fit(
    feols(
      graduates_t_plus_h ~ any_visit_t +
        pre_graduates_t_minus_1 + pre_graduates_t_minus_2 + pre_graduates_t_minus_3 +
        pre_graduates_t_minus_4 + pre_graduates_t_minus_5 |
        town_state_id + recruiting_school_year,
      cluster = ~town_state_id,
      data = analysis_panel
    )
  ),
  term = "any_visit_t",
  sample = "preferred_lag_controls"
)
model_specs$graduates_ols_predicted_control <- list(
  model = safe_fit(
    feols(
      graduates_t_plus_h ~ visits_t + predicted_graduates_t_plus_h |
        town_state_id + recruiting_school_year,
      cluster = ~town_state_id,
      data = analysis_panel
    )
  ),
  term = "visits_t",
  sample = "preferred_predicted_control"
)
model_specs$graduates_any_visit_ols_predicted_control <- list(
  model = safe_fit(
    feols(
      graduates_t_plus_h ~ any_visit_t + predicted_graduates_t_plus_h |
        town_state_id + recruiting_school_year,
      cluster = ~town_state_id,
      data = analysis_panel
    )
  ),
  term = "any_visit_t",
  sample = "preferred_predicted_control"
)
if (nrow(fixed_hs_panel) > 0) {
  model_specs$graduates_fixed_hs_share <- list(
    model = safe_fit(
      feols(
        graduates_share_fixed_hs_t_plus_h ~ visits_t |
          town_state_id + recruiting_school_year,
        cluster = ~town_state_id,
        data = fixed_hs_panel
      )
    ),
    term = "visits_t",
    sample = "fixed_hs_denominator"
  )
  model_specs$graduates_any_visit_fixed_hs_share <- list(
    model = safe_fit(
      feols(
        graduates_share_fixed_hs_t_plus_h ~ any_visit_t |
          town_state_id + recruiting_school_year,
        cluster = ~town_state_id,
        data = fixed_hs_panel
      )
    ),
    term = "any_visit_t",
    sample = "fixed_hs_denominator"
  )
  model_specs$graduates_fixed_hs_share_pretrend <- list(
    model = safe_fit(
      feols(
        graduates_share_fixed_hs_t_plus_h ~
          visits_t + pre_graduates_t_minus_1_share_fixed_hs +
          pre_graduates_trend_3_share_fixed_hs |
          town_state_id + recruiting_school_year,
        cluster = ~town_state_id,
        data = fixed_hs_panel
      )
    ),
    term = "visits_t",
    sample = "fixed_hs_denominator_pretrend_controls"
  )
  model_specs$graduates_any_visit_fixed_hs_share_pretrend <- list(
    model = safe_fit(
      feols(
        graduates_share_fixed_hs_t_plus_h ~
          any_visit_t + pre_graduates_t_minus_1_share_fixed_hs +
          pre_graduates_trend_3_share_fixed_hs |
          town_state_id + recruiting_school_year,
        cluster = ~town_state_id,
        data = fixed_hs_panel
      )
    ),
    term = "any_visit_t",
    sample = "fixed_hs_denominator_pretrend_controls"
  )
  model_specs$graduates_fixed_hs_share_lag_controls <- list(
    model = safe_fit(
      feols(
        graduates_share_fixed_hs_t_plus_h ~ visits_t +
          pre_graduates_t_minus_1_share_fixed_hs +
          pre_graduates_t_minus_2_share_fixed_hs +
          pre_graduates_t_minus_3_share_fixed_hs +
          pre_graduates_t_minus_4_share_fixed_hs +
          pre_graduates_t_minus_5_share_fixed_hs |
          town_state_id + recruiting_school_year,
        cluster = ~town_state_id,
        data = fixed_hs_panel
      )
    ),
    term = "visits_t",
    sample = "fixed_hs_denominator_lag_controls"
  )
  model_specs$graduates_any_visit_fixed_hs_share_lag_controls <- list(
    model = safe_fit(
      feols(
        graduates_share_fixed_hs_t_plus_h ~ any_visit_t +
          pre_graduates_t_minus_1_share_fixed_hs +
          pre_graduates_t_minus_2_share_fixed_hs +
          pre_graduates_t_minus_3_share_fixed_hs +
          pre_graduates_t_minus_4_share_fixed_hs +
          pre_graduates_t_minus_5_share_fixed_hs |
          town_state_id + recruiting_school_year,
        cluster = ~town_state_id,
        data = fixed_hs_panel
      )
    ),
    term = "any_visit_t",
    sample = "fixed_hs_denominator_lag_controls"
  )
  model_specs$graduates_fixed_hs_share_predicted_control <- list(
    model = safe_fit(
      feols(
        graduates_share_fixed_hs_t_plus_h ~ visits_t +
          predicted_graduates_share_fixed_hs_t_plus_h |
          town_state_id + recruiting_school_year,
        cluster = ~town_state_id,
        data = fixed_hs_panel
      )
    ),
    term = "visits_t",
    sample = "fixed_hs_denominator_predicted_control"
  )
  model_specs$graduates_any_visit_fixed_hs_share_predicted_control <- list(
    model = safe_fit(
      feols(
        graduates_share_fixed_hs_t_plus_h ~ any_visit_t +
          predicted_graduates_share_fixed_hs_t_plus_h |
          town_state_id + recruiting_school_year,
        cluster = ~town_state_id,
        data = fixed_hs_panel
      )
    ),
    term = "any_visit_t",
    sample = "fixed_hs_denominator_predicted_control"
  )
  model_specs$pre_fixed_hs_level_placebo <- list(
    model = safe_fit(
      feols(
        pre_graduates_t_minus_1_share_fixed_hs ~ visits_t |
          town_state_id + recruiting_school_year,
        cluster = ~town_state_id,
        data = fixed_hs_panel
      )
    ),
    term = "visits_t",
    sample = "fixed_hs_preperiod_placebo"
  )
  model_specs$pre_fixed_hs_any_visit_level_placebo <- list(
    model = safe_fit(
      feols(
        pre_graduates_t_minus_1_share_fixed_hs ~ any_visit_t |
          town_state_id + recruiting_school_year,
        cluster = ~town_state_id,
        data = fixed_hs_panel
      )
    ),
    term = "any_visit_t",
    sample = "fixed_hs_preperiod_placebo"
  )
  model_specs$pre_fixed_hs_trend_placebo <- list(
    model = safe_fit(
      feols(
        pre_graduates_trend_3_share_fixed_hs ~ visits_t |
          town_state_id + recruiting_school_year,
        cluster = ~town_state_id,
        data = fixed_hs_panel
      )
    ),
    term = "visits_t",
    sample = "fixed_hs_preperiod_placebo"
  )
  model_specs$pre_fixed_hs_any_visit_trend_placebo <- list(
    model = safe_fit(
      feols(
        pre_graduates_trend_3_share_fixed_hs ~ any_visit_t |
          town_state_id + recruiting_school_year,
        cluster = ~town_state_id,
        data = fixed_hs_panel
      )
    ),
    term = "any_visit_t",
    sample = "fixed_hs_preperiod_placebo"
  )
  model_specs$graduates_fixed_hs_share_in_state <- list(
    model = safe_fit(
      feols(
        graduates_share_fixed_hs_t_plus_h ~ visits_t |
          town_state_id + recruiting_school_year,
        cluster = ~town_state_id,
        data = fixed_hs_panel_in_state
      )
    ),
    term = "visits_t",
    sample = "fixed_hs_in_state"
  )
  model_specs$graduates_any_visit_fixed_hs_share_in_state <- list(
    model = safe_fit(
      feols(
        graduates_share_fixed_hs_t_plus_h ~ any_visit_t |
          town_state_id + recruiting_school_year,
        cluster = ~town_state_id,
        data = fixed_hs_panel_in_state
      )
    ),
    term = "any_visit_t",
    sample = "fixed_hs_in_state"
  )
  model_specs$graduates_fixed_hs_share_out_state <- list(
    model = safe_fit(
      feols(
        graduates_share_fixed_hs_t_plus_h ~ visits_t |
          town_state_id + recruiting_school_year,
        cluster = ~town_state_id,
        data = fixed_hs_panel_out_state
      )
    ),
    term = "visits_t",
    sample = "fixed_hs_out_state"
  )
  model_specs$graduates_any_visit_fixed_hs_share_out_state <- list(
    model = safe_fit(
      feols(
        graduates_share_fixed_hs_t_plus_h ~ any_visit_t |
          town_state_id + recruiting_school_year,
        cluster = ~town_state_id,
        data = fixed_hs_panel_out_state
      )
    ),
    term = "any_visit_t",
    sample = "fixed_hs_out_state"
  )
  model_specs$graduates_fixed_hs_share_lag_controls_in_state <- list(
    model = safe_fit(
      feols(
        graduates_share_fixed_hs_t_plus_h ~ visits_t +
          pre_graduates_t_minus_1_share_fixed_hs +
          pre_graduates_t_minus_2_share_fixed_hs +
          pre_graduates_t_minus_3_share_fixed_hs +
          pre_graduates_t_minus_4_share_fixed_hs +
          pre_graduates_t_minus_5_share_fixed_hs |
          town_state_id + recruiting_school_year,
        cluster = ~town_state_id,
        data = fixed_hs_panel_in_state
      )
    ),
    term = "visits_t",
    sample = "fixed_hs_in_state_lag_controls"
  )
  model_specs$graduates_any_visit_fixed_hs_share_lag_controls_in_state <- list(
    model = safe_fit(
      feols(
        graduates_share_fixed_hs_t_plus_h ~ any_visit_t +
          pre_graduates_t_minus_1_share_fixed_hs +
          pre_graduates_t_minus_2_share_fixed_hs +
          pre_graduates_t_minus_3_share_fixed_hs +
          pre_graduates_t_minus_4_share_fixed_hs +
          pre_graduates_t_minus_5_share_fixed_hs |
          town_state_id + recruiting_school_year,
        cluster = ~town_state_id,
        data = fixed_hs_panel_in_state
      )
    ),
    term = "any_visit_t",
    sample = "fixed_hs_in_state_lag_controls"
  )
  model_specs$graduates_fixed_hs_share_lag_controls_out_state <- list(
    model = safe_fit(
      feols(
        graduates_share_fixed_hs_t_plus_h ~ visits_t +
          pre_graduates_t_minus_1_share_fixed_hs +
          pre_graduates_t_minus_2_share_fixed_hs +
          pre_graduates_t_minus_3_share_fixed_hs +
          pre_graduates_t_minus_4_share_fixed_hs +
          pre_graduates_t_minus_5_share_fixed_hs |
          town_state_id + recruiting_school_year,
        cluster = ~town_state_id,
        data = fixed_hs_panel_out_state
      )
    ),
    term = "visits_t",
    sample = "fixed_hs_out_state_lag_controls"
  )
  model_specs$graduates_any_visit_fixed_hs_share_lag_controls_out_state <- list(
    model = safe_fit(
      feols(
        graduates_share_fixed_hs_t_plus_h ~ any_visit_t +
          pre_graduates_t_minus_1_share_fixed_hs +
          pre_graduates_t_minus_2_share_fixed_hs +
          pre_graduates_t_minus_3_share_fixed_hs +
          pre_graduates_t_minus_4_share_fixed_hs +
          pre_graduates_t_minus_5_share_fixed_hs |
          town_state_id + recruiting_school_year,
        cluster = ~town_state_id,
        data = fixed_hs_panel_out_state
      )
    ),
    term = "any_visit_t",
    sample = "fixed_hs_out_state_lag_controls"
  )
}
model_specs$bachelors_ols_same_state_fuzzy <- list(
  model = safe_fit(
    feols(
      bachelor_graduates_t_plus_h ~ visits_t_with_same_state_fuzzy | town_state_id + recruiting_school_year,
      cluster = ~town_state_id,
      data = analysis_panel
    )
  ),
  term = "visits_t_with_same_state_fuzzy",
  sample = "preferred"
)
model_specs$graduates_ols_pretrend_controls <- list(
  model = safe_fit(
    feols(
      graduates_t_plus_h ~ visits_t + pre_graduates_mean_3 + pre_graduates_trend_3 |
        town_state_id + recruiting_school_year,
      cluster = ~town_state_id,
      data = analysis_panel
    )
  ),
  term = "visits_t",
  sample = "preferred_pretrend_controls"
)
model_specs$bachelors_ols_pretrend_controls <- list(
  model = safe_fit(
    feols(
      bachelor_graduates_t_plus_h ~ visits_t +
        pre_bachelor_graduates_mean_3 + pre_bachelor_graduates_trend_3 |
        town_state_id + recruiting_school_year,
      cluster = ~town_state_id,
      data = analysis_panel
    )
  ),
  term = "visits_t",
  sample = "preferred_pretrend_controls"
)
model_specs$graduates_ols_fixed_baseline_growth <- list(
  model = safe_fit(
    feols(
      graduates_t_plus_h ~ visits_t + pre_graduates_growth_since_fixed_baseline |
        town_state_id + recruiting_school_year,
      cluster = ~town_state_id,
      data = analysis_panel
    )
  ),
  term = "visits_t",
  sample = "preferred_fixed_baseline_controls"
)
model_specs$graduates_any_visit_ols_fixed_baseline_growth <- list(
  model = safe_fit(
    feols(
      graduates_t_plus_h ~ any_visit_t + pre_graduates_growth_since_fixed_baseline |
        town_state_id + recruiting_school_year,
      cluster = ~town_state_id,
      data = analysis_panel
    )
  ),
  term = "any_visit_t",
  sample = "preferred_fixed_baseline_controls"
)
model_specs$bachelors_ols_fixed_baseline_growth <- list(
  model = safe_fit(
    feols(
      bachelor_graduates_t_plus_h ~ visits_t +
        pre_bachelor_graduates_growth_since_fixed_baseline |
        town_state_id + recruiting_school_year,
      cluster = ~town_state_id,
      data = analysis_panel
    )
  ),
  term = "visits_t",
  sample = "preferred_fixed_baseline_controls"
)
model_specs$bachelors_any_visit_ols_fixed_baseline_growth <- list(
  model = safe_fit(
    feols(
      bachelor_graduates_t_plus_h ~ any_visit_t +
        pre_bachelor_graduates_growth_since_fixed_baseline |
        town_state_id + recruiting_school_year,
      cluster = ~town_state_id,
      data = analysis_panel
    )
  ),
  term = "any_visit_t",
  sample = "preferred_fixed_baseline_controls"
)
model_specs$graduates_fixed_baseline_normalized <- list(
  model = safe_fit(
    feols(
      graduates_per_fixed_baseline_plus_one_t_plus_h ~ visits_t |
        town_state_id + recruiting_school_year,
      cluster = ~town_state_id,
      data = analysis_panel
    )
  ),
  term = "visits_t",
  sample = "preferred_fixed_baseline_normalized"
)
model_specs$graduates_any_visit_fixed_baseline_normalized <- list(
  model = safe_fit(
    feols(
      graduates_per_fixed_baseline_plus_one_t_plus_h ~ any_visit_t |
        town_state_id + recruiting_school_year,
      cluster = ~town_state_id,
      data = analysis_panel
    )
  ),
  term = "any_visit_t",
  sample = "preferred_fixed_baseline_normalized"
)
model_specs$graduates_fixed_baseline_normalized_growth <- list(
  model = safe_fit(
    feols(
      graduates_per_fixed_baseline_plus_one_t_plus_h ~
        visits_t + pre_graduates_growth_since_fixed_baseline |
        town_state_id + recruiting_school_year,
      cluster = ~town_state_id,
      data = analysis_panel
    )
  ),
  term = "visits_t",
  sample = "preferred_fixed_baseline_normalized"
)
model_specs$graduates_any_visit_fixed_baseline_normalized_growth <- list(
  model = safe_fit(
    feols(
      graduates_per_fixed_baseline_plus_one_t_plus_h ~
        any_visit_t + pre_graduates_growth_since_fixed_baseline |
        town_state_id + recruiting_school_year,
      cluster = ~town_state_id,
      data = analysis_panel
    )
  ),
  term = "any_visit_t",
  sample = "preferred_fixed_baseline_normalized"
)
model_specs$bachelors_fixed_baseline_normalized <- list(
  model = safe_fit(
    feols(
      bachelor_graduates_per_fixed_baseline_plus_one_t_plus_h ~ visits_t |
        town_state_id + recruiting_school_year,
      cluster = ~town_state_id,
      data = analysis_panel
    )
  ),
  term = "visits_t",
  sample = "preferred_fixed_baseline_normalized"
)
model_specs$bachelors_any_visit_fixed_baseline_normalized <- list(
  model = safe_fit(
    feols(
      bachelor_graduates_per_fixed_baseline_plus_one_t_plus_h ~ any_visit_t |
        town_state_id + recruiting_school_year,
      cluster = ~town_state_id,
      data = analysis_panel
    )
  ),
  term = "any_visit_t",
  sample = "preferred_fixed_baseline_normalized"
)
model_specs$pre_graduate_level_placebo <- list(
  model = safe_fit(
    feols(
      pre_graduates_mean_3 ~ visits_t | town_state_id + recruiting_school_year,
      cluster = ~town_state_id,
      data = analysis_panel
    )
  ),
  term = "visits_t",
  sample = "preperiod_placebo"
)
model_specs$pre_graduate_trend_placebo <- list(
  model = safe_fit(
    feols(
      pre_graduates_trend_3 ~ visits_t | town_state_id + recruiting_school_year,
      cluster = ~town_state_id,
      data = analysis_panel
    )
  ),
  term = "visits_t",
  sample = "preperiod_placebo"
)
model_specs$pre_fixed_baseline_growth_placebo <- list(
  model = safe_fit(
    feols(
      pre_graduates_growth_since_fixed_baseline ~ visits_t |
        town_state_id + recruiting_school_year,
      cluster = ~town_state_id,
      data = analysis_panel
    )
  ),
  term = "visits_t",
  sample = "preperiod_placebo"
)
model_specs$pre_fixed_baseline_growth_any_visit_placebo <- list(
  model = safe_fit(
    feols(
      pre_graduates_growth_since_fixed_baseline ~ any_visit_t |
        town_state_id + recruiting_school_year,
      cluster = ~town_state_id,
      data = analysis_panel
    )
  ),
  term = "any_visit_t",
  sample = "preperiod_placebo"
)
model_specs$graduates_ols_in_state <- list(
  model = safe_fit(
    feols(
      graduates_t_plus_h ~ visits_t | town_state_id + recruiting_school_year,
      cluster = ~town_state_id,
      data = analysis_panel_in_state
    )
  ),
  term = "visits_t",
  sample = "in_state"
)
model_specs$graduates_any_visit_ols_in_state <- list(
  model = safe_fit(
    feols(
      graduates_t_plus_h ~ any_visit_t | town_state_id + recruiting_school_year,
      cluster = ~town_state_id,
      data = analysis_panel_in_state
    )
  ),
  term = "any_visit_t",
  sample = "in_state"
)
model_specs$graduates_ols_lag_controls_in_state <- list(
  model = safe_fit(
    feols(
      graduates_t_plus_h ~ visits_t +
        pre_graduates_t_minus_1 + pre_graduates_t_minus_2 +
        pre_graduates_t_minus_3 + pre_graduates_t_minus_4 +
        pre_graduates_t_minus_5 |
        town_state_id + recruiting_school_year,
      cluster = ~town_state_id,
      data = analysis_panel_in_state
    )
  ),
  term = "visits_t",
  sample = "in_state_lag_controls"
)
model_specs$graduates_any_visit_ols_lag_controls_in_state <- list(
  model = safe_fit(
    feols(
      graduates_t_plus_h ~ any_visit_t +
        pre_graduates_t_minus_1 + pre_graduates_t_minus_2 +
        pre_graduates_t_minus_3 + pre_graduates_t_minus_4 +
        pre_graduates_t_minus_5 |
        town_state_id + recruiting_school_year,
      cluster = ~town_state_id,
      data = analysis_panel_in_state
    )
  ),
  term = "any_visit_t",
  sample = "in_state_lag_controls"
)
model_specs$graduates_fixed_baseline_normalized_in_state <- list(
  model = safe_fit(
    feols(
      graduates_per_fixed_baseline_plus_one_t_plus_h ~ visits_t |
        town_state_id + recruiting_school_year,
      cluster = ~town_state_id,
      data = analysis_panel_in_state
    )
  ),
  term = "visits_t",
  sample = "in_state"
)
model_specs$graduates_any_visit_fixed_baseline_normalized_in_state <- list(
  model = safe_fit(
    feols(
      graduates_per_fixed_baseline_plus_one_t_plus_h ~ any_visit_t |
        town_state_id + recruiting_school_year,
      cluster = ~town_state_id,
      data = analysis_panel_in_state
    )
  ),
  term = "any_visit_t",
  sample = "in_state"
)
model_specs$graduates_ols_out_state <- list(
  model = safe_fit(
    feols(
      graduates_t_plus_h ~ visits_t | town_state_id + recruiting_school_year,
      cluster = ~town_state_id,
      data = analysis_panel_out_state
    )
  ),
  term = "visits_t",
  sample = "out_state"
)
model_specs$graduates_any_visit_ols_out_state <- list(
  model = safe_fit(
    feols(
      graduates_t_plus_h ~ any_visit_t | town_state_id + recruiting_school_year,
      cluster = ~town_state_id,
      data = analysis_panel_out_state
    )
  ),
  term = "any_visit_t",
  sample = "out_state"
)
model_specs$graduates_ols_lag_controls_out_state <- list(
  model = safe_fit(
    feols(
      graduates_t_plus_h ~ visits_t +
        pre_graduates_t_minus_1 + pre_graduates_t_minus_2 +
        pre_graduates_t_minus_3 + pre_graduates_t_minus_4 +
        pre_graduates_t_minus_5 |
        town_state_id + recruiting_school_year,
      cluster = ~town_state_id,
      data = analysis_panel_out_state
    )
  ),
  term = "visits_t",
  sample = "out_state_lag_controls"
)
model_specs$graduates_any_visit_ols_lag_controls_out_state <- list(
  model = safe_fit(
    feols(
      graduates_t_plus_h ~ any_visit_t +
        pre_graduates_t_minus_1 + pre_graduates_t_minus_2 +
        pre_graduates_t_minus_3 + pre_graduates_t_minus_4 +
        pre_graduates_t_minus_5 |
        town_state_id + recruiting_school_year,
      cluster = ~town_state_id,
      data = analysis_panel_out_state
    )
  ),
  term = "any_visit_t",
  sample = "out_state_lag_controls"
)
model_specs$graduates_fixed_baseline_normalized_out_state <- list(
  model = safe_fit(
    feols(
      graduates_per_fixed_baseline_plus_one_t_plus_h ~ visits_t |
        town_state_id + recruiting_school_year,
      cluster = ~town_state_id,
      data = analysis_panel_out_state
    )
  ),
  term = "visits_t",
  sample = "out_state"
)
model_specs$graduates_any_visit_fixed_baseline_normalized_out_state <- list(
  model = safe_fit(
    feols(
      graduates_per_fixed_baseline_plus_one_t_plus_h ~ any_visit_t |
        town_state_id + recruiting_school_year,
      cluster = ~town_state_id,
      data = analysis_panel_out_state
    )
  ),
  term = "any_visit_t",
  sample = "out_state"
)
model_specs$graduates_poisson <- list(
  model = safe_fit(
    fepois(
      graduates_t_plus_h ~ visits_t | town_state_id + recruiting_school_year,
      cluster = ~town_state_id,
      data = analysis_panel
    )
  ),
  term = "visits_t",
  sample = "preferred"
)
model_specs$graduates_poisson_same_state_fuzzy <- list(
  model = safe_fit(
    fepois(
      graduates_t_plus_h ~ visits_t_with_same_state_fuzzy | town_state_id + recruiting_school_year,
      cluster = ~town_state_id,
      data = analysis_panel
    )
  ),
  term = "visits_t_with_same_state_fuzzy",
  sample = "preferred"
)

if (nrow(denom_panel) > 0) {
  model_specs$graduates_share_ols <- list(
    model = safe_fit(
      feols(
        graduates_share_t_plus_h ~ visits_t | town_state_id + recruiting_school_year,
        cluster = ~town_state_id,
        data = denom_panel
      )
    ),
    term = "visits_t",
    sample = "denominator"
  )
  model_specs$graduates_share_any_visit_ols <- list(
    model = safe_fit(
      feols(
        graduates_share_t_plus_h ~ any_visit_t | town_state_id + recruiting_school_year,
        cluster = ~town_state_id,
        data = denom_panel
      )
    ),
    term = "any_visit_t",
    sample = "denominator"
  )
  model_specs$graduates_share_wls <- list(
    model = safe_fit(
      feols(
        graduates_share_t_plus_h ~ visits_t | town_state_id + recruiting_school_year,
        weights = ~total_grade12_enrollment_t,
        cluster = ~town_state_id,
        data = denom_panel
      )
    ),
    term = "visits_t",
    sample = "denominator"
  )
  model_specs$graduates_share_any_visit_wls <- list(
    model = safe_fit(
      feols(
        graduates_share_t_plus_h ~ any_visit_t | town_state_id + recruiting_school_year,
        weights = ~total_grade12_enrollment_t,
        cluster = ~town_state_id,
        data = denom_panel
      )
    ),
    term = "any_visit_t",
    sample = "denominator"
  )
  model_specs$graduates_share_wls_pretrend_controls <- list(
    model = safe_fit(
      feols(
        graduates_share_t_plus_h ~ visits_t + pre_graduates_mean_3 + pre_graduates_trend_3 |
          town_state_id + recruiting_school_year,
        weights = ~total_grade12_enrollment_t,
        cluster = ~town_state_id,
        data = denom_panel
      )
    ),
    term = "visits_t",
    sample = "denominator_pretrend_controls"
  )
  model_specs$graduates_share_any_visit_wls_pretrend_controls <- list(
    model = safe_fit(
      feols(
        graduates_share_t_plus_h ~ any_visit_t + pre_graduates_mean_3 + pre_graduates_trend_3 |
          town_state_id + recruiting_school_year,
        weights = ~total_grade12_enrollment_t,
        cluster = ~town_state_id,
        data = denom_panel
      )
    ),
    term = "any_visit_t",
    sample = "denominator_pretrend_controls"
  )
  model_specs$bachelors_share_wls <- list(
    model = safe_fit(
      feols(
        bachelor_graduates_share_t_plus_h ~ visits_t | town_state_id + recruiting_school_year,
        weights = ~total_grade12_enrollment_t,
        cluster = ~town_state_id,
        data = denom_panel
      )
    ),
    term = "visits_t",
    sample = "denominator"
  )
  model_specs$graduates_ppml_exposure <- list(
    model = safe_fit(
      fepois(
        graduates_t_plus_h ~ visits_t + offset(log(total_grade12_enrollment_t)) |
          town_state_id + recruiting_school_year,
        cluster = ~town_state_id,
        data = denom_panel
      )
    ),
    term = "visits_t",
    sample = "denominator"
  )
}

#=====================================================================
# 4 - Write coefficient and summary outputs
#=====================================================================

extract_visit_coef <- function(model_name, spec) {
  if (inherits(spec$model, "error")) {
    return(data.table(
      specification = model_name,
      sample = spec$sample,
      term = spec$term,
      estimate = NA_real_,
      std_error = NA_real_,
      statistic = NA_real_,
      p_value = NA_real_,
      nobs = NA_integer_,
      status = paste("model_error:", conditionMessage(spec$model))
    ))
  }
  coef_table <- coeftable(spec$model)
  if (!spec$term %in% rownames(coef_table)) {
    return(data.table(
      specification = model_name,
      sample = spec$sample,
      term = spec$term,
      estimate = NA_real_,
      std_error = NA_real_,
      statistic = NA_real_,
      p_value = NA_real_,
      nobs = nobs(spec$model),
      status = "term_not_estimated"
    ))
  }
  visit_row <- coef_table[spec$term, ]
  data.table(
    specification = model_name,
    sample = spec$sample,
    term = spec$term,
    estimate = visit_row[1],
    std_error = visit_row[2],
    statistic = visit_row[3],
    p_value = visit_row[4],
    nobs = nobs(spec$model),
    status = "ok"
  )
}

coef_rows <- rbindlist(Map(extract_visit_coef, names(model_specs), model_specs))
fwrite(coef_rows, coef_path)

valid_model_specs <- Filter(function(spec) !inherits(spec$model, "error"), model_specs)
models <- lapply(valid_model_specs, `[[`, "model")
names(models) <- names(valid_model_specs)

summary_lines <- capture.output({
  cat("UA visits in school year t and graduates in t+5\n")
  cat("Analysis sample: preferred_regression_sample == 1\n")
  cat(sprintf("Preferred rows: %s\n", nrow(analysis_panel)))
  cat(sprintf("Denominator rows: %s\n", nrow(denom_panel)))
  cat(sprintf("Fixed high-school denominator rows: %s\n", nrow(fixed_hs_panel)))
  cat("Fixed effects: town_state_id and recruiting_school_year\n")
  cat("Clustered standard errors: town_state_id\n\n")
  if (length(models) > 0) {
    print(etable(models))
  } else {
    cat("No models estimated successfully.\n")
  }
})
writeLines(summary_lines, summary_path)

cat(sprintf("Wrote coefficient table to %s\n", coef_path))
cat(sprintf("Wrote regression summary to %s\n", summary_path))
