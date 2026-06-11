#=====================================================================
## Author: Crossan Cooper
## Last Modified: 06-08-2026
#
## file use: Run targeted town-linear-trend checks for the outside-Alabama
## recruiting sample where downward pre-period trends are a concern.
#
## inputs:
## 1. town_matching/output/ua_visit_t_graduates_t_plus_5_panel.csv -- t+5 panel
## 2. town_matching/output/robustness/ua_visit_t_graduates_t_plus_h_panel.csv -- optional horizon panels
#
## outputs:
## 1. town_matching/output/robustness/ua_visit_t_graduates_t_plus_3_6_town_trend_coefficients.csv -- trend-check estimates
## 2. town_matching/output/robustness/ua_visit_t_graduates_t_plus_3_6_town_trend_coefficients_summary.txt -- trend-check summary
#=====================================================================

#=====================================================================
# 1 - Packages, arguments, and paths
#=====================================================================

# default list of packages and cleaning command
rm(list=ls())
if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table,fixest)
setFixest_nthreads(1)
setFixest_notes(FALSE)

args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(name, default = NA_character_) {
  prefix <- paste0("--", name, "=")
  hit <- args[startsWith(args, prefix)]
  if (length(hit) == 0) {
    return(default)
  }
  sub(prefix, "", hit[[1]], fixed = TRUE)
}

default_data_root <- "/Users/crossancooper/Dropbox/Professional/active-projects/admissions_project"
repo_root <- normalizePath(
  Sys.getenv("ADMISSIONS_PROJECT_DATA_ROOT", default_data_root),
  mustWork = FALSE
)
output_dir <- file.path(repo_root, "town_matching", "output", "robustness")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

output_path <- get_arg(
  "output",
  file.path(output_dir, "ua_visit_t_graduates_t_plus_3_6_town_trend_coefficients.csv")
)

horizons <- as.integer(strsplit(get_arg("horizons", "5,6"), ",", fixed = TRUE)[[1]])

panel_path_for_horizon <- function(horizon) {
  if (horizon == 5) {
    return(file.path(repo_root, "town_matching", "output", "ua_visit_t_graduates_t_plus_5_panel.csv"))
  }
  file.path(
    repo_root,
    "town_matching",
    "output",
    "robustness",
    sprintf("ua_visit_t_graduates_t_plus_%s_panel.csv", horizon)
  )
}

#=====================================================================
# 2 - Model helpers and trend-control construction
#=====================================================================

safe_fit <- function(expr) {
  tryCatch(
    eval.parent(substitute(expr)),
    error = function(error) error
  )
}

extract_term <- function(model, term) {
  if (inherits(model, "error")) {
    return(list(
      estimate = NA_real_,
      std_error = NA_real_,
      statistic = NA_real_,
      p_value = NA_real_,
      nobs = NA_integer_,
      status = paste("model_error:", conditionMessage(model))
    ))
  }
  coef_table <- coeftable(model)
  if (!term %in% rownames(coef_table)) {
    return(list(
      estimate = NA_real_,
      std_error = NA_real_,
      statistic = NA_real_,
      p_value = NA_real_,
      nobs = nobs(model),
      status = "term_not_estimated"
    ))
  }
  row <- coef_table[term, ]
  list(
    estimate = row[[1]],
    std_error = row[[2]],
    statistic = row[[3]],
    p_value = row[[4]],
    nobs = nobs(model),
    status = "ok"
  )
}

add_numeric_and_trends <- function(panel, horizon) {
  numeric_cols <- c(
    "recruiting_school_year",
    "preferred_regression_sample",
    "fixed_hs_denominator_regression_sample",
    "fixed_hs_grade12_enrollment",
    "visits_t",
    "any_visit_t",
    "visits_t_with_same_state_fuzzy",
    "any_visit_t_with_same_state_fuzzy",
    "graduates_t_plus_h",
    "predicted_graduates_t_plus_h",
    "graduates_share_fixed_hs_t_plus_h",
    "predicted_graduates_share_fixed_hs_t_plus_h",
    "pre_graduates_t_minus_1",
    "pre_graduates_t_minus_2",
    "pre_graduates_t_minus_3",
    "pre_graduates_t_minus_4",
    "pre_graduates_t_minus_5",
    "pre_graduates_mean_5",
    "pre_graduates_trend_3",
    "pre_graduates_trend_5",
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
  panel[, recruiting_year_numeric := as.numeric(recruiting_school_year)]
  panel[, recruiting_school_year := factor(recruiting_school_year)]

  panel[, pre_graduates_linear_slope_5 := (
    -2 * pre_graduates_t_minus_5 -
      pre_graduates_t_minus_4 +
      pre_graduates_t_minus_2 +
      2 * pre_graduates_t_minus_1
  ) / 10]
  panel[, pre_graduates_linear_prediction_t_plus_h :=
    pre_graduates_mean_5 + (horizon + 3) * pre_graduates_linear_slope_5]
  panel[, pre_graduates_mean_5_share_fixed_hs := rowMeans(
    .SD,
    na.rm = FALSE
  ), .SDcols = c(
    "pre_graduates_t_minus_1_share_fixed_hs",
    "pre_graduates_t_minus_2_share_fixed_hs",
    "pre_graduates_t_minus_3_share_fixed_hs",
    "pre_graduates_t_minus_4_share_fixed_hs",
    "pre_graduates_t_minus_5_share_fixed_hs"
  )]
  panel[, pre_graduates_linear_slope_5_share_fixed_hs := (
    -2 * pre_graduates_t_minus_5_share_fixed_hs -
      pre_graduates_t_minus_4_share_fixed_hs +
      pre_graduates_t_minus_2_share_fixed_hs +
      2 * pre_graduates_t_minus_1_share_fixed_hs
  ) / 10]
  panel[, pre_graduates_linear_prediction_share_fixed_hs_t_plus_h :=
    pre_graduates_mean_5_share_fixed_hs +
      (horizon + 3) * pre_graduates_linear_slope_5_share_fixed_hs]
  panel
}

make_rhs <- function(treatment, controls) {
  paste(c(treatment, controls), collapse = " + ")
}

estimate_model <- function(data, outcome, treatment, controls) {
  formula_text <- sprintf(
    "%s ~ %s | town_state_id[recruiting_year_numeric] + recruiting_school_year",
    outcome,
    make_rhs(treatment, controls)
  )
  safe_fit(feols(as.formula(formula_text), cluster = ~town_state_id, data = data))
}

#=====================================================================
# 3 - Specification grids
#=====================================================================

treatment_specs <- list(
  visit_count = "visits_t",
  any_visit = "any_visit_t",
  visit_count_same_state_fuzzy = "visits_t_with_same_state_fuzzy",
  any_visit_same_state_fuzzy = "any_visit_t_with_same_state_fuzzy"
)

count_control_specs <- list(
  fe_only = character(),
  lag5 = c(
    "pre_graduates_t_minus_1",
    "pre_graduates_t_minus_2",
    "pre_graduates_t_minus_3",
    "pre_graduates_t_minus_4",
    "pre_graduates_t_minus_5"
  ),
  trend_deltas = c("pre_graduates_t_minus_1", "pre_graduates_trend_3", "pre_graduates_trend_5"),
  trend_prediction = "pre_graduates_linear_prediction_t_plus_h",
  trend_prediction_predicted = c(
    "pre_graduates_linear_prediction_t_plus_h",
    "predicted_graduates_t_plus_h"
  ),
  trend_projection = c("pre_graduates_mean_5", "pre_graduates_linear_slope_5"),
  trend_projection_predicted = c(
    "pre_graduates_mean_5",
    "pre_graduates_linear_slope_5",
    "predicted_graduates_t_plus_h"
  )
)

share_control_specs <- list(
  fe_only = character(),
  lag5 = c(
    "pre_graduates_t_minus_1_share_fixed_hs",
    "pre_graduates_t_minus_2_share_fixed_hs",
    "pre_graduates_t_minus_3_share_fixed_hs",
    "pre_graduates_t_minus_4_share_fixed_hs",
    "pre_graduates_t_minus_5_share_fixed_hs"
  ),
  trend_deltas = c(
    "pre_graduates_t_minus_1_share_fixed_hs",
    "pre_graduates_trend_3_share_fixed_hs",
    "pre_graduates_trend_5_share_fixed_hs"
  ),
  trend_prediction = "pre_graduates_linear_prediction_share_fixed_hs_t_plus_h",
  trend_prediction_predicted = c(
    "pre_graduates_linear_prediction_share_fixed_hs_t_plus_h",
    "predicted_graduates_share_fixed_hs_t_plus_h"
  ),
  trend_projection = c(
    "pre_graduates_mean_5_share_fixed_hs",
    "pre_graduates_linear_slope_5_share_fixed_hs"
  ),
  trend_projection_predicted = c(
    "pre_graduates_mean_5_share_fixed_hs",
    "pre_graduates_linear_slope_5_share_fixed_hs",
    "predicted_graduates_share_fixed_hs_t_plus_h"
  )
)

#=====================================================================
# 4 - Estimate outside-Alabama trend checks and write outputs
#=====================================================================

rows <- list()

for (horizon in horizons) {
  panel_path <- panel_path_for_horizon(horizon)
  if (!file.exists(panel_path)) {
    stop("Panel file does not exist for horizon ", horizon, ": ", panel_path)
  }
  panel <- add_numeric_and_trends(fread(panel_path), horizon)
  out_state_panel <- panel[preferred_regression_sample == 1 & processed_state != "AL"]
  out_state_share_panel <- out_state_panel[
    fixed_hs_denominator_regression_sample == 1 &
      !is.na(fixed_hs_grade12_enrollment) &
      fixed_hs_grade12_enrollment > 0
  ]

  for (treatment_name in names(treatment_specs)) {
    treatment_var <- treatment_specs[[treatment_name]]
    for (control_name in names(count_control_specs)) {
      model <- estimate_model(
        out_state_panel,
        "graduates_t_plus_h",
        treatment_var,
        count_control_specs[[control_name]]
      )
      extracted <- extract_term(model, treatment_var)
      rows[[length(rows) + 1]] <- data.table(
        horizon = horizon,
        sample = "out_of_state",
        support_filter = "unrestricted",
        fixed_effect_set = "town_linear_trend",
        outcome_family = "count_ols",
        control_set = control_name,
        treatment = treatment_name,
        treatment_var = treatment_var,
        estimate = extracted$estimate,
        std_error = extracted$std_error,
        statistic = extracted$statistic,
        p_value = extracted$p_value,
        nobs = extracted$nobs,
        status = extracted$status
      )
    }
    for (control_name in names(share_control_specs)) {
      model <- estimate_model(
        out_state_share_panel,
        "graduates_share_fixed_hs_t_plus_h",
        treatment_var,
        share_control_specs[[control_name]]
      )
      extracted <- extract_term(model, treatment_var)
      rows[[length(rows) + 1]] <- data.table(
        horizon = horizon,
        sample = "out_of_state",
        support_filter = "unrestricted",
        fixed_effect_set = "town_linear_trend",
        outcome_family = "fixed_hs_share_ols",
        control_set = control_name,
        treatment = treatment_name,
        treatment_var = treatment_var,
        estimate = extracted$estimate,
        std_error = extracted$std_error,
        statistic = extracted$statistic,
        p_value = extracted$p_value,
        nobs = extracted$nobs,
        status = extracted$status
      )
    }
  }
}

coef_rows <- rbindlist(rows, fill = TRUE)
fwrite(coef_rows, output_path)

summary_path <- sub("\\.csv$", "_summary.txt", output_path)
summary_lines <- capture.output({
  cat("Outside-Alabama unrestricted town-linear-trend checks\n")
  cat(sprintf("Coefficient rows: %s\n\n", nrow(coef_rows)))
  print(coef_rows[, .N, by = .(status, outcome_family)][order(status, outcome_family)])
  cat("\nSign summary:\n")
  print(coef_rows[
    status == "ok",
    .(
      models = .N,
      positive = sum(estimate > 0),
      positive_p_lt_10 = sum(estimate > 0 & p_value < 0.10),
      negative_p_lt_10 = sum(estimate < 0 & p_value < 0.10)
    ),
    by = .(outcome_family)
  ])
})
writeLines(summary_lines, summary_path)

cat(sprintf("Wrote town-trend coefficients to %s\n", output_path))
cat(sprintf("Wrote town-trend summary to %s\n", summary_path))
