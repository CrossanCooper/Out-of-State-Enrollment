#=====================================================================
## Author: Crossan Cooper
## Last Modified: 06-08-2026
#
## file use: Run the town-fixed-effect coefficient grid for town-level UA
## recruiting models. PPML rows are the main estimates; OLS/share rows and
## support-filter variants are supporting checks.
#
## inputs:
## 1. town_matching/output/ua_visit_t_graduates_t_plus_5_panel.csv -- default visit-to-graduate panel
## 2. town_matching/output/robustness/ua_visit_t_graduates_t_plus_h_panel.csv -- optional horizon panels
#
## outputs:
## 1. town_matching/output/robustness/ua_visit_t_graduates_t_plus_h_robustness_coefficients.csv -- coefficient grid
## 2. town_matching/output/robustness/ua_visit_t_graduates_t_plus_h_robustness_coefficients_summary.txt -- model status summary
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

panel_path <- get_arg(
  "panel",
  file.path(repo_root, "town_matching", "output", "ua_visit_t_graduates_t_plus_5_panel.csv")
)
horizon <- get_arg("horizon", "5")
coef_path <- get_arg(
  "output",
  file.path(output_dir, sprintf("ua_visit_t_graduates_t_plus_%s_robustness_coefficients.csv", horizon))
)
summary_path <- sub("\\.csv$", "_summary.txt", coef_path)

#=====================================================================
# 2 - Load panel and construct lagged-trend controls
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
  "visits_t_with_same_state_fuzzy",
  "any_visit_t_with_same_state_fuzzy",
  "graduates_t_plus_h",
  "predicted_graduates_t_plus_h",
  "pre_graduates_t_minus_1",
  "pre_graduates_t_minus_2",
  "pre_graduates_t_minus_3",
  "pre_graduates_t_minus_4",
  "pre_graduates_t_minus_5",
  "pre_graduates_mean_3",
  "pre_graduates_mean_5",
  "pre_graduates_trend_3",
  "pre_graduates_trend_5",
  "fixed_baseline_graduates",
  "graduates_per_fixed_baseline_plus_one_t_plus_h",
  "pre_graduates_growth_since_fixed_baseline",
  "fixed_hs_grade12_enrollment",
  "graduates_share_fixed_hs_t_plus_h",
  "predicted_graduates_share_fixed_hs_t_plus_h",
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
horizon_numeric <- as.numeric(horizon)
panel[, recruiting_year_numeric := as.numeric(recruiting_school_year)]
panel[, recruiting_school_year := factor(recruiting_school_year)]

# Trend-targeting controls use only outcomes observed before recruiting school year t.
# The five-lag linear slope fits counts/shares at t-5,...,t-1 against event time
# -5,...,-1. The t+h projection summarizes the town's pre-visit trajectory.
panel[, pre_graduates_linear_slope_5 := (
  -2 * pre_graduates_t_minus_5 -
    pre_graduates_t_minus_4 +
    pre_graduates_t_minus_2 +
    2 * pre_graduates_t_minus_1
) / 10]
panel[, pre_graduates_linear_prediction_t_plus_h :=
  pre_graduates_mean_5 + (horizon_numeric + 3) * pre_graduates_linear_slope_5]
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
    (horizon_numeric + 3) * pre_graduates_linear_slope_5_share_fixed_hs]
panel[
  preferred_regression_sample == 1,
  town_max_graduates_t_plus_h := max(graduates_t_plus_h, na.rm = TRUE),
  by = town_state_id
]

#=====================================================================
# 3 - Model helpers and specification grids
#=====================================================================

safe_fit <- function(expr) {
  tryCatch(
    eval.parent(substitute(expr)),
    error = function(error) error
  )
}

get_adj_r2 <- function(model) {
  value <- tryCatch(
    as.numeric(fitstat(model, "ar2")),
    error = function(error) NA_real_
  )
  if (length(value) == 0) {
    return(NA_real_)
  }
  value[[1]]
}

get_adj_pseudo_r2 <- function(model) {
  value <- tryCatch(
    as.numeric(fitstat(model, "apr2")),
    error = function(error) NA_real_
  )
  if (length(value) == 0) {
    return(NA_real_)
  }
  value[[1]]
}

get_log_likelihood <- function(model) {
  value <- tryCatch(
    as.numeric(logLik(model)),
    error = function(error) NA_real_
  )
  if (length(value) == 0) {
    return(NA_real_)
  }
  value[[1]]
}

sample_specs <- list(
  full = quote(TRUE),
  alabama = quote(processed_state == "AL"),
  out_of_state = quote(processed_state != "AL")
)

treatment_specs <- list(
  visit_count = "visits_t",
  any_visit = "any_visit_t",
  visit_count_same_state_fuzzy = "visits_t_with_same_state_fuzzy",
  any_visit_same_state_fuzzy = "any_visit_t_with_same_state_fuzzy"
)
visit_count_var_by_treatment <- list(
  visit_count = "visits_t",
  any_visit = "visits_t",
  visit_count_same_state_fuzzy = "visits_t_with_same_state_fuzzy",
  any_visit_same_state_fuzzy = "visits_t_with_same_state_fuzzy"
)

support_specs <- list(
  unrestricted = list(type = "none", cap = NA_real_),
  cell_visits_0_1 = list(type = "cell_visits_0_1", cap = NA_real_),
  town_max_grad_le_5 = list(type = "town_max_grad", cap = 5),
  town_max_grad_le_10 = list(type = "town_max_grad", cap = 10),
  town_max_grad_le_20 = list(type = "town_max_grad", cap = 20),
  cell_visits_0_1_town_max_grad_le_10 = list(type = "cell_visits_0_1_town_max_grad", cap = 10)
)
requested_support_filters <- strsplit(
  get_arg("support_filters", paste(names(support_specs), collapse = ",")),
  ",",
  fixed = TRUE
)[[1]]
unknown_support_filters <- setdiff(requested_support_filters, names(support_specs))
if (length(unknown_support_filters) > 0) {
  stop("Unknown support filters: ", paste(unknown_support_filters, collapse = ", "))
}
support_specs <- support_specs[requested_support_filters]

fixed_effect_specs <- list(
  town_year_fe = "town_state_id + recruiting_school_year"
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
  pretrend = c("pre_graduates_mean_3", "pre_graduates_trend_3"),
  predicted = "predicted_graduates_t_plus_h",
  lag5_predicted = c(
    "pre_graduates_t_minus_1",
    "pre_graduates_t_minus_2",
    "pre_graduates_t_minus_3",
    "pre_graduates_t_minus_4",
    "pre_graduates_t_minus_5",
    "predicted_graduates_t_plus_h"
  ),
  baseline_growth = "pre_graduates_growth_since_fixed_baseline",
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
  pretrend = c("pre_graduates_t_minus_1_share_fixed_hs", "pre_graduates_trend_3_share_fixed_hs"),
  predicted = "predicted_graduates_share_fixed_hs_t_plus_h",
  lag5_predicted = c(
    "pre_graduates_t_minus_1_share_fixed_hs",
    "pre_graduates_t_minus_2_share_fixed_hs",
    "pre_graduates_t_minus_3_share_fixed_hs",
    "pre_graduates_t_minus_4_share_fixed_hs",
    "pre_graduates_t_minus_5_share_fixed_hs",
    "predicted_graduates_share_fixed_hs_t_plus_h"
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

make_rhs <- function(treatment, controls) {
  rhs_terms <- c(treatment, controls)
  paste(rhs_terms, collapse = " + ")
}

#=====================================================================
# 4 - Estimate town-FE coefficient grid and write outputs
#=====================================================================

estimate_feols <- function(data, outcome, treatment, controls, fixed_effects, weights = NULL) {
  formula_text <- sprintf(
    "%s ~ %s | %s",
    outcome,
    make_rhs(treatment, controls),
    fixed_effects
  )
  if (!is.null(weights)) {
    return(
      safe_fit(
        feols(
          as.formula(formula_text),
          weights = as.formula(paste0("~", weights)),
          cluster = ~town_state_id,
          data = data
        )
      )
    )
  }
  safe_fit(
    feols(
      as.formula(formula_text),
      cluster = ~town_state_id,
      data = data
    )
  )
}

estimate_fepois_exposure <- function(data, outcome, treatment, controls, fixed_effects, exposure) {
  formula_text <- sprintf(
    "%s ~ %s + offset(log(%s)) | %s",
    outcome,
    make_rhs(treatment, controls),
    exposure,
    fixed_effects
  )
  safe_fit(
    fepois(
      as.formula(formula_text),
      cluster = ~town_state_id,
      glm.iter = 100,
      data = data
    )
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
      adj_r2 = NA_real_,
      adj_pseudo_r2 = NA_real_,
      log_likelihood = NA_real_,
      status = paste("model_error:", conditionMessage(model))
    ))
  }
  if (!is.null(model$convStatus) && !isTRUE(model$convStatus)) {
    return(list(
      estimate = NA_real_,
      std_error = NA_real_,
      statistic = NA_real_,
      p_value = NA_real_,
      nobs = nobs(model),
      adj_r2 = get_adj_r2(model),
      adj_pseudo_r2 = get_adj_pseudo_r2(model),
      log_likelihood = get_log_likelihood(model),
      status = "model_not_converged"
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
      adj_r2 = get_adj_r2(model),
      adj_pseudo_r2 = get_adj_pseudo_r2(model),
      log_likelihood = get_log_likelihood(model),
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
    adj_r2 = get_adj_r2(model),
    adj_pseudo_r2 = get_adj_pseudo_r2(model),
    log_likelihood = get_log_likelihood(model),
    status = "ok"
  )
}

rows <- list()

append_model <- function(
  horizon,
  sample_name,
  support_name,
  fixed_effect_name,
  fixed_effects,
  outcome_family,
  control_name,
  treatment_name,
  treatment_var,
  data,
  outcome,
  controls,
  weights = NULL
) {
  model <- estimate_feols(data, outcome, treatment_var, controls, fixed_effects, weights = weights)
  extracted <- extract_term(model, treatment_var)
  specification <- paste(
    outcome_family,
    fixed_effect_name,
    control_name,
    treatment_name,
    sample_name,
    support_name,
    sep = "__"
  )
  rows[[length(rows) + 1]] <<- data.table(
    horizon = as.integer(horizon),
    specification = specification,
    sample = sample_name,
    support_filter = support_name,
    fixed_effect_set = fixed_effect_name,
    outcome_family = outcome_family,
    control_set = control_name,
    treatment = treatment_name,
    treatment_var = treatment_var,
    estimate = extracted$estimate,
    std_error = extracted$std_error,
    statistic = extracted$statistic,
    p_value = extracted$p_value,
    nobs = extracted$nobs,
    adj_r2 = extracted$adj_r2,
    adj_pseudo_r2 = extracted$adj_pseudo_r2,
    log_likelihood = extracted$log_likelihood,
    status = extracted$status
  )
}

append_ppml_model <- function(
  horizon,
  sample_name,
  support_name,
  fixed_effect_name,
  fixed_effects,
  control_name,
  treatment_name,
  treatment_var,
  data,
  controls
) {
  model <- estimate_fepois_exposure(
    data = data,
    outcome = "graduates_t_plus_h",
    treatment = treatment_var,
    controls = controls,
    fixed_effects = fixed_effects,
    exposure = "fixed_hs_grade12_enrollment"
  )
  extracted <- extract_term(model, treatment_var)
  specification <- paste(
    "count_ppml_exposure",
    fixed_effect_name,
    control_name,
    treatment_name,
    sample_name,
    support_name,
    sep = "__"
  )
  rows[[length(rows) + 1]] <<- data.table(
    horizon = as.integer(horizon),
    specification = specification,
    sample = sample_name,
    support_filter = support_name,
    fixed_effect_set = fixed_effect_name,
    outcome_family = "count_ppml_exposure",
    control_set = control_name,
    treatment = treatment_name,
    treatment_var = treatment_var,
    estimate = extracted$estimate,
    std_error = extracted$std_error,
    statistic = extracted$statistic,
    p_value = extracted$p_value,
    nobs = extracted$nobs,
    adj_r2 = extracted$adj_r2,
    adj_pseudo_r2 = extracted$adj_pseudo_r2,
    log_likelihood = extracted$log_likelihood,
    status = extracted$status
  )
}

for (sample_name in names(sample_specs)) {
  sample_expr <- sample_specs[[sample_name]]
  sample_panel <- panel[preferred_regression_sample == 1 & eval(sample_expr)]

  for (treatment_name in names(treatment_specs)) {
    treatment_var <- treatment_specs[[treatment_name]]
    visit_count_var <- visit_count_var_by_treatment[[treatment_name]]
    for (support_name in names(support_specs)) {
      support_spec <- support_specs[[support_name]]
      support_keep <- rep(TRUE, nrow(sample_panel))
      if (support_spec$type == "cell_visits_0_1") {
        support_keep <- sample_panel[[visit_count_var]] <= 1
      } else if (support_spec$type == "town_max_grad") {
        support_keep <- sample_panel[["town_max_graduates_t_plus_h"]] <= support_spec$cap
      } else if (support_spec$type == "cell_visits_0_1_town_max_grad") {
        support_keep <- sample_panel[[visit_count_var]] <= 1 &
          sample_panel[["town_max_graduates_t_plus_h"]] <= support_spec$cap
      }
      support_panel <- sample_panel[which(support_keep), ]
      fixed_hs_keep <- support_panel[["fixed_hs_denominator_regression_sample"]] == 1 &
        !is.na(support_panel[["fixed_hs_grade12_enrollment"]]) &
        support_panel[["fixed_hs_grade12_enrollment"]] > 0
      fixed_hs_sample <- support_panel[which(fixed_hs_keep), ]

      for (fixed_effect_name in names(fixed_effect_specs)) {
        fixed_effects <- fixed_effect_specs[[fixed_effect_name]]
        for (control_name in names(count_control_specs)) {
          append_model(
            horizon = horizon,
            sample_name = sample_name,
            support_name = support_name,
            fixed_effect_name = fixed_effect_name,
            fixed_effects = fixed_effects,
            outcome_family = "count_ols",
            control_name = control_name,
            treatment_name = treatment_name,
            treatment_var = treatment_var,
            data = support_panel,
            outcome = "graduates_t_plus_h",
            controls = count_control_specs[[control_name]]
          )
        }
        if (nrow(fixed_hs_sample) > 0) {
          for (control_name in names(share_control_specs)) {
            append_model(
              horizon = horizon,
              sample_name = sample_name,
              support_name = support_name,
              fixed_effect_name = fixed_effect_name,
              fixed_effects = fixed_effects,
              outcome_family = "fixed_hs_share_ols",
              control_name = control_name,
              treatment_name = treatment_name,
              treatment_var = treatment_var,
              data = fixed_hs_sample,
              outcome = "graduates_share_fixed_hs_t_plus_h",
              controls = share_control_specs[[control_name]]
            )
            append_ppml_model(
              horizon = horizon,
              sample_name = sample_name,
              support_name = support_name,
              fixed_effect_name = fixed_effect_name,
              fixed_effects = fixed_effects,
              control_name = control_name,
              treatment_name = treatment_name,
              treatment_var = treatment_var,
              data = fixed_hs_sample,
              controls = share_control_specs[[control_name]]
            )
          }
        }
      }
    }
  }
}

coef_rows <- rbindlist(rows, fill = TRUE)
fwrite(coef_rows, coef_path)

summary_lines <- capture.output({
  cat(sprintf("UA recruiting town-FE coefficient grid, horizon t+%s\n", horizon))
  cat(sprintf("Panel path: %s\n", panel_path))
  cat(sprintf("Coefficient rows: %s\n", nrow(coef_rows)))
  cat("Samples: full, alabama, out_of_state\n")
  cat("Treatments: visit_count, any_visit, same-state fuzzy versions\n")
  cat("Support filters: unrestricted, cell_visits_0_1, town max graduate caps, combined 0/1 visits + max <= 10\n")
  cat("Fixed-effect sets: town_year_fe\n")
  cat("Outcome families: count_ols, fixed_hs_share_ols, count_ppml_exposure\n\n")
  cat("Status counts:\n")
  print(coef_rows[, .N, by = .(status, fixed_effect_set, outcome_family)][
    order(status, fixed_effect_set, outcome_family)
  ])
  cat("\nSign summary for successful models:\n")
  print(coef_rows[
    status == "ok",
    .(
      models = .N,
      positive = sum(estimate > 0),
      positive_p_lt_10 = sum(estimate > 0 & p_value < 0.10),
      negative_p_lt_10 = sum(estimate < 0 & p_value < 0.10)
    ),
    by = .(sample, fixed_effect_set, outcome_family)
  ][order(sample, fixed_effect_set, outcome_family)])
})
writeLines(summary_lines, summary_path)

cat(sprintf("Wrote town-FE coefficient grid to %s\n", coef_path))
cat(sprintf("Wrote town-FE grid summary to %s\n", summary_path))
