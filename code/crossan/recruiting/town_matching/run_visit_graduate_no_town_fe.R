#=====================================================================
## Author: Crossan Cooper
## Last Modified: 06-08-2026
#
## file use: Estimate no-town-fixed-effect PPML specifications and supporting
## count/share checks linking UA admissions visits to later town-level UA
## graduate outcomes.
#
## inputs:
## 1. town_matching/output/ua_visit_t_graduates_t_plus_5_panel.csv -- t+5 visit-to-graduate panel
## 2. town_matching/output/robustness/ua_visit_t_graduates_t_plus_h_panel.csv -- optional t+3/t+4/t+6 panels
## 3. town_matching/output/ua_high_school_denominators_online_panel_matched.csv -- fixed grade-12 denominators
## 4. town_matching/output/ua_acs_town_covariates_2017.csv -- ACS town covariates
#
## outputs:
## 1. town_matching/output/no_town_fe/ua_visit_graduate_no_town_fe_coefficients.csv -- no-town-FE estimates
## 2. town_matching/output/no_town_fe/ua_visit_graduate_no_town_fe_summary.txt -- no-town-FE summary
## 3. town_matching/output/no_town_fe/figures -- no-town-FE diagnostic figures
#=====================================================================

#=====================================================================
# 1 - Packages, arguments, and paths
#=====================================================================

# default list of packages and cleaning command
rm(list=ls())
if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table,fixest,ggplot2)
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
output_dir <- file.path(repo_root, "town_matching", "output", "no_town_fe")
figure_dir <- file.path(output_dir, "figures")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

horizons <- as.integer(strsplit(get_arg("horizons", "5,6"), ",", fixed = TRUE)[[1]])
denominator_path <- get_arg(
  "denominators",
  file.path(repo_root, "town_matching", "output", "ua_high_school_denominators_online_panel_matched.csv")
)
acs_path <- get_arg(
  "acs",
  file.path(repo_root, "town_matching", "output", "ua_acs_town_covariates_2017.csv")
)
coef_path <- file.path(output_dir, "ua_visit_graduate_no_town_fe_coefficients.csv")
summary_path <- file.path(output_dir, "ua_visit_graduate_no_town_fe_summary.txt")

#=====================================================================
# 2 - Load denominators, ACS controls, and horizon panels
#=====================================================================

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

denom <- fread(denominator_path)
acs <- if (file.exists(acs_path)) {
  fread(acs_path)
} else {
  data.table(processed_town = character(), processed_state = character())
}
denom_numeric_cols <- c(
  "school_year",
  "public_grade12_enrollment",
  "private_grade12_enrollment",
  "total_grade12_enrollment",
  "public_grade12_school_count",
  "private_grade12_school_count",
  "denominator_available"
)
denom_numeric_cols <- intersect(denom_numeric_cols, names(denom))
denom[, (denom_numeric_cols) := lapply(.SD, as.numeric), .SDcols = denom_numeric_cols]
fixed_denom <- denom[
  school_year == 2017,
  .(
    processed_town,
    processed_state,
    fixed_public_grade12_enrollment = public_grade12_enrollment,
    fixed_private_grade12_enrollment = private_grade12_enrollment,
    fixed_total_grade12_enrollment = total_grade12_enrollment,
    fixed_public_grade12_school_count = public_grade12_school_count,
    fixed_private_grade12_school_count = private_grade12_school_count
  )
]

required_acs_cols <- c(
  "processed_town",
  "processed_state",
  "acs_geoid",
  "acs_place_name",
  "acs_match_method",
  "acs_total_population",
  "acs_black_share",
  "acs_poverty_share",
  "acs_median_household_income",
  "acs_match_score"
)
missing_acs_cols <- setdiff(required_acs_cols, names(acs))
for (col in missing_acs_cols) {
  acs[, (col) := NA]
}
numeric_acs_cols <- intersect(
  c(
    "acs_total_population",
    "acs_black_share",
    "acs_poverty_share",
    "acs_median_household_income",
    "acs_match_score"
  ),
  names(acs)
)
acs[, (numeric_acs_cols) := lapply(.SD, as.numeric), .SDcols = numeric_acs_cols]
acs <- acs[
  acs_match_method %in% c("exact", "fuzzy"),
  .(
    processed_town,
    processed_state,
    acs_geoid,
    acs_place_name,
    acs_match_method,
    acs_total_population,
    acs_black_share,
    acs_poverty_share,
    acs_median_household_income
  )
]

standardize_in_place <- function(dt, source, target) {
  values <- dt[[source]]
  scale_mean <- mean(values, na.rm = TRUE)
  scale_sd <- sd(values, na.rm = TRUE)
  if (is.na(scale_sd) || scale_sd == 0) {
    dt[, (target) := NA_real_]
    return(invisible(NULL))
  }
  dt[, (target) := (get(source) - scale_mean) / scale_sd]
}

prepare_panel <- function(path, horizon) {
  panel <- fread(path)
  numeric_cols <- c(
    "recruiting_school_year",
    "preferred_regression_sample",
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
    "pre_graduates_mean_5",
    "pre_graduates_trend_5",
    "fixed_hs_grade12_enrollment",
    "graduates_share_fixed_hs_t_plus_h",
    "predicted_graduates_share_fixed_hs_t_plus_h",
    "pre_graduates_t_minus_1_share_fixed_hs",
    "pre_graduates_t_minus_2_share_fixed_hs",
    "pre_graduates_t_minus_3_share_fixed_hs",
    "pre_graduates_t_minus_4_share_fixed_hs",
    "pre_graduates_t_minus_5_share_fixed_hs",
    "pre_graduates_trend_5_share_fixed_hs"
  )
  numeric_cols <- intersect(numeric_cols, names(panel))
  panel[, (numeric_cols) := lapply(.SD, as.numeric), .SDcols = numeric_cols]

  panel <- merge(
    panel,
    fixed_denom,
    by = c("processed_town", "processed_state"),
    all.x = TRUE,
    sort = FALSE
  )
  panel <- merge(
    panel,
    acs,
    by = c("processed_town", "processed_state"),
    all.x = TRUE,
    sort = FALSE
  )

  panel[, horizon := horizon]
  panel[, recruiting_school_year := factor(recruiting_school_year)]
  panel[, state_group := fifelse(processed_state == "AL", "alabama", "out_of_state")]
  panel[, log_prior_ua_mean_5 := log1p(pre_graduates_mean_5)]
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
  panel[, log_fixed_grade12_enrollment := fifelse(
    fixed_total_grade12_enrollment > 0,
    log(fixed_total_grade12_enrollment),
    NA_real_
  )]
  panel[, fixed_private_grade12_share := fifelse(
    fixed_total_grade12_enrollment > 0,
    fixed_private_grade12_enrollment / fixed_total_grade12_enrollment,
    NA_real_
  )]
  panel[, log_acs_total_population := fifelse(
    acs_total_population > 0,
    log(acs_total_population),
    NA_real_
  )]
  panel[, log_acs_median_household_income := fifelse(
    acs_median_household_income > 0,
    log(acs_median_household_income),
    NA_real_
  )]
  panel[, has_acs_controls := (
    !is.na(log_acs_total_population) &
      !is.na(log_acs_median_household_income) &
      !is.na(acs_poverty_share) &
      !is.na(acs_black_share)
  )]

  standardized_vars <- c(
    "log_prior_ua_mean_5",
    "pre_graduates_trend_5",
    "predicted_graduates_t_plus_h",
    "pre_graduates_mean_5_share_fixed_hs",
    "pre_graduates_trend_5_share_fixed_hs",
    "predicted_graduates_share_fixed_hs_t_plus_h",
    "log_fixed_grade12_enrollment",
    "log_acs_total_population",
    "log_acs_median_household_income",
    "acs_poverty_share",
    "acs_black_share",
    "fixed_private_grade12_share",
    "fixed_public_grade12_school_count",
    "fixed_private_grade12_school_count"
  )
  for (var in standardized_vars) {
    standardize_in_place(panel, var, paste0("z_", var))
  }
  panel
}

#=====================================================================
# 3 - Model helpers and control sets
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

make_formula <- function(outcome, treatment, controls, fixed_effects) {
  rhs <- paste(c(treatment, controls), collapse = " + ")
  as.formula(sprintf("%s ~ %s | %s", outcome, rhs, fixed_effects))
}

estimate_model <- function(data, outcome, treatment, controls, fixed_effects) {
  safe_fit(feols(
    make_formula(outcome, treatment, controls, fixed_effects),
    cluster = ~town_state_id,
    data = data
  ))
}

make_ppml_exposure_formula <- function(outcome, treatment, controls, fixed_effects, exposure) {
  rhs <- paste(c(treatment, controls, sprintf("offset(log(%s))", exposure)), collapse = " + ")
  as.formula(sprintf("%s ~ %s | %s", outcome, rhs, fixed_effects))
}

estimate_ppml_exposure_model <- function(data, outcome, treatment, controls, fixed_effects, exposure) {
  safe_fit(fepois(
    make_ppml_exposure_formula(outcome, treatment, controls, fixed_effects, exposure),
    cluster = ~town_state_id,
    glm.iter = 100,
    data = data
  ))
}

estimate_ppml_baseline_rhs_model <- function(data, outcome, treatment, controls, fixed_effects) {
  safe_fit(fepois(
    make_formula(outcome, treatment, controls, fixed_effects),
    cluster = ~town_state_id,
    glm.iter = 200,
    data = data
  ))
}

complete_control_sample <- function(data, controls) {
  if (length(controls) == 0) {
    return(data)
  }
  existing_controls <- controls[controls %in% names(data)]
  if (length(existing_controls) == 0) {
    return(data)
  }
  data[complete.cases(data[, ..existing_controls])]
}

treatment_specs <- list(
  visit_count = "visits_t",
  any_visit = "any_visit_t",
  visit_count_same_state_fuzzy = "visits_t_with_same_state_fuzzy",
  any_visit_same_state_fuzzy = "any_visit_t_with_same_state_fuzzy"
)

sample_specs <- list(
  full = quote(TRUE),
  alabama = quote(processed_state == "AL"),
  out_of_state = quote(processed_state != "AL")
)

fixed_effect_specs <- list(
  year_fe = "recruiting_school_year",
  state_year_fe = "processed_state + recruiting_school_year"
)

acs_control_vars <- c(
  "z_log_acs_total_population",
  "z_log_acs_median_household_income",
  "z_acs_poverty_share",
  "z_acs_black_share"
)

count_control_specs <- list(
  no_controls = character(),
  prior_ua = c("z_log_prior_ua_mean_5", "z_pre_graduates_trend_5"),
  prior_predicted = c(
    "z_log_prior_ua_mean_5",
    "z_pre_graduates_trend_5",
    "z_predicted_graduates_t_plus_h"
  ),
  town_size = c(
    "z_log_prior_ua_mean_5",
    "z_pre_graduates_trend_5",
    "z_log_fixed_grade12_enrollment",
    "z_fixed_private_grade12_share",
    "z_fixed_public_grade12_school_count",
    "z_fixed_private_grade12_school_count"
  ),
  town_size_predicted = c(
    "z_log_prior_ua_mean_5",
    "z_pre_graduates_trend_5",
    "z_predicted_graduates_t_plus_h",
    "z_log_fixed_grade12_enrollment",
    "z_fixed_private_grade12_share",
    "z_fixed_public_grade12_school_count",
    "z_fixed_private_grade12_school_count"
  ),
  town_size_predicted_acs = c(
    "z_log_prior_ua_mean_5",
    "z_pre_graduates_trend_5",
    "z_predicted_graduates_t_plus_h",
    "z_log_fixed_grade12_enrollment",
    "z_fixed_private_grade12_share",
    "z_fixed_public_grade12_school_count",
    "z_fixed_private_grade12_school_count",
    acs_control_vars
  )
)

share_control_specs <- list(
  no_controls = character(),
  prior_ua_share = c(
    "z_pre_graduates_mean_5_share_fixed_hs",
    "z_pre_graduates_trend_5_share_fixed_hs"
  ),
  prior_share_predicted = c(
    "z_pre_graduates_mean_5_share_fixed_hs",
    "z_pre_graduates_trend_5_share_fixed_hs",
    "z_predicted_graduates_share_fixed_hs_t_plus_h"
  ),
  exposure_predicted_acs = c(
    "z_pre_graduates_mean_5_share_fixed_hs",
    "z_pre_graduates_trend_5_share_fixed_hs",
    "z_predicted_graduates_share_fixed_hs_t_plus_h",
    "z_fixed_private_grade12_share",
    "z_fixed_public_grade12_school_count",
    "z_fixed_private_grade12_school_count",
    acs_control_vars
  ),
  town_size = c(
    "z_pre_graduates_mean_5_share_fixed_hs",
    "z_pre_graduates_trend_5_share_fixed_hs",
    "z_log_fixed_grade12_enrollment",
    "z_fixed_private_grade12_share",
    "z_fixed_public_grade12_school_count",
    "z_fixed_private_grade12_school_count"
  ),
  town_size_predicted = c(
    "z_pre_graduates_mean_5_share_fixed_hs",
    "z_pre_graduates_trend_5_share_fixed_hs",
    "z_predicted_graduates_share_fixed_hs_t_plus_h",
    "z_log_fixed_grade12_enrollment",
    "z_fixed_private_grade12_share",
    "z_fixed_public_grade12_school_count",
    "z_fixed_private_grade12_school_count"
  ),
  town_size_predicted_acs = c(
    "z_pre_graduates_mean_5_share_fixed_hs",
    "z_pre_graduates_trend_5_share_fixed_hs",
    "z_predicted_graduates_share_fixed_hs_t_plus_h",
    "z_log_fixed_grade12_enrollment",
    "z_fixed_private_grade12_share",
    "z_fixed_public_grade12_school_count",
    "z_fixed_private_grade12_school_count",
    acs_control_vars
  )
)

# These control sets include log fixed grade-12 enrollment on the RHS. This is
# the PPML counterpart to the no-town-FE count models where baseline town size
# is estimated as a covariate instead of imposed as an exposure with unit
# elasticity.
ppml_baseline_rhs_control_sets <- c("town_size", "town_size_predicted", "town_size_predicted_acs")

#=====================================================================
# 4 - Estimate model grid
#=====================================================================

rows <- list()

for (horizon in horizons) {
  panel <- prepare_panel(panel_path_for_horizon(horizon), horizon)
  preferred <- panel[preferred_regression_sample == 1]
  fixed_hs <- preferred[
    fixed_hs_denominator_regression_sample == 1 &
      !is.na(fixed_hs_grade12_enrollment) &
      fixed_hs_grade12_enrollment > 0
  ]

  for (sample_name in names(sample_specs)) {
    sample_expr <- sample_specs[[sample_name]]
    count_sample_full <- preferred[eval(sample_expr)]
    share_sample <- fixed_hs[eval(sample_expr)]

    for (fixed_effect_name in names(fixed_effect_specs)) {
      fixed_effects <- fixed_effect_specs[[fixed_effect_name]]
      for (treatment_name in names(treatment_specs)) {
        treatment_var <- treatment_specs[[treatment_name]]

        for (control_name in names(count_control_specs)) {
          controls <- count_control_specs[[control_name]]
          count_sample <- count_sample_full
          if (control_name %in% c("town_size", "town_size_predicted", "town_size_predicted_acs")) {
            count_sample <- count_sample[
              !is.na(log_fixed_grade12_enrollment)
            ]
          }
          count_sample <- complete_control_sample(count_sample, controls)
          model <- estimate_model(
            count_sample,
            "graduates_t_plus_h",
            treatment_var,
            controls,
            fixed_effects
          )
          extracted <- extract_term(model, treatment_var)
          rows[[length(rows) + 1]] <- data.table(
            horizon = horizon,
            sample = sample_name,
            fixed_effect_set = fixed_effect_name,
            outcome_family = "count_ols",
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

        for (control_name in names(share_control_specs)) {
          controls <- share_control_specs[[control_name]]
          share_model_sample <- complete_control_sample(share_sample, controls)
          model <- estimate_model(
            share_model_sample,
            "graduates_share_fixed_hs_t_plus_h",
            treatment_var,
            controls,
            fixed_effects
          )
          extracted <- extract_term(model, treatment_var)
          rows[[length(rows) + 1]] <- data.table(
            horizon = horizon,
            sample = sample_name,
            fixed_effect_set = fixed_effect_name,
            outcome_family = "fixed_hs_share_ols",
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

        for (control_name in names(share_control_specs)) {
          controls <- share_control_specs[[control_name]]
          ppml_exposure_sample <- complete_control_sample(share_sample, controls)
          model <- estimate_ppml_exposure_model(
            ppml_exposure_sample,
            "graduates_t_plus_h",
            treatment_var,
            controls,
            fixed_effects,
            "fixed_hs_grade12_enrollment"
          )
          extracted <- extract_term(model, treatment_var)
          rows[[length(rows) + 1]] <- data.table(
            horizon = horizon,
            sample = sample_name,
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

        for (control_name in ppml_baseline_rhs_control_sets) {
          controls <- count_control_specs[[control_name]]
          ppml_rhs_sample <- count_sample_full[
            !is.na(log_fixed_grade12_enrollment)
          ]
          ppml_rhs_sample <- complete_control_sample(ppml_rhs_sample, controls)
          model <- estimate_ppml_baseline_rhs_model(
            ppml_rhs_sample,
            "graduates_t_plus_h",
            treatment_var,
            controls,
            fixed_effects
          )
          extracted <- extract_term(model, treatment_var)
          rows[[length(rows) + 1]] <- data.table(
            horizon = horizon,
            sample = sample_name,
            fixed_effect_set = fixed_effect_name,
            outcome_family = "count_ppml_baseline_rhs",
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
      }
    }
  }
}

coef_dt <- rbindlist(rows, fill = TRUE)
fwrite(coef_dt, coef_path)

#=====================================================================
# 5 - Figures and summary output
#=====================================================================

save_plot <- function(plot_obj, basename, width = 10, height = 7) {
  pdf_path <- file.path(figure_dir, paste0(basename, ".pdf"))
  png_path <- file.path(figure_dir, paste0(basename, ".png"))
  ggsave(pdf_path, plot_obj, width = width, height = height, units = "in", device = "pdf")
  ggsave(png_path, plot_obj, width = width, height = height, units = "in", dpi = 220)
}

plot_theme <- theme_minimal(base_size = 10) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold")
  )

plot_dt <- coef_dt[
  status == "ok" &
    fixed_effect_set == "state_year_fe" &
    outcome_family == "fixed_hs_share_ols" &
    sample %in% c("full", "alabama", "out_of_state") &
    treatment %in% c("any_visit", "visit_count") &
    control_set %in% c("no_controls", "prior_ua_share", "town_size", "town_size_predicted")
]
plot_dt[, low := estimate - std_error]
plot_dt[, high := estimate + std_error]
plot_dt[, horizon_label := factor(paste0("t+", horizon), levels = paste0("t+", sort(unique(horizon))))]
plot_dt[, sample_label := fifelse(
  sample == "full",
  "Full sample",
  fifelse(sample == "alabama", "Alabama", "Outside Alabama")
)]
plot_dt[, treatment_label := fifelse(treatment == "any_visit", "Any visit", "Visit count")]
plot_dt[, control_label := factor(
  control_set,
  levels = c("no_controls", "prior_ua_share", "town_size", "town_size_predicted"),
  labels = c("No controls", "Prior UA shares", "Town size controls", "Town size + predicted")
)]

p_share <- ggplot(
  plot_dt,
  aes(x = horizon_label, y = estimate, ymin = low, ymax = high, color = control_label)
) +
  geom_hline(yintercept = 0, color = "gray40", linewidth = 0.3) +
  geom_errorbar(position = position_dodge(width = 0.6), width = 0.12, linewidth = 0.35) +
  geom_point(position = position_dodge(width = 0.6), size = 1.2) +
  facet_grid(sample_label ~ treatment_label) +
  labs(
    x = "Graduate outcome year relative to recruiting school year",
    y = "Coefficient on visits, fixed high-school share",
    title = "No-town-FE share models with state and recruiting-year fixed effects",
    subtitle = "Points are coefficient estimates; bars show one standard error"
  ) +
  plot_theme
save_plot(p_share, "no_town_fe_fixed_hs_share_state_year_fe", width = 12, height = 7)

summary_lines <- capture.output({
  cat("No-town-FE visits-to-graduates coefficient grid\n")
  cat(sprintf("Coefficient rows: %s\n\n", nrow(coef_dt)))
  cat("Status counts:\n")
  print(coef_dt[, .N, by = .(status, fixed_effect_set, outcome_family)][
    order(status, fixed_effect_set, outcome_family)
  ])
  cat("\nSign summary, state/year FE:\n")
  print(coef_dt[
    status == "ok" & fixed_effect_set == "state_year_fe",
    .(
      models = .N,
      positive = sum(estimate > 0),
      positive_p_lt_10 = sum(estimate > 0 & p_value < 0.10),
      negative_p_lt_10 = sum(estimate < 0 & p_value < 0.10)
    ),
    by = .(sample, outcome_family)
  ][order(sample, outcome_family)])
  cat("\nPreferred controlled share models, any visit, state/year FE:\n")
  print(coef_dt[
    status == "ok" &
      fixed_effect_set == "state_year_fe" &
      outcome_family == "fixed_hs_share_ols" &
      treatment == "any_visit" &
      control_set %in% c("town_size", "town_size_predicted"),
    .(horizon, sample, control_set, estimate, std_error, p_value, nobs)
  ][order(horizon, sample, control_set)])
})
writeLines(summary_lines, summary_path)

cat(sprintf("Wrote no-town-FE coefficients to %s\n", coef_path))
cat(sprintf("Wrote no-town-FE summary to %s\n", summary_path))
cat(sprintf("Wrote no-town-FE figures to %s\n", figure_dir))
