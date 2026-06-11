#=====================================================================
## Author: Crossan Cooper
## Last Modified: 06-08-2026
#
## file use: Describe and estimate town-level predictors of UA admissions
## officer visits, including prior UA demand, town size, and ACS covariates.
#
## inputs:
## 1. town_matching/output/ua_visit_t_graduates_t_plus_5_panel.csv -- visit panel
## 2. town_matching/output/ua_high_school_denominators_online_panel_matched.csv -- fixed grade-12 denominators
## 3. town_matching/output/ua_acs_town_covariates_2017.csv -- ACS town covariates
#
## outputs:
## 1. town_matching/output/visit_drivers/recruiting_visit_driver_coefficients.csv -- visit-driver estimates
## 2. town_matching/output/visit_drivers/recruiting_visit_driver_balance.csv -- balance table
## 3. town_matching/output/visit_drivers/recruiting_visit_rate_by_pretrend.csv -- pretrend visit rates
## 4. town_matching/output/visit_drivers/figures -- coefficient and pretrend figures
## 5. town_matching/output/visit_drivers/recruiting_visit_driver_summary.txt -- summary text
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
output_dir <- file.path(repo_root, "town_matching", "output", "visit_drivers")
figure_dir <- file.path(output_dir, "figures")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

panel_path <- get_arg(
  "panel",
  file.path(repo_root, "town_matching", "output", "ua_visit_t_graduates_t_plus_5_panel.csv")
)
denominator_path <- get_arg(
  "denominators",
  file.path(repo_root, "town_matching", "output", "ua_high_school_denominators_online_panel_matched.csv")
)
acs_path <- get_arg(
  "acs",
  file.path(repo_root, "town_matching", "output", "ua_acs_town_covariates_2017.csv")
)
coef_path <- file.path(output_dir, "recruiting_visit_driver_coefficients.csv")
summary_path <- file.path(output_dir, "recruiting_visit_driver_summary.txt")
balance_path <- file.path(output_dir, "recruiting_visit_driver_balance.csv")
trend_rate_path <- file.path(output_dir, "recruiting_visit_rate_by_pretrend.csv")

#=====================================================================
# 2 - Load and merge visit, denominator, and ACS data
#=====================================================================

panel <- fread(panel_path)
denom <- fread(denominator_path)
acs <- if (file.exists(acs_path)) {
  fread(acs_path)
} else {
  data.table(processed_town = character(), processed_state = character())
}

numeric_panel_cols <- c(
  "recruiting_school_year",
  "preferred_regression_sample",
  "visits_t",
  "any_visit_t",
  "visits_t_with_same_state_fuzzy",
  "any_visit_t_with_same_state_fuzzy",
  "pre_graduates_t_minus_1",
  "pre_graduates_t_minus_2",
  "pre_graduates_t_minus_3",
  "pre_graduates_t_minus_4",
  "pre_graduates_t_minus_5",
  "pre_graduates_mean_3",
  "pre_graduates_mean_5",
  "pre_graduates_trend_3",
  "pre_graduates_trend_5"
)
existing_panel_cols <- intersect(numeric_panel_cols, names(panel))
panel[, (existing_panel_cols) := lapply(.SD, as.numeric), .SDcols = existing_panel_cols]

numeric_denom_cols <- c(
  "school_year",
  "public_grade12_enrollment",
  "private_grade12_enrollment",
  "total_grade12_enrollment",
  "public_grade12_school_count",
  "private_grade12_school_count",
  "denominator_available"
)
existing_denom_cols <- intersect(numeric_denom_cols, names(denom))
denom[, (existing_denom_cols) := lapply(.SD, as.numeric), .SDcols = existing_denom_cols]

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

fixed_denom <- denom[
  school_year == 2017,
  .(
    processed_town,
    processed_state,
    fixed_public_grade12_enrollment = public_grade12_enrollment,
    fixed_private_grade12_enrollment = private_grade12_enrollment,
    fixed_total_grade12_enrollment = total_grade12_enrollment,
    fixed_public_grade12_school_count = public_grade12_school_count,
    fixed_private_grade12_school_count = private_grade12_school_count,
    fixed_denominator_available = denominator_available
  )
]

analysis <- merge(
  panel[preferred_regression_sample == 1],
  fixed_denom,
  by = c("processed_town", "processed_state"),
  all.x = TRUE,
  sort = FALSE
)
analysis <- merge(
  analysis,
  acs,
  by = c("processed_town", "processed_state"),
  all.x = TRUE,
  sort = FALSE
)

#=====================================================================
# 3 - Construct targeting controls
#=====================================================================

analysis[, recruiting_school_year := factor(recruiting_school_year)]
analysis[, in_alabama := as.integer(processed_state == "AL")]
analysis[, state_group := ifelse(processed_state == "AL", "Alabama", "Outside Alabama")]

# Pre-period slope from t-5,...,t-1. This is only based on information before
# the recruiting school year and is useful for testing downward-trend targeting.
analysis[, pre_graduates_linear_slope_5 := (
  -2 * pre_graduates_t_minus_5 -
    pre_graduates_t_minus_4 +
    pre_graduates_t_minus_2 +
    2 * pre_graduates_t_minus_1
) / 10]
analysis[, prior_any_ua_graduates := as.integer(pre_graduates_mean_5 > 0)]

analysis[, fixed_private_grade12_share := fifelse(
  fixed_total_grade12_enrollment > 0,
  fixed_private_grade12_enrollment / fixed_total_grade12_enrollment,
  NA_real_
)]
analysis[, fixed_public_grade12_share := fifelse(
  fixed_total_grade12_enrollment > 0,
  fixed_public_grade12_enrollment / fixed_total_grade12_enrollment,
  NA_real_
)]
analysis[, log_fixed_grade12_enrollment := fifelse(
  fixed_total_grade12_enrollment > 0,
  log(fixed_total_grade12_enrollment),
  NA_real_
)]
analysis[, log_acs_total_population := fifelse(
  acs_total_population > 0,
  log(acs_total_population),
  NA_real_
)]
analysis[, log_acs_median_household_income := fifelse(
  acs_median_household_income > 0,
  log(acs_median_household_income),
  NA_real_
)]
analysis[, has_acs_controls := (
  !is.na(log_acs_total_population) &
    !is.na(log_acs_median_household_income) &
    !is.na(acs_poverty_share) &
    !is.na(acs_black_share)
)]
analysis[, log_prior_ua_mean_5 := log1p(pre_graduates_mean_5)]
analysis[, log_prior_ua_t_minus_1 := log1p(pre_graduates_t_minus_1)]

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

standardized_vars <- c(
  "pre_graduates_mean_5",
  "pre_graduates_trend_5",
  "pre_graduates_linear_slope_5",
  "log_prior_ua_mean_5",
  "log_acs_total_population",
  "log_acs_median_household_income",
  "acs_poverty_share",
  "acs_black_share",
  "log_fixed_grade12_enrollment",
  "fixed_private_grade12_share",
  "fixed_public_grade12_school_count",
  "fixed_private_grade12_school_count"
)
for (var in standardized_vars) {
  standardize_in_place(analysis, var, paste0("z_", var))
}

label_map <- c(
  z_pre_graduates_mean_5 = "Prior UA graduates, 5-year mean",
  z_pre_graduates_trend_5 = "Prior UA graduate change, t-1 minus t-5",
  z_pre_graduates_linear_slope_5 = "Prior UA graduate linear slope",
  z_log_prior_ua_mean_5 = "Log prior UA graduate mean",
  prior_any_ua_graduates = "Any prior UA graduates",
  z_log_acs_total_population = "Log ACS total population",
  z_log_acs_median_household_income = "Log ACS median household income",
  z_acs_poverty_share = "ACS poverty share",
  z_acs_black_share = "ACS Black-alone share",
  z_log_fixed_grade12_enrollment = "Log fixed grade-12 enrollment",
  z_fixed_private_grade12_share = "Private grade-12 share",
  z_fixed_public_grade12_school_count = "Public grade-12 schools",
  z_fixed_private_grade12_school_count = "Private grade-12 schools",
  in_alabama = "Alabama town"
)

#=====================================================================
# 4 - Estimate visit-driver models
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

extract_terms <- function(model, terms) {
  if (inherits(model, "error")) {
    return(data.table(
      term = terms,
      estimate = NA_real_,
      std_error = NA_real_,
      statistic = NA_real_,
      p_value = NA_real_,
      nobs = NA_integer_,
      adj_r2 = NA_real_,
      status = paste("model_error:", conditionMessage(model))
    ))
  }
  coef_table <- coeftable(model)
  model_adj_r2 <- get_adj_r2(model)
  rbindlist(lapply(terms, function(term) {
    if (!term %in% rownames(coef_table)) {
      return(data.table(
        term = term,
        estimate = NA_real_,
        std_error = NA_real_,
        statistic = NA_real_,
        p_value = NA_real_,
        nobs = nobs(model),
        adj_r2 = model_adj_r2,
        status = "term_not_estimated"
      ))
    }
    row <- coef_table[term, ]
    data.table(
      term = term,
      estimate = row[[1]],
      std_error = row[[2]],
      statistic = row[[3]],
      p_value = row[[4]],
      nobs = nobs(model),
      adj_r2 = model_adj_r2,
      status = "ok"
    )
  }))
}

make_formula <- function(outcome, controls, fixed_effects) {
  as.formula(sprintf("%s ~ %s | %s", outcome, paste(controls, collapse = " + "), fixed_effects))
}

fit_model <- function(data, outcome, controls, fixed_effects) {
  safe_fit(feols(
    make_formula(outcome, controls, fixed_effects),
    cluster = ~town_state_id,
    data = data
  ))
}

model_specs <- list(
  list(
    model_id = "prior_year_fe",
    sample = "full",
    sample_filter = quote(TRUE),
    fixed_effects = "recruiting_school_year",
    controls = c("z_log_prior_ua_mean_5", "z_pre_graduates_trend_5", "in_alabama")
  ),
  list(
    model_id = "prior_state_year_fe",
    sample = "full",
    sample_filter = quote(TRUE),
    fixed_effects = "processed_state + recruiting_school_year",
    controls = c("z_log_prior_ua_mean_5", "z_pre_graduates_trend_5")
  ),
  list(
    model_id = "prior_town_year_fe",
    sample = "full",
    sample_filter = quote(TRUE),
    fixed_effects = "town_state_id + recruiting_school_year",
    controls = c("z_log_prior_ua_mean_5", "z_pre_graduates_trend_5")
  ),
  list(
    model_id = "prior_alabama",
    sample = "alabama",
    sample_filter = quote(processed_state == "AL"),
    fixed_effects = "recruiting_school_year",
    controls = c("z_log_prior_ua_mean_5", "z_pre_graduates_trend_5")
  ),
  list(
    model_id = "prior_out_of_state",
    sample = "out_of_state",
    sample_filter = quote(processed_state != "AL"),
    fixed_effects = "processed_state + recruiting_school_year",
    controls = c("z_log_prior_ua_mean_5", "z_pre_graduates_trend_5")
  ),
  list(
    model_id = "town_chars_state_year_fe",
    sample = "fixed_hs",
    sample_filter = quote(!is.na(log_fixed_grade12_enrollment)),
    fixed_effects = "processed_state + recruiting_school_year",
    controls = c(
      "z_log_prior_ua_mean_5",
      "z_pre_graduates_trend_5",
      "z_log_fixed_grade12_enrollment",
      "z_fixed_private_grade12_share",
      "z_fixed_public_grade12_school_count",
      "z_fixed_private_grade12_school_count"
    )
  ),
  list(
    model_id = "town_chars_acs_state_year_fe",
    sample = "fixed_hs_acs",
    sample_filter = quote(!is.na(log_fixed_grade12_enrollment) & has_acs_controls),
    fixed_effects = "processed_state + recruiting_school_year",
    controls = c(
      "z_log_prior_ua_mean_5",
      "z_pre_graduates_trend_5",
      "z_log_acs_total_population",
      "z_log_acs_median_household_income",
      "z_acs_poverty_share",
      "z_acs_black_share",
      "z_log_fixed_grade12_enrollment",
      "z_fixed_private_grade12_share",
      "z_fixed_public_grade12_school_count",
      "z_fixed_private_grade12_school_count"
    )
  ),
  list(
    model_id = "town_chars_alabama",
    sample = "alabama_fixed_hs",
    sample_filter = quote(processed_state == "AL" & !is.na(log_fixed_grade12_enrollment)),
    fixed_effects = "recruiting_school_year",
    controls = c(
      "z_log_prior_ua_mean_5",
      "z_pre_graduates_trend_5",
      "z_log_fixed_grade12_enrollment",
      "z_fixed_private_grade12_share",
      "z_fixed_public_grade12_school_count",
      "z_fixed_private_grade12_school_count"
    )
  ),
  list(
    model_id = "town_chars_acs_alabama",
    sample = "alabama_fixed_hs_acs",
    sample_filter = quote(
      processed_state == "AL" &
        !is.na(log_fixed_grade12_enrollment) &
        has_acs_controls
    ),
    fixed_effects = "recruiting_school_year",
    controls = c(
      "z_log_prior_ua_mean_5",
      "z_pre_graduates_trend_5",
      "z_log_acs_total_population",
      "z_log_acs_median_household_income",
      "z_acs_poverty_share",
      "z_acs_black_share",
      "z_log_fixed_grade12_enrollment",
      "z_fixed_private_grade12_share",
      "z_fixed_public_grade12_school_count",
      "z_fixed_private_grade12_school_count"
    )
  ),
  list(
    model_id = "town_chars_out_of_state",
    sample = "out_of_state_fixed_hs",
    sample_filter = quote(processed_state != "AL" & !is.na(log_fixed_grade12_enrollment)),
    fixed_effects = "processed_state + recruiting_school_year",
    controls = c(
      "z_log_prior_ua_mean_5",
      "z_pre_graduates_trend_5",
      "z_log_fixed_grade12_enrollment",
      "z_fixed_private_grade12_share",
      "z_fixed_public_grade12_school_count",
      "z_fixed_private_grade12_school_count"
    )
  ),
  list(
    model_id = "town_chars_acs_out_of_state",
    sample = "out_of_state_fixed_hs_acs",
    sample_filter = quote(
      processed_state != "AL" &
        !is.na(log_fixed_grade12_enrollment) &
        has_acs_controls
    ),
    fixed_effects = "processed_state + recruiting_school_year",
    controls = c(
      "z_log_prior_ua_mean_5",
      "z_pre_graduates_trend_5",
      "z_log_acs_total_population",
      "z_log_acs_median_household_income",
      "z_acs_poverty_share",
      "z_acs_black_share",
      "z_log_fixed_grade12_enrollment",
      "z_fixed_private_grade12_share",
      "z_fixed_public_grade12_school_count",
      "z_fixed_private_grade12_school_count"
    )
  )
)

outcome_specs <- list(
  any_visit = "any_visit_t",
  visit_count = "visits_t",
  any_visit_same_state_fuzzy = "any_visit_t_with_same_state_fuzzy",
  visit_count_same_state_fuzzy = "visits_t_with_same_state_fuzzy"
)

coef_rows <- list()

for (spec in model_specs) {
  model_data <- analysis[eval(spec$sample_filter)]
  for (outcome_name in names(outcome_specs)) {
    outcome_var <- outcome_specs[[outcome_name]]
    model <- fit_model(model_data, outcome_var, spec$controls, spec$fixed_effects)
    extracted <- extract_terms(model, spec$controls)
    extracted[, `:=`(
      model_id = spec$model_id,
      sample = spec$sample,
      outcome = outcome_name,
      outcome_var = outcome_var,
      fixed_effects = spec$fixed_effects,
      term_label = label_map[term]
    )]
    coef_rows[[length(coef_rows) + 1]] <- extracted
  }
}

coef_dt <- rbindlist(coef_rows, fill = TRUE)
setcolorder(coef_dt, c(
  "model_id",
  "sample",
  "outcome",
  "outcome_var",
  "fixed_effects",
  "term",
  "term_label",
  "estimate",
  "std_error",
  "statistic",
  "p_value",
  "nobs",
  "adj_r2",
  "status"
))
fwrite(coef_dt, coef_path)

#=====================================================================
# 5 - Balance tables, figures, and summary output
#=====================================================================

balance <- analysis[, .(
  town_years = .N,
  towns = uniqueN(town_state_id),
  mean_visits = mean(visits_t),
  share_any_visit = mean(any_visit_t),
  mean_prior_ua_5yr = mean(pre_graduates_mean_5),
  mean_prior_ua_change_5yr = mean(pre_graduates_trend_5),
  mean_acs_population = mean(acs_total_population, na.rm = TRUE),
  mean_acs_median_household_income = mean(acs_median_household_income, na.rm = TRUE),
  mean_acs_poverty_share = mean(acs_poverty_share, na.rm = TRUE),
  mean_acs_black_share = mean(acs_black_share, na.rm = TRUE),
  mean_fixed_grade12 = mean(fixed_total_grade12_enrollment, na.rm = TRUE),
  mean_private_grade12_share = mean(fixed_private_grade12_share, na.rm = TRUE)
), by = .(state_group, any_visit_t)]
setorder(balance, state_group, any_visit_t)
fwrite(balance, balance_path)

analysis[, prior_trend_group := fifelse(
  pre_graduates_trend_5 < 0,
  "Declining prior UA grads",
  fifelse(pre_graduates_trend_5 > 0, "Rising prior UA grads", "Flat prior UA grads")
)]
analysis[, prior_level_group := fifelse(
  pre_graduates_mean_5 == 0,
  "No prior UA grads",
  fifelse(pre_graduates_mean_5 < 1, "Low prior UA grads", "Higher prior UA grads")
)]

trend_rates <- analysis[, .(
  town_years = .N,
  visit_rate = mean(any_visit_t),
  mean_visits = mean(visits_t),
  mean_prior_ua_5yr = mean(pre_graduates_mean_5),
  mean_prior_ua_change_5yr = mean(pre_graduates_trend_5)
), by = .(state_group, prior_level_group, prior_trend_group)]
setorder(trend_rates, state_group, prior_level_group, prior_trend_group)
fwrite(trend_rates, trend_rate_path)

save_plot <- function(plot_obj, basename, width = 10, height = 6) {
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

plot_coefs <- coef_dt[
  status == "ok" &
    outcome %in% c("any_visit", "visit_count") &
    model_id %in% c("prior_state_year_fe", "prior_town_year_fe", "town_chars_acs_state_year_fe") &
    term %in% c(
      "z_log_prior_ua_mean_5",
      "z_pre_graduates_trend_5",
      "z_log_acs_total_population",
      "z_log_acs_median_household_income",
      "z_acs_poverty_share",
      "z_acs_black_share",
      "z_log_fixed_grade12_enrollment",
      "z_fixed_private_grade12_share",
      "z_fixed_public_grade12_school_count",
      "z_fixed_private_grade12_school_count"
    )
]
plot_coefs[, model_label := fifelse(
  model_id == "prior_state_year_fe",
  "State + year FE",
  fifelse(model_id == "prior_town_year_fe", "Town + year FE", "Town chars + ACS + state/year FE")
)]
plot_coefs[, outcome_label := fifelse(outcome == "any_visit", "Any visit", "Visit count")]
plot_coefs[, term_label := factor(term_label, levels = rev(unique(term_label)))]
plot_coefs[, low := estimate - std_error]
plot_coefs[, high := estimate + std_error]

p_coef <- ggplot(
  plot_coefs,
  aes(x = estimate, y = term_label, color = model_label)
) +
  geom_vline(xintercept = 0, color = "gray40", linewidth = 0.3) +
  geom_errorbar(
    aes(xmin = low, xmax = high),
    orientation = "y",
    position = position_dodge(width = 0.55),
    width = 0.16,
    linewidth = 0.35
  ) +
  geom_point(position = position_dodge(width = 0.55), size = 1.4) +
  facet_wrap(~outcome_label, scales = "free_x") +
  labs(
    x = "Coefficient on standardized covariate",
    y = NULL,
    title = "Drivers of UA recruiting visits",
    subtitle = "Points are coefficient estimates; bars show one standard error"
  ) +
  scale_color_viridis_d() +
  plot_theme
save_plot(p_coef, "recruiting_visit_driver_coefficients", width = 11, height = 6)

trend_plot_dt <- trend_rates[state_group == "Outside Alabama"]
trend_plot_dt[, prior_trend_group := factor(
  prior_trend_group,
  levels = c("Declining prior UA grads", "Flat prior UA grads", "Rising prior UA grads")
)]
trend_plot_dt[, prior_level_group := factor(
  prior_level_group,
  levels = c("No prior UA grads", "Low prior UA grads", "Higher prior UA grads")
)]
trend_plot_dt[, trend_short := fifelse(
  prior_trend_group == "Declining prior UA grads",
  "Declining",
  fifelse(prior_trend_group == "Rising prior UA grads", "Rising", "Flat")
)]
trend_plot_dt[, row_label := fifelse(
  prior_level_group == "No prior UA grads",
  "No prior UA graduates",
  sprintf("%s: %s prior trend", sub(" prior UA grads", "", prior_level_group), trend_short)
)]
trend_plot_dt[, row_label := factor(
  row_label,
  levels = rev(c(
    "No prior UA graduates",
    "Low: Declining prior trend",
    "Low: Flat prior trend",
    "Low: Rising prior trend",
    "Higher: Declining prior trend",
    "Higher: Flat prior trend",
    "Higher: Rising prior trend"
  ))
)]
trend_plot_dt[, visit_rate_pct := 100 * visit_rate]
trend_plot_dt[, label := sprintf(
  "%.1f%%  (N=%s)",
  visit_rate_pct,
  format(town_years, big.mark = ",", scientific = FALSE, trim = TRUE)
)]

p_trend <- ggplot(
  trend_plot_dt,
  aes(y = row_label, color = prior_level_group)
) +
  geom_segment(
    aes(x = 0, xend = visit_rate_pct, yend = row_label),
    linewidth = 0.55,
    alpha = 0.65
  ) +
  geom_point(aes(x = visit_rate_pct), size = 3.1) +
  geom_text(
    aes(x = visit_rate_pct, label = label),
    hjust = -0.08,
    color = "gray15",
    size = 3.2
  ) +
  scale_color_viridis_d() +
  scale_x_continuous(
    limits = c(0, 82),
    breaks = seq(0, 80, by = 20),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    x = "Town-years receiving at least one UA recruiting visit",
    y = NULL,
    color = "Prior UA graduate level",
    title = "Outside-Alabama visit rates by prior UA history",
    subtitle = "Labels report the visit rate and number of town-years in each bin"
  ) +
  plot_theme
save_plot(p_trend, "outside_alabama_visit_rate_by_prior_trend", width = 9, height = 4.8)

summary_lines <- capture.output({
  cat("Drivers of UA recruiting visits\n")
  cat(sprintf("Panel: %s\n", panel_path))
  cat(sprintf("ACS covariates: %s\n", acs_path))
  cat(sprintf("Coefficient file: %s\n", coef_path))
  cat(sprintf("Preferred town-year rows: %s\n", nrow(analysis)))
  cat(sprintf("Towns: %s\n", uniqueN(analysis$town_state_id)))
  cat(sprintf("Total visits: %s\n", sum(analysis$visits_t)))
  cat(sprintf("Visited town-years: %s\n\n", sum(analysis$any_visit_t)))
  cat(sprintf(
    "Town-years with matched ACS population: %s\n\n",
    sum(!is.na(analysis$acs_total_population))
  ))
  cat(sprintf(
    "Town-years with complete ACS socioeconomic controls: %s\n\n",
    sum(analysis$has_acs_controls, na.rm = TRUE)
  ))
  cat("Model status counts:\n")
  print(coef_dt[, .N, by = .(status, model_id, outcome)][order(status, model_id, outcome)])
  cat("\nKey town-characteristic LPM coefficients for any visit:\n")
  print(coef_dt[
    model_id == "town_chars_state_year_fe" &
      outcome == "any_visit" &
      status == "ok",
    .(term_label, estimate, std_error, p_value, nobs)
  ])
  cat("\nOutside-Alabama town-characteristic LPM coefficients for any visit:\n")
  print(coef_dt[
    model_id == "town_chars_out_of_state" &
      outcome == "any_visit" &
      status == "ok",
    .(term_label, estimate, std_error, p_value, nobs)
  ])
  cat("\nACS-augmented town-characteristic LPM coefficients for any visit:\n")
  print(coef_dt[
    model_id == "town_chars_acs_state_year_fe" &
      outcome == "any_visit" &
      status == "ok",
    .(term_label, estimate, std_error, p_value, nobs)
  ])
  cat("\nOutside-Alabama ACS-augmented town-characteristic LPM coefficients for any visit:\n")
  print(coef_dt[
    model_id == "town_chars_acs_out_of_state" &
      outcome == "any_visit" &
      status == "ok",
    .(term_label, estimate, std_error, p_value, nobs)
  ])
  cat("\nVisit rates by prior trend:\n")
  print(trend_rates)
})
writeLines(summary_lines, summary_path)

cat(sprintf("Wrote driver coefficients to %s\n", coef_path))
cat(sprintf("Wrote driver balance table to %s\n", balance_path))
cat(sprintf("Wrote driver trend-rate table to %s\n", trend_rate_path))
cat(sprintf("Wrote driver figures to %s\n", figure_dir))
cat(sprintf("Wrote driver summary to %s\n", summary_path))
