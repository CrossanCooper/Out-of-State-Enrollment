#=====================================================================
## Author: Crossan Cooper
## Last Modified: 06-08-2026
#
## file use: Diagnose how much recruiting-treatment variation remains after
## absorbing town and recruiting-year fixed effects.
#
## inputs:
## 1. town_matching/output/ua_visit_t_graduates_t_plus_5_panel.csv -- visit-to-graduate panel
#
## outputs:
## 1. town_matching/output/visit_drivers/town_fe_identification_diagnostics.csv -- treatment variation diagnostics
## 2. town_matching/output/visit_drivers/town_fe_identification_diagnostics_summary.txt -- readable summary
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
panel_path <- get_arg(
  "panel",
  file.path(repo_root, "town_matching", "output", "ua_visit_t_graduates_t_plus_5_panel.csv")
)
output_path <- get_arg(
  "output",
  file.path(repo_root, "town_matching", "output", "visit_drivers", "town_fe_identification_diagnostics.csv")
)
summary_path <- get_arg(
  "summary",
  file.path(repo_root, "town_matching", "output", "visit_drivers", "town_fe_identification_diagnostics_summary.txt")
)
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)

#=====================================================================
# 2 - Load panel and define samples/treatments
#=====================================================================

panel <- fread(panel_path)
numeric_cols <- c(
  "preferred_regression_sample",
  "fixed_hs_denominator_regression_sample",
  "recruiting_school_year",
  "visits_t",
  "any_visit_t",
  "visits_t_with_same_state_fuzzy",
  "any_visit_t_with_same_state_fuzzy",
  "fixed_hs_grade12_enrollment"
)
numeric_cols <- intersect(numeric_cols, names(panel))
panel[, (numeric_cols) := lapply(.SD, as.numeric), .SDcols = numeric_cols]
panel <- panel[preferred_regression_sample == 1]
panel[, recruiting_school_year := factor(recruiting_school_year)]

sample_specs <- list(
  full = quote(TRUE),
  alabama = quote(processed_state == "AL"),
  out_of_state = quote(processed_state != "AL"),
  fixed_hs = quote(
    fixed_hs_denominator_regression_sample == 1 &
      !is.na(fixed_hs_grade12_enrollment) &
      fixed_hs_grade12_enrollment > 0
  ),
  fixed_hs_alabama = quote(
    processed_state == "AL" &
      fixed_hs_denominator_regression_sample == 1 &
      !is.na(fixed_hs_grade12_enrollment) &
      fixed_hs_grade12_enrollment > 0
  ),
  fixed_hs_out_of_state = quote(
    processed_state != "AL" &
      fixed_hs_denominator_regression_sample == 1 &
      !is.na(fixed_hs_grade12_enrollment) &
      fixed_hs_grade12_enrollment > 0
  )
)

treatment_specs <- list(
  any_visit = "any_visit_t",
  visit_count = "visits_t",
  any_visit_same_state_fuzzy = "any_visit_t_with_same_state_fuzzy",
  visit_count_same_state_fuzzy = "visits_t_with_same_state_fuzzy"
)

#=====================================================================
# 3 - Residualize treatments and summarize switcher variation
#=====================================================================

safe_residual_sd <- function(data, treatment_var) {
  model <- tryCatch(
    feols(
      as.formula(sprintf("%s ~ 1 | town_state_id + recruiting_school_year", treatment_var)),
      data = data
    ),
    error = function(error) error
  )
  if (inherits(model, "error")) {
    return(list(sd = NA_real_, status = paste("model_error:", conditionMessage(model))))
  }
  list(sd = sd(resid(model)), status = "ok")
}

summarize_sample_treatment <- function(data, sample_name, treatment_name, treatment_var) {
  town <- data[, .(
    years = .N,
    treatment_sum = sum(get(treatment_var)),
    treatment_min = min(get(treatment_var)),
    treatment_max = max(get(treatment_var))
  ), by = town_state_id]

  residual <- safe_residual_sd(data, treatment_var)
  raw_sd <- sd(data[[treatment_var]])
  switcher_towns <- if (treatment_name %in% c("any_visit", "any_visit_same_state_fuzzy")) {
    sum(town$treatment_sum > 0 & town$treatment_sum < town$years)
  } else {
    sum(town$treatment_max > town$treatment_min)
  }

  data.table(
    sample = sample_name,
    treatment = treatment_name,
    treatment_var = treatment_var,
    town_years = nrow(data),
    towns = uniqueN(data$town_state_id),
    treated_town_years = sum(data[[treatment_var]] > 0),
    total_treatment = sum(data[[treatment_var]]),
    never_treated_towns = sum(town$treatment_sum == 0),
    always_treated_towns = if (treatment_name %in% c("any_visit", "any_visit_same_state_fuzzy")) {
      sum(town$treatment_sum == town$years)
    } else {
      NA_integer_
    },
    switching_or_varying_towns = switcher_towns,
    share_switching_or_varying_towns = switcher_towns / nrow(town),
    raw_treatment_sd = raw_sd,
    town_year_fe_residual_sd = residual$sd,
    residual_to_raw_sd_ratio = residual$sd / raw_sd,
    residual_status = residual$status
  )
}

rows <- list()
for (sample_name in names(sample_specs)) {
  sample_data <- panel[eval(sample_specs[[sample_name]])]
  for (treatment_name in names(treatment_specs)) {
    rows[[length(rows) + 1]] <- summarize_sample_treatment(
      sample_data,
      sample_name,
      treatment_name,
      treatment_specs[[treatment_name]]
    )
  }
}

diagnostics <- rbindlist(rows, fill = TRUE)
fwrite(diagnostics, output_path)

#=====================================================================
# 4 - Write summary output
#=====================================================================

summary_lines <- capture.output({
  cat("Town-FE identifying-variation diagnostics\n")
  cat(sprintf("Panel: %s\n", panel_path))
  cat(sprintf("Output: %s\n\n", output_path))
  cat("Any-visit diagnostics:\n")
  print(diagnostics[
    treatment == "any_visit",
    .(
      sample,
      town_years,
      towns,
      treated_town_years,
      never_treated_towns,
      always_treated_towns,
      switching_or_varying_towns,
      share_switching_or_varying_towns,
      raw_treatment_sd,
      town_year_fe_residual_sd,
      residual_to_raw_sd_ratio
    )
  ])
  cat("\nVisit-count diagnostics:\n")
  print(diagnostics[
    treatment == "visit_count",
    .(
      sample,
      town_years,
      towns,
      treated_town_years,
      total_treatment,
      never_treated_towns,
      switching_or_varying_towns,
      share_switching_or_varying_towns,
      raw_treatment_sd,
      town_year_fe_residual_sd,
      residual_to_raw_sd_ratio
    )
  ])
})
writeLines(summary_lines, summary_path)

cat(sprintf("Wrote diagnostics to %s\n", output_path))
cat(sprintf("Wrote summary to %s\n", summary_path))
