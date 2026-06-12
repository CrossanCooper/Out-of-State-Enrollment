#!/usr/bin/env Rscript
#=====================================================================
## Author: Crossan Cooper
## Last Modified: 06-12-2026
#
## file use: Re-estimates the linked-only location-spillover IV model under
## alternative baseline years for the market-share instrument with fixest.
#
## workflow position:
## 1. Run after linked_only_spillover_ols_iv.R, which writes the fixed
##    linked-only estimating panel.
## 2. Run after build_baseline_year_robustness_iv.py, which writes the
##    alternative-baseline instrument panel.
#
## inputs:
## 1. linked_only_balanced_panel_with_iv.csv -- preferred linked-only panel.
## 2. baseline_year_robustness_z_panel.csv -- alternative-baseline instruments.
#
## outputs:
## 1. baseline_year_robustness_estimates.csv -- full robustness estimates.
## 2. baseline_year_robustness_compact.csv -- compact robustness table.
## 3. baseline_year_robustness.md -- Markdown summary.
#=====================================================================

#=====================================================================
# 1 - Load packages and define project paths
#=====================================================================

pacman::p_load(dplyr,tidyr,fixest,readr)

file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(file_arg) != 1) {
  stop("Could not infer script path from commandArgs(). Run with Rscript.", call. = FALSE)
}
script_path <- normalizePath(sub("^--file=", "", file_arg))
active_projects_root <- Sys.getenv(
  "ACTIVE_PROJECTS_ROOT",
  "/Users/crossancooper/Dropbox/Professional/active-projects"
)
source_root <- Sys.getenv(
  "ADMISSIONS_PROJECT_ROOT",
  file.path(active_projects_root, "admissions_project")
)
output_root <- Sys.getenv(
  "MARKET_IV_OUTPUT_ROOT",
  file.path(source_root, "data", "market_iv")
)
linked_only_dir <- file.path(output_root, "linked_only")
out_dir <- file.path(output_root, "baseline_year_robustness")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

#=====================================================================
# 2 - Define helper functions
#=====================================================================

required_file <- function(...) {
  ### i. Stop early if the preferred panel or robustness IV file has not been built.
  path <- file.path(...)
  if (!file.exists(path)) {
    stop("Missing required input: ", path, call. = FALSE)
  }
  path
}

stars_from_p <- function(p_value) {
  case_when(
    is.na(p_value) ~ "",
    p_value < 0.01 ~ "***",
    p_value < 0.05 ~ "**",
    p_value < 0.10 ~ "*",
    TRUE ~ ""
  )
}

coefficient_row <- function(model, terms) {
  ### i. coeftable() uses clustered SE because cluster is set inside feols().
  coefs <- coeftable(model)
  for (term in terms) {
    if (term %in% rownames(coefs)) {
      return(tibble(
        coef = unname(coefs[term, "Estimate"]),
        se = unname(coefs[term, "Std. Error"]),
        p_value = unname(coefs[term, "Pr(>|t|)"])
      ))
    }
  }
  tibble(coef = NA_real_, se = NA_real_, p_value = NA_real_)
}

term_string <- function(terms) {
  paste(terms[nzchar(terms)], collapse = " + ")
}

#=====================================================================
# 3 - Estimate one robustness specification
#=====================================================================

estimate_spec <- function(panel, baseline_label, baseline_years, column, controls, trends) {
  ### i. Keep the fixed-effect structure identical to the preferred estimator.
  fe_terms <- "d_state + grad_y"
  if (trends) {
    ## (a) d_state[trend] adds destination-state-specific linear slopes.
    fe_terms <- paste(fe_terms, "+ d_state[trend]")
  }

  ### ii. Only the IV varies across baseline-year rows; controls and FE do not.
  ols_rhs <- term_string(c("N_origin", controls))
  fs_rhs <- term_string(c("z_lso", controls))
  iv_rhs <- ifelse(length(controls) == 0, "1", term_string(controls))

  ### iii. feols uses the formula blocks y ~ x | FE | endogenous ~ instrument.
  ols_formula <- as.formula(paste0("N_dest ~ ", ols_rhs, " | ", fe_terms))
  fs_formula <- as.formula(paste0("N_origin ~ ", fs_rhs, " | ", fe_terms))
  iv_formula <- if (length(controls) == 0) {
    as.formula(paste0("N_dest ~ ", iv_rhs, " | ", fe_terms, " | N_origin ~ z_lso"))
  } else {
    as.formula(paste0(
      "N_dest ~ ",
      iv_rhs,
      " | ",
      fe_terms,
      " | N_origin ~ z_lso"
    ))
  }

  ### iv. Cluster all reported standard errors by destination state.
  ols <- feols(ols_formula, data = panel, cluster = ~ d_state, notes = FALSE)
  first_stage <- feols(fs_formula, data = panel, cluster = ~ d_state, notes = FALSE)
  iv <- feols(iv_formula, data = panel, cluster = ~ d_state, notes = FALSE)

  ### v. Store OLS rows too so robustness output can be audited against the base run.
  ols_n <- coefficient_row(ols, "N_origin")
  fs_z <- coefficient_row(first_stage, "z_lso")
  iv_n <- coefficient_row(iv, "fit_N_origin")

  tibble(
    baseline_label = baseline_label,
    baseline_years = baseline_years,
    column = column,
    controls = ifelse(length(controls) == 0, "none", term_string(controls)),
    state_trends = trends,
    n_obs = nobs(ols),
    n_states = n_distinct(panel$d_state),
    first_stage_coef = fs_z$coef,
    first_stage_se = fs_z$se,
    first_stage_p = fs_z$p_value,
    first_stage_f = (fs_z$coef / fs_z$se)^2,
    first_stage_stars = stars_from_p(fs_z$p_value),
    iv_coef = iv_n$coef,
    iv_se = iv_n$se,
    iv_p = iv_n$p_value,
    iv_stars = stars_from_p(iv_n$p_value),
    ols_coef = ols_n$coef,
    ols_se = ols_n$se,
    ols_p = ols_n$p_value,
    ols_stars = stars_from_p(ols_n$p_value)
  )
}

#=====================================================================
# 4 - Format robustness output
#=====================================================================

format_est <- function(coef, se, stars) {
  ### i. Compact output follows the coefficient-with-SE convention used in tables.
  sprintf("%.3f%s (%.3f)", coef, stars, se)
}

write_markdown <- function(results, path) {
  lines <- c(
    "# Baseline-Year Robustness for Linked-Only Leave-State-Out IV",
    "",
    "The table re-estimates the same five linked-only specifications after changing only the baseline years used for the peer-market share and leave-state-out UA growth baseline.",
    "",
    "| Baseline | Spec | Controls | State trends | First-stage F | IV |",
    "|---|---:|---|:---:|---:|---:|"
  )

  for (i in seq_len(nrow(results))) {
    row <- results[i, ]
    lines <- c(lines, sprintf(
      "| %s | (%d) | %s | %s | %.2f | %s |",
      row$baseline_years,
      row$column,
      row$controls,
      ifelse(row$state_trends, "yes", "no"),
      row$first_stage_f,
      format_est(row$iv_coef, row$iv_se, row$iv_stars)
    ))
  }

  lines <- c(
    lines,
    "",
    "Notes: All specifications include destination-state and cohort fixed effects. Standard errors are clustered by destination state."
  )
  writeLines(lines, path)
}

#=====================================================================
# 5 - Load linked-only panel and alternative-baseline instruments
#=====================================================================

panel_base <- read_csv(
  required_file(linked_only_dir, "linked_only_balanced_panel_with_iv.csv"),
  show_col_types = FALSE
) %>%
  ### i. Drop the preferred IV before joining each alternative-baseline version.
  select(-z_lso)

### ii. Keep only the keys and candidate leave-state-out IV needed for estimation.
z_panel <- read_csv(
  required_file(out_dir, "baseline_year_robustness_z_panel.csv"),
  col_types = cols(
    baseline_label = col_character(),
    baseline_years = col_character(),
    origin_state = col_character(),
    entry_year = col_integer(),
    grad_year = col_integer(),
    peer_oos_count = col_double(),
    peer_share = col_double(),
    baseline_ua_oos_total_lso = col_double(),
    ua_oos_total_lso = col_double(),
    ua_oos_growth_lso = col_double(),
    z_lso = col_double()
  )
) %>%
  transmute(
    baseline_label,
    baseline_years,
    state_abbr = origin_state,
    grad_y = grad_year,
    z_lso
  )

#=====================================================================
# 6 - Run robustness grid and write outputs
#=====================================================================

specs <- list(
  ### i. Use the same five columns as the linked-only preferred specification.
  list(column = 1, controls = character(0), trends = FALSE),
  list(column = 2, controls = c("unemp"), trends = FALSE),
  list(column = 3, controls = c("netmig"), trends = FALSE),
  list(column = 4, controls = c("unemp", "netmig"), trends = FALSE),
  list(column = 5, controls = c("unemp", "netmig"), trends = TRUE)
)

### ii. Preserve a stable ordering in printed and written robustness tables.
baseline_keys <- z_panel %>%
  distinct(baseline_label, baseline_years) %>%
  mutate(
    baseline_order = match(
      baseline_label,
      c("2000", "2001", "2002", "2000_2001", "2000_2002")
    )
  ) %>%
  arrange(baseline_order) %>%
  select(-baseline_order)

results <- bind_rows(lapply(seq_len(nrow(baseline_keys)), function(i) {
  key <- baseline_keys[i, ]
  ### iii. Attach one candidate IV at a time to the common linked-only panel.
  panel <- panel_base %>%
    left_join(
      z_panel %>% filter(baseline_label == key$baseline_label),
      by = c("state_abbr", "grad_y")
    )

  ### iv. Guard against losing cohorts or states when changing the baseline year.
  stopifnot(nrow(panel) == 850)
  stopifnot(sum(is.na(panel$z_lso)) == 0)

  bind_rows(lapply(
    specs,
    function(spec) estimate_spec(
      panel = panel,
      baseline_label = key$baseline_label,
      baseline_years = key$baseline_years,
      column = spec$column,
      controls = spec$controls,
      trends = spec$trends
    )
  ))
}))

compact <- results %>%
  select(
    baseline_years,
    column,
    controls,
    state_trends,
    first_stage_f,
    iv_coef,
    iv_se,
    iv_p,
    iv_stars
  )

write_csv(results, file.path(out_dir, "baseline_year_robustness_estimates.csv"))
write_csv(compact, file.path(out_dir, "baseline_year_robustness_compact.csv"))
write_markdown(compact, file.path(out_dir, "baseline_year_robustness.md"))

cat("Baseline-year robustness estimation complete.\n")
print(compact, n = Inf)
cat("Outputs written under:", out_dir, "\n")
