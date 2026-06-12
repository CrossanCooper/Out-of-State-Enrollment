#!/usr/bin/env Rscript
#=====================================================================
## Author: Crossan Cooper
## Last Modified: 06-12-2026
#
## file use: Estimates linked-only OLS and leave-state-out IV versions of the
## state-by-cohort UA location-spillover count model with fixest::feols().
#
## workflow position:
## 1. Run after build_market_iv.py.
## 2. Run before estimate_baseline_year_robustness.R because that robustness
##    script reuses the linked-only balanced panel written here.
#
## inputs:
## 1. revelio_data/first_spell_join.rds -- first observed Revelio job spells.
## 2. data/linked_commencement_revelio_profile_data.csv -- linked UA profiles.
## 3. data/pull_factors.rds -- state-year unemployment and migration controls.
## 4. data/market_iv_panel.csv -- market-IV state-by-cohort panel.
#
## outputs:
## 1. linked_only_balanced_panel_with_iv.csv -- estimation panel.
## 2. linked_only_ols_iv_summary.csv -- OLS, first-stage, and IV estimates.
## 3. linked_only_ols_iv_summary.md -- compact Markdown table.
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
out_dir <- file.path(output_root, "linked_only")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

#=====================================================================
# 2 - Define sample constants and helper functions
#=====================================================================

state_xwalk <- tibble(
  originState = state.abb,
  state = state.name,
  state_abbr = state.abb
) %>%
  bind_rows(tibble(originState = "DC", state = "Washington, D.C.", state_abbr = "DC"))

### i. The estimating panel excludes Alabama destinations and the 2020 outcome year.
non_alabama_states <- sort(base::setdiff(unique(state_xwalk$state), "Alabama"))
paper_years <- sort(base::setdiff(2006:2023, 2020))

required_file <- function(...) {
  ### i. Centralize input validation so missing files fail with explicit paths.
  path <- file.path(...)
  if (!file.exists(path)) {
    stop("Missing required input: ", path, call. = FALSE)
  }
  path
}

clean_origin_state <- function(data, state_col) {
  ### i. Commencement origin abbreviations sometimes contain spaces; strip before join.
  data %>%
    mutate(originState = gsub(" ", "", {{ state_col }})) %>%
    left_join(state_xwalk, by = "originState") %>%
    filter(!is.na(state))
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
# 3 - Estimate one linked-only specification
#=====================================================================

estimate_spec <- function(panel, column, controls, trends) {
  ### i. All specifications include destination-state and graduation-cohort FE.
  fe_terms <- "d_state + grad_y"
  if (trends) {
    ## (a) d_state[trend] adds destination-state-specific linear slopes.
    fe_terms <- paste(fe_terms, "+ d_state[trend]")
  }

  ### ii. Build formulas programmatically so the five columns share one estimator.
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

  ### v. Report OLS, first stage, and IV rows using clustered standard errors.
  ols_n <- coefficient_row(ols, "N_origin")
  fs_z <- coefficient_row(first_stage, "z_lso")
  iv_n <- coefficient_row(iv, "fit_N_origin")

  tibble(
    column = column,
    controls = ifelse(length(controls) == 0, "none", term_string(controls)),
    state_trends = trends,
    n_obs = nobs(ols),
    n_states = n_distinct(panel$d_state),
    ols_adj_r2 = unname(r2(ols, "ar2")),
    ols_coef = ols_n$coef,
    ols_se = ols_n$se,
    ols_p = ols_n$p_value,
    ols_stars = stars_from_p(ols_n$p_value),
    first_stage_coef = fs_z$coef,
    first_stage_se = fs_z$se,
    first_stage_p = fs_z$p_value,
    first_stage_f = (fs_z$coef / fs_z$se)^2,
    first_stage_stars = stars_from_p(fs_z$p_value),
    iv_coef = iv_n$coef,
    iv_se = iv_n$se,
    iv_p = iv_n$p_value,
    iv_stars = stars_from_p(iv_n$p_value)
  )
}

#=====================================================================
# 4 - Format output tables
#=====================================================================

format_est <- function(coef, se, stars) {
  ### i. Markdown tables use a line break to mimic coefficient/SE table rows.
  sprintf("%.3f%s<br>(%.3f)", coef, stars, se)
}

write_markdown <- function(results, path) {
  lines <- c(
    "# Linked-Only Location-Spillover OLS and IV",
    "",
    "Outcome: Alabama-origin linked UA graduates whose first observed job is in state j.",
    "Endogenous regressor: linked UA graduates in cohort c from state j.",
    "Instrument: leave-state-out market exposure IV, `z_peer_non_alabama_count_growth_lso`.",
    "Sample: 50 non-Alabama states by 17 cohorts, 2006-2023 excluding 2020.",
    "",
    "| Spec | Controls | State trends | OLS | First stage | Cluster F | IV |",
    "|---:|---|:---:|---:|---:|---:|---:|"
  )

  for (i in seq_len(nrow(results))) {
    row <- results[i, ]
    lines <- c(lines, sprintf(
      "| (%d) | %s | %s | %s | %s | %.2f | %s |",
      row$column,
      row$controls,
      ifelse(row$state_trends, "yes", "no"),
      format_est(row$ols_coef, row$ols_se, row$ols_stars),
      format_est(row$first_stage_coef, row$first_stage_se, row$first_stage_stars),
      row$first_stage_f,
      format_est(row$iv_coef, row$iv_se, row$iv_stars)
    ))
  }

  lines <- c(
    lines,
    "",
    "Notes: Standard errors are clustered by destination state via `fixest::feols(..., cluster = ~ d_state)`."
  )
  writeLines(lines, path)
}

#=====================================================================
# 5 - Load and construct the balanced linked-only panel
#=====================================================================

### i. Source data from the existing linked commencement/Revelio workflow.
dest <- readRDS(required_file(source_root, "revelio_data", "first_spell_join.rds")) %>%
  ungroup() %>%
  filter(country == "United States")

### ii. Origin states come from linked UA commencement records.
origin <- read.csv(required_file(source_root, "data", "linked_commencement_revelio_profile_data.csv")) %>%
  filter(!is.na(Year)) %>%
  clean_origin_state(originState) %>%
  rename(grad_y = Year, o_state = state, o_state_abbr = state_abbr)

### iii. Link first observed job states to linked commencement origins by user_id.
join <- dest %>%
  select(-c(first_name, last_name, grad_y, fullname, field, user_location)) %>%
  rename(d_state = state) %>%
  inner_join(origin, by = "user_id") %>%
  filter(grad_y < 2024) %>%
  ungroup()

### iv. Outcome is Alabama-origin linked graduates whose first job is out of Alabama.
alabama_outflows <- join %>%
  filter(o_state == "Alabama", d_state != "Alabama") %>%
  count(grad_y, d_state, name = "N_dest")

### v. Endogenous regressor is linked UA graduates from destination state j.
linked_origin_counts <- origin %>%
  group_by(grad_y) %>%
  mutate(N_cohort = n()) %>%
  group_by(grad_y, d_state = o_state) %>%
  summarize(
    N_origin = n(),
    o_share = n() / mean(N_cohort) * 100,
    .groups = "drop"
  ) %>%
  filter(d_state != "Alabama")

### vi. Pull factors mirror the controls in the draft count-model table.
pull_factors <- readRDS(required_file(source_root, "data", "pull_factors.rds")) %>%
  select(-c(pop, arrive, depart)) %>%
  mutate(state = if_else(state == "District of Columbia", "Washington, D.C.", state)) %>%
  rename(grad_y = y, d_state = state, unemp = ur, netmig = net_rate) %>%
  distinct(d_state, grad_y, .keep_all = TRUE)

### vii. Use the preferred leave-state-out peer-market instrument.
market_panel <- read_csv(
  required_file(output_root, "data", "market_iv_panel.csv"),
  show_col_types = FALSE
) %>%
  select(
    state_abbr = origin_state,
    grad_y = grad_year,
    z_lso = z_peer_non_alabama_count_growth_lso
  )

### viii. Balance the panel to 50 destination states by 17 non-2020 cohorts.
panel <- expand_grid(d_state = non_alabama_states, grad_y = paper_years) %>%
  left_join(state_xwalk %>% distinct(state, state_abbr), by = c("d_state" = "state")) %>%
  left_join(alabama_outflows, by = c("d_state", "grad_y")) %>%
  left_join(linked_origin_counts, by = c("d_state", "grad_y")) %>%
  left_join(pull_factors, by = c("d_state", "grad_y")) %>%
  left_join(market_panel, by = c("state_abbr", "grad_y")) %>%
  mutate(
    N_dest = replace_na(N_dest, 0),
    N_origin = replace_na(N_origin, 0),
    o_share = replace_na(o_share, 0),
    unemp = replace_na(unemp, 0),
    netmig = replace_na(netmig, 0),
    trend = grad_y - min(grad_y)
  )

### ix. These checks protect against silent sample changes in upstream files.
stopifnot(nrow(panel) == 850)
stopifnot(n_distinct(panel$d_state) == 50)
stopifnot(sum(is.na(panel$z_lso)) == 0)

#=====================================================================
# 6 - Run specifications and write outputs
#=====================================================================

specs <- list(
  ### i. Columns mirror the paper table: baseline, controls, then state trends.
  list(column = 1, controls = character(0), trends = FALSE),
  list(column = 2, controls = c("unemp"), trends = FALSE),
  list(column = 3, controls = c("netmig"), trends = FALSE),
  list(column = 4, controls = c("unemp", "netmig"), trends = FALSE),
  list(column = 5, controls = c("unemp", "netmig"), trends = TRUE)
)

results <- bind_rows(lapply(
  specs,
  function(spec) estimate_spec(panel, spec$column, spec$controls, spec$trends)
))

write_csv(panel, file.path(out_dir, "linked_only_balanced_panel_with_iv.csv"))
write_csv(results, file.path(out_dir, "linked_only_ols_iv_summary.csv"))
write_markdown(results, file.path(out_dir, "linked_only_ols_iv_summary.md"))

cat("Linked-only OLS/IV estimation complete.\n")
print(results %>%
  select(column, controls, state_trends, ols_coef, ols_se, ols_stars,
         first_stage_f, iv_coef, iv_se, iv_stars),
  n = Inf)
cat("Outputs written under:", out_dir, "\n")
