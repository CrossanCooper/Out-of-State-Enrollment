#!/usr/bin/env Rscript
#=====================================================================
## Author: Crossan Cooper
## Last Modified: 06-12-2026
#
## file use: Estimates market-IV first-stage diagnostics with fixest,
## builds long-difference origin-state growth diagnostics, and writes the
## market-IV diagnostic figures and Markdown summary.
#
## workflow position:
## 1. Run after build_market_iv.py.
## 2. This script is diagnostic only; linked_only_spillover_ols_iv.R does not
##    depend on its outputs.
#
## inputs:
## 1. data/market_iv_panel.csv -- state-by-entry-cohort market-IV panel.
## 2. tables/market_exposure_by_state.csv -- 2000 baseline exposure measures.
## 3. data/ua_commencement_origin_panel.csv -- parsed commencement origin panel.
#
## outputs:
## 1. tables/first_stage_diagnostics.csv -- fixest first-stage diagnostics.
## 2. tables/state_growth_diagnostics.csv -- state-level long-difference file.
## 3. market_iv_build_summary.md -- compact diagnostic summary.
## 4. figures/market-iv/ipeds_growth_vs_exposure.png -- reduced-form scatterplot figure.
## 5. figures/market-iv/reduced_form_scatterplots.png -- same scatterplot figure
##    under a descriptive reduced-form filename.
#=====================================================================

#=====================================================================
# 1 - Load packages and define project paths
#=====================================================================

pacman::p_load(dplyr,tidyr,readr,fixest,ggplot2)

### i. Keep paths explicit and overridable so the same script can run on Ryan's machine.
active_projects_root <- Sys.getenv(
  "ACTIVE_PROJECTS_ROOT",
  "/Users/crossancooper/Dropbox/Professional/active-projects"
)
source_root <- Sys.getenv(
  "ADMISSIONS_PROJECT_ROOT",
  file.path(active_projects_root, "admissions_project")
)
shared_ipeds_root <- Sys.getenv(
  "SHARED_IPEDS_ROOT",
  file.path(source_root, "data", "pgp-ipeds")
)
output_root <- Sys.getenv(
  "MARKET_IV_OUTPUT_ROOT",
  file.path(source_root, "data", "market_iv")
)
figure_root <- Sys.getenv(
  "MARKET_IV_FIGURE_ROOT",
  file.path(source_root, "figures", "market-iv")
)
data_dir <- file.path(output_root, "data")
table_dir <- file.path(output_root, "tables")

dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_root, recursive = TRUE, showWarnings = FALSE)

#=====================================================================
# 2 - Define externally calibrated constants and helper functions
#=====================================================================

### i. These years match the preferred 2000 baseline and the last IPEDS entry year.
baseline_year_label <- "2000"
growth_start_year <- 2000
growth_end_year <- 2020
label_states <- c("CA", "FL", "GA", "IL", "NJ", "NY", "TN", "TX")

### ii. These diagnostics reproduce the earlier first-stage grid in fixest.
diagnostic_instruments <- c(
  "z_ua_baseline_count_growth",
  "z_peer_non_alabama_count_growth",
  "z_peer_non_alabama_count_growth_lso",
  "z_underpenetrated_signed_non_alabama_count_growth",
  "z_underpenetrated_positive_non_alabama_count_growth"
)
diagnostic_outcomes <- c(
  "ua_ipeds_state_share_all_domestic_pct",
  "ua_ipeds_state_share_oos_pct",
  "commencement_state_share_all_domestic_pct",
  "commencement_state_share_oos_pct"
)

required_file <- function(...) {
  ### i. Fail early with the full missing path rather than a later readr error.
  path <- file.path(...)
  if (!file.exists(path)) {
    stop("Missing required input: ", path, call. = FALSE)
  }
  path
}

safe_corr <- function(data, x_var, y_var) {
  ### i. Drop non-finite values so correlations are not driven by missing cells.
  complete <- data %>%
    transmute(
      x = .data[[x_var]],
      y = .data[[y_var]]
    ) %>%
    filter(is.finite(x), is.finite(y))

  if (nrow(complete) < 3 || sd(complete$x) == 0 || sd(complete$y) == 0) {
    return(NA_real_)
  }
  cor(complete$x, complete$y)
}

format_cell <- function(value, digits = 4) {
  if (is.numeric(value)) {
    if (is.na(value)) {
      return("")
    }
    return(formatC(value, digits = digits, format = "f"))
  }
  as.character(value)
}

markdown_table <- function(data, digits = 4) {
  ### i. Build small Markdown tables without requiring knitr or kableExtra.
  header <- paste0("| ", paste(names(data), collapse = " | "), " |")
  divider <- paste0("| ", paste(rep("---", ncol(data)), collapse = " | "), " |")
  rows <- vapply(seq_len(nrow(data)), function(row_id) {
    row <- data[row_id, , drop = FALSE]
    cells <- vapply(row, format_cell, character(1), digits = digits)
    paste0("| ", paste(cells, collapse = " | "), " |")
  }, character(1))
  paste(c(header, divider, rows), collapse = "\n")
}

#=====================================================================
# 3 - Estimate first-stage diagnostic regressions with fixest
#=====================================================================

coefficient_row <- function(model, term) {
  ### i. coeftable() returns the clustered SE because cluster is set in feols().
  coefs <- coeftable(model)
  if (!term %in% rownames(coefs)) {
    stop("Term not found in model output: ", term, call. = FALSE)
  }

  tibble(
    beta = unname(coefs[term, "Estimate"]),
    cluster_se = unname(coefs[term, "Std. Error"]),
    p_value = unname(coefs[term, "Pr(>|t|)"])
  )
}

estimate_one_diagnostic <- function(panel, outcome, instrument, with_state_trends) {
  ### i. State-year FE absorb fixed origin-state differences and common cohort shocks.
  fixed_effects <- if (with_state_trends) {
    ## (a) origin_state[entry_year] adds an origin-state-specific linear slope.
    "origin_state + entry_year + origin_state[entry_year]"
  } else {
    "origin_state + entry_year"
  }

  ### ii. The coefficient asks whether the candidate IV predicts origin shares.
  formula_text <- paste0(outcome, " ~ ", instrument, " | ", fixed_effects)
  model <- feols(
    as.formula(formula_text),
    data = panel,
    cluster = ~ origin_state,
    notes = FALSE
  )

  row <- coefficient_row(model, instrument)
  row %>%
    mutate(
      outcome = outcome,
      instrument = instrument,
      controls = ifelse(
        with_state_trends,
        "state_year_fe_plus_state_trends",
        "state_year_fe"
      ),
      n = nobs(model),
      state_clusters = n_distinct(panel$origin_state),
      cluster_t = beta / cluster_se,
      cluster_f = cluster_t^2,
      within_r2 = unname(r2(model, "wr2")),
      .before = beta
    )
}

run_first_stage_diagnostics <- function(panel) {
  ### i. Estimate every outcome-instrument pair with and without state trends.
  spec_grid <- expand_grid(
    outcome = diagnostic_outcomes,
    instrument = diagnostic_instruments,
    with_state_trends = c(FALSE, TRUE)
  )

  bind_rows(lapply(seq_len(nrow(spec_grid)), function(row_id) {
    spec <- spec_grid[row_id, ]
    estimate_one_diagnostic(
      panel = panel,
      outcome = spec$outcome,
      instrument = spec$instrument,
      with_state_trends = spec$with_state_trends
    )
  }))
}

#=====================================================================
# 4 - Build long-difference growth diagnostics
#=====================================================================

wide_change <- function(panel, source, outcome, start_year, end_year) {
  ### i. Convert state-year levels into start/end levels and a long difference.
  wide <- panel %>%
    filter(entry_year %in% c(start_year, end_year)) %>%
    transmute(
      origin_state,
      entry_year,
      value = .data[[outcome]]
    ) %>%
    pivot_wider(
      names_from = entry_year,
      values_from = value,
      names_prefix = "level_"
    )

  start_col <- paste0("level_", start_year)
  end_col <- paste0("level_", end_year)
  level_start_name <- paste0(source, "_level_", start_year)
  level_end_name <- paste0(source, "_level_", end_year)
  change_name <- paste0(source, "_change_", start_year, "_", end_year)

  wide %>%
    transmute(
      origin_state,
      !!level_start_name := .data[[start_col]],
      !!level_end_name := .data[[end_col]],
      !!change_name := .data[[end_col]] - .data[[start_col]]
    )
}

build_growth_diagnostics <- function(panel, exposures) {
  ### i. IPEDS is the preferred source for first-time entering-cohort residence.
  ipeds_growth <- wide_change(
    panel = panel,
    source = "ipeds_share_all_domestic_pct",
    outcome = "ua_ipeds_state_share_all_domestic_pct",
    start_year = growth_start_year,
    end_year = growth_end_year
  )

  ### ii. Commencement growth is retained as a secondary check on parsed records.
  commencement_growth <- wide_change(
    panel = panel,
    source = "commencement_share_all_domestic_pct",
    outcome = "commencement_state_share_all_domestic_pct",
    start_year = growth_start_year,
    end_year = growth_end_year
  )

  growth_col <- paste0(
    "ipeds_share_all_domestic_pct_change_",
    growth_start_year,
    "_",
    growth_end_year
  )

  exposures %>%
    left_join(ipeds_growth, by = "origin_state") %>%
    left_join(commencement_growth, by = "origin_state") %>%
    arrange(desc(.data[[growth_col]]))
}

#=====================================================================
# 5 - Make diagnostic figures
#=====================================================================

plot_growth_vs_exposure <- function(growth) {
  change_col <- paste0(
    "ipeds_share_all_domestic_pct_change_",
    growth_start_year,
    "_",
    growth_end_year
  )

  ### i. Panel A is the old feeder measure; Panel B is the preferred IV share.
  plot_data <- bind_rows(
    growth %>%
      transmute(
        origin_state,
        exposure_measure = "UA feeder share",
        panel = paste0(
          "A. UA feeder share\ncomparison exposure; r = ",
          sprintf("%.2f", safe_corr(growth, "ua_baseline_share_pre", change_col))
        ),
        exposure_pct = 100 * ua_baseline_share_pre,
        growth_pp = .data[[change_col]]
      ),
    growth %>%
      transmute(
        origin_state,
        exposure_measure = "Peer-flagship market share",
        panel = paste0(
          "B. Peer-flagship market share\npreferred IV share; r = ",
          sprintf(
            "%.2f",
            safe_corr(growth, "peer_flagship_flow_share_pre_non_alabama", change_col)
          )
        ),
        exposure_pct = 100 * peer_flagship_flow_share_pre_non_alabama,
        growth_pp = .data[[change_col]]
      )
  )

  ### ii. Only label large sending states so the scatter remains readable.
  label_data <- plot_data %>%
    filter(origin_state %in% label_states) %>%
    mutate(
      ## (a) Use state- and panel-specific locations rather than one global nudge.
      label_x = case_when(
        exposure_measure == "UA feeder share" & origin_state %in% c("NJ", "CA", "NY") ~
          exposure_pct + 0.65,
        exposure_measure == "UA feeder share" & origin_state == "IL" ~
          exposure_pct + 0.55,
        exposure_measure == "UA feeder share" & origin_state == "GA" ~
          exposure_pct - 1.00,
        exposure_measure == "UA feeder share" ~
          exposure_pct + 0.45,
        exposure_measure == "Peer-flagship market share" & origin_state == "IL" ~
          exposure_pct - 0.30,
        exposure_measure == "Peer-flagship market share" & origin_state %in% c("CA", "NY") ~
          exposure_pct + 0.18,
        exposure_measure == "Peer-flagship market share" & origin_state == "NJ" ~
          exposure_pct + 0.16,
        exposure_measure == "Peer-flagship market share" & origin_state == "TX" ~
          exposure_pct - 0.18,
        exposure_measure == "Peer-flagship market share" ~
          exposure_pct + 0.16,
        TRUE ~ exposure_pct
      ),
      label_y = case_when(
        exposure_measure == "UA feeder share" & origin_state == "NJ" ~ growth_pp + 0.18,
        exposure_measure == "UA feeder share" & origin_state == "CA" ~ growth_pp + 0.04,
        exposure_measure == "UA feeder share" & origin_state == "NY" ~ growth_pp - 0.10,
        exposure_measure == "UA feeder share" & origin_state == "TX" ~ growth_pp + 0.04,
        exposure_measure == "UA feeder share" & origin_state == "IL" ~ growth_pp + 0.04,
        exposure_measure == "UA feeder share" & origin_state == "GA" ~ growth_pp - 0.08,
        exposure_measure == "Peer-flagship market share" & origin_state == "IL" ~ growth_pp + 0.04,
        exposure_measure == "Peer-flagship market share" & origin_state == "TX" ~ growth_pp + 0.05,
        exposure_measure == "Peer-flagship market share" & origin_state == "NJ" ~ growth_pp + 0.06,
        exposure_measure == "Peer-flagship market share" & origin_state == "CA" ~ growth_pp - 0.06,
        exposure_measure == "Peer-flagship market share" & origin_state == "NY" ~ growth_pp - 0.08,
        exposure_measure == "Peer-flagship market share" & origin_state == "GA" ~ growth_pp - 0.09,
        TRUE ~ growth_pp + 0.04
      )
    )

  ggplot(plot_data, aes(x = exposure_pct, y = growth_pp, color = exposure_measure)) +
    geom_hline(yintercept = 0, color = "grey70", linewidth = 0.35) +
    ## (a) Use panel-specific colors to match the original Python scatterplot.
    geom_point(alpha = 0.78, size = 2.2) +
    geom_segment(
      data = label_data,
      aes(x = exposure_pct, y = growth_pp, xend = label_x, yend = label_y),
      inherit.aes = FALSE,
      color = "grey45",
      linewidth = 0.25
    ) +
    geom_text(
      data = label_data,
      aes(x = label_x, y = label_y, label = origin_state),
      inherit.aes = FALSE,
      color = "grey15",
      size = 3,
      show.legend = FALSE
    ) +
    scale_color_manual(
      values = c(
        "UA feeder share" = "#4E79A7",
        "Peer-flagship market share" = "#2F6F4E"
      ),
      guide = "none"
    ) +
    scale_x_continuous(expand = expansion(mult = c(0.04, 0.08))) +
    scale_y_continuous(expand = expansion(mult = c(0.08, 0.10))) +
    facet_wrap(~ panel, scales = "free_x") +
    labs(
      title = paste0(
        "Candidate ",
        baseline_year_label,
        " exposure measures and subsequent UA origin-state growth"
      ),
      x = "Baseline exposure share, percent",
      y = paste0(
        "Change in state's share of UA domestic first-time students,\n",
        growth_start_year,
        "-",
        growth_end_year,
        " percentage points"
      ),
      caption = paste0(
        "Y-axis is the change in each state's share of UA domestic first-time ",
        "students from ",
        growth_start_year,
        " to ",
        growth_end_year,
        ". Panel B supplies the share in the preferred IV: Z_jc = S_j(",
        baseline_year_label,
        ") x G_c(UA, -j)."
      )
    ) +
    theme_bw(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "grey95", color = "grey75"),
      plot.caption = element_text(hjust = 0),
      plot.margin = margin(t = 12, r = 12, b = 12, l = 22)
    )
}

#=====================================================================
# 6 - Write Markdown diagnostic summary
#=====================================================================

write_summary <- function(growth, diagnostics, commencement_panel, path) {
  change_col <- paste0(
    "ipeds_share_all_domestic_pct_change_",
    growth_start_year,
    "_",
    growth_end_year
  )

  correlations <- tibble(
    exposure = c(
      "UA baseline feeder share",
      "Peer-flagship flow share",
      "Positive underpenetration gap"
    ),
    correlation = c(
      safe_corr(growth, "ua_baseline_share_pre", change_col),
      safe_corr(growth, "peer_flagship_flow_share_pre_non_alabama", change_col),
      safe_corr(growth, "underpenetrated_positive_non_alabama", change_col)
    )
  )

  ### i. This table mirrors the figure but keeps exact coefficients and F stats.
  selected_diagnostics <- diagnostics %>%
    filter(
      outcome == "ua_ipeds_state_share_all_domestic_pct",
      instrument %in% c(
        "z_ua_baseline_count_growth",
        "z_peer_non_alabama_count_growth",
        "z_peer_non_alabama_count_growth_lso",
        "z_underpenetrated_positive_non_alabama_count_growth"
      )
    ) %>%
    select(instrument, controls, beta, cluster_se, cluster_f, within_r2)

  top_growth <- growth %>%
    slice_head(n = 10) %>%
    select(
      origin_state,
      starts_with("ipeds_share_all_domestic_pct_level_"),
      all_of(change_col),
      ua_baseline_share_pre,
      peer_flagship_flow_share_pre_non_alabama,
      underpenetrated_positive_non_alabama
    )

  coverage <- commencement_panel %>%
    distinct(grad_year, commencement_state_parse_coverage) %>%
    arrange(commencement_state_parse_coverage) %>%
    slice_head(n = 5)

  lines <- c(
    "# Market IV Diagnostic Summary",
    "",
    "## Inputs",
    "",
    paste0(
      "- IPEDS DuckDB default: `",
      file.path(shared_ipeds_root, "ipeds-database", "ipeds.duckdb"),
      "`"
    ),
    paste0("- Market-IV output root: `", output_root, "`"),
    paste0("- Figure root: `", figure_root, "`"),
    "",
    "## Core Exposure Correlations",
    "",
    paste0(
      "Correlations are with the long-difference change in UA IPEDS ",
      "state-origin share from entering cohort ",
      growth_start_year,
      " to entering cohort ",
      growth_end_year,
      "."
    ),
    "",
    markdown_table(correlations, digits = 3),
    "",
    "## Selected First-Stage Diagnostics",
    "",
    paste0(
      "Outcome: UA IPEDS state-origin share of all domestic first-time ",
      "students, measured in percentage points. Regressions are estimated ",
      "with `fixest::feols()` and standard errors are clustered by origin state."
    ),
    "",
    markdown_table(selected_diagnostics, digits = 4),
    "",
    "## States With Largest IPEDS Origin-Share Growth",
    "",
    markdown_table(top_growth, digits = 4),
    "",
    "## Commencement Parser Coverage Caveat",
    "",
    paste0(
      "The commencement panel is included as a rough diagnostic because it ",
      "uses the parsed commencement file in this repo. Early commencement ",
      "records often omit a two-letter state for Alabama hometowns, so the ",
      "simple parser recognizes many out-of-state records but not all ",
      "domestic records. IPEDS entering-cohort residence data should be ",
      "treated as the reliable first-stage panel in this production build."
    ),
    "",
    "Lowest recognized-state coverage years:",
    "",
    markdown_table(coverage, digits = 4)
  )

  writeLines(lines, path)
}

#=====================================================================
# 7 - Execute diagnostic build
#=====================================================================

### i. Read data products written by build_market_iv.py.
market_panel <- read_csv(
  required_file(data_dir, "market_iv_panel.csv"),
  show_col_types = FALSE
)
exposures <- read_csv(
  required_file(table_dir, "market_exposure_by_state.csv"),
  show_col_types = FALSE
)
commencement_panel <- read_csv(
  required_file(data_dir, "ua_commencement_origin_panel.csv"),
  show_col_types = FALSE
)

### ii. Run diagnostics and write machine-readable CSV outputs.
first_stage_diagnostics <- run_first_stage_diagnostics(market_panel)
growth_diagnostics <- build_growth_diagnostics(market_panel, exposures)

write_csv(
  first_stage_diagnostics,
  file.path(table_dir, "first_stage_diagnostics.csv")
)
write_csv(
  growth_diagnostics,
  file.path(table_dir, "state_growth_diagnostics.csv")
)

### iii. Write figures outside data/ so data/market_iv remains table oriented.
ggsave(
  filename = file.path(figure_root, "ipeds_growth_vs_exposure.png"),
  plot = plot_growth_vs_exposure(growth_diagnostics),
  width = 12,
  height = 5.4,
  dpi = 300
)
ggsave(
  filename = file.path(figure_root, "reduced_form_scatterplots.png"),
  plot = plot_growth_vs_exposure(growth_diagnostics),
  width = 12,
  height = 5.4,
  dpi = 300
)
### iv. Write a compact text summary for coauthor review.
write_summary(
  growth = growth_diagnostics,
  diagnostics = first_stage_diagnostics,
  commencement_panel = commencement_panel,
  path = file.path(output_root, "market_iv_build_summary.md")
)

cat("Market-IV diagnostics complete.\n")
cat("Diagnostics written under:", table_dir, "\n")
cat("Figures written under:", figure_root, "\n")
