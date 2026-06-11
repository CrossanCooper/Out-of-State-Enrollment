#=====================================================================
## Created by: Codex for Crossan Cooper
## Last Modified: 6-9-26
##
## estimate Chetty / institution-level IPEDS regressions separately
## by parental income bin using a state-support measure:
## state appropriations + state operating grants and contracts
##
## inputs:
## 1. admissions_project/data/CollegeAdmissions_Data.csv
## 2. code/extract_ipeds_institution_state_appropriations_2011_2015.py
## 3. pgp-ipeds/ipeds-database/ipeds.duckdb
##
## outputs:
## 1. output/tables/chetty_ipeds_institution_state_support_income_bin_results_2011_2015.csv
## 2. output/tables/chetty_ipeds_institution_state_support_top1_results_2011_2015.csv
## 3. output/tables/chetty_ipeds_institution_state_support_top1_table.tex
## 4. output/figures/deprecated/chetty_ipeds_institution_state_support_income_bin_2011_2015_levels.png
## 5. output/figures/deprecated/chetty_ipeds_institution_state_support_income_bin_2011_2015_log.png
## 6. output/figures/chetty_ipeds_institution_state_support_income_bin_2011_2015_levels_full_distribution.png
## 7. output/figures/chetty_ipeds_institution_state_support_income_bin_2011_2015_log_full_distribution.png
## 8. output/figures/deprecated/chetty_ipeds_institution_state_support_per_fte_distribution_2011_2015.png
#=====================================================================

#=====================================================================
# 0 - clean environment, load libraries, and define paths
#=====================================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
  library(ggplot2)
})

args_full <- commandArgs(trailingOnly = FALSE)
file_flag <- "--file="
script_path <- sub(file_flag, "", args_full[grepl(file_flag, args_full)])
script_dir <- if (length(script_path) > 0) {
  dirname(normalizePath(script_path[1], mustWork = TRUE))
} else {
  getwd()
}

crossan_shared_path <- file.path(script_dir, "..", "_shared", "project_paths.R")

if (file.exists(crossan_shared_path)) {
  source(crossan_shared_path)

  project_root <- normalizePath(file.path(pathCode, ".."), mustWork = TRUE)
  source_root <- pathHomeRoot
} else {
  project_root <- normalizePath(file.path(script_dir, ".."), mustWork = TRUE)
  source_root <- Sys.getenv(
    "ADMISSIONS_SOURCE_ROOT",
    unset = "/Users/crossancooper/Dropbox/Professional/active-projects/admissions_project"
  )
}

output_root <- Sys.getenv(
  "ADMISSIONS_OUTPUT_ROOT",
  unset = file.path(project_root, "output")
)
ipeds_project_root <- Sys.getenv(
  "ADMISSIONS_IPEDS_PROJECT_ROOT",
  unset = project_root
)
default_codex_ipeds_root <- file.path(
  dirname(project_root),
  "admissions-project-codex"
)

if (!file.exists(file.path(
  ipeds_project_root,
  "pgp-ipeds",
  "ipeds-database",
  "ipeds.duckdb"
)) && file.exists(file.path(
  default_codex_ipeds_root,
  "pgp-ipeds",
  "ipeds-database",
  "ipeds.duckdb"
))) {
  ipeds_project_root <- default_codex_ipeds_root
}

deprecated_figures_dir <- file.path(output_root, "figures", "deprecated")

chetty_path <- file.path(source_root, "data", "CollegeAdmissions_Data.csv")
extract_script_path <- file.path(
  script_dir,
  "extract_ipeds_institution_state_appropriations_2011_2015.py"
)
if (!file.exists(extract_script_path)) {
  extract_script_path <- file.path(
    project_root,
    "code",
    "extract_ipeds_institution_state_appropriations_2011_2015.py"
  )
}
python_path <- file.path(
  ipeds_project_root,
  "pgp-ipeds",
  "ipeds-database",
  ".venv",
  "bin",
  "python"
)
python_path <- Sys.getenv("ADMISSIONS_IPEDS_PYTHON", unset = python_path)
db_path <- Sys.getenv(
  "ADMISSIONS_IPEDS_DUCKDB",
  unset = file.path(
    ipeds_project_root,
    "pgp-ipeds",
    "ipeds-database",
    "ipeds.duckdb"
  )
)
measures_input_path <- file.path(
  output_root,
  "tables",
  "chetty_ipeds_institution_state_appropriations_2011_2015.csv"
)
coefficients_output_path <- file.path(
  output_root,
  "tables",
  "chetty_ipeds_institution_state_support_income_bin_results_2011_2015.csv"
)
top1_output_path <- file.path(
  output_root,
  "tables",
  "chetty_ipeds_institution_state_support_top1_results_2011_2015.csv"
)
table_output_path <- file.path(
  output_root,
  "tables",
  "chetty_ipeds_institution_state_support_top1_table.tex"
)
levels_figure_path <- file.path(
  deprecated_figures_dir,
  "chetty_ipeds_institution_state_support_income_bin_2011_2015_levels.png"
)
log_figure_path <- file.path(
  deprecated_figures_dir,
  "chetty_ipeds_institution_state_support_income_bin_2011_2015_log.png"
)
levels_full_distribution_figure_path <- file.path(
  output_root,
  "figures",
  "chetty_ipeds_institution_state_support_income_bin_2011_2015_levels_full_distribution.png"
)
log_full_distribution_figure_path <- file.path(
  output_root,
  "figures",
  "chetty_ipeds_institution_state_support_income_bin_2011_2015_log_full_distribution.png"
)
distribution_figure_path <- file.path(
  deprecated_figures_dir,
  "chetty_ipeds_institution_state_support_per_fte_distribution_2011_2015.png"
)

output_paths <- c(
  coefficients_output_path,
  top1_output_path,
  table_output_path,
  levels_figure_path,
  log_figure_path,
  levels_full_distribution_figure_path,
  log_full_distribution_figure_path,
  distribution_figure_path
)

for (path in output_paths) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
}

if (!file.exists(chetty_path)) {
  stop("Could not find CollegeAdmissions_Data.csv at: ", chetty_path)
}

if (!file.exists(extract_script_path)) {
  stop("Could not find extraction script at: ", extract_script_path)
}

if (!file.exists(python_path)) {
  stop("Could not find IPEDS Python environment at: ", python_path)
}

if (!file.exists(db_path)) {
  stop("Could not find IPEDS DuckDB at: ", db_path)
}

#=====================================================================
# 1 - helper functions
#=====================================================================

build_chetty_public <- function(chetty_dt) {
  copy(chetty_dt[public == "Public"])
}

run_ipeds_extraction <- function() {
  status <- system2(
    command = python_path,
    args = c(extract_script_path),
    env = c(
      sprintf("ADMISSIONS_SOURCE_ROOT=%s", source_root),
      sprintf("ADMISSIONS_OUTPUT_ROOT=%s", output_root),
      sprintf("ADMISSIONS_IPEDS_DUCKDB=%s", db_path)
    ),
    stdout = TRUE,
    stderr = TRUE
  )

  attr_status <- attr(status, "status")

  if (!is.null(attr_status) && attr_status != 0) {
    stop(
      "IPEDS extraction step failed.\n",
      paste(status, collapse = "\n")
    )
  }
}

extract_model_row <- function(model,
                              income_bin,
                              outcome_name,
                              rhs_name) {
  model_summary <- summary(model)
  coef_table <- as.data.table(model_summary$coeftable, keep.rownames = "term")
  coef_row <- coef_table[term == rhs_name]

  p_col <- grep("^Pr\\(", names(coef_row), value = TRUE)
  stat_col <- grep("value$", names(coef_row), value = TRUE)

  ci_low <- coef_row[["Estimate"]] - 1.96 * coef_row[["Std. Error"]]
  ci_high <- coef_row[["Estimate"]] + 1.96 * coef_row[["Std. Error"]]

  data.table(
    income_bin = income_bin,
    outcome = outcome_name,
    rhs = rhs_name,
    model_status = "estimated",
    estimate = coef_row[["Estimate"]],
    std_error = coef_row[["Std. Error"]],
    ci_low = ci_low,
    ci_high = ci_high,
    statistic = coef_row[[stat_col[1]]],
    p_value = coef_row[[p_col[1]]],
    n = as.integer(nobs(model)),
    r2 = as.numeric(unname(fitstat(model, "r2"))),
    adj_r2 = as.numeric(unname(fitstat(model, "ar2")))
  )
}

build_missing_model_row <- function(income_bin,
                                    outcome_name,
                                    rhs_name,
                                    n_used,
                                    model_status) {
  data.table(
    income_bin = income_bin,
    outcome = outcome_name,
    rhs = rhs_name,
    model_status = model_status,
    estimate = NA_real_,
    std_error = NA_real_,
    ci_low = NA_real_,
    ci_high = NA_real_,
    statistic = NA_real_,
    p_value = NA_real_,
    n = n_used,
    r2 = NA_real_,
    adj_r2 = NA_real_
  )
}

format_significance_stars <- function(p_value) {
  if (is.na(p_value)) {
    return("")
  }

  if (p_value < 0.01) {
    return("$^{***}$")
  }

  if (p_value < 0.05) {
    return("$^{**}$")
  }

  if (p_value < 0.10) {
    return("$^{*}$")
  }

  ""
}

format_estimate_cell <- function(model_row) {
  if (nrow(model_row) == 0 || is.na(model_row$estimate[1])) {
    return("---")
  }

  sprintf(
    "%.3f%s",
    round(model_row$estimate[1], 3),
    format_significance_stars(model_row$p_value[1])
  )
}

format_se_cell <- function(model_row) {
  if (nrow(model_row) == 0 || is.na(model_row$std_error[1])) {
    return("---")
  }

  sprintf("(%.3f)", round(model_row$std_error[1], 3))
}

format_r2_cell <- function(model_row) {
  if (nrow(model_row) == 0 || is.na(model_row$adj_r2[1])) {
    return("---")
  }

  sprintf("%.2f", round(model_row$adj_r2[1], 2))
}

format_n_cell <- function(model_row) {
  if (nrow(model_row) == 0 || is.na(model_row$n[1])) {
    return("---")
  }

  as.character(as.integer(model_row$n[1]))
}

build_top1_latex_table <- function(results_dt) {
  table_cells <- list(
    instate_attend_log = results_dt[income_bin == "Top 1" & outcome == "rel_attend_instate" & rhs == "LogStateSupportPerStudent"],
    instate_attend_levels = results_dt[income_bin == "Top 1" & outcome == "rel_attend_instate" & rhs == "avgStateSupportPerStudentThousands"],
    instate_apply_log = results_dt[income_bin == "Top 1" & outcome == "rel_apply_instate" & rhs == "LogStateSupportPerStudent"],
    instate_apply_levels = results_dt[income_bin == "Top 1" & outcome == "rel_apply_instate" & rhs == "avgStateSupportPerStudentThousands"],
    oostate_attend_log = results_dt[income_bin == "Top 1" & outcome == "rel_attend_oostate" & rhs == "LogStateSupportPerStudent"],
    oostate_attend_levels = results_dt[income_bin == "Top 1" & outcome == "rel_attend_oostate" & rhs == "avgStateSupportPerStudentThousands"],
    oostate_apply_log = results_dt[income_bin == "Top 1" & outcome == "rel_apply_oostate" & rhs == "LogStateSupportPerStudent"],
    oostate_apply_levels = results_dt[income_bin == "Top 1" & outcome == "rel_apply_oostate" & rhs == "avgStateSupportPerStudentThousands"]
  )

  paste(
    c(
      "\\begin{table}[!htbp]",
      "    \\centering",
      "    \\caption{Top 1\\% Students and State Support}",
      "    \\resizebox{\\textwidth}{!}{",
      "    \\begin{tabular}{lcccccccc}",
      "        \\toprule",
      "         & \\multicolumn{4}{c}{In-State}  & \\multicolumn{4}{c}{Out-of-State}\\\\",
      "        Top 1\\%& \\multicolumn{2}{c}{Relative Attend Rate}  & \\multicolumn{2}{c}{Relative Apply Rate} & \\multicolumn{2}{c}{Relative Attend Rate}  & \\multicolumn{2}{c}{Relative Apply Rate} \\\\",
      "        \\midrule",
      sprintf(
        "        $\\log(\\text{State Support})$ & %s & --- & %s & --- & %s & --- & %s & --- \\\\",
        format_estimate_cell(table_cells$instate_attend_log),
        format_estimate_cell(table_cells$instate_apply_log),
        format_estimate_cell(table_cells$oostate_attend_log),
        format_estimate_cell(table_cells$oostate_apply_log)
      ),
      sprintf(
        "        & %s & --- & %s & --- & %s & --- & %s & --- \\\\",
        format_se_cell(table_cells$instate_attend_log),
        format_se_cell(table_cells$instate_apply_log),
        format_se_cell(table_cells$oostate_attend_log),
        format_se_cell(table_cells$oostate_apply_log)
      ),
      sprintf(
        "         State Support (\\$1000s) & --- & %s & --- & %s & --- & %s & --- & %s\\\\",
        format_estimate_cell(table_cells$instate_attend_levels),
        format_estimate_cell(table_cells$instate_apply_levels),
        format_estimate_cell(table_cells$oostate_attend_levels),
        format_estimate_cell(table_cells$oostate_apply_levels)
      ),
      sprintf(
        "         & --- & %s & --- & %s & --- & %s & --- & %s\\\\",
        format_se_cell(table_cells$instate_attend_levels),
        format_se_cell(table_cells$instate_apply_levels),
        format_se_cell(table_cells$oostate_attend_levels),
        format_se_cell(table_cells$oostate_apply_levels)
      ),
      sprintf(
        "        \\midrule\n        Adjusted $R^2$ & %s & %s & %s & %s & %s & %s & %s & %s \\\\",
        format_r2_cell(table_cells$instate_attend_log),
        format_r2_cell(table_cells$instate_attend_levels),
        format_r2_cell(table_cells$instate_apply_log),
        format_r2_cell(table_cells$instate_apply_levels),
        format_r2_cell(table_cells$oostate_attend_log),
        format_r2_cell(table_cells$oostate_attend_levels),
        format_r2_cell(table_cells$oostate_apply_log),
        format_r2_cell(table_cells$oostate_apply_levels)
      ),
      sprintf(
        "        $N$ & %s & %s & %s & %s & %s & %s & %s & %s \\\\",
        format_n_cell(table_cells$instate_attend_log),
        format_n_cell(table_cells$instate_attend_levels),
        format_n_cell(table_cells$instate_apply_log),
        format_n_cell(table_cells$instate_apply_levels),
        format_n_cell(table_cells$oostate_attend_log),
        format_n_cell(table_cells$oostate_attend_levels),
        format_n_cell(table_cells$oostate_apply_log),
        format_n_cell(table_cells$oostate_apply_levels)
      ),
      "\\midrule\\midrule",
      "\\multicolumn{9}{l}{\\parbox{21cm}{Note: OLS estimates of Equation \\ref{eqn:approp}. $^*$, $^{**}$, and $^{***}$ represent statistical significance at the 10\\%, 5\\%, and 1\\% significance levels, respectively. Standard errors reported in parentheses and clustered at the state-level. The relative attendance (application) rate - defined in \\citealt{chetty2023diversifying} - is a weighted average of attendance (application) rates by test score in each parental income bin, where the weights are given by the college-specific distribution of attending (applying) student test scores. A relative attendance rate of 2 means that students from the corresponding parental income bin are twice as likely to attend a given university than they would be if they attended at the same average rate as all students with their same test score. State support equals state appropriations and state operating grants and contracts from institution-level IPEDS finance data. Support is measured per full-time equivalent student. }}\\\\",
      "\\end{tabular}}",
      "\\label{tbl:appropriations_ipeds_state_support}",
      "\\end{table}"
    ),
    collapse = "\n"
  )
}

estimate_model_or_placeholder <- function(merged_dt,
                                          income_bin,
                                          outcome_name,
                                          rhs_name) {
  lhs <- merged_dt[[outcome_name]]
  rhs <- merged_dt[[rhs_name]]
  keep <- !is.na(lhs) & !is.na(rhs)
  n_used <- sum(keep)

  if (n_used == 0) {
    return(build_missing_model_row(
      income_bin = income_bin,
      outcome_name = outcome_name,
      rhs_name = rhs_name,
      n_used = n_used,
      model_status = "no_outcome_data"
    ))
  }

  if (length(unique(lhs[keep])) < 2) {
    return(build_missing_model_row(
      income_bin = income_bin,
      outcome_name = outcome_name,
      rhs_name = rhs_name,
      n_used = n_used,
      model_status = "constant_outcome"
    ))
  }

  if (length(unique(rhs[keep])) < 2) {
    return(build_missing_model_row(
      income_bin = income_bin,
      outcome_name = outcome_name,
      rhs_name = rhs_name,
      n_used = n_used,
      model_status = "constant_regressor"
    ))
  }

  model_formula <- as.formula(sprintf("%s ~ %s", outcome_name, rhs_name))

  model <- tryCatch(
    feols(model_formula, data = merged_dt, vcov = ~state_abbr),
    error = function(e) NULL
  )

  if (is.null(model)) {
    return(build_missing_model_row(
      income_bin = income_bin,
      outcome_name = outcome_name,
      rhs_name = rhs_name,
      n_used = n_used,
      model_status = "estimation_failed"
    ))
  }

  extract_model_row(
    model = model,
    income_bin = income_bin,
    outcome_name = outcome_name,
    rhs_name = rhs_name
  )
}

build_coefficient_plot <- function(results_dt,
                                   rhs_name,
                                   income_bin_levels,
                                   income_bin_labels,
                                   y_axis_label) {
  plot_dt <- copy(results_dt[rhs == rhs_name & income_bin %in% income_bin_levels])

  plot_dt[, income_bin := factor(income_bin, levels = income_bin_levels)]

  plot_dt[, measure_type := fcase(
    outcome %in% c("rel_apply_oostate", "rel_apply_instate"), "Application",
    outcome %in% c("rel_attend_oostate", "rel_attend_instate"), "Attendance",
    default = outcome
  )]

  plot_dt[, residence_type := fcase(
    outcome %in% c("rel_apply_oostate", "rel_attend_oostate"), "Out-of-State",
    outcome %in% c("rel_apply_instate", "rel_attend_instate"), "In-State",
    default = outcome
  )]

  plot_dt[, measure_type := factor(
    measure_type,
    levels = c("Application", "Attendance")
  )]

  plot_dt[, residence_type := factor(
    residence_type,
    levels = c("Out-of-State", "In-State")
  )]

  plot_dt <- plot_dt[model_status == "estimated"]

  dodge_width <- 0.55

  ggplot(
    plot_dt,
    aes(
      x = income_bin,
      y = estimate,
      color = residence_type,
      shape = residence_type
    )
  ) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.5, color = "gray45") +
    geom_errorbar(
      aes(ymin = ci_low, ymax = ci_high),
      position = position_dodge(width = dodge_width),
      width = 0.15,
      linewidth = 0.7
    ) +
    geom_point(
      position = position_dodge(width = dodge_width),
      size = 2.5
    ) +
    facet_wrap(~ measure_type, scales = "free_y", ncol = 2) +
    coord_flip() +
    scale_x_discrete(labels = income_bin_labels, drop = FALSE) +
    scale_color_viridis_d() +
    scale_shape_manual(
      values = c(
        "Out-of-State" = 16,
        "In-State" = 17
      )
    ) +
    labs(
      x = "Parental Income Bin",
      y = y_axis_label,
      color = NULL,
      shape = NULL
    ) +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      strip.text = element_text(face = "bold"),
      legend.position = "top",
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA)
    )
}

build_distribution_plot <- function(measures_dt) {
  plot_dt <- copy(measures_dt[!is.na(avgStateSupportPerStudentThousands)])

  ggplot(plot_dt, aes(x = avgStateSupportPerStudentThousands)) +
    geom_histogram(
      bins = 16,
      fill = "#440154FF",
      color = "#FDE725FF",
      linewidth = 0.4
    ) +
    labs(
      x = "State Support per FTE ($1000s, 2011-2015 avg., HECA-adjusted)",
      y = "Number of Institutions"
    ) +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA)
    )
}

#=====================================================================
# 2 - read and clean data
#=====================================================================

run_ipeds_extraction()

if (!file.exists(measures_input_path)) {
  stop("Could not find institution measures file at: ", measures_input_path)
}

chetty_dt <- fread(chetty_path)
measures_dt <- fread(measures_input_path)

chetty_public_dt <- build_chetty_public(chetty_dt)

usable_measures_dt <- measures_dt[
  usable_state_support_2011_2015 == 1 &
    !is.na(avgStateSupportPerStudentThousands) &
    !is.na(LogStateSupportPerStudent)
]

#=====================================================================
# 3 - estimate regressions by income bin
#=====================================================================

income_bins <- c(
  "0-20",
  "20-40",
  "40-60",
  "60-70",
  "70-80",
  "80-90",
  "90-95",
  "95-96",
  "96-97",
  "97-98",
  "98-99",
  "99-99.9",
  "Top 0.1",
  "Top 1"
)

outcomes <- c(
  "rel_apply_oostate",
  "rel_attend_oostate",
  "rel_apply_instate",
  "rel_attend_instate"
)

rhs_vars <- c("avgStateSupportPerStudentThousands", "LogStateSupportPerStudent")

results_list <- list()
list_index <- 1L

for (income_bin in income_bins) {
  chetty_bin_dt <- copy(chetty_public_dt[par_income_lab == income_bin])
  merged_dt <- merge(
    usable_measures_dt,
    chetty_bin_dt,
    by.x = "chetty_name",
    by.y = "name"
  )

  for (outcome_name in outcomes) {
    for (rhs_name in rhs_vars) {
      results_list[[list_index]] <- estimate_model_or_placeholder(
        merged_dt = merged_dt,
        income_bin = income_bin,
        outcome_name = outcome_name,
        rhs_name = rhs_name
      )

      list_index <- list_index + 1L
    }
  }
}

results_dt <- rbindlist(results_list, use.names = TRUE, fill = TRUE)
top1_dt <- copy(results_dt[income_bin == "Top 1"])
latex_table <- build_top1_latex_table(results_dt)

setorder(results_dt, income_bin, outcome, rhs)
setorder(top1_dt, outcome, rhs)

#=====================================================================
# 4 - build figures
#=====================================================================

high_income_levels <- c(
  "90-95",
  "95-96",
  "96-97",
  "97-98",
  "98-99",
  "Top 1"
)

full_distribution_levels <- c(
  "0-20",
  "20-40",
  "40-60",
  "60-70",
  "70-80",
  "80-90",
  "90-95",
  "95-96",
  "96-97",
  "97-98",
  "98-99",
  "Top 1"
)

income_bin_labels <- c(
  "0-20" = "0-20",
  "20-40" = "20-40",
  "40-60" = "40-60",
  "60-70" = "60-70",
  "70-80" = "70-80",
  "80-90" = "80-90",
  "90-95" = "90-95",
  "95-96" = "95-96",
  "96-97" = "96-97",
  "97-98" = "97-98",
  "98-99" = "98-99",
  "Top 1" = "99-100"
)

levels_plot <- build_coefficient_plot(
  results_dt = results_dt,
  rhs_name = "avgStateSupportPerStudentThousands",
  income_bin_levels = high_income_levels,
  income_bin_labels = income_bin_labels[high_income_levels],
  y_axis_label = "Coefficient Estimate: State Support in $1000s"
)

log_plot <- build_coefficient_plot(
  results_dt = results_dt,
  rhs_name = "LogStateSupportPerStudent",
  income_bin_levels = high_income_levels,
  income_bin_labels = income_bin_labels[high_income_levels],
  y_axis_label = "Coefficient Estimate: Log State Support"
)

levels_full_distribution_plot <- build_coefficient_plot(
  results_dt = results_dt,
  rhs_name = "avgStateSupportPerStudentThousands",
  income_bin_levels = full_distribution_levels,
  income_bin_labels = income_bin_labels[full_distribution_levels],
  y_axis_label = "Coefficient Estimate: State Support in $1000s"
)

log_full_distribution_plot <- build_coefficient_plot(
  results_dt = results_dt,
  rhs_name = "LogStateSupportPerStudent",
  income_bin_levels = full_distribution_levels,
  income_bin_labels = income_bin_labels[full_distribution_levels],
  y_axis_label = "Coefficient Estimate: Log State Support"
)

distribution_plot <- build_distribution_plot(usable_measures_dt)

ggsave(
  filename = levels_figure_path,
  plot = levels_plot,
  width = 8,
  height = 4.5,
  dpi = 600
)

ggsave(
  filename = log_figure_path,
  plot = log_plot,
  width = 8,
  height = 4.5,
  dpi = 600
)

ggsave(
  filename = levels_full_distribution_figure_path,
  plot = levels_full_distribution_plot,
  width = 8,
  height = 4.5,
  dpi = 600
)

ggsave(
  filename = log_full_distribution_figure_path,
  plot = log_full_distribution_plot,
  width = 8,
  height = 4.5,
  dpi = 600
)

ggsave(
  filename = distribution_figure_path,
  plot = distribution_plot,
  width = 8,
  height = 4.5,
  dpi = 600
)

#=====================================================================
# 5 - write results
#=====================================================================

fwrite(results_dt, coefficients_output_path)
fwrite(top1_dt, top1_output_path)
writeLines(latex_table, table_output_path)

#=====================================================================
# 6 - print a compact console summary
#=====================================================================

print(
  top1_dt[
    ,
    .(
      income_bin,
      outcome,
      rhs,
      estimate = round(estimate, 4),
      std_error = round(std_error, 4),
      p_value = round(p_value, 4),
      n
    )
  ]
)
