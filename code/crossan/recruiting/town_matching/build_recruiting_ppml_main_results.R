#=====================================================================
## Author: Crossan Cooper
## Last Modified: 06-08-2026
#
## file use: Extract the main PPML recruiting estimates from the town-FE and
## no-town-FE coefficient grids and write paper-facing tables.
#
## inputs:
## 1. town_matching/output/robustness/ua_visit_t_graduates_t_plus_5_robustness_coefficients.csv -- town-FE t+5 grid
## 2. town_matching/output/robustness/ua_visit_t_graduates_t_plus_6_robustness_coefficients.csv -- town-FE t+6 grid, if available
## 3. town_matching/output/no_town_fe/ua_visit_graduate_no_town_fe_coefficients.csv -- no-town-FE grid
#
## outputs:
## 1. town_matching/output/main_results/recruiting_ppml_town_fe_coefficients.csv -- main town-FE PPML rows
## 2. town_matching/output/main_results/recruiting_ppml_no_town_fe_coefficients.csv -- main no-town-FE PPML rows
## 3. town_matching/output/main_results/recruiting_ppml_town_fe.tex -- paper-facing town-FE PPML table
## 4. town_matching/output/main_results/recruiting_ppml_no_town_fe.tex -- paper-facing no-town-FE PPML table
#=====================================================================

#=====================================================================
# 1 - Packages, arguments, and paths
#=====================================================================

# default list of packages and cleaning command
rm(list=ls())
if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table)

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

town_output_dir <- file.path(repo_root, "town_matching", "output")
robustness_dir <- file.path(town_output_dir, "robustness")
no_town_fe_dir <- file.path(town_output_dir, "no_town_fe")
main_results_dir <- file.path(town_output_dir, "main_results")
paper_table_dir <- file.path(repo_root, "output", "docs", "tables")

dir.create(main_results_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(paper_table_dir, recursive = TRUE, showWarnings = FALSE)

town_fe_t5_path <- get_arg(
  "town-fe-t5",
  file.path(robustness_dir, "ua_visit_t_graduates_t_plus_5_robustness_coefficients.csv")
)
town_fe_t6_path <- get_arg(
  "town-fe-t6",
  file.path(robustness_dir, "ua_visit_t_graduates_t_plus_6_robustness_coefficients.csv")
)
no_town_fe_path <- get_arg(
  "no-town-fe",
  file.path(no_town_fe_dir, "ua_visit_graduate_no_town_fe_coefficients.csv")
)

town_fe_csv_path <- file.path(main_results_dir, "recruiting_ppml_town_fe_coefficients.csv")
no_town_fe_csv_path <- file.path(main_results_dir, "recruiting_ppml_no_town_fe_coefficients.csv")
town_fe_tex_path <- file.path(main_results_dir, "recruiting_ppml_town_fe.tex")
no_town_fe_tex_path <- file.path(main_results_dir, "recruiting_ppml_no_town_fe.tex")

#=====================================================================
# 2 - Helpers
#=====================================================================

read_required <- function(path) {
  if (!file.exists(path)) {
    stop("Required input does not exist: ", path, call. = FALSE)
  }
  fread(path)
}

read_optional <- function(path) {
  if (!file.exists(path)) {
    return(NULL)
  }
  fread(path)
}

stars <- function(p_value) {
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

format_coef <- function(value, p_value, digits = 3) {
  if (is.na(value)) {
    return("")
  }
  paste0(formatC(value, format = "f", digits = digits), stars(p_value))
}

format_se <- function(value, digits = 3) {
  if (is.na(value)) {
    return("")
  }
  paste0("(", formatC(value, format = "f", digits = digits), ")")
}

format_fit <- function(value, digits = 3) {
  if (is.na(value)) {
    return("")
  }
  formatC(value, format = "f", digits = digits)
}

format_log_likelihood <- function(value) {
  if (is.na(value)) {
    return("")
  }
  formatC(value, format = "f", digits = 1, big.mark = ",")
}

format_n <- function(value) {
  if (is.na(value)) {
    return("")
  }
  formatC(as.integer(value), format = "d", big.mark = ",")
}

latex_row <- function(...) {
  paste0(paste(..., sep = " & "), " \\\\")
}

first_ok_row <- function(dt, keep) {
  keep[is.na(keep)] <- FALSE
  out <- dt[keep & dt[["status"]] == "ok"]
  if (nrow(out) == 0) {
    return(NULL)
  }
  out[1]
}

cell_for <- function(dt, treatment_value, control_value, field) {
  row <- first_ok_row(
    dt,
    dt[["treatment"]] == treatment_value & dt[["control_set"]] == control_value
  )
  if (is.null(row)) {
    return("")
  }
  if (field == "coef") {
    return(format_coef(row$estimate, row$p_value))
  }
  if (field == "se") {
    return(format_se(row$std_error))
  }
  if (field == "nobs") {
    return(format_n(row$nobs))
  }
  if (field == "apr2") {
    return(format_fit(row$adj_pseudo_r2))
  }
  if (field == "ll") {
    return(format_log_likelihood(row$log_likelihood))
  }
  ""
}

ppml_table_row <- function(dt, horizon_value, sample_value, outcome_value, control_value) {
  count_row <- first_ok_row(
    dt,
    dt[["horizon"]] == horizon_value &
      dt[["sample"]] == sample_value &
      dt[["outcome_family"]] == outcome_value &
      dt[["control_set"]] == control_value &
      dt[["treatment"]] == "visit_count"
  )
  any_row <- first_ok_row(
    dt,
    dt[["horizon"]] == horizon_value &
      dt[["sample"]] == sample_value &
      dt[["outcome_family"]] == outcome_value &
      dt[["control_set"]] == control_value &
      dt[["treatment"]] == "any_visit"
  )

  sample_label <- if (sample_value == "alabama") "Alabama" else "Outside Alabama"
  nobs_value <- if (!is.null(count_row)) count_row$nobs else if (!is.null(any_row)) any_row$nobs else NA_integer_

  estimate_line <- latex_row(
    paste0("$t+", horizon_value, "$"),
    sample_label,
    format_n(nobs_value),
    if (!is.null(count_row)) format_coef(count_row$estimate, count_row$p_value) else "",
    if (!is.null(any_row)) format_coef(any_row$estimate, any_row$p_value) else "",
    if (!is.null(count_row)) format_fit(count_row$adj_pseudo_r2) else "",
    if (!is.null(any_row)) format_fit(any_row$adj_pseudo_r2) else ""
  )
  se_line <- latex_row(
    "",
    "",
    "",
    if (!is.null(count_row)) format_se(count_row$std_error) else "",
    if (!is.null(any_row)) format_se(any_row$std_error) else "",
    "",
    ""
  )
  c(estimate_line, se_line)
}

#=====================================================================
# 3 - Extract main PPML estimates
#=====================================================================

town_fe_sources <- list(read_required(town_fe_t5_path), read_optional(town_fe_t6_path))
town_fe <- rbindlist(Filter(Negate(is.null), town_fe_sources), fill = TRUE)
no_town_fe <- read_required(no_town_fe_path)

town_fe_main <- town_fe[
  outcome_family == "count_ppml_exposure" &
    fixed_effect_set == "town_year_fe" &
    support_filter == "unrestricted" &
    treatment %in% c("visit_count", "any_visit") &
    control_set %in% c("fe_only", "lag5", "predicted") &
    sample %in% c("full", "alabama", "out_of_state")
]
if (nrow(town_fe_main) == 0) {
  stop("No main town-FE PPML rows found in the town-FE coefficient grid.", call. = FALSE)
}

no_town_fe_main <- no_town_fe[
  outcome_family %in% c("count_ppml_exposure", "count_ppml_baseline_rhs") &
    fixed_effect_set == "state_year_fe" &
    treatment %in% c("visit_count", "any_visit") &
    control_set %in% c("exposure_predicted_acs", "town_size_predicted_acs") &
    sample %in% c("alabama", "out_of_state") &
    horizon %in% c(5, 6)
]
if (nrow(no_town_fe_main) == 0) {
  stop("No main no-town-FE PPML rows found in the no-town-FE coefficient grid.", call. = FALSE)
}

fwrite(town_fe_main, town_fe_csv_path)
fwrite(no_town_fe_main, no_town_fe_csv_path)

#=====================================================================
# 4 - Write paper-facing PPML tables
#=====================================================================

town_fe_t5_full <- town_fe_main[horizon == 5 & sample == "full"]
town_fe_table <- c(
  "% Auto-generated by build_recruiting_ppml_main_results.R",
  "\\begin{table}[t]",
  "\\centering",
  "\\caption{Town-fixed-effect PPML estimates with fixed grade-12 exposure}",
  "\\label{tbl:recruiting-ppml-town-fe}",
  "\\scriptsize",
  "\\setlength{\\tabcolsep}{3.5pt}",
  "\\begin{tabular}{@{}lcccccc@{}}",
  "\\toprule",
  " & \\multicolumn{6}{c}{Outcome: UA graduate count; exposure: $H_m^0$} \\\\",
  "\\cmidrule(lr){2-7}",
  paste0(
    " & \\multicolumn{2}{c}{Town/year FE} & \\multicolumn{2}{c}{Lagged shares} & ",
    "\\multicolumn{2}{c}{Predicted share} \\\\"
  ),
  "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}\\cmidrule(l){6-7}",
  " & Count & Any & Count & Any & Count & Any \\\\",
  "\\midrule",
  latex_row(
    "Visits",
    cell_for(town_fe_t5_full, "visit_count", "fe_only", "coef"),
    "",
    cell_for(town_fe_t5_full, "visit_count", "lag5", "coef"),
    "",
    cell_for(town_fe_t5_full, "visit_count", "predicted", "coef"),
    ""
  ),
  latex_row(
    "",
    cell_for(town_fe_t5_full, "visit_count", "fe_only", "se"),
    "",
    cell_for(town_fe_t5_full, "visit_count", "lag5", "se"),
    "",
    cell_for(town_fe_t5_full, "visit_count", "predicted", "se"),
    ""
  ),
  latex_row(
    "Any visit",
    "",
    cell_for(town_fe_t5_full, "any_visit", "fe_only", "coef"),
    "",
    cell_for(town_fe_t5_full, "any_visit", "lag5", "coef"),
    "",
    cell_for(town_fe_t5_full, "any_visit", "predicted", "coef")
  ),
  latex_row(
    "",
    "",
    cell_for(town_fe_t5_full, "any_visit", "fe_only", "se"),
    "",
    cell_for(town_fe_t5_full, "any_visit", "lag5", "se"),
    "",
    cell_for(town_fe_t5_full, "any_visit", "predicted", "se")
  ),
  "\\midrule",
  "Town fixed effects & Yes & Yes & Yes & Yes & Yes & Yes \\\\",
  "Alabama fixed effect & Absorbed & Absorbed & Absorbed & Absorbed & Absorbed & Absorbed \\\\",
  "Recruiting-year fixed effects & Yes & Yes & Yes & Yes & Yes & Yes \\\\",
  "Lagged graduate-share controls & No & No & Yes & Yes & No & No \\\\",
  "Predicted graduate-share control & No & No & No & No & Yes & Yes \\\\",
  latex_row(
    "Observations",
    cell_for(town_fe_t5_full, "visit_count", "fe_only", "nobs"),
    cell_for(town_fe_t5_full, "any_visit", "fe_only", "nobs"),
    cell_for(town_fe_t5_full, "visit_count", "lag5", "nobs"),
    cell_for(town_fe_t5_full, "any_visit", "lag5", "nobs"),
    cell_for(town_fe_t5_full, "visit_count", "predicted", "nobs"),
    cell_for(town_fe_t5_full, "any_visit", "predicted", "nobs")
  ),
  latex_row(
    "Adjusted pseudo-$R^2$",
    cell_for(town_fe_t5_full, "visit_count", "fe_only", "apr2"),
    cell_for(town_fe_t5_full, "any_visit", "fe_only", "apr2"),
    cell_for(town_fe_t5_full, "visit_count", "lag5", "apr2"),
    cell_for(town_fe_t5_full, "any_visit", "lag5", "apr2"),
    cell_for(town_fe_t5_full, "visit_count", "predicted", "apr2"),
    cell_for(town_fe_t5_full, "any_visit", "predicted", "apr2")
  ),
  latex_row(
    "Log likelihood",
    cell_for(town_fe_t5_full, "visit_count", "fe_only", "ll"),
    cell_for(town_fe_t5_full, "any_visit", "fe_only", "ll"),
    cell_for(town_fe_t5_full, "visit_count", "lag5", "ll"),
    cell_for(town_fe_t5_full, "any_visit", "lag5", "ll"),
    cell_for(town_fe_t5_full, "visit_count", "predicted", "ll"),
    cell_for(town_fe_t5_full, "any_visit", "predicted", "ll")
  ),
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{minipage}{0.92\\textwidth}",
  "\\footnotesize",
  "\\emph{Notes}: The table reports the main town-fixed-effect PPML recruiting estimates.",
  "Coefficients are semi-elasticities. The exposure is fixed 2017--18 grade-12 enrollment.",
  "Standard errors, in parentheses, are clustered by town. $^{*}p<0.10$, $^{**}p<0.05$,",
  "$^{***}p<0.01$.",
  "\\end{minipage}",
  "\\end{table}"
)

no_town_fe_table <- c(
  "% Auto-generated by build_recruiting_ppml_main_results.R",
  "\\begin{table}[t]",
  "\\centering",
  "\\caption{No-town-fixed-effect PPML count estimates with ACS controls by Alabama status}",
  "\\label{tbl:recruiting-no-town-fe-ppml}",
  "\\scriptsize",
  "\\setlength{\\tabcolsep}{3.8pt}",
  "\\begin{tabular}{@{}llrrrrr@{}}",
  "\\toprule",
  "Horizon & Sample & Observations & Visit count & Any visit & \\multicolumn{2}{c}{Adjusted pseudo-$R^2$} \\\\",
  "\\cmidrule(l){6-7}",
  " &  &  &  &  & Count & Any \\\\",
  "\\midrule",
  "\\multicolumn{7}{@{}l}{\\emph{Panel A. Fixed grade-12 enrollment imposed as exposure}} \\\\",
  ppml_table_row(no_town_fe_main, 5, "alabama", "count_ppml_exposure", "exposure_predicted_acs"),
  ppml_table_row(no_town_fe_main, 5, "out_of_state", "count_ppml_exposure", "exposure_predicted_acs"),
  ppml_table_row(no_town_fe_main, 6, "alabama", "count_ppml_exposure", "exposure_predicted_acs"),
  ppml_table_row(no_town_fe_main, 6, "out_of_state", "count_ppml_exposure", "exposure_predicted_acs"),
  "\\addlinespace",
  "\\multicolumn{7}{@{}l}{\\emph{Panel B. Log fixed grade-12 enrollment included on the RHS}} \\\\",
  ppml_table_row(no_town_fe_main, 5, "alabama", "count_ppml_baseline_rhs", "town_size_predicted_acs"),
  ppml_table_row(no_town_fe_main, 5, "out_of_state", "count_ppml_baseline_rhs", "town_size_predicted_acs"),
  ppml_table_row(no_town_fe_main, 6, "alabama", "count_ppml_baseline_rhs", "town_size_predicted_acs"),
  ppml_table_row(no_town_fe_main, 6, "out_of_state", "count_ppml_baseline_rhs", "town_size_predicted_acs"),
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{minipage}{0.92\\textwidth}",
  "\\footnotesize",
  "\\emph{Notes}: The table reports the main ACS-controlled no-town-fixed-effect PPML recruiting",
  "estimates. All specifications include state fixed effects, recruiting-year fixed effects,",
  "private grade-12 share, public/private grade-12 school counts, and ACS controls for total",
  "population, median household income, poverty share, and Black-alone population share.",
  "Panel A uses fixed 2017--18 grade-12 enrollment as an exposure and controls for prior UA",
  "graduate share mean and trend and predicted graduate share. Panel B estimates count models",
  "that instead include log fixed grade-12 enrollment on the right-hand side, along with prior",
  "UA graduate count mean and trend and predicted graduate count. Coefficients are",
  "semi-elasticities. Standard errors, in parentheses, are clustered by town.",
  "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$.",
  "\\end{minipage}",
  "\\end{table}"
)

writeLines(town_fe_table, town_fe_tex_path)
writeLines(no_town_fe_table, no_town_fe_tex_path)
writeLines(town_fe_table, file.path(paper_table_dir, "recruiting_ppml_town_fe.tex"))
writeLines(no_town_fe_table, file.path(paper_table_dir, "recruiting_no_town_fe_ppml.tex"))

cat("Wrote main PPML town-FE coefficients to ", town_fe_csv_path, "\n", sep = "")
cat("Wrote main PPML no-town-FE coefficients to ", no_town_fe_csv_path, "\n", sep = "")
cat("Wrote main PPML tables to ", main_results_dir, "\n", sep = "")
