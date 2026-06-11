#=====================================================================
## Author: Crossan Cooper
## Last Modified: 06-08-2026
#
## file use: Plot broad robustness-grid coefficient estimates for internal
## review of horizon, sample, treatment, and support-filter sensitivity.
#
## inputs:
## 1. town_matching/output/robustness/ua_visit_t_graduates_t_plus_3_6_robustness_coefficients.csv -- robustness grid
#
## outputs:
## 1. town_matching/output/robustness/figures -- robustness coefficient figures
#=====================================================================

#=====================================================================
# 1 - Packages, arguments, and paths
#=====================================================================

# default list of packages and cleaning command
rm(list=ls())
if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table,ggplot2)

args <- commandArgs(trailingOnly = TRUE)

input_path <- if (length(args) >= 1) {
  args[[1]]
} else {
  "town_matching/output/robustness/ua_visit_t_graduates_t_plus_3_6_robustness_coefficients.csv"
}

output_dir <- if (length(args) >= 2) {
  args[[2]]
} else {
  "town_matching/output/robustness/figures"
}

if (!file.exists(input_path)) {
  stop("Input coefficient file does not exist: ", input_path)
}

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

coef_dt <- fread(input_path)
coef_dt <- coef_dt[status == "ok" & is.finite(estimate) & is.finite(std_error)]

if (!"fixed_effect_set" %in% names(coef_dt)) {
  coef_dt[, fixed_effect_set := "town_year_fe"]
}

if (nrow(coef_dt) == 0) {
  stop("No successful coefficient estimates found in: ", input_path)
}

#=====================================================================
# 2 - Label coefficient grid and define plotting helpers
#=====================================================================

# The coefficient plots use one-standard-error bars rather than 95 percent
# intervals so that the visual scale matches the requested tabular output.
coef_dt[, se_low := estimate - std_error]
coef_dt[, se_high := estimate + std_error]
coef_dt[, horizon_label := factor(paste0("t+", horizon), levels = paste0("t+", sort(unique(horizon))))]

sample_labels <- c(
  full = "Full sample",
  alabama = "Alabama",
  out_of_state = "Outside Alabama"
)

treatment_labels <- c(
  visit_count = "Visit count",
  any_visit = "Any visit",
  visit_count_same_state_fuzzy = "Visit count, same-state fuzzy",
  any_visit_same_state_fuzzy = "Any visit, same-state fuzzy"
)

control_labels <- c(
  fe_only = "Town + year FE",
  lag5 = "5 lagged outcomes",
  pretrend = "Pre-period trend",
  predicted = "Predicted graduates",
  lag5_predicted = "5 lags + predicted",
  baseline_growth = "Baseline growth",
  trend_deltas = "Trend deltas",
  trend_prediction = "Trend prediction",
  trend_prediction_predicted = "Trend prediction + predicted",
  trend_projection = "Trend projection",
  trend_projection_predicted = "Trend projection + predicted"
)

fixed_effect_labels <- c(
  town_year_fe = "Town + year FE",
  town_linear_trend = "Town linear trends"
)

support_labels <- c(
  unrestricted = "Unrestricted",
  cell_visits_0_1 = "0/1 visits",
  town_max_grad_le_5 = "Town max grads <= 5",
  town_max_grad_le_10 = "Town max grads <= 10",
  town_max_grad_le_20 = "Town max grads <= 20",
  cell_visits_0_1_town_max_grad_le_10 = "0/1 visits and max <= 10"
)

coef_dt[, sample_label := factor(sample_labels[sample], levels = unname(sample_labels))]
coef_dt[, treatment_label := factor(treatment_labels[treatment], levels = unname(treatment_labels))]
coef_dt[, control_label := factor(control_labels[control_set], levels = unname(control_labels))]
coef_dt[, support_label := factor(support_labels[support_filter], levels = unname(support_labels))]
coef_dt[, fixed_effect_label := factor(fixed_effect_labels[fixed_effect_set], levels = unname(fixed_effect_labels))]

save_plot <- function(plot_obj, basename, width = 10, height = 7) {
  pdf_path <- file.path(output_dir, paste0(basename, ".pdf"))
  png_path <- file.path(output_dir, paste0(basename, ".png"))
  ggsave(pdf_path, plot_obj, width = width, height = height, units = "in", device = "pdf")
  ggsave(png_path, plot_obj, width = width, height = height, units = "in", dpi = 220)
}

plot_theme <- theme_minimal(base_size = 10) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  )

#=====================================================================
# 3 - Generate robustness coefficient figures
#=====================================================================

share_unrestricted <- coef_dt[
  outcome_family == "fixed_hs_share_ols" &
    support_filter == "unrestricted" &
    fixed_effect_set == "town_year_fe"
]

p_share_unrestricted <- ggplot(
  share_unrestricted,
  aes(x = horizon_label, y = estimate, ymin = se_low, ymax = se_high, color = control_label)
) +
  geom_hline(yintercept = 0, linewidth = 0.3, color = "gray35") +
  geom_errorbar(position = position_dodge(width = 0.55), width = 0.12, linewidth = 0.35) +
  geom_point(position = position_dodge(width = 0.55), size = 1.3) +
  facet_grid(sample_label ~ treatment_label) +
  labs(
    x = "Graduate outcome year relative to recruiting school year",
    y = "Coefficient on visits, fixed high-school share",
    title = "Fixed-denominator share estimates, unrestricted sample",
    subtitle = "Points are coefficient estimates; bars show one standard error"
  ) +
  plot_theme

save_plot(p_share_unrestricted, "fixed_hs_share_unrestricted_by_horizon", width = 13, height = 8)

count_unrestricted <- coef_dt[
  outcome_family == "count_ols" &
    support_filter == "unrestricted" &
    fixed_effect_set == "town_year_fe"
]

p_count_unrestricted <- ggplot(
  count_unrestricted,
  aes(x = horizon_label, y = estimate, ymin = se_low, ymax = se_high, color = control_label)
) +
  geom_hline(yintercept = 0, linewidth = 0.3, color = "gray35") +
  geom_errorbar(position = position_dodge(width = 0.55), width = 0.12, linewidth = 0.35) +
  geom_point(position = position_dodge(width = 0.55), size = 1.3) +
  facet_grid(sample_label ~ treatment_label) +
  labs(
    x = "Graduate outcome year relative to recruiting school year",
    y = "Coefficient on visits, UA graduate count",
    title = "Count estimates, unrestricted sample",
    subtitle = "Points are coefficient estimates; bars show one standard error"
  ) +
  plot_theme

save_plot(p_count_unrestricted, "count_unrestricted_by_horizon", width = 13, height = 8)

alabama_any_share <- coef_dt[
  outcome_family == "fixed_hs_share_ols" &
    fixed_effect_set == "town_year_fe" &
    sample == "alabama" &
    treatment %in% c("any_visit", "any_visit_same_state_fuzzy")
]

p_alabama_support <- ggplot(
  alabama_any_share,
  aes(x = support_label, y = estimate, ymin = se_low, ymax = se_high, color = control_label)
) +
  geom_hline(yintercept = 0, linewidth = 0.3, color = "gray35") +
  geom_errorbar(position = position_dodge(width = 0.62), width = 0.15, linewidth = 0.35) +
  geom_point(position = position_dodge(width = 0.62), size = 1.2) +
  facet_grid(horizon_label ~ treatment_label) +
  coord_flip() +
  labs(
    x = NULL,
    y = "Coefficient on any visit, fixed high-school share",
    title = "Alabama fixed-denominator share estimates by support filter",
    subtitle = "Points are coefficient estimates; bars show one standard error"
  ) +
  plot_theme +
  theme(axis.text.y = element_text(size = 8))

save_plot(p_alabama_support, "fixed_hs_share_alabama_any_visit_support_filters", width = 12, height = 9)

positive_grid <- coef_dt[
  outcome_family == "fixed_hs_share_ols" &
    fixed_effect_set == "town_year_fe",
  .(
    models = .N,
    positive = sum(estimate > 0),
    positive_p_lt_10 = sum(estimate > 0 & p_value < 0.10)
  ),
  by = .(horizon_label, sample_label, support_label)
]
positive_grid[, share_positive := positive / models]

p_positive_grid <- ggplot(
  positive_grid,
  aes(x = support_label, y = horizon_label, fill = positive_p_lt_10)
) +
  geom_tile(color = "white", linewidth = 0.4) +
  geom_text(aes(label = positive_p_lt_10), size = 3) +
  facet_wrap(~sample_label, ncol = 1) +
  scale_fill_gradient(low = "#f3f5f7", high = "#2b6cb0") +
  labs(
    x = NULL,
    y = NULL,
    fill = "Positive\np < 0.10",
    title = "Positive fixed-denominator share estimates by sample, horizon, and support filter",
    subtitle = "Cell values count specifications with a positive coefficient and p-value below 0.10"
  ) +
  plot_theme +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

save_plot(p_positive_grid, "fixed_hs_share_positive_estimate_grid", width = 10, height = 8)

trend_focus_controls <- c(
  "fe_only",
  "pretrend",
  "trend_deltas",
  "trend_prediction",
  "trend_prediction_predicted",
  "trend_projection",
  "trend_projection_predicted",
  "lag5",
  "lag5_predicted"
)

out_state_trend_share <- coef_dt[
  outcome_family == "fixed_hs_share_ols" &
    sample == "out_of_state" &
    support_filter == "unrestricted" &
    treatment %in% c("visit_count", "any_visit", "visit_count_same_state_fuzzy", "any_visit_same_state_fuzzy") &
    control_set %in% trend_focus_controls
]

p_out_state_trend <- ggplot(
  out_state_trend_share,
  aes(x = horizon_label, y = estimate, ymin = se_low, ymax = se_high, color = control_label)
) +
  geom_hline(yintercept = 0, linewidth = 0.3, color = "gray35") +
  geom_errorbar(position = position_dodge(width = 0.6), width = 0.12, linewidth = 0.35) +
  geom_point(position = position_dodge(width = 0.6), size = 1.2) +
  facet_grid(fixed_effect_label ~ treatment_label) +
  labs(
    x = "Graduate outcome year relative to recruiting school year",
    y = "Coefficient on visits, fixed high-school share",
    title = "Outside-Alabama share estimates with pre-period trend controls",
    subtitle = "Points are coefficient estimates; bars show one standard error"
  ) +
  plot_theme

save_plot(p_out_state_trend, "fixed_hs_share_out_of_state_trend_controls", width = 13, height = 8)

cat("Wrote coefficient figures to ", output_dir, "\n", sep = "")
