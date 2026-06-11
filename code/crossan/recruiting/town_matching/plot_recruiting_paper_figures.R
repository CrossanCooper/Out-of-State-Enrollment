#=====================================================================
## Author: Crossan Cooper
## Last Modified: 06-08-2026
#
## file use: Create paper-style coefficient figures for the UA recruiting
## subsection from the town-FE and no-town-FE model outputs.
#
## inputs:
## 1. town_matching/output/robustness/ua_visit_t_graduates_t_plus_5_robustness_coefficients.csv -- town-FE estimates
## 2. town_matching/output/no_town_fe/ua_visit_graduate_no_town_fe_coefficients.csv -- no-town-FE estimates
#
## outputs:
## 1. output/docs/figures/recruiting_town_fe_share_coefficients.pdf/png -- town-FE coefficient plot
## 2. output/docs/figures/recruiting_no_town_fe_share_coefficients.pdf/png -- no-town-FE coefficient plot
#=====================================================================

#=====================================================================
# 1 - Packages, paths, and plot helpers
#=====================================================================

# default list of packages and cleaning command
rm(list=ls())
if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table,ggplot2)

default_data_root <- "/Users/crossancooper/Dropbox/Professional/active-projects/admissions_project"
repo_root <- normalizePath(
  Sys.getenv("ADMISSIONS_PROJECT_DATA_ROOT", default_data_root),
  mustWork = FALSE
)

robustness_path <- file.path(
  repo_root,
  "town_matching",
  "output",
  "robustness",
  "ua_visit_t_graduates_t_plus_5_robustness_coefficients.csv"
)
no_town_fe_path <- file.path(
  repo_root,
  "town_matching",
  "output",
  "no_town_fe",
  "ua_visit_graduate_no_town_fe_coefficients.csv"
)
figure_dir <- file.path(repo_root, "output", "docs", "figures")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

format_data <- function(dt) {
  dt[, horizon_label := factor(paste0("r+", horizon), levels = paste0("r+", sort(unique(horizon))))]
  dt[, sample_label := fifelse(
    sample == "full",
    "Full sample",
    fifelse(sample == "alabama", "Alabama", "Outside Alabama")
  )]
  dt[, sample_label := factor(sample_label, levels = c("Full sample", "Alabama", "Outside Alabama"))]
  dt[, treatment_label := fifelse(treatment == "any_visit", "Any visit", "Visit count")]
  dt[, treatment_label := factor(treatment_label, levels = c("Visit count", "Any visit"))]
  dt[, control_label := fifelse(
    control_set == "lag5",
    "Lagged shares",
    fifelse(
      control_set == "predicted",
      "Predicted share",
      fifelse(
        control_set == "town_size_predicted",
        "Town size + predicted",
        fifelse(control_set == "town_size_predicted_acs", "Town size + predicted + ACS", control_set)
      )
    )
  )]
  dt[, estimate := 1000 * estimate]
  dt[, std_error := 1000 * std_error]
  dt[, low := estimate - std_error]
  dt[, high := estimate + std_error]
  dt
}

plot_theme <- theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.background = element_rect(fill = "white", color = "black", linetype = "solid", linewidth = 0.25),
    legend.key = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    text = element_text(size = 12)
  )

# Keep both treatment panels and both coefficient figures on the same vertical
# scale so visual comparisons are not driven by facet-specific y-axes.
common_y_limits <- c(-4, 4)

save_plot <- function(plot_obj, basename, width = 7.2, height = 4.8) {
  pdf_path <- file.path(figure_dir, paste0(basename, ".pdf"))
  png_path <- file.path(figure_dir, paste0(basename, ".png"))
  ggsave(pdf_path, plot_obj, width = width, height = height, units = "in", device = "pdf")
  ggsave(png_path, plot_obj, width = width, height = height, units = "in", dpi = 250)
}

#=====================================================================
# 2 - Town-fixed-effect coefficient figure
#=====================================================================

robustness <- fread(robustness_path)
town_fe <- robustness[
  status == "ok" &
    fixed_effect_set == "town_year_fe" &
    support_filter == "unrestricted" &
    outcome_family == "fixed_hs_share_ols" &
    horizon == 5 &
    sample %in% c("full", "alabama", "out_of_state") &
    treatment %in% c("visit_count", "any_visit") &
    control_set %in% c("lag5", "predicted")
]
town_fe <- format_data(town_fe)
town_fe[, control_label := factor(control_label, levels = c("Lagged shares", "Predicted share"))]

p_town_fe <- ggplot(
  town_fe,
  aes(
    x = sample_label,
    y = estimate,
    ymin = low,
    ymax = high,
    color = control_label,
    fill = control_label,
    shape = control_label
  )
) +
  geom_hline(yintercept = 0, color = "gray35", linewidth = 0.3) +
  geom_errorbar(position = position_dodge(width = 0.45), width = 0.16, linewidth = 0.35) +
  geom_point(position = position_dodge(width = 0.45), size = 2.0, color = "black", stroke = 0.35) +
  facet_wrap(~ treatment_label, scales = "fixed", nrow = 1) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  scale_y_continuous(limits = common_y_limits, breaks = seq(-4, 4, by = 2)) +
  scale_shape_manual(values = c(21, 24)) +
  labs(
    x = NULL,
    y = "Effect per 1,000 students",
    color = "Specification",
    fill = "Specification",
    shape = "Specification"
  ) +
  plot_theme
save_plot(p_town_fe, "recruiting_town_fe_share_coefficients", width = 7.5, height = 3.8)

#=====================================================================
# 3 - No-town-fixed-effect coefficient figure
#=====================================================================

no_town_fe <- fread(no_town_fe_path)
no_town_fe <- no_town_fe[
  status == "ok" &
    fixed_effect_set == "state_year_fe" &
    outcome_family == "fixed_hs_share_ols" &
    horizon == 5 &
    sample %in% c("full", "alabama", "out_of_state") &
    treatment %in% c("visit_count", "any_visit") &
    control_set %in% c("town_size_predicted", "town_size_predicted_acs")
]
no_town_fe <- format_data(no_town_fe)
no_town_fe[, control_label := factor(
  control_label,
  levels = c("Town size + predicted", "Town size + predicted + ACS"),
  labels = c("No ACS controls", "ACS controls")
)]

p_no_town_fe <- ggplot(
  no_town_fe,
  aes(
    x = sample_label,
    y = estimate,
    ymin = low,
    ymax = high,
    color = control_label,
    fill = control_label,
    shape = control_label
  )
) +
  geom_hline(yintercept = 0, color = "gray35", linewidth = 0.3) +
  geom_errorbar(position = position_dodge(width = 0.45), width = 0.16, linewidth = 0.35) +
  geom_point(position = position_dodge(width = 0.45), size = 2.0, color = "black", stroke = 0.35) +
  facet_wrap(~ treatment_label, scales = "fixed", nrow = 1) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  scale_y_continuous(limits = common_y_limits, breaks = seq(-4, 4, by = 2)) +
  scale_shape_manual(values = c(21, 24)) +
  labs(
    x = NULL,
    y = "Effect per 1,000 students",
    color = "Specification",
    fill = "Specification",
    shape = "Specification"
  ) +
  plot_theme
save_plot(p_no_town_fe, "recruiting_no_town_fe_share_coefficients", width = 7.5, height = 3.8)

cat(sprintf("Wrote paper figures to %s\n", figure_dir))
