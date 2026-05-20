#////////////////////////////////////////////////////////////////////////////////
# Filename: analysis_fiscal.R
# Author: Ryan Haygood
# Date: 5/9/26
# Description: Calculates the net fiscal implications of out-of-state enrollment
# on a per-OOS-student basis for University of Alabama.
#////////////////////////////////////////////////////////////////////////////////

# This file implements the calculations in "Net Fiscal Implications.xlsx".
# The workbook uses one major distribution for both in-state push and OOS pull
# revenue terms. The objects below keep separate in/out major-share columns so
# they can be replaced with distinct distributions when available.

# Setup
source('C:/Users/ryanh/OneDrive/Documents/Grad School/Research/Out-of-State-Enrollment/code/setup.R')

# Parameters --------------------------------------------------------------------

tax_rate <- 0.05
ua_fips <- 1
standard_deduction <- 3000
personal_exemption <- 1500
federal_tax_deduction_rate <- 0.10
discount_factor <- 0.98
retirement_t <- 48

n_in_state <- 3401
pull_effect <- 0.10
chain_rule_adjustment <- 1 / (206 * 100)
annual_leave_rate <- 0.02
grad_rate <- 0.74
# OOS second-/third-year retention rates from UA OIRA:
# https://oira.ua.edu/factbooklegacy/reports/other-academic-information/first-time-full-time-undergraduate-cohort-retention-rates/
enrollment_survival <- c(1, 0.86, 0.81, grad_rate)

aid_year <- 2019
fte_year <- 2019

tuition <- read.csv(paste0(pathHome, 'data/ipeds_tuition/ic',
                           aid_year, '_ay.csv')) %>%
  rename_with(toupper) %>%
  filter(UNITID == 100751)

posted_oos_tuition <- as.numeric(tuition$TUITION3)
required_fees <- if_else('FEE3' %in% names(tuition),
                         as.numeric(tuition$FEE3),
                         0)
posted_oos_tuition_fees <- posted_oos_tuition + required_fees

# The grant-aid estimates come from analysis_grant_aid.R's main
# 2023-24 specification: CDS gap-preserving ACT/SAT score distributions,
# the IPEDS SFA tuition target, and a 50/50 split of residual institutional
# tuition grants after Alabama Advantage. To keep this fiscal file on its
# original 2019 baseline, scale those grant estimates by the ratio of 2019
# to 2023-24 headline OOS tuition.
posted_oos_tuition_2023 <- 32400
tuition_scale_2019_from_2023 <- posted_oos_tuition / posted_oos_tuition_2023
avg_oos_auto_merit_2023 <- 8618.605
avg_oos_residual_tuition_grant_2023 <- 2402.373
avg_oos_auto_merit <- avg_oos_auto_merit_2023 *
  tuition_scale_2019_from_2023
avg_oos_residual_tuition_grant <- avg_oos_residual_tuition_grant_2023 *
  tuition_scale_2019_from_2023
avg_oos_tuition_grant <-
  avg_oos_auto_merit + avg_oos_residual_tuition_grant
oos_net_tuition_fees <- posted_oos_tuition_fees - avg_oos_tuition_grant

fin <- read.csv(paste0(pathHome, 'data/ipeds_finance/f',
                       substr(aid_year, 3, 4),
                       substr(aid_year + 1, 3, 4),
                       '_f1a_rv.csv')) %>%
  rename_with(toupper) %>%
  filter(UNITID == 100751)

fte <- read_xlsx(paste0(pathHome, 'data/factbook/fte_by_college_in_out.xlsx')) %>%
  rename(origin = Origin, college = `By College/School`, y = STYEAR, fte = sum_fte) %>%
  mutate(y = as.numeric(y), fte = as.numeric(fte))
average_cost <- (fin$F1C011 + fin$F1C051 + fin$F1C061) /
  sum(fte$fte[fte$y == fte_year], na.rm = TRUE)

# Major-specific inputs ---------------------------------------------------------

major_inputs <- data.frame(
  major = c(
    'Business',
    'Engineering',
    'Marketing',
    'Finance',
    'Accounting',
    'Nursing',
    'Economics',
    'Education',
    'Other STEM',
    'All other'
  ),
  webber_category = c(
    'Business',
    'STEM',
    'Business',
    'Business',
    'Business',
    'STEM',
    'Social',
    'Arts / Humanities',
    'STEM',
    'Arts / Humanities'
  ),
  # This is the marginal effect used in the workbook. Negative values reduce
  # the probability that an in-state student works in Alabama.
  marginal_effect = c(-0.41, -0.30, -0.49, -0.48, -0.66,
                      -0.17, -0.29, -0.09, -0.29, -0.28),
  initial_earnings = c(63835, 72152, 60452, 71114, 54407,
                       65377, 63458, 45345, 54978, 54273),
  share = c(649, 723, 510, 497, 207, 374, 69, 363, 340, 2773) / 6505,
  stringsAsFactors = FALSE
)

major_inputs$in_major_share <- major_inputs$share
major_inputs$out_major_share <- major_inputs$share

# Earnings profiles -------------------------------------------------------------

# Log earnings by broad major category and age from Table 3 of Webber (2014).
earnings_anchors <- data.frame(
  webber_category = c('STEM', 'Business', 'Social', 'Arts / Humanities'),
  log_earn_age_26 = c(9.88 + 0.818, 9.88 + 0.738, 9.88 + 0.545, 9.88 + 0.294),
  log_earn_age_41 = c(10.28 + 0.876, 10.28 + 0.850, 10.28 + 0.755, 10.28 + 0.550),
  log_earn_age_61 = c(10.17 + 0.692, 10.17 + 0.597, 10.17 + 0.467, 10.17 + 0.324),
  stringsAsFactors = FALSE
)

earnings_anchors$quadratic <- (
  ((earnings_anchors$log_earn_age_61 - earnings_anchors$log_earn_age_41) /
     (61 - 41)) -
    ((earnings_anchors$log_earn_age_41 - earnings_anchors$log_earn_age_26) /
       (41 - 26))
) / (61 - 26)

earnings_anchors$linear <- (
  (earnings_anchors$log_earn_age_41 - earnings_anchors$log_earn_age_26) /
    (41 - 26)
) - earnings_anchors$quadratic * (41 + 26)

major_inputs <- merge(major_inputs, earnings_anchors, by = 'webber_category')
major_inputs <- major_inputs[match(
  c('Business', 'Engineering', 'Marketing', 'Finance', 'Accounting',
    'Nursing', 'Economics', 'Education', 'Other STEM', 'All other'),
  major_inputs$major
), ]

al_taxable_income <- function(earnings) {
  pmax(earnings * (1 - federal_tax_deduction_rate) -
         standard_deduction - personal_exemption, 0)
}

discounted_earnings <- function(initial_earnings, linear, quadratic,
                                delta = discount_factor, T = retirement_t) {
  t <- 5:T
  age <- 17 + t
  postgrad_survival <- grad_rate * (1 - annual_leave_rate)^(t - 5)
  earnings <- initial_earnings *
    exp(linear * (age - 22) + quadratic * (age^2 - 22^2))
  sum(postgrad_survival *
        al_taxable_income(earnings) *
        delta^t)
}

major_inputs$pdv_earnings <- mapply(
  discounted_earnings,
  major_inputs$initial_earnings,
  major_inputs$linear,
  major_inputs$quadratic
)

# Fiscal calculations -----------------------------------------------------------

push_effect <- tax_rate * n_in_state * chain_rule_adjustment *
  sum(major_inputs$marginal_effect *
        major_inputs$pdv_earnings *
        major_inputs$in_major_share)

pull_effect_revenue <- tax_rate * pull_effect *
  sum(major_inputs$pdv_earnings * major_inputs$out_major_share)

net_migration_revenue <- push_effect + pull_effect_revenue

annual_cost <- average_cost - oos_net_tuition_fees
discounted_cost <- annual_cost * sum(enrollment_survival * discount_factor^(1:4))

net_fiscal_effect <- net_migration_revenue - discounted_cost

# Output ------------------------------------------------------------------------

major_revenue_components <- data.frame(
  major = major_inputs$major,
  pdv_earnings = major_inputs$pdv_earnings,
  in_major_share = major_inputs$in_major_share,
  out_major_share = major_inputs$out_major_share,
  push_revenue = tax_rate * n_in_state * chain_rule_adjustment *
    major_inputs$marginal_effect * major_inputs$pdv_earnings *
    major_inputs$in_major_share,
  pull_revenue = tax_rate * pull_effect * major_inputs$pdv_earnings *
    major_inputs$out_major_share,
  stringsAsFactors = FALSE
)

fiscal_results <- data.frame(
  component = c(
    'Push effect',
    'Pull effect',
    'Net marginal revenue effect of migration',
    'Annual cost net of OOS tuition/fees',
    'Total discounted cost',
    'Total marginal profit'
  ),
  value = c(
    push_effect,
    pull_effect_revenue,
    net_migration_revenue,
    annual_cost,
    discounted_cost,
    net_fiscal_effect
  ),
  stringsAsFactors = FALSE
)

print(fiscal_results)

grant_aid_inputs <- data.frame(
  component = c(
    'Posted OOS tuition, 2019',
    'Required fees, 2019',
    'Posted OOS tuition and fees, 2019',
    'Posted OOS tuition, 2023 grant-aid baseline',
    'Grant-aid scale factor, 2019/2023',
    'Average OOS automatic merit aid, 2023 main estimate',
    'Average OOS residual tuition grants, 2023 main estimate',
    'Average OOS automatic merit aid, scaled to 2019',
    'Average OOS residual tuition grants, scaled to 2019',
    'Average OOS tuition grants, scaled to 2019',
    'Average OOS net tuition and fees, 2019',
    'Average core cost per FTE, 2019',
    'Average annual margin before migration effects, 2019'
  ),
  value = c(
    posted_oos_tuition,
    required_fees,
    posted_oos_tuition_fees,
    posted_oos_tuition_2023,
    tuition_scale_2019_from_2023,
    avg_oos_auto_merit_2023,
    avg_oos_residual_tuition_grant_2023,
    avg_oos_auto_merit,
    avg_oos_residual_tuition_grant,
    avg_oos_tuition_grant,
    oos_net_tuition_fees,
    average_cost,
    oos_net_tuition_fees - average_cost
  ),
  stringsAsFactors = FALSE
)

print(grant_aid_inputs)

# Internal rate of return -------------------------------------------------------

# IRR is undefined when recruiting costs are zero: there is no initial negative
# cash flow, and every annual cash flow is positive under these assumptions.
annual_earnings <- function(initial_earnings, linear, quadratic,
                            T = retirement_t) {
  t <- 5:T
  age <- 17 + t
  earnings <- initial_earnings *
    exp(linear * (age - 22) + quadratic * (age^2 - 22^2))
  al_taxable_income(earnings)
}

earnings_paths <- mapply(
  annual_earnings,
  major_inputs$initial_earnings,
  major_inputs$linear,
  major_inputs$quadratic
)

t_postgrad <- 5:retirement_t
postgrad_survival <- grad_rate * (1 - annual_leave_rate)^(t_postgrad - 5)

annual_migration_revenue <- postgrad_survival * tax_rate * (
  pull_effect *
    as.numeric(earnings_paths %*% major_inputs$out_major_share) +
    n_in_state * chain_rule_adjustment *
    as.numeric(earnings_paths %*%
                 (major_inputs$marginal_effect * major_inputs$in_major_share))
)

base_cash_flows <- c(
  enrollment_survival * (oos_net_tuition_fees - average_cost),
  annual_migration_revenue
)

npv_at_rate <- function(rate, cash_flows) {
  sum(cash_flows / (1 + rate)^(0:(length(cash_flows) - 1)))
}

irr <- function(cash_flows) {
  if (!any(cash_flows < 0) || !any(cash_flows > 0)) {
    return(NA_real_)
  }
  
  upper <- 1
  while (npv_at_rate(upper, cash_flows) > 0 && upper < 1e10) {
    upper <- upper * 2
  }
  
  if (upper >= 1e10 && npv_at_rate(upper, cash_flows) > 0) {
    return(Inf)
  }
  
  uniroot(
    f = function(rate) npv_at_rate(rate, cash_flows),
    interval = c(-0.999999, upper)
  )$root
}

recruiting_costs <- c(0, 500, 1000, 2500, 5000, 10000, 25000, 50000, 100000)

irr_results <- data.frame(
  recruiting_cost = recruiting_costs,
  irr = sapply(
    recruiting_costs,
    function(cost) irr(c(-cost, base_cash_flows))
  )
)

irr_results$irr_percent <- 100 * irr_results$irr
irr_results$note <- ifelse(
  is.na(irr_results$irr),
  'Undefined: no negative cash flow',
  ''
)

print(irr_results)

# PDV and IRR sensitivity by ACT score ------------------------------------------

award_oos_act <- function(act, gpa = 3.50) {
  
  # Mirrors the 2023 OOS automatic merit schedule used in
  # analysis_grant_aid.R, then scales dollar amounts to the 2019
  # headline tuition level.
  aid <- numeric(length(act))
  
  gpa_300_349 <- gpa >= 3.00 & gpa < 3.50
  gpa_350_up <- gpa >= 3.50
  
  aid[gpa_300_349 & act >= 27 & act < 28] <- 6000
  aid[gpa_300_349 & act >= 28 & act < 30] <- 8000
  aid[gpa_300_349 & act >= 30] <- 15000
  
  aid[gpa_350_up & act >= 25 & act < 27] <- 6000
  aid[gpa_350_up & act >= 27 & act < 28] <- 8000
  aid[gpa_350_up & act >= 28 & act < 29] <- 10000
  aid[gpa_350_up & act >= 29 & act < 30] <- 15000
  aid[gpa_350_up & act >= 30 & act < 32] <- 24000
  aid[gpa_350_up & act >= 32] <- 28000
  
  aid * tuition_scale_2019_from_2023
  
}

oos_net_tuition_fees_by_act <- function(act, gpa = 3.50) {
  pmax(
    posted_oos_tuition_fees -
      award_oos_act(act, gpa = gpa) -
      avg_oos_residual_tuition_grant,
    0
  )
}

act_grid <- 18:36

pdv_by_act <- data.frame(act = act_grid) %>%
  mutate(
    net_oos_tuition_fees = oos_net_tuition_fees_by_act(act),
    discounted_tuition_margin =
      (net_oos_tuition_fees - average_cost) *
      sum(enrollment_survival * discount_factor^(1:4)),
    state_pdv = net_migration_revenue + discounted_tuition_margin
  )

print(pdv_by_act)

pdv_plot <- ggplot(pdv_by_act, aes(x = act, y = state_pdv)) +
  geom_hline(yintercept = 0, color = 'gray40') +
  geom_line(color = '#440154FF', linewidth = 1) +
  scale_x_continuous(breaks = seq(18, 36, by = 3)) +
  scale_y_continuous(labels = scales::dollar) +
  labs(x = 'ACT score',
       y = 'PDV to state, zero recruitment cost') +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = 'gray80', linetype = 'dashed'),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0))

ggsave(paste0(pathFigures, 'pdv_by_act_score.png'), width = 6.5, height = 5)

act_irr_scenarios <- data.frame(
  scenario = c('ACT <= 24', 'ACT 26', 'ACT 28', 'ACT 30'),
  act = c(18, 26, 28, 30),
  stringsAsFactors = FALSE
)

irr_cash_flow_summaries <- lapply(seq_len(nrow(act_irr_scenarios)), function(i) {
  
  net_oos_tuition_fees_alt <-
    oos_net_tuition_fees_by_act(act_irr_scenarios$act[i])
  cash_flows_alt <- c(
    enrollment_survival * (net_oos_tuition_fees_alt - average_cost),
    annual_migration_revenue
  )
  
  data.frame(
    scenario = act_irr_scenarios$scenario[i],
    act = act_irr_scenarios$act[i],
    net_oos_tuition_fees = net_oos_tuition_fees_alt,
    zero_irr_recruiting_cost = sum(cash_flows_alt),
    stringsAsFactors = FALSE
  )
  
}) %>%
  bind_rows()

max_plot_recruiting_cost <- ceiling(
  1.20 * max(irr_cash_flow_summaries$zero_irr_recruiting_cost[
    irr_cash_flow_summaries$zero_irr_recruiting_cost > 0
  ]) / 5000
) * 5000

plot_recruiting_costs <- seq(
  from = 1,
  to = max_plot_recruiting_cost,
  length.out = 500
)

irr_plot_data <- lapply(seq_len(nrow(act_irr_scenarios)), function(i) {
  
  net_oos_tuition_fees_alt <-
    oos_net_tuition_fees_by_act(act_irr_scenarios$act[i])
  cash_flows_alt <- c(
    enrollment_survival * (net_oos_tuition_fees_alt - average_cost),
    annual_migration_revenue
  )
  
  data.frame(
    scenario = act_irr_scenarios$scenario[i],
    act = act_irr_scenarios$act[i],
    net_oos_tuition_fees = net_oos_tuition_fees_alt,
    recruiting_cost = plot_recruiting_costs,
    irr_percent = 100 * sapply(
      plot_recruiting_costs,
      function(cost) irr(c(-cost, cash_flows_alt))
    )
  )
  
}) %>%
  bind_rows() %>%
  mutate(scenario = factor(scenario, levels = act_irr_scenarios$scenario))

min_plot_irr <- floor(min(irr_plot_data$irr_percent, na.rm = TRUE) / 5) * 5 - 3

print(irr_cash_flow_summaries)

irr_plot <- ggplot(irr_plot_data,
                   aes(x = recruiting_cost,
                       y = irr_percent,
                       color = scenario)) +
  geom_hline(yintercept = 0, color = 'gray40') +
  geom_line(linewidth = 1) +
  coord_cartesian(xlim = c(0, max_plot_recruiting_cost),
                  ylim = c(min_plot_irr, 100),
                  expand = FALSE) +
  scale_x_continuous(breaks = seq(0, max_plot_recruiting_cost, by = 10000),
                     labels = function(x) x / 1000) +
  scale_y_continuous(breaks = seq(0, 100, by = 20), labels = function(x) paste0(x, '%')) +
  scale_color_manual(
    values = c('ACT <= 24' = '#440154FF',
               'ACT 26' = '#21908CFF',
               'ACT 28' = '#FDE725FF',
               'ACT 30' = 'grey50'),
    name = 'ACT score'
  ) +
  labs(x = "Recruitment cost per OOS student ($1000s)",
       y = 'Internal rate of return') +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = 'gray80', linetype = 'dashed'),
        legend.position = 'right',
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0))

ggsave(paste0(pathFigures, 'irr_by_act_score.png'), width = 6.5, height = 5)