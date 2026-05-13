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

lambda <- 1
lambda_values <- c(1, 2, 3)
aid_year <- 2019
ua_fips <- 1
ef_res <- read.csv(paste0(pathHome, 'data/ipeds_fe/ef', aid_year, 'c.csv')) %>%
  rename_with(toupper) %>%
  filter(UNITID == 100751)
P_R <- read.csv(paste0(pathHome, 'data/ipeds_tuition/ic', aid_year, '_ay.csv')) %>%
  rename_with(toupper) %>%
  filter(UNITID == 100751) %>%
  pull(TUITION2) %>%
  as.numeric()
P_O <- read.csv(paste0(pathHome, 'data/ipeds_tuition/ic', aid_year, '_ay.csv')) %>%
  rename_with(toupper) %>%
  filter(UNITID == 100751) %>%
  pull(TUITION3) %>%
  as.numeric()
N_R <- 4 * ef_res$EFRES01[ef_res$LINE == ua_fips]
N_O <- 4 * (ef_res$EFRES01[ef_res$LINE == 99] -
              ef_res$EFRES01[ef_res$LINE == ua_fips] -
              sum(ef_res$EFRES01[ef_res$LINE %in% c(90, 98)], na.rm = TRUE))
fin <- read.csv(paste0(pathHome, 'data/ipeds_finance/f', substr(aid_year, 3, 4),
                       substr(aid_year + 1, 3, 4), '_f1a_rv.csv')) %>%
  rename_with(toupper) %>%
  filter(UNITID == 100751)
A <- as.numeric(fin$F1E08)
allocate_oos_grant <- function(lambda) {
  G_R <- A / (N_R + lambda * N_O)
  lambda * G_R
}
G_O <- allocate_oos_grant(lambda)
oos_tuition <- P_O - G_O
fte <- read_xlsx(paste0(pathHome, 'data/factbook/fte_by_college_in_out.xlsx')) %>%
  rename(origin = Origin, college = `By College/School`, y = STYEAR, fte = sum_fte) %>%
  mutate(y = as.numeric(y), fte = as.numeric(fte))
average_cost <- (fin$F1C011 + fin$F1C051 + fin$F1C061) /
  sum(fte$fte[fte$y == aid_year], na.rm = TRUE)

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

annual_cost <- average_cost - oos_tuition
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
    'Annual cost',
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

lambda_sensitivity <- data.frame(
  lambda = lambda_values,
  avg_oos_grant_aid = sapply(lambda_values, allocate_oos_grant)
) %>%
  mutate(net_oos_tuition = P_O - avg_oos_grant_aid,
         annual_cost = average_cost - net_oos_tuition,
         total_discounted_cost = annual_cost *
           sum(enrollment_survival * discount_factor^(1:4)),
         net_migration_revenue = net_migration_revenue,
         total_marginal_profit = net_migration_revenue - total_discounted_cost)

print(lambda_sensitivity)

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
  enrollment_survival * (oos_tuition - average_cost),
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

lambda_irr_sensitivity <- lapply(lambda_values, function(lambda_alt) {
  
  grant_alt <- allocate_oos_grant(lambda_alt)
  net_oos_tuition_alt <- P_O - grant_alt
  cash_flows_alt <- c(
    enrollment_survival * (net_oos_tuition_alt - average_cost),
    annual_migration_revenue
  )
  
  data.frame(
    lambda = lambda_alt,
    recruiting_cost = recruiting_costs,
    irr = sapply(
      recruiting_costs,
      function(cost) irr(c(-cost, cash_flows_alt))
    )
  )
  
}) %>%
  bind_rows() %>%
  mutate(irr_percent = 100 * irr,
         note = ifelse(is.na(irr), 'Undefined: no negative cash flow', ''))

print(lambda_irr_sensitivity)

# Plot IRR sensitivity ----------------------------------------------------------

max_plot_recruiting_cost <- 20000

plot_recruiting_costs <- seq(
  from = 1,
  to = max_plot_recruiting_cost,
  length.out = 500
)

plot_irr <- sapply(
  plot_recruiting_costs,
  function(cost) irr(c(-cost, base_cash_flows))
)

irr_plot_data <- data.frame(
  recruiting_cost = plot_recruiting_costs,
  irr_percent = 100 * plot_irr
)

min_plot_irr <- floor(min(irr_plot_data$irr_percent, na.rm = TRUE) / 5) * 5

ggplot(irr_plot_data, aes(x = recruiting_cost, y = irr_percent)) +
  geom_hline(yintercept = 0, color = 'gray40') +
  geom_line(color = '#440154FF', linewidth = 1) +
  coord_cartesian(xlim = c(0, max_plot_recruiting_cost),
                  ylim = c(min_plot_irr, 100),
                  expand = FALSE) +
  scale_x_continuous(breaks = seq(0, max_plot_recruiting_cost, by = 10000),
                     labels = function(x) x / 1000) +
  scale_y_continuous(breaks = seq(0, 100, by = 20), labels = function(x) paste0(x, '%')) +
  labs(#title = 'IRR by Assumed Recruiting Cost',
       x = "Recruitment cost per OOS student ($1000s)",
       y = 'Internal rate of return') +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = 'gray80', linetype = 'dashed'),
        legend.position = 'right',
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0))

ggsave(paste0(pathFigures, 'irr_by_recruiting_cost.png'), width = 5.5, height = 5)

# OOS grant-aid estimates over time --------------------------------------------

read_ipeds_tuition <- function(y) {
  
  if (y %in% 2009:2012) {
    read_xlsx(paste0(pathHome, 'data/ipeds_tuition/ic', y, '_ay.xlsx'))
  } else {
    read.csv(paste0(pathHome, 'data/ipeds_tuition/ic', y, '_ay.csv'))
  }
  
}

aid_allocation_by_year <- lapply(2001:2019, function(y) {
  
  ef_y <- read.csv(paste0(pathHome, 'data/ipeds_fe/ef', y, 'c.csv')) %>%
    rename_with(toupper) %>%
    filter(UNITID == 100751)
  
  tuition_y <- read_ipeds_tuition(y) %>%
    rename_with(toupper) %>%
    filter(UNITID == 100751)
  
  fin_y <- read.csv(paste0(pathHome, 'data/ipeds_finance/f',
                           substr(y, 3, 4), substr(y + 1, 3, 4),
                           if_else(y <= 2002, '_f1a.csv', '_f1a_rv.csv'))) %>%
    rename_with(toupper) %>%
    filter(UNITID == 100751)
  
  N_R_y <- 4 * ef_y$EFRES01[ef_y$LINE == ua_fips]
  N_O_y <- 4 * (ef_y$EFRES01[ef_y$LINE == 99] -
                  ef_y$EFRES01[ef_y$LINE == ua_fips] -
                  sum(ef_y$EFRES01[ef_y$LINE %in% c(90, 98)], na.rm = TRUE))
  P_O_y <- as.numeric(tuition_y$TUITION3)
  A_y <- as.numeric(fin_y$F1E08)
  G_O_y <- sapply(lambda_values, function(lambda_alt) {
    lambda_alt * A_y / (N_R_y + lambda_alt * N_O_y)
  })
  
  data.frame(
    y = y,
    lambda = lambda_values,
    N_R = N_R_y,
    N_O = N_O_y,
    posted_oos_tuition = P_O_y,
    institutional_grant_aid = A_y,
    avg_oos_grant_aid = G_O_y,
    avg_oos_grant_aid_share = G_O_y / P_O_y
  )
  
}) %>%
  bind_rows()

print(aid_allocation_by_year)

# SHEF-calibrated OOS net tuition benchmark ------------------------------------

### CAREFUL: It looks like SHEF actually only obtained an "estimate" from Alabama,
### which seems to assume equal discounts for in- and out-of-state students...

# SHEF estimates average net price after grants paid by all OOS students at
# Alabama public four-year colleges as $23,151 in 2020. SHEF appears to include
# graduate students; the IPEDS residence weights below use available undergraduate
# residence counts, so treat this as a benchmark rather than the baseline.
shef_oos_net_price <- 23151
shef_year <- 2020
ipeds_shef_year <- 2019

al_public4 <- read.csv(paste0(pathHome, 'data/ipeds_ins/hd',
                              ipeds_shef_year, '.csv')) %>%
  rename_with(toupper) %>%
  filter(STABBR == 'AL',
         CONTROL == 1,
         ICLEVEL == 1,
         SECTOR == 1) %>%
  select(UNITID, INSTNM)

tuition_shef <- read_ipeds_tuition(ipeds_shef_year) %>%
  rename_with(toupper) %>%
  filter(UNITID %in% al_public4$UNITID) %>%
  transmute(UNITID, posted_oos_tuition = as.numeric(TUITION3))

ef_shef <- read.csv(paste0(pathHome, 'data/ipeds_fe/ef',
                           ipeds_shef_year, 'c.csv')) %>%
  rename_with(toupper) %>%
  filter(UNITID %in% al_public4$UNITID)

oos_counts_shef <- ef_shef %>%
  group_by(UNITID) %>%
  summarize(
    total = sum(EFRES01[LINE == 99], na.rm = TRUE),
    resident = sum(EFRES01[LINE == ua_fips], na.rm = TRUE),
    foreign_or_unknown = sum(EFRES01[LINE %in% c(90, 98)], na.rm = TRUE),
    oos_enrollment = total - resident - foreign_or_unknown,
    .groups = 'drop'
  )

shef_calibration_institutions <- al_public4 %>%
  left_join(tuition_shef, by = 'UNITID') %>%
  left_join(oos_counts_shef, by = 'UNITID') %>%
  filter(!is.na(posted_oos_tuition),
         !is.na(oos_enrollment),
         oos_enrollment > 0) %>%
  mutate(weight = oos_enrollment / sum(oos_enrollment))

weighted_posted_oos_tuition <- sum(shef_calibration_institutions$weight *
                                     shef_calibration_institutions$posted_oos_tuition)
common_oos_discount_rate <- 1 - shef_oos_net_price / weighted_posted_oos_tuition
ua_posted_oos_tuition_shef <- shef_calibration_institutions$posted_oos_tuition[
  shef_calibration_institutions$UNITID == 100751
]

shef_common_discount_summary <- data.frame(
  shef_year = shef_year,
  ipeds_year = ipeds_shef_year,
  shef_oos_net_price = shef_oos_net_price,
  weighted_posted_oos_tuition = weighted_posted_oos_tuition,
  common_oos_discount_rate = common_oos_discount_rate,
  ua_posted_oos_tuition = ua_posted_oos_tuition_shef,
  ua_shef_net_price = ua_posted_oos_tuition_shef * (1 - common_oos_discount_rate),
  ua_shef_grant_aid = ua_posted_oos_tuition_shef * common_oos_discount_rate
)

ua_discount_multipliers <- c(0.5, 0.75, 1, 1.25, 1.5, 2)

shef_ua_discount_sensitivity <- lapply(ua_discount_multipliers, function(kappa_ua) {
  
  discount_denom <- sum(
    shef_calibration_institutions$weight *
      shef_calibration_institutions$posted_oos_tuition *
      if_else(shef_calibration_institutions$UNITID == 100751, kappa_ua, 1)
  )
  other_discount_rate <- (weighted_posted_oos_tuition - shef_oos_net_price) /
    discount_denom
  ua_discount_rate <- kappa_ua * other_discount_rate
  
  data.frame(
    ua_discount_multiplier = kappa_ua,
    other_discount_rate = other_discount_rate,
    ua_discount_rate = ua_discount_rate,
    ua_shef_grant_aid = ua_posted_oos_tuition_shef * ua_discount_rate,
    ua_shef_net_price = ua_posted_oos_tuition_shef * (1 - ua_discount_rate)
  )
  
}) %>%
  bind_rows()

print(shef_calibration_institutions)
print(shef_common_discount_summary)
print(shef_ua_discount_sensitivity)
