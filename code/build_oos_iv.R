#////////////////////////////////////////////////////////////////////////////////
# Filename: build_oos_iv.R
# Author: Ryan Haygood
# Date: 11/13/24
# Description: Evaluates changes in the selectivity of origin-state flagships as
# an IV for UA's OOS enrollment.
#////////////////////////////////////////////////////////////////////////////////

# Setup
source('C:/Users/ryanh/OneDrive/Documents/Grad School/Research/Out-of-State-Enrollment/code/setup.R')

# Read in cleaned IPEDS panel
ipeds <- readRDS(paste0(pathHome, 'data/ipeds_panel.rds'))

# Read in enrollment by state-of-origin panel
ef <- readRDS(paste0(pathHome, 'data/ef_by_state_panel.rds'))

# Do changes in other state flagships' selectivity drive students to UA?

# Get each state's flagship selectivity over time
flag_adm <- ipeds %>%
  filter(flagship) %>%
  group_by(STABBR, y) %>%
  summarize(adm_rate = 100 * mean(adm_rate, na.rm = T))

# Plot trends in selectivity over time by state
ggplot(flag_adm, aes(x = y, y = adm_rate)) +
  geom_line() +
  facet_wrap(~ STABBR)

# Predict share of students coming to UA from each state based on selectivity
# of that state's flagship(s)
stabbr_cw <- ipeds %>%
  filter(!is.na(FIPS) & FIPS > 0 & FIPS <= 56) %>%
  select(FIPS, STABBR) %>%
  distinct()

# Get shares of students from other states going to UA in each year
oos_shares <- ef %>%
  # UA only
  filter(UNITID == 100751) %>%
  # Join on state names
  rename(FIPS = LINE,
         enroll = EFRES01) %>%
  left_join(stabbr_cw) %>%
  filter(!is.na(STABBR))

# Balance the panel
oos_shares_bal <- data.frame(y = rep(2000:2019, n = length(unique(oos_shares$STABBR))),
                             STABBR = rep(unique(oos_shares$STABBR), each = length(2000:2019))) %>%
  left_join(oos_shares) %>%
  select(-c('FIPS', 'UNITID')) %>%
  mutate(enroll = if_else(is.na(enroll), 0, enroll)) %>%
  left_join(flag_adm)

# Regress OOS enrollment on selectivity of in-state flagship
felm(enroll ~ adm_rate | factor(STABBR), data = oos_shares_bal %>% filter(STABBR != 'AL')) %>%
  summary(robust = T)
felm(enroll ~ adm_rate | factor(STABBR) + factor(y), data = oos_shares_bal %>% filter(STABBR != 'AL')) %>%
  summary(robust = T)

# Now try trimming outlier admission rates and states with very little UA enrollment ever

# Remove NA admissions rates
oos_shares_bal <- oos_shares_bal %>%
  filter(!is.na(adm_rate))

# Save for analysis file
saveRDS(oos_shares_bal, paste0(pathHome, 'data/selectivity_iv.rds'))

# Remove Alabama
oos_shares_bal <- oos_shares_bal %>%
  filter(STABBR != 'AL')

# Project out state and year FEs
adm_rate_res <- felm(adm_rate ~ factor(STABBR) + factor(y), data = oos_shares_bal)$resid
enroll_res <- felm(enroll ~ factor(STABBR) + factor(y), data = oos_shares_bal)$resid

# Identify outliers
oos_shares_bal <- oos_shares_bal %>%
  mutate(adm_rate_res = adm_rate_res,
         enroll_res = enroll_res) %>%
  # Identify outlier residual admission rates
  mutate(outlier = abs(adm_rate_res) > 25) %>%
  # Identify states with essentially zero UA enrollment ever
  group_by(STABBR) %>%
  mutate(rare_enroll = max(enroll, na.rm = T) < 10)

# Removing outlier admission rates
felm(enroll ~ adm_rate | factor(STABBR) + factor(y), data = filter(oos_shares_bal, !outlier)) %>%
  summary(robust = T)
# Also removing states with almost no UA enrollment
felm(enroll ~ adm_rate | factor(STABBR) + factor(y), data = filter(oos_shares_bal, !outlier & !rare_enroll)) %>%
  summary(robust = T)

# Plot residual enrollment vs. admission rates
ggplot(filter(oos_shares_bal, !outlier & !rare_enroll), aes(x = adm_rate_res, y = enroll_res)) +
  geom_point(alpha = 0.4) +
  stat_smooth(method = 'lm') +
  labs(x = 'Residual home-state admission rate (%)',
       y = '# enrolling in UA (residualized)',
       title = 'First stage: home-state selectivity predicts UA enrollment',
       caption = 'Note: State/year-level data from 2000-2019. Both variables residualized on state and year FEs.\nTrimming outliers with admission rate residuals exceeding +/-25pp. Slope equal to -0.95,\nF-statistic of 17.13.') +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = 'gray80', linetype = 'dashed'),
        legend.position = 'right',
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0))
ggsave(paste0(pathFigures, 'selecivity_first_stage.png'), width = 6, height = 6)

# May need to cluster by state, which reduces first-stage strength quite a bit...

# Test exclusion restriction on labor market conditions -------------------------

# Changes in home-state flagship selectivity shouldn't predict other factors
# influencing students' location choices.
# Key among these will be state labor market conditions, like the unemployment rate.

# Read in unemployment rates by state from BLS, 2004-2023
# Derived from FRED: https://fredaccount.stlouisfed.org/datalists/250706
bls <- read_xls(paste0(pathHome, 'data/BLS/State_Unemployment_Rates.xls'), sheet = 2)

# At monthly level; get annual average
bls <- bls %>%
  pivot_longer(cols = -any_of('DATE')) %>%
  mutate(STABBR = substr(name, 1, 2),
         ur = value,
         y = as.numeric(substr(DATE, 1, 4))) %>%
  group_by(STABBR, y) %>%
  summarize(ur = mean(ur)) %>%
  filter(y %in% 2004:2023) %>%
  # Set year back four years before merge, so regression will be of future unemployment
  # on current selectivity
  mutate(y = y - 4)

# Join state flagship selectivity to unemployment rate four years into the future
oos_shares_bal <- oos_shares_bal %>%
  left_join(bls)

# Insert future unemployment rate into the LHS; run on each of the two samples
felm(ur ~ adm_rate | factor(STABBR) + factor(y), data = filter(oos_shares_bal, !outlier)) %>%
  summary(robust = T)
felm(ur ~ adm_rate | factor(STABBR) + factor(y), data = filter(oos_shares_bal, !outlier & !rare_enroll)) %>%
  summary(robust = T)
# Statistically significant, but economically small

# Now try controlling for UR in the first stage

# Removing outlier admission rates
felm(enroll ~ adm_rate + ur | factor(STABBR) + factor(y), data = filter(oos_shares_bal, !outlier)) %>%
  summary(robust = T)
# Also removing states with almost no UA enrollment
felm(enroll ~ adm_rate + ur | factor(STABBR) + factor(y), data = filter(oos_shares_bal, !outlier & !rare_enroll)) %>%
  summary(robust = T)
# Thankfully this doesn't impact the first stage
