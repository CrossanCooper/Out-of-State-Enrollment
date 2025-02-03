#////////////////////////////////////////////////////////////////////////////////
# Filename: analysis_main.R
# Author: Ryan Haygood
# Date: 1/31/25
# Description: Runs main regression specifications for evaluating pull effects of
# out-of-state students.
#////////////////////////////////////////////////////////////////////////////////

# Setup
source('C:/Users/ryanh/OneDrive/Documents/Grad School/Research/Out-of-State-Enrollment/code/setup.R')

# Build data --------------------------------------------------------------------

# Read cleaned first-job data
dest <- readRDS(paste0(pathHome, 'revelio_data/first_spell_join.rds'))

# Read origin data
origin <- read.csv(paste0(pathHome, 'data/linked_commencement_revelio_profile_data.csv'))

# Make state abbreviation/name crosswalk
states <- data.frame(originState = state.abb,
                     state = state.name) %>%
  bind_rows(data.frame(originState = 'DC', state = 'Washington, D.C.'))

# Clean up states of origin
origin <- origin %>%
  # Remove students with missing graduation years
  filter(!is.na(Year)) %>%
  mutate(originState = gsub(' ', '', originState)) %>%
  # State will be NA for origins outside the US
  left_join(states) %>%
  # Filter to US origins for now
  filter(!is.na(state)) %>%
  rename(grad_y = Year)

# Filter to US destinations (50 states + DC)
dest <- dest %>%
  filter(country == 'United States')

# Define origin shares using Commencement data
shares <- origin %>%
  group_by(grad_y) %>%
  mutate(N_cohort = n()) %>%
  group_by(grad_y, state) %>%
  summarize(o_share = n() / mean(N_cohort),
            N_origin = n())

# Add in destination shares
shares <- dest %>%
  group_by(grad_y) %>%
  mutate(N_cohort = n()) %>%
  group_by(grad_y, state) %>%
  summarize(d_share = n() / mean(N_cohort),
            N_dest = n()) %>%
  full_join(shares) %>%
  # Impute zeroes
  # Multiple origin share by 100
  mutate(o_share = if_else(is.na(o_share), 0, o_share) * 100,
         d_share = if_else(is.na(d_share), 0, d_share)) %>%
  # Remove zero-share destinations (can't take log)
  filter(d_share > 0) %>%
  # Get log destination shares
  mutate(log_d_share = log(d_share)) %>%
  # Remove 2024 (incomplete data)
  filter(grad_y < 2024)

# Get log Alabama shares (outside good)
shares <- shares %>%
  filter(state == 'Alabama') %>%
  select(log_d_share, grad_y) %>%
  rename(log_al_share = log_d_share) %>%
  right_join(shares) %>%
  # Get difference in log shares (relative to outside good)
  mutate(diff_log_d_share = log_d_share - log_al_share)

# Join origin and destination data
# Used for conditioning on origin, as well as full conditional logit model
join <- dest %>%
  select(-c('first_name', 'last_name', 'grad_y', 'fullname', 'field', 'user_location')) %>%
  # Get destination state
  rename(d_state = state) %>%
  # Only keep those showing up in both
  inner_join(origin) %>%
  # Get origin state
  rename(o_state = state) %>%
  filter(grad_y < 2024)

# Build in-state student share data
# Get destination shares of in-state students
share_ins <- join %>%
  filter(o_state == 'Alabama') %>%
  group_by(grad_y) %>%
  mutate(N_cohort = n()) %>%
  group_by(grad_y, d_state, N_cohort) %>%
  summarize(d_share = n() / mean(N_cohort),
            N_dest = n()) %>%
  # Remove zero-share destinations (can't take log)
  filter(d_share > 0) %>%
  # Get log destination shares
  mutate(log_d_share = log(d_share)) %>%
  # Remove 2024 (incomplete data)
  filter(grad_y < 2024)

# Attach on the old origin shares (want to use full sample here)
share_ins <- shares %>%
  filter(state != 'Alabama') %>%
  rename(d_state = state) %>%
  select(d_state, grad_y, o_share, N_origin) %>%
  right_join(share_ins) %>%
  # Impute zero origin students
  mutate(N_origin = if_else(is.na(N_origin), 0, N_origin))

# Get log Alabama shares (outside good)
share_ins <- share_ins %>%
  filter(d_state == 'Alabama') %>%
  ungroup() %>%
  select(log_d_share, grad_y) %>%
  rename(log_al_share = log_d_share) %>%
  right_join(share_ins) %>%
  # Remove the outside good
  filter(d_state != 'Alabama') %>%
  # Get difference in log shares (relative to outside good)
  mutate(diff_log_d_share = log_d_share - log_al_share)

# Sample descriptives -----------------------------------------------------------

# Compare Commencement origin shares to IPEDS origin shares

# Bring in IPEDS shares (along with selectivity IV)
iv <- readRDS(paste0(pathHome, 'data/selectivity_iv.rds')) %>%
  left_join(rename(states, STABBR = originState)) %>%
  # Translate year of enrollment to likely graduation year for that cohort
  mutate(grad_y = y + 4)

# Get IPEDS origin shares
ipeds_shares <- iv %>%
  group_by(grad_y) %>%
  mutate(N_cohort = sum(enroll)) %>%
  mutate(o_share_ipeds = enroll / N_cohort * 100) %>%
  select(state, grad_y, o_share_ipeds)

# Attach to shares dataframe
shares <- shares %>%
  left_join(ipeds_shares)

# Plot
shares %>%
  mutate(source = if_else(state == 'Alabama', 'Alabama', 'Other')) %>%
  ggplot(aes(x = o_share_ipeds, y = o_share, col = source)) +
  geom_point() +
  geom_abline(aes(slope = 1, intercept = 0), lty = 2, lwd = 1) +
  scale_color_manual(values = c('indianred3', 'steelblue3')) +
  facet_wrap(~ source, scales = 'free') +
  guides(col = 'none') +
  labs(x = 'Source: IPEDS',
       y = 'Source: Commencement records',
       title = 'State-of-origin shares') +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = 'gray80', linetype = 'dashed'),
        legend.position = 'right',
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0))
ggsave(paste0(pathFigures, 'analysis_main/ipeds_vs_comm_shares.png'), width = 9, height = 5)

# Now plot the joined sample size by cohort and OOS/in-state
join %>%
  mutate(oos = if_else(o_state == 'Alabama', 'In-state', 'Out-of-state')) %>%
  group_by(grad_y, oos) %>%
  summarize(N = n()) %>%
  ggplot(aes(x = grad_y, y = N, col = oos)) +
  geom_line(lwd = 1) +
  scale_color_manual(values = c('indianred3', 'steelblue3')) +
  labs(x = 'Graduation year',
       title = 'Sample size, merged Commencement/Revelio data',
       col = NULL) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = 'gray80', linetype = 'dashed'),
        legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5),
        axis.title.y = element_blank(),
        plot.caption = element_text(hjust = 0))
ggsave(paste0(pathFigures, 'analysis_main/merge_sample_size.png'), width = 6, height = 5)

# Share regressions -------------------------------------------------------------

# Simple spec: raw # of students to/from the state
felm(N_dest ~ N_origin | factor(d_state) | 0 | d_state, data = share_ins) %>%
  summary(robust = T)
# Now including cohort effects
felm(N_dest ~ N_origin | factor(d_state) + factor(grad_y) | 0 | d_state, data = share_ins) %>%
  summary(robust = T)

# Plain logit OLS (full sample, unconditional)

# Base specification (unweighted)
felm(diff_log_d_share ~ o_share | factor(state) | 0 | state, data = shares %>% filter(state != 'Alabama')) %>%
  summary(robust = T)

# Filtering to origins with more than 10 students in that cohort
felm(diff_log_d_share ~ o_share | factor(state) | 0 | state, data = shares %>% filter(state != 'Alabama' & N_origin >= 10)) %>%
  summary(robust = T)

# Get weights: average # of students going to that destination state
shares <- shares %>%
  group_by(state) %>%
  mutate(avg_N_dest = mean(N_dest))
# Weighted
felm(diff_log_d_share ~ o_share | factor(state) | 0 | state, data = shares %>% filter(state != 'Alabama'), weights = shares$avg_N_dest[shares$state != 'Alabama']) %>%
  summary(robust = T)

# Plain logit OLS (conditional on in-state students)

# Base specification (unweighted)
felm(diff_log_d_share ~ o_share | factor(d_state) | 0 | d_state, data = share_ins) %>%
  summary(robust = T)

# Including linear trend
felm(diff_log_d_share ~ o_share + grad_y | factor(d_state) | 0 | d_state, data = share_ins) %>%
  summary(robust = T)

# Including cohort FE
felm(diff_log_d_share ~ o_share | factor(d_state) + factor(grad_y) | 0 | d_state, data = share_ins) %>%
  summary(robust = T)

# Compute marginal effect of a 1pp increase in out-of-state share
# Use effect from linear-trend OLS specification
gamma <- 0.086
# Get overall choice probabilities
s <- join %>%
  # In-state students only
  filter(o_state == 'Alabama') %>%
  ungroup() %>%
  mutate(N = n()) %>%
  group_by(d_state) %>%
  summarize(d_share = n() / mean(N)) %>%
  filter(d_state != 'Alabama') %>%
  pull(d_share)
# Get overall origin shares
d <- join %>%
  ungroup() %>%
  mutate(N = n()) %>%
  group_by(o_state) %>%
  summarize(o_share = n() / mean(N)) %>%
  filter(o_state != 'Alabama') %>%
  pull(o_share)
# Get each origin state's share of the 1pp increase in OOS share (proportional to
# base share)
weights <- d / sum(d)
marg_effect <- gamma * (weights %*% (s * (1 - sum(s))))

# IV share regressions ----------------------------------------------------------

# Now attach OOS selectivity instrument
shares <- iv %>%
  select(grad_y, state, adm_rate) %>%
  right_join(shares)
# Also to in-state share data
share_ins <- iv %>%
  select(grad_y, state, adm_rate) %>%
  rename(d_state = state) %>%
  right_join(share_ins)

# Simple spec: raw # of students to/from the state

# Robust SE, not clustered
# First stage
felm(N_origin ~ adm_rate | factor(d_state), data = share_ins) %>%
  summary(robust = T)
# IV
felm(N_dest ~ 0 | factor(d_state) | (N_origin ~ adm_rate), data = share_ins) %>%
  summary(robust = T)

# Robust, clustered SE
# First stage
felm(N_origin ~ adm_rate | factor(d_state) | 0 | d_state, data = share_ins) %>%
  summary(robust = T)
# IV
felm(N_dest ~ 0 | factor(d_state) | (N_origin ~ adm_rate) | d_state, data = share_ins) %>%
  summary(robust = T)

# Plain logit IV

# Pooling all origins
# First stage
felm(o_share ~ adm_rate | factor(state) | 0 | state, data = shares %>% filter(state != 'Alabama')) %>%
  summary(robust = T)
# IV
felm(diff_log_d_share ~ 0 | factor(state) | (o_share ~ adm_rate) | state, data = shares %>% filter(state != 'Alabama')) %>%
  summary(robust = T)

# Conditioning on in-state students
# First stage
felm(o_share ~ adm_rate + grad_y | factor(d_state) | 0 | d_state, data = share_ins) %>%
  summary(robust = T)
# IV
felm(diff_log_d_share ~ 0 | factor(d_state) | (o_share ~ adm_rate) | d_state, data = share_ins) %>%
  summary(robust = T)

# Save to Stata for weak IV tests
write_dta(share_ins, paste0(pathHome, 'data/share_ins.dta'))

# Estimate full model -----------------------------------------------------------

# Get individual/product-level dataframe
join_bal <- data.frame(user_id = rep(unique(join$user_id), n = length(unique(join$d_state))),
                       alternative = rep(unique(join$d_state), each = length(unique(join$user_id)))) %>%
  left_join(join) %>%
  # Remove 2024
  filter(grad_y < 2024) %>%
  # Restrict to necessary variables
  select(user_id, alternative, o_state, d_state, grad_y) %>%
  # Generate indicator for whether the destination is your home state
  mutate(home_state = o_state == alternative,
         # Generate choice indicator
         choice = d_state == alternative,
         # Generate out-of-state indicator
         oos = o_state != 'Alabama') %>%
  # Join on origin-share data
  left_join(rename(shares, alternative = state)) %>%
  # Impute zero origin shares where missing
  mutate(o_share = if_else(is.na(o_share), 0, o_share)) %>%
  # Fix the Alabama origin share to zero
  mutate(o_share = if_else(alternative == 'Alabama', 0, o_share)) %>%
  # Manually generate interactions with OOS indicator
  mutate(home_state_oos = home_state * oos,
         home_state_ins = home_state * !oos,
         o_share_oos = o_share * oos,
         o_share_ins = o_share * !oos,
         o_share_oos = o_share * oos,
         o_share_ins = o_share * !oos) %>%
  # Get grad_y interacted with Alabama (for a trend in Alabama's mean utility)
  mutate(t_AL = (grad_y - 2006) * (alternative == 'Alabama')) %>%
  # Interact this with OOS
  mutate(t_AL_oos = t_AL * oos,
         t_AL_ins = t_AL * !oos) %>%
  # Arrange dataframe
  arrange(user_id, alternative)

# Multinomial logit, no heterogeneity by in-state
mod0 <- mlogit(choice ~ home_state + o_share, data = join_bal)

# Multinomial logit, no heterogeneity by in-state, with a trend
mod1 <- mlogit(choice ~ home_state + o_share + t_AL, data = join_bal)

# Multinomial logit, heterogeneity but no trend in Alabama utility
mod2 <- mlogit(choice ~ home_state_oos + home_state_ins + o_share_oos + o_share_ins, data = join_bal)

# Multinomial logit, heterogeneity and trends in Alabama utilities
mod3 <- mlogit(choice ~ home_state_oos + home_state_ins + o_share_oos + o_share_ins + t_AL_oos + t_AL_ins, data = join_bal)

# Multinomial logit, heterogeneity and cohort effects on Alabama utilities
mod4 <- mlogit(choice ~ home_state_oos + home_state_ins + o_share_oos + o_share_ins + t_AL_oos + t_AL_ins, data = join_bal %>% mutate(t_AL_oos = factor(t_AL_oos), t_AL_ins = factor(t_AL_ins)))

# Multinomial logit, heterogeneity and trends, with a RC on home-state preference
mod5 <- mlogit(choice ~ home_state + o_share_oos + o_share_ins + t_AL_oos + t_AL_ins, data = join_bal %>% mutate(home_state = as.numeric(home_state)), rpar = c(home_state = 'n'))

# Get estimates
# Need to figure out robust/clustered SEs
summary(mod4)

# Output table
stargazer(mod0, mod1, mod2, mod3, mod4, omit = c('Intercept'))

# Get marginal effect of a 1pp increase in the out-of-state share
# Get weights
weights <- join %>%
  group_by(o_state) %>%
  summarize(N = n()) %>%
  filter(o_state != 'Alabama') %>%
  pull(N)
# Get average marginal effect
me0 <- (weights %*% effects(mod0, covariate = 'o_share')['Alabama', -1]) / sum(weights)
me1 <- (weights %*% effects(mod1, covariate = 'o_share')['Alabama', -1]) / sum(weights)
me2 <- (weights %*% effects(mod2, covariate = 'o_share_ins')['Alabama', -1]) / sum(weights)
me3 <- (weights %*% effects(mod3, covariate = 'o_share_ins')['Alabama', -1]) / sum(weights)
me4 <- (weights %*% effects(mod4, covariate = 'o_share_ins')['Alabama', -1]) / sum(weights)
# This formula is different if I don't fix the Alabama origin share to zero -- then
# I need to subtract off the marginal effect of raising the Alabama origin share on
# the probability of remaining in Alabama.

# Now simulate probability of staying in Alabama (among in-state students) assuming
# UA's origin-state composition had (1) remained at 2006 levels vs. (2) continued
# along its actual path

# Set up data for predict() function

# Get evolving o_share from join_bal
o_share_updating <- join_bal %>%
  group_by(grad_y) %>%
  filter(row_number() <= 51) %>%
  arrange(grad_y) %>%
  pull(o_share)
# Get static o_share (2006 level)
o_share_static <- join_bal %>%
  filter(grad_y == 2006) %>%
  filter(row_number() <= 51) %>%
  pull(o_share)
# Get relative year, t_AL_ins
t_AL_ins <- join_bal %>%
  filter(!oos) %>%
  group_by(grad_y) %>%
  filter(row_number() <= 51) %>%
  arrange(grad_y) %>%
  pull(t_AL_ins)

# Form datasets for prediction
data_static <- data.frame(alternative = rep(join_bal$alternative[1:51], 18),
                          home_state_oos = 0,
                          home_state_ins = rep(c(1, rep(0, 50)), 18),
                          o_share_oos = 0,
                          o_share_ins = rep(o_share_static, 18),
                          t_AL_oos = 0,
                          t_AL_ins = t_AL_ins)
data_updating <- data.frame(alternative = rep(join_bal$alternative[1:51], 18),
                            home_state_oos = 0,
                            home_state_ins = rep(c(1, rep(0, 50)), 18),
                            o_share_oos = 0,
                            o_share_ins = o_share_updating,
                            t_AL_oos = 0,
                            t_AL_ins = t_AL_ins)

# Predict for model with linear trends
prob_AL_static <- predict(mod3, data_static)[, 1]
prob_AL_updating <- predict(mod3, data_updating)[, 1]
# Get actual choice probabilities from data
prob_AL_data <- join %>%
  filter(o_state == 'Alabama') %>%
  group_by(grad_y) %>%
  summarize(prob_AL = mean(d_state == 'Alabama')) %>%
  pull(prob_AL)

# Plot counterfactuals and data
data.frame(t = rep(2006:2023, 3),
           world = c(rep('Fixed OOS share', 18), rep('Actual OOS share (model prediction)', 18), rep('Data', 18)),
           prob_AL = c(prob_AL_static, prob_AL_updating, prob_AL_data)) %>%
  ggplot(aes(x = t, y = prob_AL, col = world)) +
  geom_line(lwd = 1) +
  scale_color_manual(values = c('indianred3', 'grey70', 'steelblue3')) +
  labs(x = 'Graduation year',
       title = 'Counterfactual estimates for in-state students (linear trends)',
       y = 'Probability of staying in Alabama',
       col = NULL) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = 'gray80', linetype = 'dashed'),
        legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0))
ggsave(paste0(pathFigures, 'analysis_main/cf_prob_AL_linear.png'), width = 6, height = 5)

# Now predict for model with flexible cohort FE

# Have to manipulate factor variables to have same levels as during estimation
data_static <- data_static %>%
  bind_rows(data.frame(t_AL_oos = 1:17)) %>%
  mutate(t_AL_oos = factor(t_AL_oos),
         t_AL_ins = factor(t_AL_ins)) %>%
  filter(!is.na(alternative))
data_updating <- data_updating %>%
  bind_rows(data.frame(t_AL_oos = 1:17)) %>%
  mutate(t_AL_oos = factor(t_AL_oos),
         t_AL_ins = factor(t_AL_ins)) %>%
  filter(!is.na(alternative))

# Get new model predictions
prob_AL_static <- predict(mod4, data_static)[, 1]
prob_AL_updating <- predict(mod4, data_updating)[, 1]

# Plot
# Note that here the simulated model exactly fits the data
data.frame(t = rep(2006:2023, 2),
           world = c(rep('Fixed OOS share (model)', 18), rep('Actual OOS share (model = data)', 18)),
           prob_AL = c(prob_AL_static, prob_AL_updating)) %>%
  ggplot(aes(x = t, y = prob_AL, col = world)) +
  geom_line(lwd = 1) +
  scale_color_manual(values = c('indianred3', 'steelblue3')) +
  labs(x = 'Graduation year',
       title = 'Counterfactual estimates for in-state students (year FE)',
       y = 'Probability of staying in Alabama',
       col = NULL) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = 'gray80', linetype = 'dashed'),
        legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0))
ggsave(paste0(pathFigures, 'analysis_main/cf_prob_AL_cohortFE.png'), width = 6, height = 5)

# Now compute the cumulative number of in-state UA students pulled from Alabama since 2006

# Using cohort FE model, get difference in probability of staying in Alabama by year
prob_AL_diff <- prob_AL_static - prob_AL_updating

# Now need # of UA in-state students by year
# Read in first-year enrollment by state-of-origin panel
ef <- readRDS(paste0(pathHome, 'data/ef_by_state_panel.rds')) %>%
  # Filter to UA students from Alabama
  filter(UNITID == 100751 & LINE == 1) %>%
  # Get graduation year
  mutate(grad_y = y + 4) %>%
  filter(grad_y %in% 2006:2023) %>%
  arrange(grad_y)

# Estimated # of in-state students induced to leave Alabama
effects <- data.frame(grad_y = 2006:2023,
                      N_leaving = ef$EFRES01 * prob_AL_diff,
                      cum_leaving = cumsum(ef$EFRES01 * prob_AL_diff))

# Finally, try replicating the plain-logit share regressions using mlogit
mod <- mlogit(choice ~ o_share + t_AL, data = join_bal %>% filter(o_state == 'Alabama'))
summary(mod)
# Get marginal effect
me <- (weights %*% effects(mod, covariate = 'o_share')['Alabama', -1]) / sum(weights)
