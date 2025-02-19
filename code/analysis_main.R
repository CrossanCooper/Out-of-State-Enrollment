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

# Define origin shares using IPEDS data

# Bring in IPEDS shares (along with selectivity IV)
iv <- readRDS(paste0(pathHome, 'data/selectivity_iv.rds')) %>%
  left_join(rename(states, STABBR = originState)) %>%
  # Translate year of enrollment to likely graduation year for that cohort
  mutate(grad_y = y + 4)

# Get IPEDS origin shares
ipeds_shares <- iv %>%
  group_by(grad_y) %>%
  mutate(N_cohort = sum(enroll)) %>%
  mutate(o_share = enroll / N_cohort * 100,
         N_origin = enroll) %>%
  select(state, grad_y, o_share, N_origin)

# Instead of IPEDS, try defining origin shares with the full Commencement records

# Read in full Commencement records
comm <- read.csv(paste0(pathHome, 'data/all_alabama_data.csv'))

# Clean up states of origin; exclude international students
comm <- comm %>%
  mutate(originState = gsub(' ', '', Origin.State)) %>%
  # State will be NA for origins outside the US
  left_join(states) %>%
  # Filter to US origins for now
  filter(!is.na(state)) %>%
  rename(grad_y = Year)

# Get origin-state shares
shares <- comm %>%
  group_by(grad_y) %>%
  mutate(N_cohort = n()) %>%
  group_by(grad_y, state) %>%
  summarize(o_share = n() / mean(N_cohort) * 100,
            N_origin = n()) %>%
  select(state, grad_y, o_share, N_origin)

# Add in destination shares
shares <- dest %>%
  group_by(grad_y) %>%
  mutate(N_cohort = n()) %>%
  group_by(grad_y, state) %>%
  summarize(d_share = n() / mean(N_cohort),
            N_dest = n()) %>%
  # Attach on full Commencement origin shares
  full_join(shares) %>%
  # Impute zeroes
  mutate(o_share = if_else(is.na(o_share), 0, o_share),
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

# Attach ACS income data --------------------------------------------------------

# Excerpting Crossan's code to attach ACS data to Commencement records at town-level

# Get linked Commencement records
alabama_data <- origin %>%
  mutate(town = trimws(tolower(gsub(",.*", "", originTown))),
         state = gsub(' ', '', tolower(originState)))

# Read ACS data
acs_data <- read.csv(paste0(pathHome, 'data/acs_2018_2022_town_estimates.csv')) %>%
  mutate(town = tolower(city),
         state = tolower(state))

# Merge (92.4% of in-state students)
merge_acs_alabama <- alabama_data %>%
  left_join(acs_data)

# Get baseline median HH income across in-state UA students in our sample
median_instate_income <- merge_acs_alabama %>%
  filter(state == 'al') %>%
  pull(median_hh_income) %>%
  median(na.rm = T)

# Split Alabama into high/low-income
origin_acs <- merge_acs_alabama %>%
  mutate(state = toupper(state)) %>%
  select(-originState) %>%
  rename(originState = state) %>%
  # State will be NA for origins outside the US
  left_join(states) %>%
  # Filter to US origins for now
  filter(!is.na(state)) %>%
  mutate(state = if_else(state == 'Alabama', if_else(median_hh_income < median_instate_income, 'AL-poor', 'AL-rich'), state))

# Join origin and destination data
# Used for conditioning on origin, as well as full conditional logit model
join_acs <- dest %>%
  select(-c('first_name', 'last_name', 'grad_y', 'fullname', 'field', 'user_location')) %>%
  # Get destination state
  rename(d_state = state) %>%
  # Only keep those showing up in both
  inner_join(origin_acs) %>%
  # Get origin state
  rename(o_state = state) %>%
  filter(grad_y < 2024)

# Sample descriptives -----------------------------------------------------------

# Attach to shares dataframe
shares <- ipeds_shares %>%
  rename(o_share_ipeds = o_share,
         N_origin_ipeds = N_origin) %>%
  right_join(shares)

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
gamma <- 0.104
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
felm(o_share ~ adm_rate | factor(d_state) | 0 | d_state, data = share_ins) %>%
  summary(robust = T)
# IV
felm(diff_log_d_share ~ 0 | factor(d_state) | (o_share ~ adm_rate) | d_state, data = share_ins) %>%
  summary(robust = T)

# Conditioning on in-state students
# First stage, with linear trends
felm(o_share ~ adm_rate  + grad_y | factor(d_state) | 0 | d_state, data = share_ins) %>%
  summary(robust = T)
# IV, with linear trends
felm(diff_log_d_share ~ grad_y | factor(d_state) | (o_share ~ adm_rate) | d_state, data = share_ins) %>%
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
         o_share_ins = o_share * !oos) %>%
  # Get grad_y interacted with Alabama (for a trend in Alabama's mean utility)
  mutate(t_AL = (grad_y - 2006) * (alternative == 'Alabama')) %>%
  # Interact this with OOS
  mutate(t_AL_oos = t_AL * oos,
         t_AL_ins = t_AL * !oos) %>%
  # Arrange dataframe
  arrange(user_id, alternative)

# For ACS-linked version splitting Alabama by income
join_bal_acs <- data.frame(user_id = rep(unique(join$user_id), n = length(unique(join$d_state))),
                           alternative = rep(unique(join$d_state), each = length(unique(join$user_id)))) %>%
  left_join(join_acs) %>%
  # Remove 2024
  filter(grad_y < 2024) %>%
  # Restrict to necessary variables
  select(user_id, alternative, o_state, d_state, grad_y) %>%
  # Generate indicator for whether the destination is your home state
  mutate(home_state = if_else(o_state %in% c('AL-poor', 'AL-rich'), alternative == 'Alabama', o_state == alternative),
         # Generate choice indicator
         choice = d_state == alternative,
         # Generate out-of-state indicator
         oos = !(o_state %in% c('AL-poor', 'AL-rich'))) %>%
  # Join on origin-share data
  left_join(rename(shares, alternative = state)) %>%
  # Impute zero origin shares where missing
  mutate(o_share = if_else(is.na(o_share), 0, o_share)) %>%
  # Fix the Alabama origin share to zero
  mutate(o_share = if_else(alternative == 'Alabama', 0, o_share)) %>%
  # Manually generate interactions with OOS indicator
  mutate(home_state_oos = home_state * oos,
         home_state_ins = home_state * !oos,
         home_state_ins_poor = home_state * (o_state == 'AL-poor'),
         home_state_ins_rich = home_state * (o_state == 'AL-rich'),
         o_share_oos = o_share * oos,
         o_share_ins = o_share * !oos,
         o_share_ins_poor = o_share * (o_state == 'AL-poor'),
         o_share_ins_rich = o_share * (o_state == 'AL-rich')) %>%
  # Get grad_y interacted with Alabama (for a trend in Alabama's mean utility)
  mutate(t_AL = (grad_y - 2006) * (alternative == 'Alabama')) %>%
  # Interact this with OOS
  mutate(t_AL_oos = t_AL * oos,
         t_AL_ins = t_AL * !oos,
         t_AL_ins_poor = t_AL * (o_state == 'AL-poor'),
         t_AL_ins_rich = t_AL * (o_state == 'AL-rich')) %>%
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

# Now split in-state students by rich/poor

# Multinomial logit, heterogeneity and trends in Alabama utilities
mod3het <- mlogit(choice ~ home_state_oos + home_state_ins_poor + home_state_ins_rich + o_share_oos + o_share_ins_poor + o_share_ins_rich + t_AL_oos + t_AL_ins_poor + t_AL_ins_rich, data = join_bal_acs)

# Multinomial logit, heterogeneity and cohort effects on Alabama utilities
mod4het <- mlogit(choice ~ home_state_oos + home_state_ins_poor + home_state_ins_rich + o_share_oos + o_share_ins_poor + o_share_ins_rich + t_AL_oos + t_AL_ins_poor + t_AL_ins_rich, data = join_bal_acs %>% mutate(t_AL_oos = factor(t_AL_oos), t_AL_ins_poor = factor(t_AL_ins_poor), t_AL_ins_rich = factor(t_AL_ins_rich)))

# Get robust SEs
se0 <- sqrt(diag(vcovCL(mod0, type = 'HC1')))
se1 <- sqrt(diag(vcovCL(mod1, type = 'HC1')))
se2 <- sqrt(diag(vcovCL(mod2, type = 'HC1')))
se3 <- sqrt(diag(vcovCL(mod3, type = 'HC1')))
se4 <- sqrt(diag(vcovCL(mod4, type = 'HC1')))
# Can also cluster by origin state if we wish
# This isn't working at the moment
sqrt(diag(vcovCL(mod0, cluster = ~o_state, type = 'HC1')))

# Output table
stargazer(mod0, mod1, mod2, mod3, mod4, omit = c('Intercept'), se = list(se0, se1, se2, se3, se4))

# Get AMEs ----------------------------------------------------------------------

# Models 0 and 1 don't interact coefficients with in-state/out-of-state

# Get average marginal effect of a 1pp increase in the out-of-state share for in-state students

# Get each state's portion of the 1pp increase
weights <- join %>%
  filter(o_state != 'Alabama') %>%
  # Get total number of OOS students
  ungroup() %>%
  mutate(N = n()) %>%
  # Get each state's overall OOS share over the whole period
  group_by(o_state) %>%
  summarize(pct = n() / mean(N))

# Get dataframe for prediction
# In-state student predictions are identical up to cohorts, so just get one per cohort
# and then weight according to cohort sizes for ease of computation
pred <- join %>%
  # Filter to in-state students
  filter(o_state == 'Alabama') %>%
  # Get each cohort's weight for average of marginal effects
  group_by(grad_y) %>%
  mutate(N = n()) %>%
  ungroup() %>%
  mutate(pred_weight = N / n()) %>%
  # Get a single representative from each cohort
  group_by(grad_y) %>%
  filter(row_number() == 1) %>%
  select(user_id, grad_y, pred_weight)

# Get dataframe for prediction (works for models without cohort FE)
pred_bal <- join_bal %>%
  filter(user_id %in% pred$user_id)

# Function giving average marginal effect of 1pp increase in OOS share in terms of
# model coefficients
ame <- function(coefs, x, mod) {
  
  # Replace model coefficients with coefs
  mod$coefficients <- coefs
  
  # Compute AME (explicit formula from derivative of Pr(j) wrt d_{k})
  pred$pred_weight %*% t(weights$pct %*% t(-mod$coefficients[x] * predict(mod, pred_bal)[, 'Alabama'] * predict(mod, pred_bal)[, weights$o_state]))
  
}

# Point estimate
ame(mod0$coefficients, x = 'o_share', mod = mod0)
ame(mod1$coefficients, x = 'o_share', mod = mod1)
ame(mod2$coefficients, x = 'o_share_ins', mod = mod2)
ame(mod3$coefficients, x = 'o_share_ins', mod = mod3)

# Get Jacobians
jac0 <- jacobian(function(coefs) {ame(coefs, 'o_share', mod = mod0)}, mod0$coefficients)
jac1 <- jacobian(function(coefs) {ame(coefs, 'o_share', mod = mod1)}, mod1$coefficients)
jac2 <- jacobian(function(coefs) {ame(coefs, 'o_share_ins', mod = mod2)}, mod2$coefficients)
jac3 <- jacobian(function(coefs) {ame(coefs, 'o_share_ins', mod = mod3)}, mod3$coefficients)

# Standard error via Delta method
ame0_se <- sqrt(jac0 %*% vcovCL(mod0, type = 'HC1') %*% t(jac0))
ame1_se <- sqrt(jac1 %*% vcovCL(mod1, type = 'HC1') %*% t(jac1))
ame2_se <- sqrt(jac2 %*% vcovCL(mod2, type = 'HC1') %*% t(jac2))
ame3_se <- sqrt(jac3 %*% vcovCL(mod3, type = 'HC1') %*% t(jac3))

# Get dataframe for prediction
# This is for the model with non-parametric trends
pred_bal <- join_bal %>%
  mutate(t_AL_oos = factor(t_AL_oos),
         t_AL_ins = factor(t_AL_ins)) %>%
  filter(user_id %in% pred$user_id)

# Function giving average marginal effect of 1pp increase in OOS share in terms of
# model coefficients
ame <- function(coefs, x, mod) {
  
  # Replace model coefficients with coefs
  mod$coefficients <- coefs
  
  # Compute AME (explicit formula from derivative of Pr(j) wrt d_{k})
  pred$pred_weight %*% t(weights$pct %*% t(-mod$coefficients[x] * predict(mod, pred_bal)[, 'Alabama'] * predict(mod, pred_bal)[, weights$o_state]))
  
}

# Point estimate
ame(mod4$coefficients, x = 'o_share_ins', mod = mod4)

# Get Jacobian
jac4 <- jacobian(function(coefs) {ame(coefs, 'o_share_ins', mod = mod4)}, mod4$coefficients)

# Standard error via Delta method
ame4_se <- sqrt(jac4 %*% vcovCL(mod4, type = 'HC1') %*% t(jac4))

# Counterfactuals ---------------------------------------------------------------

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
# These condition on in-state (home_state)
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

# Now compute the cumulative number of in-state UA students pulled from Alabama since 2006

# Now need # of UA in-state students by year
# Read in first-year enrollment by state-of-origin panel
ef <- readRDS(paste0(pathHome, 'data/ef_by_state_panel.rds')) %>%
  # Filter to UA students from Alabama
  filter(UNITID == 100751 & LINE == 1) %>%
  # Get graduation year
  mutate(grad_y = y + 4) %>%
  filter(grad_y %in% 2006:2023) %>%
  arrange(grad_y)

# Get cumulative effect on # students pulled out with standard error via Delta method
# This is for the richest model (model 4 with cohort FE)
tot_leaving <- function(coefs, mod) {
  
  # Replace estimated coefficients
  mod$coefficients <- coefs
  
  # For each year, get probability of staying in Alabama for static and updating counterfactuals
  prob_AL_static <- predict(mod, data_static)[, 1]
  prob_AL_updating <- predict(mod, data_updating)[, 1]
  
  # Calculating number of students induced to leave from 2006-2023
  sum(ef$EFRES01 * (prob_AL_static - prob_AL_updating))
  
}

# Get cumulative number of students leaving
tot_leaving(mod4$coefficients, mod4)

# Get Jacobian
jac_tot <- jacobian(function(coefs) {tot_leaving(coefs, mod = mod4)}, mod4$coefficients)

# Get standard error via Delta method
sqrt(jac_tot %*% vcovCL(mod4, type = 'HC1') %*% t(jac_tot))

# Get predicted effect on probability of leaving for each cohort
prob_staying <- function(coefs, cf, y) {
  
  # Replace estimated coefficients
  mod4$coefficients <- coefs
  
  if (cf == 'no_OOS_growth') {
    
    # Get probability of staying in Alabama in year y, under no OOS share growth
    predict(mod4, data_static[(51*(y - 2006) + 1):(51*(y - 2005)), ])[1]
    
  } else {
    
    # Get probability of staying in Alabama in year y, under actual OOS share growth
    predict(mod4, data_updating[(51*(y - 2006) + 1):(51*(y - 2005)), ])[1]
    
  }
  
}

# Get estimates and standard errors for each year
years <- 2006:2023
cf_probs <- data.frame(y = rep(years, 2),
                       cf = c(rep('no_OOS_growth', length(years)), rep('actual_OOS_growth', length(years))),
                       estimate = NA,
                       se = NA)

# Fill in estimates and standard errors for each counterfactual
for (i in 1:nrow(cf_probs)) {
  
  # Get estimate
  cf_probs$estimate[i] <- prob_staying(mod4$coefficients, cf_probs$cf[i], cf_probs$y[i])
  
  # Get Jacobian
  jac_temp <- jacobian(function(coefs) {prob_staying(coefs, cf = cf_probs$cf[i], y = cf_probs$y[i])}, mod4$coefficients)
  
  # Get standard error via Delta method
  cf_probs$se[i] <- sqrt(jac_temp %*% vcovCL(mod4, type = 'HC1') %*% t(jac_temp))
  
  # Tracker
  print(years[i])
  
}

# Plot
# Note that here the simulated model exactly fits the data
cf_probs %>%
  mutate(cf = if_else(cf == 'no_OOS_growth', 'Fixed OOS share', 'Actual OOS share growth')) %>%
  ggplot(aes(x = y, y = estimate, col = cf, fill = cf)) +
  geom_line(lwd = 1) +
  # Add 95% CIs
  geom_ribbon(aes(ymin = estimate - 1.96*se, ymax = estimate + 1.96*se), alpha = 0.2, col = NA) +
  scale_color_manual(values = c('indianred3', 'steelblue3')) +
  scale_fill_manual(values = c('indianred3', 'steelblue3')) +
  labs(x = 'Graduation year',
       title = 'Counterfactual estimates for in-state students',
       y = 'Probability of staying in Alabama',
       col = NULL) +
  guides(fill = 'none') +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = 'gray80', linetype = 'dashed'),
        legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0))
ggsave(paste0(pathFigures, 'analysis_main/cf_prob_AL_cohortFE_ribbon.png'), width = 6, height = 5)

# Now get flow exit with error bars

# Get predicted effect on probability of leaving for each cohort
flow_exit <- function(coefs, y) {
  
  # Replace estimated coefficients
  mod4$coefficients <- coefs
  
  # Get probability of staying in Alabama in year y, under no OOS share growth
  prob_AL_static <- predict(mod4, data_static[(51*(y - 2006) + 1):(51*(y - 2005)), ])[1]
  # Get probability of staying in Alabama in year y, under actual OOS share growth
  prob_AL_updating <- predict(mod4, data_updating[(51*(y - 2006) + 1):(51*(y - 2005)), ])[1]
  
  # Get number of students induced to leave in this year
  ef$EFRES01[ef$grad_y == y] * (prob_AL_static - prob_AL_updating)
  
}

# Get estimates and standard errors for each year
exit_over_time <- data.frame(y = years,
                             estimate = NA,
                             se = NA)

for (i in 1:length(years)) {
  
  # Get estimate
  exit_over_time$estimate[i] <- flow_exit(mod4$coefficients, exit_over_time$y[i])
  
  # Get Jacobian
  jac_temp <- jacobian(function(coefs) {flow_exit(coefs, y = exit_over_time$y[i])}, mod4$coefficients)
  
  # Get standard error via Delta method
  exit_over_time$se[i] <- sqrt(jac_temp %*% vcovCL(mod4, type = 'HC1') %*% t(jac_temp))
  
  # Tracker
  print(years[i])
  
}

# Plot the number of students induced to leave in each cohort
exit_over_time %>%
  # Set the CI to zero in the base year, where induced exit is zero by construction
  ggplot(aes(x = y, y = estimate)) +
  geom_line(col = 'indianred3', lwd = 1) +
  geom_ribbon(aes(ymin = estimate - 1.96*se, ymax = estimate + 1.96*se), alpha = 0.2, fill = 'indianred3') +
  labs(x = 'Graduation year',
       title = 'Counterfactual estimates for in-state students',
       y = 'Flow # induced to leave Alabama',
       col = NULL) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = 'gray80', linetype = 'dashed'),
        legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0))
ggsave(paste0(pathFigures, 'analysis_main/flow_exit.png'), width = 6, height = 5)
