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

# Get other pull factors (unemployment rates, net migration rates) as controls
pull_factors <- readRDS(paste0(pathHome, 'data/pull_factors.rds')) %>%
  select(-c('pop', 'arrive', 'depart')) %>%
  # Rename DC
  mutate(state = if_else(state == 'District of Columbia', 'Washington, D.C.', state))

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

# Save origin-state shares
saveRDS(shares, paste0(pathHome, 'data/shares.rds'))

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

# Attach unemployment rates (and migration rates)
shares <- shares %>%
  left_join(rename(pull_factors, grad_y = y))

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

# Build zero-inclusive in-state destination panel.
full_cohorts <- comm %>%
  ungroup() %>%
  filter(grad_y < 2024) %>%
  group_by(grad_y) %>%
  summarize(N_full = n(),
            N_AL_full = sum(state == 'Alabama'))

ipeds_origins <- iv %>%
  filter(grad_y < 2024,
         state != 'Alabama') %>%
  transmute(grad_y,
            d_state = state,
            N_origin_ipeds = enroll)

ipeds_origins_offsets <- iv %>%
  filter(state != 'Alabama') %>%
  transmute(origin_grad_y = grad_y,
            d_state = state,
            N_origin_ipeds_offset = enroll)

ipeds_origin_years <- sort(unique(ipeds_origins_offsets$origin_grad_y))

obs_al_cohorts <- join %>%
  ungroup() %>%
  filter(o_state == 'Alabama',
         grad_y < 2024) %>%
  count(grad_y, name = 'M_AL_obs')

obs_al_dest <- join %>%
  ungroup() %>%
  filter(o_state == 'Alabama',
         d_state != 'Alabama',
         grad_y < 2024) %>%
  count(grad_y, d_state, name = 'N_dest_obs')

obs_al_home <- join %>%
  ungroup() %>%
  filter(o_state == 'Alabama',
         d_state == 'Alabama',
         grad_y < 2024) %>%
  count(grad_y, name = 'N_home_obs')

pull_factors_dest <- pull_factors %>%
  rename(d_state = state,
         grad_y = y) %>%
  select(d_state, grad_y, ur, net_rate)

share_ins <- expand_grid(d_state = states$state[states$state != 'Alabama'],
                         grad_y = sort(unique(full_cohorts$grad_y))) %>%
  left_join(full_cohorts) %>%
  left_join(ipeds_origins) %>%
  left_join(obs_al_cohorts) %>%
  left_join(obs_al_dest) %>%
  left_join(obs_al_home) %>%
  left_join(pull_factors_dest) %>%
  mutate(N_origin_ipeds = if_else(is.na(N_origin_ipeds), 0, N_origin_ipeds),
         N_dest_obs = if_else(is.na(N_dest_obs), 0L, N_dest_obs),
         N_home_obs = if_else(is.na(N_home_obs), 0L, N_home_obs),
         p_dest = N_dest_obs / M_AL_obs,
         d_share = p_dest,
         o_share = N_origin_ipeds / N_full * 100,
         log_d_share = if_else(p_dest > 0, log(p_dest), NA_real_),
         log_al_share = if_else(N_home_obs > 0, log(N_home_obs / M_AL_obs), NA_real_),
         diff_log_d_share = log_d_share - log_al_share,
         N_dest = p_dest * N_AL_full,
         N_origin = N_origin_ipeds) %>%
  filter(N_full > 0,
         M_AL_obs > 0) %>%
  left_join(ipeds_origins_offsets %>%
              transmute(d_state,
                        grad_y = origin_grad_y + 1,
                        N_origin_lag = N_origin_ipeds_offset)) %>%
  left_join(ipeds_origins_offsets %>%
              transmute(d_state,
                        grad_y = origin_grad_y + 2,
                        N_origin_lag2 = N_origin_ipeds_offset)) %>%
  left_join(ipeds_origins_offsets %>%
              transmute(d_state,
                        grad_y = origin_grad_y - 1,
                        N_origin_lead = N_origin_ipeds_offset)) %>%
  left_join(ipeds_origins_offsets %>%
              transmute(d_state,
                        grad_y = origin_grad_y - 2,
                        N_origin_lead2 = N_origin_ipeds_offset)) %>%
  mutate(N_origin_lag = if_else(is.na(N_origin_lag) & ((grad_y - 1) %in% ipeds_origin_years), 0, N_origin_lag),
         N_origin_lag2 = if_else(is.na(N_origin_lag2) & ((grad_y - 2) %in% ipeds_origin_years), 0, N_origin_lag2),
         N_origin_lead = if_else(is.na(N_origin_lead) & ((grad_y + 1) %in% ipeds_origin_years), 0, N_origin_lead),
         N_origin_lead2 = if_else(is.na(N_origin_lead2) & ((grad_y + 2) %in% ipeds_origin_years), 0, N_origin_lead2))

# Save student- and destination-level datasets for use in GruMPS estimator

# Student data (minimal set of required variables)
students <- join %>%
  ungroup() %>%
  select(o_state, d_state, grad_y) %>%
  # Define out-of-state indicator
  # Define market variable
  mutate(oos = o_state != 'Alabama',
         market = grad_y) %>%
  # Make OOS-specific origin-state variables (for use in interactions)
  mutate(o_state_oos = if_else(oos, o_state, 'None'),
         o_state_ins = if_else(!oos, o_state, 'None')) %>%
  # Rename variables
  rename(choice = d_state) %>%
  # Remove cases where selectivity IV is missing
  left_join(select(rename(iv, choice = state), c('choice', 'grad_y', 'adm_rate'))) %>%
  filter(!is.na(adm_rate))

# Destination data (minimal set of required variables)
destinations <- shares %>%
  ungroup() %>%
  select(state, o_share, ur, net_rate, grad_y) %>%
  # Define market and product variable
  mutate(market = grad_y,
         product = state,
         # Also get t_AL
         t_AL = grad_y - 2006) %>%
  # Remove outside-good Alabama
  filter(product != 'Alabama')

# Or could get destination data from student-level data
destinations <- join %>%
  ungroup() %>%
  select(d_state, grad_y) %>%
  rename(state = d_state) %>%
  distinct() %>%
  # Attach origin shares
  left_join(select(shares, c('grad_y', 'state', 'o_share', 'ur', 'net_rate'))) %>%
  # Attach selectivity IV
  left_join(select(iv, c('state', 'grad_y', 'adm_rate'))) %>%
  # Define market and product variable
  mutate(market = grad_y,
         product = state,
         # Also get t_AL
         t_AL = grad_y - 2006,
         # And impute zero origin share where necessary
         o_share = if_else(is.na(o_share), 0, o_share)) %>%
  # Remove outside-good Alabama
  filter(product != 'Alabama') %>%
  # Remove missing flagship selectivities
  filter(!is.na(adm_rate))

# Save for use in Julia
write.csv(students, paste0(pathHome, 'data/students.csv'), row.names = F)
write.csv(destinations, paste0(pathHome, 'data/destinations.csv'), row.names = F)

# Also get honors/non-honors data

# Get in-state student shares, split by honors status
share_hon <- join %>%
  mutate(hon = Honors != 'No') %>%
  filter(o_state == 'Alabama') %>%
  group_by(grad_y, hon) %>%
  mutate(N_cohort = n()) %>%
  group_by(grad_y, d_state, N_cohort, hon) %>%
  summarize(d_share = n() / mean(N_cohort),
            N_dest = n()) %>%
  # Remove zero-share destinations (can't take log)
  filter(d_share > 0) %>%
  # Get log destination shares
  mutate(log_d_share = log(d_share)) %>%
  # Remove 2024 (incomplete data)
  filter(grad_y < 2024)
share_hon <- shares %>%
  filter(state != 'Alabama') %>%
  rename(d_state = state) %>%
  select(d_state, grad_y, o_share, N_origin) %>%
  right_join(share_hon) %>%
  # Impute zero origin students
  mutate(N_origin = if_else(is.na(N_origin), 0, N_origin))
share_hon <- share_hon %>%
  filter(d_state == 'Alabama') %>%
  ungroup() %>%
  select(log_d_share, grad_y) %>%
  rename(log_al_share = log_d_share) %>%
  right_join(share_hon) %>%
  # Remove the outside good
  filter(d_state != 'Alabama') %>%
  # Get difference in log shares (relative to outside good)
  mutate(diff_log_d_share = log_d_share - log_al_share)

# Get earnings of UA graduates
majors <- dest %>%
  mutate(rel_y = grad_y - 2023,
         oos = first_state != 'Alabama',
         field = case_when(field %in% c('Business', 'Marketing', 'Finance', 'Nursing', 'Economics', 'Education', 'Accounting', 'Engineering') ~ field,
                           field %in% c('Biology', 'Mathematics', 'Chemistry', 'Statistics', 'Physics', 'Medicine') ~ 'Other STEM',
                           !(field %in% c('Business', 'Marketing', 'Finance', 'Nursing', 'Economics', 'Education', 'Accounting', 'Engineering', 'Biology', 'Mathematics', 'Chemistry', 'Statistics', 'Physics', 'Medicine')) ~ 'All other majors'))

# Average total earnings by field, correcting for wage growth and an in-state penalty
lm(total_compensation ~ 0 + factor(field) + rel_y + oos, data = majors %>% filter(grad_y >= 2015)) %>%
  summary(robust = T)
# Here I'm using the "total compensation" variable -- could also use salary

# Get major distributions of UA graduates
majors %>%
  group_by(oos) %>%
  mutate(N = n()) %>%
  group_by(oos, field) %>%
  summarize(prop = n() / mean(N))
ggplot(majors, aes(x = field, fill = oos)) +
  geom_histogram(stat = 'count') +
  facet_wrap(~ oos)

# Use Commencement records
comm %>%
  filter(grad_y == 2023) %>%
  mutate(oos = Origin.State != 'AL') %>%
  mutate(field = case_when(Degree == 'Bachelors of Science in Commerce and Business Administration' ~ 'Business',
                           Degree == '')) %>%
  group_by(oos) %>%
  summarize()

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

# Split by honors status --------------------------------------------------------

# Identify honors students
join <- join %>%
  mutate(hon = Honors != 'No')

# What share of students are Honors in each cohort?
join %>%
  group_by(grad_y, hon) %>%
  summarize(N = n()) %>%
  ggplot(aes(x = grad_y, y = N, fill = hon)) +
  geom_bar(stat = 'identity', position = 'stack') +
  scale_fill_manual(values = c('indianred3', 'steelblue3')) +
  labs(fill = 'Honors',
       x = 'Graduation year') +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = 'gray80', linetype = 'dashed'),
        legend.position = 'right',
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0))
ggsave(paste0(pathFigures, 'analysis_main/honors_sample.png'), width = 6, height = 5)

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

# Residualized scatterplot for main probability-adjusted count specification
resid_share_ins <- share_ins %>%
  filter(grad_y != 2020) %>%
  mutate(N_dest_resid = resid(lm(N_dest ~ factor(d_state) + factor(grad_y), data = .)),
         N_origin_resid = resid(lm(N_origin ~ factor(d_state) + factor(grad_y), data = .)))

ggplot(resid_share_ins, aes(x = N_origin_resid, y = N_dest_resid)) +
  geom_hline(yintercept = 0, col = 'gray80', lty = 2) +
  geom_vline(xintercept = 0, col = 'gray80', lty = 2) +
  geom_point(col = '#21908CFF', alpha = 0.35, size = 2) +
  geom_smooth(method = 'lm', se = F, col = '#440154FF', lwd = 1) +
  labs(x = 'Residualized origin count',
       y = 'Residualized destination count',
       title = 'Residualized destination and origin counts') +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = 'gray80', linetype = 'dashed'),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0))
ggsave(paste0(pathFigures, 'analysis_main/resid_origin_dest_scatter.png'), width = 6, height = 5)

# Simple spec: probability-adjusted # of students to/from the state
felm(N_dest ~ N_origin | factor(d_state) + factor(grad_y) | 0 | d_state, data = share_ins %>% filter(grad_y != 2020)) %>%
  summary(robust = T)

# Once-lagged origin count
felm(N_dest ~ N_origin + N_origin_lag | factor(d_state) + factor(grad_y) | 0 | d_state, data = share_ins %>% filter(grad_y != 2020)) %>%
  summary(robust = T)

# One lag and one lead of the origin count
felm(N_dest ~ N_origin + N_origin_lag + N_origin_lead | factor(d_state) + factor(grad_y) | 0 | d_state, data = share_ins %>% filter(grad_y != 2020)) %>%
  summary(robust = T)

# With controls
felm(N_dest ~ N_origin + N_origin_lag + N_origin_lead + ur + net_rate | factor(d_state) + factor(grad_y) | 0 | d_state, data = share_ins %>% filter(grad_y != 2020)) %>%
  summary(robust = T)

# Now including state-specific trends
felm(N_dest ~ N_origin + N_origin_lag + N_origin_lead + ur + net_rate | factor(d_state) + factor(grad_y) + factor(d_state):grad_y | 0 | d_state, data = share_ins %>% filter(grad_y != 2020)) %>%
  summary(robust = T)

# Two lags of the origin count
felm(N_dest ~ N_origin + N_origin_lag + N_origin_lag2 + ur + net_rate | factor(d_state) + factor(grad_y) | 0 | d_state, data = share_ins %>% filter(grad_y != 2020)) %>%
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

# First stages for probability-adjusted count specification

share_ins_iv <- share_ins %>%
  filter(grad_y != 2020,
         !is.na(adm_rate))

# No controls; robust SE, not clustered
felm(N_origin ~ adm_rate | factor(d_state) + factor(grad_y),
     data = share_ins_iv) %>%
  summary(robust = T)

# No controls; clustered SE
felm(N_origin ~ adm_rate | factor(d_state) + factor(grad_y) | 0 | d_state,
     data = share_ins_iv) %>%
  summary(robust = T)

# IV; robust SE, not clustered
felm(N_dest ~ 0 | factor(d_state) + factor(grad_y) | (N_origin ~ adm_rate),
     data = share_ins_iv) %>%
  summary(robust = T)

# IV; clustered SE
felm(N_dest ~ 0 | factor(d_state) + factor(grad_y) | (N_origin ~ adm_rate) | d_state,
     data = share_ins_iv) %>%
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
  select(user_id, alternative, o_state, d_state, grad_y, field, f_prob) %>%
  # Generate indicator for whether the destination is your home state
  mutate(home_state = o_state == alternative,
         # Generate choice indicator
         choice = d_state == alternative,
         # Generate out-of-state indicator
         oos = o_state != 'Alabama') %>%
  # Join on origin-share data
  left_join(rename(shares, alternative = state)) %>%
  # Remove unemployment rate and reattach separately (to fix NAs)
  select(-c('ur', 'in_rate', 'out_rate', 'net_rate')) %>%
  left_join(rename(pull_factors, grad_y = y, alternative = state)) %>%
  # Impute zero origin shares where missing
  mutate(o_share = if_else(is.na(o_share), 0, o_share)) %>%
  # Fix the Alabama origin share to zero
  mutate(o_share = if_else(alternative == 'Alabama', 0, o_share)) %>%
  # Manually generate interactions with OOS indicator
  mutate(home_state_oos = home_state * oos,
         home_state_ins = home_state * !oos,
         o_share_oos = o_share * oos,
         o_share_ins = o_share * !oos) %>%
  # Get major and gender interactions for heterogeneity
  mutate(home_state_oos_bus = home_state_oos * (field %in% c('Business', 'Marketing', 'Finance')),
         home_state_ins_bus = home_state_ins * (field == 'Business'),
         home_state_ins_female = home_state_ins * (f_prob > 0.5),
         o_share_oos_bus = o_share_oos * (field == 'Business'),
         o_share_oos_nurse = o_share_oos * (field == 'Nursing'),
         o_share_oos_eng = o_share_oos * (field == 'Engineering'),
         o_share_oos_mkt = o_share_oos * (field == 'Marketing'),
         o_share_oos_fin = o_share_oos * (field == 'Finance'),
         o_share_oos_acc = o_share_oos * (field == 'Accounting'),
         o_share_oos_ed = o_share_oos * (field == 'Education'),
         o_share_oos_econ = o_share_oos * (field == 'Economics'),
         o_share_oos_stem = o_share_oos * (field %in% c('Biology', 'Mathematics', 'Chemistry', 'Statistics', 'Physics', 'Medicine')),
         o_share_oos_other = o_share_oos * !(field %in% c('Business', 'Marketing', 'Finance', 'Nursing', 'Economics', 'Education', 'Accounting', 'Engineering', 'Biology', 'Mathematics', 'Chemistry', 'Statistics', 'Physics', 'Medicine')),
         o_share_ins_bus = o_share_ins * (field == 'Business'),
         o_share_ins_nurse = o_share_ins * (field == 'Nursing'),
         o_share_ins_eng = o_share_ins * (field == 'Engineering'),
         o_share_ins_mkt = o_share_ins * (field == 'Marketing'),
         o_share_ins_fin = o_share_ins * (field == 'Finance'),
         o_share_ins_acc = o_share_ins * (field == 'Accounting'),
         o_share_ins_ed = o_share_ins * (field == 'Education'),
         o_share_ins_econ = o_share_ins * (field == 'Economics'),
         o_share_ins_stem = o_share_ins * (field %in% c('Biology', 'Mathematics', 'Chemistry', 'Statistics', 'Physics', 'Medicine')),
         o_share_ins_other = o_share_ins * !(field %in% c('Business', 'Marketing', 'Finance', 'Nursing', 'Economics', 'Education', 'Accounting', 'Engineering', 'Biology', 'Mathematics', 'Chemistry', 'Statistics', 'Physics', 'Medicine')),
         o_share_ins_female = o_share_ins * (f_prob > 0.5)) %>%
  # Get grad_y interacted with Alabama (for a trend in Alabama's mean utility)
  mutate(t_AL = (grad_y - 2006) * (alternative == 'Alabama')) %>%
  # Interact this with OOS
  mutate(t_AL_oos = t_AL * oos,
         t_AL_oos_bus = t_AL * (field %in% c('Business', 'Marketing', 'Finance')),
         t_AL_ins = t_AL * !oos,
         t_AL_ins_bus = t_AL * (field %in% c('Business', 'Marketing', 'Finance')),
         t_AL_ins_female = t_AL * (f_prob > 0.5)) %>%
  # Arrange dataframe
  arrange(user_id, alternative)

# Convert alternatives to simpler names for Stata
alternative_dict <- data.frame(alternative = unique(join_bal$alternative),
                               code = paste0('alt', c(paste0('0', 1:9), 10:length(unique(join_bal$alternative)))))
# Attach new alternative names
join_bal <- join_bal %>%
  left_join(alternative_dict)

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

# Now control for unemployment rate and net migration rate
# Exclude 2020 (Covid year) due to missing migration data
mod5 <- mlogit(choice ~ home_state_oos + home_state_ins + o_share_oos + o_share_ins + t_AL_oos + t_AL_ins + ur + net_rate, data = join_bal %>% filter(grad_y != 2020) %>% mutate(t_AL_oos = factor(t_AL_oos), t_AL_ins = factor(t_AL_ins)))

# Now split in-state students by rich/poor

# Multinomial logit, heterogeneity and trends in Alabama utilities
mod3het <- mlogit(choice ~ home_state_oos + home_state_ins_poor + home_state_ins_rich + o_share_oos + o_share_ins_poor + o_share_ins_rich + t_AL_oos + t_AL_ins_poor + t_AL_ins_rich, data = join_bal_acs)

# Multinomial logit, heterogeneity and cohort effects on Alabama utilities
mod4het <- mlogit(choice ~ home_state_oos + home_state_ins_poor + home_state_ins_rich + o_share_oos + o_share_ins_poor + o_share_ins_rich + t_AL_oos + t_AL_ins_poor + t_AL_ins_rich, data = join_bal_acs %>% mutate(t_AL_oos = factor(t_AL_oos), t_AL_ins_poor = factor(t_AL_ins_poor), t_AL_ins_rich = factor(t_AL_ins_rich)))

# Now split in-state students by gender
mod4_gender <- mlogit(choice ~ home_state_oos + home_state_ins + home_state_ins_female + home_state_ins_female + o_share_oos + o_share_ins + o_share_ins_female + t_AL_oos + t_AL_ins + t_AL_ins_female, data = join_bal %>% mutate(t_AL_oos = factor(t_AL_oos), t_AL_ins = factor(t_AL_ins), t_AL_ins_female = factor(t_AL_ins_female)))

# Now split in-state students by major
mod4_major <- mlogit(choice ~ home_state_oos + home_state_oos_bus + home_state_ins + home_state_ins_bus + o_share_oos_bus + o_share_oos_other + o_share_ins_bus + o_share_ins_other + o_share_ins_nurse + t_AL_oos + t_AL_ins, data = join_bal %>% mutate(t_AL_oos = factor(t_AL_oos), t_AL_oos_bus = factor(t_AL_oos_bus), t_AL_ins = factor(t_AL_ins), t_AL_ins_bus = factor(t_AL_ins_bus)))

# With more granular major groupings
mod4_major <- mlogit(choice ~ home_state_oos + home_state_ins + o_share_oos_bus + o_share_oos_mkt + o_share_oos_fin + o_share_oos_eng + o_share_oos_acc + o_share_oos_ed + o_share_oos_nurse + o_share_oos_econ + o_share_oos_stem + o_share_oos_other + o_share_ins_bus + o_share_ins_mkt + o_share_ins_fin + o_share_ins_eng + o_share_ins_acc + o_share_ins_ed + o_share_ins_nurse + o_share_ins_econ + o_share_ins_stem + o_share_ins_other + t_AL_oos + t_AL_ins, data = join_bal %>% mutate(t_AL_oos = factor(t_AL_oos), t_AL_oos_bus = factor(t_AL_oos_bus), t_AL_ins = factor(t_AL_ins), t_AL_ins_bus = factor(t_AL_ins_bus)))

# Get robust SEs
se0 <- sqrt(diag(vcovCL(mod0, type = 'HC1')))
se1 <- sqrt(diag(vcovCL(mod1, type = 'HC1')))
se2 <- sqrt(diag(vcovCL(mod2, type = 'HC1')))
se3 <- sqrt(diag(vcovCL(mod3, type = 'HC1')))
se4 <- sqrt(diag(vcovCL(mod4, type = 'HC1')))
se5 <- sqrt(diag(vcovCL(mod5, type = 'HC1')))

# Output table
stargazer(mod0, mod1, mod2, mod3, mod4, mod5, omit = c('Intercept'), se = list(se0, se1, se2, se3, se4, se5))

# Also get cluster SE from Stata
vcov0 <- read.csv(paste0(pathHome, 'data/vcov_mod0.csv'))
vcov1 <- read.csv(paste0(pathHome, 'data/vcov_mod1.csv'))
vcov2 <- read.csv(paste0(pathHome, 'data/vcov_mod2.csv'))
vcov3 <- read.csv(paste0(pathHome, 'data/vcov_mod3.csv'))
vcov4 <- read.csv(paste0(pathHome, 'data/vcov_mod4.csv'))
vcov5 <- read.csv(paste0(pathHome, 'data/vcov_mod5.csv'))
vcov4_major <- read.csv(paste0(pathHome, 'data/vcov_mod4_major.csv'))

# Get function to clean Stata SEs
clean_stata_se <- function(vcov, mod) {
  
  # Clean up to match R version
  vcov <- data.frame(lapply(vcov, function(x) gsub("=", "", x)), stringsAsFactors = FALSE) %>%
    mutate(X. = case_when(X. == '_cons' ~ lag(X.),
                          str_detect(X., 'alt') ~ NA,
                          .default = X.)) %>%
    filter(!is.na(X.) & X. != 'code')
  
  # Get stata and R variable names (in their original orders)
  stata_vars <- vcov$X.
  R_vars <- gsub('\\(Intercept\\):|TRUE|FALSE', '', colnames(vcov(mod)))
  
  # Replace Stata variable names with R variable names
  stata_vars <- if_else(stata_vars %in% alternative_dict$code, 
                        alternative_dict$alternative[match(stata_vars, alternative_dict$code)], 
                        stata_vars)
  
  # Replace cohort FE names
  stata_cohort <- if_else(grepl("^\\d", stata_vars), sub("^([0-9]+).*", "\\1", stata_vars), '')
  stata_vars <- paste0(sub("^\\d+\\.", "", stata_vars), stata_cohort)
  # Remove variable-name column
  vcov <- vcov %>%
    select(-X.) %>%
    # Turn into numeric
    mutate_all(as.numeric)
  # Assign cleaned names
  rownames(vcov) <- colnames(vcov) <- stata_vars
  # Remove cohort zero
  vcov <- vcov[!str_detect(rownames(vcov), '0b'), !str_detect(colnames(vcov), '0b')]
  # Reorder matrix to match R variable ordering
  vcov <- vcov[R_vars, R_vars]
  
  as.matrix(vcov)
  
}

# Clean cluster-robust SEs from Stata to match R formatting
vcov0 <- clean_stata_se(vcov0, mod0)
vcov1 <- clean_stata_se(vcov1, mod1)
vcov2 <- clean_stata_se(vcov2, mod2)
vcov3 <- clean_stata_se(vcov3, mod3)
vcov4 <- clean_stata_se(vcov4, mod4)
vcov5 <- clean_stata_se(vcov5, mod5)
vcov4_major <- clean_stata_se(vcov4_major, mod4_major)

# Now can get SEs from diagonal and apply Delta method using these v-cov matrices
se0 <- sqrt(diag(vcov0))
se1 <- sqrt(diag(vcov1))
se2 <- sqrt(diag(vcov2))
se3 <- sqrt(diag(vcov3))
se4 <- sqrt(diag(vcov4))
se5 <- sqrt(diag(vcov5))

# Rename a few SEs for output table
names(se0) <- if_else(names(se0) == 'home_state', 'home_stateTRUE', names(se0))
names(se1) <- if_else(names(se1) == 'home_state', 'home_stateTRUE', names(se1))

# Output table with cluster-robust SEs
stargazer(mod0, mod1, mod2, mod3, mod4, mod5, omit = c('Intercept', 't_AL_ins', 't_AL_oos'), se = list(se0, se1, se2, se3, se4, se5))

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

# Cluster-robust standard errors
ame0_se_cl <- sqrt(jac0 %*% vcov0 %*% t(jac0))
ame1_se_cl <- sqrt(jac1 %*% vcov1 %*% t(jac1))
ame2_se_cl <- sqrt(jac2 %*% vcov2 %*% t(jac2))
ame3_se_cl <- sqrt(jac3 %*% vcov3 %*% t(jac3))

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

# Cluster-robust SE
ame4_se_cl <- sqrt(jac4 %*% vcov4 %*% t(jac4))

# Get dataframe for prediction
# This is for the model with non-parametric trends that omits the Covid year
pred_bal <- join_bal %>%
  filter(grad_y != 2020) %>%
  mutate(t_AL_oos = factor(t_AL_oos),
         t_AL_ins = factor(t_AL_ins)) %>%
  filter(user_id %in% pred$user_id)

# Reweight to account for missing 2020
pred <- pred %>%
  mutate(pred_weight = pred_weight / (1 - pred$pred_weight[pred$grad_y == 2020])) %>%
  filter(grad_y != 2020)

# Function giving average marginal effect of 1pp increase in OOS share in terms of
# model coefficients
ame <- function(coefs, x, mod) {
  
  # Replace model coefficients with coefs
  mod$coefficients <- coefs
  
  # Compute AME (explicit formula from derivative of Pr(j) wrt d_{k})
  pred$pred_weight %*% t(weights$pct %*% t(-mod$coefficients[x] * predict(mod, pred_bal)[, 'Alabama'] * predict(mod, pred_bal)[, weights$o_state]))
  
}

# Point estimate
ame(mod5$coefficients, x = 'o_share_ins', mod = mod5)

# Get Jacobian
jac5 <- jacobian(function(coefs) {ame(coefs, 'o_share_ins', mod = mod5)}, mod5$coefficients)

# Standard error via Delta method
ame5_se <- sqrt(jac5 %*% vcovCL(mod5, type = 'HC1') %*% t(jac5))

# Cluster-robust SE
ame5_se_cl <- sqrt(jac5 %*% vcov5 %*% t(jac5))

# Heterogeneity by major

# Initialize AME dataframe
ame_by_major <- data.frame(major = rep(c('Business', 'Marketing', 'Finance', 'Nursing', 'Economics', 'Education', 'Accounting', 'Engineering', 'STEM', 'Other'), 2),
                           abbrev = rep(c('bus', 'mkt', 'fin', 'nurse', 'econ', 'ed', 'acc', 'eng', 'stem', 'other'), 2),
                           oos = c(rep(F, 10), rep(T, 10)),
                           estimate = NA,
                           se = NA)

# Loop through major X in-/out-of-state combinations
for (i in 1:nrow(ame_by_major)) {
  
  # Get dataframe for prediction
  if (ame_by_major$oos[i] == T) {
    
    # For out-of-state students
    pred <- join %>%
      # Filter to out-of-state students
      # Filter to the major
      filter(o_state != 'Alabama' & case_when(ame_by_major$major[i] == 'STEM' ~ field %in% c('Biology', 'Mathematics', 'Chemistry', 'Statistics', 'Physics', 'Medicine'),
                                              ame_by_major$major[i] == 'Other' ~ !(field %in% c('Business', 'Marketing', 'Finance', 'Nursing', 'Economics', 'Education', 'Accounting', 'Engineering', 'Biology', 'Mathematics', 'Chemistry', 'Statistics', 'Physics', 'Medicine')),
                                              .default = field == ame_by_major$major[i])) %>%
      # Get each cohort's weight for average of marginal effects
      group_by(grad_y, o_state) %>%
      mutate(N = n()) %>%
      ungroup() %>%
      mutate(pred_weight = N / n()) %>%
      # Get a single representative from each cohort
      group_by(grad_y, o_state) %>%
      filter(row_number() == 1) %>%
      select(user_id, grad_y, o_state, pred_weight)
    
  } else {
    
    # For in-state students
    pred <- join %>%
      # Filter to in-state students
      # Filter to the major
      filter(o_state == 'Alabama' & case_when(ame_by_major$major[i] == 'STEM' ~ field %in% c('Biology', 'Mathematics', 'Chemistry', 'Statistics', 'Physics', 'Medicine'),
                                              ame_by_major$major[i] == 'Other' ~ !(field %in% c('Business', 'Marketing', 'Finance', 'Nursing', 'Economics', 'Education', 'Accounting', 'Engineering', 'Biology', 'Mathematics', 'Chemistry', 'Statistics', 'Physics', 'Medicine')),
                                              .default = field == ame_by_major$major[i])) %>%
      # Get each cohort's weight for average of marginal effects
      group_by(grad_y) %>%
      mutate(N = n()) %>%
      ungroup() %>%
      mutate(pred_weight = N / n()) %>%
      # Get a single representative from each cohort
      group_by(grad_y) %>%
      filter(row_number() == 1) %>%
      select(user_id, grad_y, pred_weight)
    
  }
  
  # Get dataframe for prediction (works for models with cohort FE)
  pred_bal <- join_bal %>%
    mutate(t_AL_oos = factor(t_AL_oos),
           t_AL_ins = factor(t_AL_ins)) %>%
    filter(user_id %in% pred$user_id)
  
  # Function giving average marginal effect of 1pp increase in OOS share in terms of model coefficients
  ame <- function(coefs, x, mod) {
    
    # Replace model coefficients with coefs
    mod$coefficients <- coefs
    
    # Compute AME (explicit formula from derivative of Pr(j) wrt d_{k})
    pred$pred_weight %*% t(weights$pct %*% t(-mod$coefficients[x] * predict(mod, pred_bal)[, 'Alabama'] * predict(mod, pred_bal)[, weights$o_state]))
    
  }
  
  # Point estimate
  ame_by_major$estimate[i] <- ame(mod4_major$coefficients, x = paste0('o_share_', if_else(ame_by_major$oos[i] == T, 'oos_', 'ins_'), ame_by_major$abbrev[i]), mod = mod4_major)
  
  # Get Jacobian
  jac4 <- jacobian(function(coefs) {ame(coefs, paste0('o_share_', if_else(ame_by_major$oos[i] == T, 'oos_', 'ins_'), ame_by_major$abbrev[i]), mod = mod4_major)}, mod4_major$coefficients)
  
  # Cluster-robust SE
  ame_by_major$se[i] <- sqrt(jac4 %*% vcov4_major %*% t(jac4))
  
  # Tracker
  print(paste0(100 * i / 20, '% done'))
  
}

# Multiply by 100
ame_by_major <- ame_by_major %>%
  mutate(estimate = 100 * estimate,
         se = 100 * se)

# Manually create table of AMEs by major
stargazer(ame_by_major, summary = F)

# Save data for use in Stata
write_dta(join_bal, paste0(pathHome, 'data/join_bal.dta'))

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

# Cluster-robust SE
sqrt(jac_tot %*% vcov4 %*% t(jac_tot))

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
  mutate(cf = if_else(cf == 'no_OOS_growth', 'Fixed OOS Shares', 'Actual OOS Shares'),
         estimate = 100*(1 - estimate)) %>%
  ggplot(aes(x = y, y = estimate, col = cf, fill = cf)) +
  geom_line(lwd = 1) +
  geom_point() +
  # Add 95% CIs
  #geom_ribbon(aes(ymin = estimate - 1.96*se, ymax = estimate + 1.96*se), alpha = 0.2, col = NA) +
  scale_color_manual(values = c('#440154FF', '#FDE725FF')) +
  #scale_fill_manual(values = c('indianred3', 'steelblue3')) +
  labs(x = 'Graduation Year',
       y = 'Share Out-Migrating (%)',
       col = NULL) +
  guides(fill = 'none') +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = 'gray80', linetype = 'dashed'),
        legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0))
ggsave(paste0(pathFigures, 'analysis_main/cf_prob_AL_cohortFE.png'), width = 7, height = 5)

# Plot with 95% CIs for the actual vs. fixed OOS share difference
cf_prob_diff <- data.frame(y = years,
                           diff = NA,
                           se = NA)

for (i in 1:nrow(cf_prob_diff)) {
  
  # Get difference in probability of staying in Alabama
  cf_prob_diff$diff[i] <- prob_staying(mod4$coefficients, cf = 'actual_OOS_growth', y = cf_prob_diff$y[i]) -
    prob_staying(mod4$coefficients, cf = 'no_OOS_growth', y = cf_prob_diff$y[i])
  
  # Get Jacobian of the difference so the Delta method accounts for covariance
  jac_temp <- jacobian(function(coefs) {
    prob_staying(coefs, cf = 'actual_OOS_growth', y = cf_prob_diff$y[i]) -
      prob_staying(coefs, cf = 'no_OOS_growth', y = cf_prob_diff$y[i])
  }, mod4$coefficients)
  
  # Get standard error via Delta method
  cf_prob_diff$se[i] <- sqrt(jac_temp %*% vcov4 %*% t(jac_temp))
  
  # Tracker
  print(cf_prob_diff$y[i])
  
}

cf_probs_diff_plot <- cf_probs %>%
  select(y, cf, estimate) %>%
  left_join(cf_prob_diff %>% select(y, diff_se = se), by = 'y') %>%
  mutate(cf = if_else(cf == 'no_OOS_growth', 'Fixed OOS Shares', 'Actual OOS Shares'),
         estimate = 100*(1 - estimate),
         diff_se = 100*diff_se,
         ci_low = if_else(cf == 'Fixed OOS Shares', estimate - 1.96*diff_se, NA_real_),
         ci_high = if_else(cf == 'Fixed OOS Shares', estimate + 1.96*diff_se, NA_real_))

cf_probs_diff_plot %>%
  ggplot(aes(x = y, y = estimate, col = cf, fill = cf)) +
  geom_ribbon(data = cf_probs_diff_plot %>% filter(cf == 'Fixed OOS Shares'),
              aes(ymin = ci_low, ymax = ci_high), alpha = 0.2, col = NA) +
  geom_line(lwd = 1) +
  scale_color_manual(values = c('Fixed OOS Shares' = '#21908CFF', 'Actual OOS Shares' = '#440154FF')) +
  scale_fill_manual(values = c('Actual OOS Shares' = '#440154FF', 'Fixed OOS Shares' = '#21908CFF')) +
  labs(x = 'Graduation Year',
       y = 'Share Out-Migrating (%)',
       col = NULL) +
  guides(fill = 'none') +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = 'gray80', linetype = 'dashed'),
        legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0))
ggsave(paste0(pathFigures, 'analysis_main/cf_prob_AL_cohortFE_diffCI.png'), width = 7, height = 5)

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
  exit_over_time$se[i] <- sqrt(jac_temp %*% vcov4 %*% t(jac_temp))
  
  # Tracker
  print(years[i])
  
}

# Plot the number of students induced to leave in each cohort
exit_over_time %>%
  ggplot(aes(x = y, y = estimate)) +
  geom_line(col = '#440154FF', lwd = 1) +
  geom_ribbon(aes(ymin = estimate - 1.96*se, ymax = estimate + 1.96*se), alpha = 0.2, fill = '#440154FF') +
  labs(x = 'Graduation Year',
       y = 'Flow # Induced to Leave Alabama',
       col = NULL) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = 'gray80', linetype = 'dashed'),
        legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0))
ggsave(paste0(pathFigures, 'analysis_main/flow_exit.png'), width = 5.5, height = 4.5)

# Get cumulative number of students induced to leave

# Get cumulative effect over time with standard error via Delta method
# This is for the richest model (model 4 with cohort FE)
tot_leaving_over_time <- function(coefs, y) {
  
  # Replace estimated coefficients
  mod4$coefficients <- coefs
  
  # For each year, get probability of staying in Alabama for static and updating counterfactuals
  prob_AL_static <- predict(mod4, data_static)[1:(y - 2005), 1]
  prob_AL_updating <- predict(mod4, data_updating)[1:(y - 2005), 1]
  
  # Calculating number of students induced to leave from 2006
  sum(ef$EFRES01[1:(y - 2005)] * (prob_AL_static - prob_AL_updating))
  
}

# Get estimates and standard errors for each year
exit_over_time <- exit_over_time %>%
  mutate(estimate_cum = NA,
         se_cum = NA)

for (i in 1:length(years)) {
  
  # Get estimate
  exit_over_time$estimate_cum[i] <- tot_leaving_over_time(mod4$coefficients, exit_over_time$y[i])
  
  # Get Jacobian
  jac_temp <- jacobian(function(coefs) {tot_leaving_over_time(coefs, y = exit_over_time$y[i])}, mod4$coefficients)
  
  # Get cluster-robust standard error via Delta method
  exit_over_time$se_cum[i] <- sqrt(jac_temp %*% vcov4 %*% t(jac_temp))
  
  # Tracker
  print(years[i])
  
}

# Plot the total number of students induced to leave over time
exit_over_time %>%
  ggplot(aes(x = y, y = estimate_cum)) +
  geom_line(col = '#440154FF', lwd = 1) +
  geom_ribbon(aes(ymin = estimate_cum - 1.96*se_cum, ymax = estimate_cum + 1.96*se_cum), alpha = 0.2, fill = '#440154FF') +
  labs(x = 'Graduation Year',
       y = 'Total # Induced to Leave Alabama Since 2006',
       col = NULL) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = 'gray80', linetype = 'dashed'),
        legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0))
ggsave(paste0(pathFigures, 'analysis_main/total_exit.png'), width = 5.5, height = 4.5)

# Net migration -----------------------------------------------------------------

# Get counterfactual OOS student counts assuming OOS share fixed at 2006 level
# (but total enrollment still grew, with OOS student growth replaced 1-for-1 by
# in-state students)

# Get 2006 total OOS enrollment share
oos_share_2006 <- sum(o_share_static) / 100
# Or use the IPEDS data
oos_share_2006 <- sum(ipeds_shares$N_origin[ipeds_shares$grad_y == 2006 & ipeds_shares$state != 'Alabama']) / sum(ipeds_shares$N_origin[ipeds_shares$grad_y == 2006])
# Get total first-time first-year enrollment, 2006-2023
tot_enroll <- ipeds_shares %>%
  group_by(grad_y) %>%
  summarize(N = sum(N_origin)) %>%
  filter(grad_y %in% 2006:2023)
# Counterfactual OOS student counts
cf_oos_enroll <- oos_share_2006 * tot_enroll$N

# Get actual OOS student counts, 2006-2023
true_oos_enroll <- ipeds_shares %>%
  filter(state != 'Alabama') %>%
  group_by(grad_y) %>%
  summarize(N = sum(N_origin)) %>%
  filter(grad_y %in% 2006:2023) %>%
  pull(N)

# OOS student inflows

# Calibration from Groen (2004)
sum(true_oos_enroll - cf_oos_enroll) * 0.10
# Standard error
sum(true_oos_enroll - cf_oos_enroll) * 0.016

# Upper bound, assuming that no OOS students would have migrated to Alabama, had
# they not enrolled
# This assumes monotonicity, that attending UA doesn't make you less likely to
# reside in Alabama (so that everyone who leaves Alabama would have left still
# had they not enrolled). In other words, we assume there are only compliers and
# never-takers (no defiers or always-takers). This gives the largest possible in-
# migration effect.
# Overall OOS student in-migration rate:
oos_in_rate <- mean(join$d_state[join$o_state != 'Alabama'] == 'Alabama')
sum(true_oos_enroll - cf_oos_enroll) * oos_in_rate

# Using Groen (2004) estimates, but assuming the effect scales proportionally
# with the observed OOS staying rate (which was 0.15 in their study)
effect_scaled <- 0.10 * (oos_in_rate / 0.15)
sum(true_oos_enroll - cf_oos_enroll) * effect_scaled

# Using Groen & White (2004) estimates of the p_{nn} potential outcome -- the
# counterfactual in-migration rate, had these students not enrolled
effect_hybrid <- oos_in_rate - 0.05
sum(true_oos_enroll - cf_oos_enroll) * effect_hybrid

# In-state student outflows

# Predicted total outflows from TWFE LPM
sum(true_oos_enroll - cf_oos_enroll) * 0.091

# Predicted total outflows from location-choice model
tot_leaving(mod4$coefficients, mod4)
