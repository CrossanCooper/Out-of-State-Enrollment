#////////////////////////////////////////////////////////////////////////////////
# Filename: build_destinations.R
# Author: Ryan Haygood
# Date: 11/12/24
# Description: Combines LinkedIn graduate profiles with job spells for defining
# destination outcomes.
#////////////////////////////////////////////////////////////////////////////////

# Setup
source('C:/Users/ryanh/OneDrive/Documents/Grad School/Research/Out-of-State-Enrollment/code/setup.R')

# Build data --------------------------------------------------------------------

# Read in cleaned LinkedIn post-grad snapshots
ln <- read.csv(paste0(pathHome, 'data/cleaned_ua_revelio.csv'))

# Read in Revelio job spells
rv <- read.csv(paste0(pathHome, 'revelio_data/job_spells_data_ua.csv'))

# 14k students don't show up in job-spell data
sum(!(ln$user_id %in% rv$user_id))

# Clean job-spell data first
rv <- rv %>%
  # Remove few spells with unspecified start dates
  filter(startdate != '') %>%
  # Fill missing end dates and locations with NAs
  mutate(enddate = if_else(enddate == '', NA, enddate),
         state = if_else(state == '', NA, state),
         country = if_else(country == '', NA, country)) %>%
  # Get start year and end year (if available)
  mutate(start_y = as.numeric(substr(startdate, 1, 4)),
         end_y = as.numeric(substr(enddate, 1, 4))) %>%
  # Get start- and end-month (if available)
  mutate(start_m = as.numeric(substr(startdate, 6, 7)),
         end_m = as.numeric(substr(enddate, 6, 7)))

# Clean LinkedIn graduate data
ln <- ln %>%
  # Rename graduation year
  rename(grad_y = Year) %>%
  # Impute missing field and degree and location and race/gender
  mutate(field = if_else(field == '', NA, field),
         degree = if_else(degree == '', NA, degree),
         highest_degree = if_else(highest_degree == '', NA, highest_degree),
         user_country = if_else(user_country == '', NA, user_country),
         user_location = if_else(user_location == '', NA, user_location),
         sex_predicted = if_else(sex_predicted == '.', NA, sex_predicted),
         ethnicity_predicted = if_else(ethnicity_predicted == '', NA, ethnicity_predicted))

# How many students in each?
length(unique(ln$user_id))
length(unique(rv$user_id))

# Distribution of graduation years
ggplot(ln, aes(x = grad_y)) +
  geom_bar()

# Join spells onto individual LinkedIn snapshots
join <- ln %>%
  select(-c('enddate', 'startdate')) %>%
  left_join(rv) %>%
  # Flag students without any observed job spells (15,400 cases)
  mutate(no_spells = is.na(start_y))

# As expected, cases with no observed job spells are most likely to be the most recent cohorts
join %>%
  # no_spells is constant within individuals; just collapse to individual level here
  group_by(user_id, grad_y) %>%
  summarize(no_spells = mean(no_spells)) %>%
  group_by(grad_y) %>%
  summarize(no_spells = mean(no_spells)) %>%
  ggplot(aes(x = grad_y, y = no_spells)) +
  geom_bar(stat = 'identity') +
  labs(x = 'Graduation year',
       y = 'Fraction with no job spells') +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = 'gray80', linetype = 'dashed'),
        legend.position = 'right',
        plot.title = element_text(hjust = 0.5))
ggsave(paste0(pathFigures, 'build_destinations/pct_no_spells_by_year.png'), width = 6, height = 5)

# Remove students without any job spells
join <- join %>%
  filter(!no_spells)

# Now how many students have at least one *post-grad* job?
join %>%
  group_by(user_id) %>%
  summarize(post_grad = sum(start_y >= grad_y) > 0) %>%
  pull(post_grad) %>%
  table()
# For the students without one, plot the start year of their last job
join %>%
  group_by(user_id) %>%
  #filter(sum(start_y >= grad_y) == 0) %>%
  filter(start_y == max(start_y)) %>%
  # If multiple, take one arbitrarily
  filter(row_number() == 1) %>%
  # Get start year relative to graduation
  mutate(y_rel_grad = start_y - grad_y) %>%
  # Get share out-of-state
  group_by(y_rel_grad) %>%
  summarize(N = n(),
            N_ins = sum(state == 'Alabama', na.rm = T),
            N_oos = sum(state != 'Alabama', na.rm = T),
            oos_share = mean(state != 'Alabama', na.rm = T)) %>%
  filter(y_rel_grad %in% -5:10) %>%
  pivot_longer(cols = c('N_ins', 'N_oos')) %>%
  mutate(name = if_else(name == 'N_ins', 'In-state', 'Out-of-state')) %>%
  ggplot(aes(x = y_rel_grad, y = value, fill = name)) +
  geom_bar(stat = 'identity', position = 'stack') +
  scale_fill_manual(values = c('steelblue1', 'indianred1')) +
  labs(x = 'Year since graduation',
       y = 'Number of students',
       fill = NULL,
       title = 'Last observed job: start year relative to graduation') +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = 'gray80', linetype = 'dashed'),
        legend.position = 'right',
        plot.title = element_text(hjust = 0.5))

# For now filter to job spells beginning after graduation
# This removes those 11k who only have job spells starting prior to their graduation
# Need to be careful about which cohorts we use (e.g., can't see locations for many 2024 grads)
join <- join %>%
  filter(start_y >= grad_y) %>%
  # Also remove the handful of cases where end year comes before start year
  filter(is.na(end_y) | !(start_y > end_y))

# Distribution of # of job spells
join %>%
  group_by(user_id) %>%
  summarize(N = n()) %>%
  ggplot(aes(x = N)) +
  stat_ecdf() +
  coord_cartesian(xlim = c(0, 20)) +
  labs(x = 'Number of job spells',
       title = 'Distribution of number of observed job spells') +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = 'gray80', linetype = 'dashed'),
        legend.position = 'right',
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5))

# How soon do we see students' first job spell after graduation?
join %>%
  # Get time until first recorded job spell (years since graduation)
  group_by(user_id) %>%
  summarize(rel_y = min(start_y) - mean(grad_y)) %>%
  ggplot(aes(x = rel_y)) +
  stat_ecdf() +
  labs(x = 'Year since graduation',
       lty = NULL,
       title = 'CDF of time until first job after graduation',
       caption = 'Notes: Filtering to those with at least one post-grad job, for a total of 71,195 students.') +
  coord_cartesian(xlim = c(0, 8)) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = 'gray80', linetype = 'dashed'),
        legend.position = 'right',
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0))
ggsave(paste0(pathFigures, 'build_destinations/dist_years_until_first_job.png'), width = 6, height = 5)

# Describe geographic variation in individual post-grad job spells
st_variation <- join %>%
  # Restrict to earlier part of sample so we have a chance of seeing spells
  # Remove spells without state information
  filter(grad_y <= 2020 & !is.na(state)) %>%
  # Get spell length in years (capped at last year of observation, 2024)
  mutate(spell_length = (if_else(is.na(end_y), 2024, end_y) - start_y) + 1) %>%
  # Get number of spells and job-years in each state
  group_by(user_id, state) %>%
  summarize(N_spells = n(),
            # Number of job-years accrued in the state
            N_years = sum(spell_length)) %>%
  group_by(user_id) %>%
  # All spells in one state
  summarize(one_state = n() == 1,
            # Fraction of spells in the modal state (modal in terms of # of jobs, not tenure)
            pct_modal_state_spells = max(N_spells) / sum(N_spells),
            # Fraction of job-years spent in the modal state
            pct_modal_state_years = max(N_years) / sum(N_years))
# Proportion only working in one state post-grad
prop.table(table(st_variation$one_state))
# Proportion with strictly >50% of spells in one state
mean(st_variation$pct_modal_state_spells > 0.5)
# Proportion with strictly >50% of job-years in one state
mean(st_variation$pct_modal_state_years > 0.5)
# Distributions
ggplot(st_variation, aes(x = pct_modal_state_spells)) +
  stat_ecdf() +
  labs(x = 'Proportion of job spells in modal state',
       title = 'Distribution of # of spells occurring in modal state') +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = 'gray80', linetype = 'dashed'),
        legend.position = 'right',
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5))
ggplot(st_variation, aes(x = pct_modal_state_years)) +
  stat_ecdf() +
  labs(x = 'Proportion of job tenure in modal state',
       title = 'Distribution of time spent in modal state',
       caption = 'Notes: Filtering to students graduating in 2020 or earlier. Modal state is the state with the longest total\ntenure, i.e. the largest number of distinct job-years.') +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = 'gray80', linetype = 'dashed'),
        legend.position = 'right',
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0))
ggsave(paste0(pathFigures, 'build_destinations/dist_pct_tenure_modal_state.png'), width = 6, height = 5)

# Subset to relevant variables
join <- join %>%
  # Restrict to jobs with location information
  filter(!is.na(state)) %>%
  # Get the relevant subset of variables
  select(first_name, last_name, grad_y, fullname, user_id, field, highest_degree,
         sex_predicted, ethnicity_predicted, user_location, user_country, updated_dt,
         numconnections, count, lastUpdate, middle_name, suffix,
         position_id, country, state, metro_area, startdate, enddate, role_k1500,
         remote_suitability, weight, seniority, salary, position_number, total_compensation,
         additional_compensation, start_y, end_y, start_m, end_m)

# Save a cleaned version at individual/job-spell level
saveRDS(join, paste0(pathHome, 'revelio_data/all_spells_join.rds'))

# Now build individual-level data that collapses to the first post-grad job with location
# information. Also collect the modal state, in terms of total job duration, and in terms
# of number of job spells. Also collect total job duration in that state. Also collect the
# first- and last-job state, the start- and end-year of those jobs, the number of states
# you're ever observed in, the fraction of job years and job spells spent in the modal state.
first_spell <- join %>%
  # Remove missing states
  filter(!is.na(state)) %>%
  # Get spell length in years (capped at last year of observation, 2024)
  mutate(spell_length = (if_else(is.na(end_y), 2024, end_y) - start_y) + (if_else(is.na(end_m), 5, end_m) + 1 - start_m) / 12) %>%
  # Eliminate the tiny handful of jobs with negative spell length (end date prior to start date)
  filter(spell_length > 0) %>%
  # Get calendar year-month of spell start
  mutate(start_y_m = start_y + start_m / 12) %>%
  # Get number of spells and job-years in each state
  group_by(user_id, state) %>%
  mutate(N_spells = n(),
         # Number of job-years accrued in the state
         N_years = sum(spell_length)) %>%
  # Get first and last state, modal state by # spells and length of spells
  group_by(user_id) %>%
  mutate(first_state = pick(everything())[['state']][which(start_y_m == min(start_y_m))][1],
         last_state = pick(everything())[['state']][which(start_y_m == max(start_y_m))][1],
         modal_state_spells = pick(everything())[['state']][which(N_spells == max(N_spells))][1],
         modal_state_years = pick(everything())[['state']][which(N_years == max(N_years))][1],
         pct_modal_state_spells = max(N_spells) / n(),
         pct_modal_state_years = max(N_years) / sum(spell_length)) %>%
  # Filter to first job out of college
  filter(start_y_m == min(start_y_m)) %>%
  # If multiple, filter arbitrarily
  filter(row_number() == 1)

# Save first-job data
saveRDS(first_spell, paste0(pathHome, 'revelio_data/first_spell_join.rds'))

# Plot distribution of first spell length
# Filter to jobs starting before 2015 so we can see farther out
ggplot(filter(first_spell, start_y < 2015), aes(x = spell_length)) +
  stat_ecdf() +
  coord_cartesian(xlim = c(0, 9)) +
  labs(x = 'Length of first job spell',
       title = 'Distribution of tenure at first job after graduation') +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = 'gray80', linetype = 'dashed'),
        legend.position = 'right',
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5))

# Get OOS share of first post-grad job over time
first_spell %>%
  group_by(grad_y) %>%
  summarize(oos_share = mean(state != 'Alabama')) %>%
  filter(grad_y <= 2022) %>%
  ggplot(aes(x = grad_y, y = oos_share)) +
  geom_line(size = 1, col = 'indianred1') +
  labs(x = 'Graduation year',
       y = '% first job out-of-state') +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = 'gray80', linetype = 'dashed'),
        legend.position = 'right',
        plot.title = element_blank())
ggsave(paste0(pathFigures, 'build_destinations/oos_share_by_year.png'), width = 7, height = 5)

# Another outcome we might want is whether you ever work in a state
# Can we just get a few columns for state of 1st, 2nd, 3rd, ... job spell?
# How many states are people typically observed in?
join %>%
  # Filter to non-missing US states
  filter(!is.na(state) & country == 'United States') %>%
  group_by(user_id) %>%
  summarize(N_st = length(unique(state))) %>%
  ggplot(aes(x = N_st)) +
  geom_bar(aes(y = ..count../sum(..count..))) +
  coord_cartesian(xlim = c(0.7, 6.3)) +
  scale_x_continuous(breaks = 1:6, labels = 1:6) +
  labs(x = 'Number of states',
       y = 'Density',
       title = 'Distribution of # of states observed in after graduation') +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = 'gray80', linetype = 'dashed'),
        legend.position = 'right',
        plot.title = element_text(hjust = 0.5))
ggsave(paste0(pathFigures, 'build_destinations/dist_N_states.png'), width = 6, height = 5)

# Can do a heat map of US state destinations over time!

# Make a yearly panel of job locations, with missing values where unobserved?
# How often do we have overlapping jobs in different locations?
# Also get post-grad location 1 year out, 3 years out, 5 years out (if observed)
