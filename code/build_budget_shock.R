#////////////////////////////////////////////////////////////////////////////////
# Filename: build_budget_shock.R
# Author: Ryan Haygood
# Date: 1/31/25
# Description: Builds out-of-state flagship budget shock instruments following
# Deming & Walters (2018) and tests first-stage strength.
#////////////////////////////////////////////////////////////////////////////////

# Setup
source('C:/Users/ryanh/OneDrive/Documents/Grad School/Research/Out-of-State-Enrollment/code/setup.R')

# Build data --------------------------------------------------------------------

# Grapevine data (state-level shifts)
grape <- read_xlsx(paste0(pathHome, 'data/grapevine_appropriations.xlsx'), skip = 16) %>%
  rename(y = `Fiscal Year`,
         funding = `State Support`,
         state = State) %>%
  select(y, state, funding)

# Delta Cost Project data (baseline public reliance shares)
delta <- read.csv(paste0(pathHome, 'data/delta_cost_project/delta_public_87_99.csv')) %>%
  select(state, academicyear, unitid, flagship, state_local_app, stable_operating_rev, total03_revenue) %>%
  rename(y = academicyear)

# Make state abbreviation/name crosswalk
states <- data.frame(state = state.abb,
                     state_name = state.name) %>%
  bind_rows(data.frame(state = 'DC', state_name = 'Washington, D.C.'))

# State-level population
# Get state populations over time
pop00 <- read.csv(paste0(pathHome, 'data/state_pop/st-est00int-alldata.csv')) %>%
  filter(NAME %in% states$state_name & SEX == 0 & ORIGIN == 0 & RACE == 0 & AGEGRP == 0) %>%
  select(NAME, starts_with('POPEST')) %>%
  pivot_longer(cols = -NAME) %>%
  rename(state = NAME,
         pop = value) %>%
  mutate(y = as.numeric(substr(name, nchar(name) - 3, nchar(name)))) %>%
  select(-name) %>%
  # Use next dataset for 2010
  filter(y != 2010)
pop10 <- read.csv(paste0(pathHome, 'data/state_pop/nst-est2020-alldata.csv')) %>%
  filter(NAME %in% states$state_name) %>%
  select(NAME, starts_with('POPEST')) %>%
  pivot_longer(cols = -NAME) %>%
  rename(state = NAME,
         pop = value) %>%
  mutate(y = as.numeric(substr(name, nchar(name) - 3, nchar(name)))) %>%
  select(-name) %>%
  # Use next dataset for 2020
  filter(y != 2020)
pop20 <- read.csv(paste0(pathHome, 'data/state_pop/NST-EST2024-ALLDATA.csv')) %>%
  filter(NAME %in% states$state_name) %>%
  select(NAME, starts_with('POPEST')) %>%
  pivot_longer(cols = -NAME) %>%
  rename(state = NAME,
         pop = value) %>%
  mutate(y = as.numeric(substr(name, nchar(name) - 3, nchar(name)))) %>%
  select(-name)
# Join together
pop <- pop00 %>%
  bind_rows(pop10) %>%
  bind_rows(pop20) %>%
  rename(state_name = state)

# Form baseline public reliance of each state's flagship
flag <- delta %>%
  filter(y == 1999 & flagship == 1) %>%
  mutate(share = state_local_app / stable_operating_rev) %>%
  filter(!is.na(share)) %>%
  select(state, share)

# Public reliance shares in 1990 for public flagships (as identified in DCP, not our definitions)
summary(flag$share)

# Form shift-share using state-level shifts in public appropriations per resident
shock <- grape %>%
  # Just use 2002-2019 for now
  filter(y %in% 2002:2019) %>%
  rename(state_name = state) %>%
  left_join(states) %>%
  # Filter to valid states
  filter(!is.na(state)) %>%
  # Join on state-level population
  left_join(pop) %>%
  # Join on 1990 flagship shares
  left_join(flag) %>%
  # Form shift-share IV
  mutate(shift_share = share * funding / pop) %>%
  # Remove states without shift-shares
  filter(!(state %in% c('IL', 'MA', 'NJ', 'TN'))) %>%
  select(-state_name)

# Get enrollment data
shares <- readRDS(paste0(pathHome, 'data/shares.rds')) %>%
  rename(state_name = state) %>%
  left_join(states) %>%
  select(-state_name)

# Backdate the students' grad_y to their likely enrollment year
shares <- shares %>%
  mutate(y = grad_y - 4) %>%
  left_join(shock) %>%
  filter(y %in% 2002:2019) %>%
  # Remove states without shift-shares
  filter(!(state %in% c('IL', 'MA', 'NJ', 'TN', 'DC'))) %>%
  # Generate lags
  group_by(state) %>%
  arrange(state, y) %>%
  mutate(lag1_shift_share = lag(shift_share),
         lag2_shift_share = lag(shift_share, n = 2),
         lag3_shift_share = lag(shift_share, n = 3),
         lag1_funding = lag(funding),
         lag2_funding = lag(funding, n = 2),
         lag3_funding = lag(funding, n = 3))

# Regress UA's OOS shares and enrollment counts on shift-share IV (trying different lags)
# Need to cluster by origin state?
# Need to exclude Alabama -- but this kills the coefficient
felm(o_share ~ shift_share + lag1_shift_share + lag2_shift_share | factor(state) + factor(y), data = shares %>% filter(state != 'AL')) %>%
  summary(robust = T)
felm(N_origin ~ shift_share + lag1_shift_share + lag2_shift_share | factor(state) + factor(y), data = shares %>% filter(state != 'AL')) %>%
  summary(robust = T)

# Instead just try state-level appropriations
felm(o_share ~ log(funding) | factor(state) + factor(y), data = shares %>% filter(state != 'AL')) %>%
  summary(robust = T)
felm(N_origin ~ log(funding) | factor(state) + factor(y), data = shares %>% filter(state != 'AL')) %>%
  summary(robust = T)

# Instead try OOS flagship tuition

# Read in tuition data
years <- 2000:2019
ic <- data.frame()
for (i in 1:length(years)) {
  
  if (years[i] %in% 2009:2012) {
    
    ic <- read_xlsx(paste0(pathHome, 'data/ipeds_tuition/ic', years[i], '_ay.xlsx')) %>%
      rename_with(toupper) %>%
      select(UNITID, CHG2AY3, TUITION2) %>%
      mutate_all(~as.numeric(as.character(.))) %>%
      mutate(y = years[i]) %>%
      bind_rows(ic)
    
  } else {
    
    ic <- read.csv(paste0(pathHome, 'data/ipeds_tuition/ic', years[i], '_ay.csv')) %>%
      rename_with(toupper) %>%
      select(UNITID, CHG2AY3, TUITION2) %>%
      mutate_all(~as.numeric(as.character(.))) %>%
      mutate(y = years[i]) %>%
      bind_rows(ic)
    
  }
  
}

# Identify flagships for each state
flag_ids <- delta %>%
  filter(flagship == 1) %>%
  select(state, unitid) %>%
  distinct() %>%
  rename(UNITID = unitid)

# Filter to flagship tuitions
ic <- ic %>%
  left_join(flag_ids) %>%
  filter(!is.na(state))

# Join to origin enrollment data
shares <- shares %>%
  left_join(ic) %>%
  rename(tuition = CHG2AY3,
         tuition2 = TUITION2) %>%
  # Generate lags
  group_by(state) %>%
  arrange(state, y) %>%
  mutate(lag1_tuition = lag(tuition),
         lag2_tuition = lag(tuition, n = 2),
         lag3_tuition = lag(tuition, n = 3),
         lag4_tuition = lag(tuition, n = 4)) %>%
  # Get average tuition over the last 2 years
  mutate(avg_tuition = (tuition + lag1_tuition))

# Regress UA's OOS shares and enrollment counts on shift-share IV (trying different lags)
# Need to cluster by origin state?
# Need to exclude Alabama
felm(o_share ~ tuition + lag1_tuition + lag2_tuition + lag3_tuition + lag4_tuition | factor(state) + factor(y), data = shares %>% filter(state != 'AL')) %>%
  summary(robust = T)
felm(N_origin ~ tuition | factor(state) + factor(y), data = shares %>% filter(state != 'AL')) %>%
  summary(robust = T)
# Trying log tuition
felm(o_share ~ log(tuition) | factor(state) + factor(y), data = shares %>% filter(state != 'AL')) %>%
  summary(robust = T)
felm(N_origin ~ log(tuition) | factor(state) + factor(y), data = shares %>% filter(state != 'AL')) %>%
  summary(robust = T)

# That was posted tuition and fees
# Also try average in-state tuition (which may be post-grant?)
felm(o_share ~ tuition2 | factor(state) + factor(y), data = shares %>% filter(state != 'AL')) %>%
  summary(robust = T)
felm(N_origin ~ tuition2 | factor(state) + factor(y), data = shares %>% filter(state != 'AL')) %>%
  summary(robust = T)
# These are significant but only due to zeroes for a handful of schools... Why would it be zero?
# Trying log tuition
felm(o_share ~ log(tuition2) | factor(state) + factor(y), data = shares %>% filter(state != 'AL' & tuition2 > 0)) %>%
  summary(robust = T)
felm(N_origin ~ log(tuition2) | factor(state) + factor(y), data = shares %>% filter(state != 'AL' & tuition2 > 0)) %>%
  summary(robust = T)

# IV ----------------------------------------------------------------------------

# Read cleaned first-job data
dest <- readRDS(paste0(pathHome, 'revelio_data/first_spell_join.rds')) %>%
  filter(country == 'United States')

# Read origin data
origin <- read.csv(paste0(pathHome, 'data/linked_commencement_revelio_profile_data.csv'))

# Clean up states of origin
origin <- origin %>%
  # Remove students with missing graduation years
  filter(!is.na(Year)) %>%
  mutate(originState = gsub(' ', '', originState)) %>%
  rename(state = originState) %>%
  # Filter to US origins for now
  filter(!is.na(state)) %>%
  rename(grad_y = Year)

# Add in destination shares
shares <- dest %>%
  group_by(grad_y) %>%
  mutate(N_cohort = n()) %>%
  group_by(grad_y, state) %>%
  summarize(d_share = n() / mean(N_cohort),
            N_dest = n()) %>%
  # Get state abbreviations
  rename(state_name = state) %>%
  left_join(states) %>%
  # Attach on full Commencement origin shares
  full_join(shares) %>%
  # Impute zeroes
  mutate(o_share = if_else(is.na(o_share), 0, o_share),
         d_share = if_else(is.na(d_share), 0, d_share)) %>%
  # Remove 2024 (incomplete data)
  filter(grad_y < 2024)

# Join origin and destination data
# Used for conditioning on origin, as well as full conditional logit model
join <- dest %>%
  # Get state abbreviations
  rename(state_name = state) %>%
  left_join(states) %>%
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
  filter(o_state == 'AL') %>%
  group_by(grad_y) %>%
  mutate(N_cohort = n()) %>%
  group_by(grad_y, d_state, N_cohort) %>%
  summarize(d_share = n() / mean(N_cohort),
            N_dest = n()) %>%
  # Remove 2024 (incomplete data)
  filter(grad_y < 2024)

# Attach on the old origin shares (want to use full sample here)
share_ins <- shares %>%
  filter(state != 'AL') %>%
  rename(d_state = state) %>%
  select(d_state, grad_y, o_share, N_origin, shift_share) %>%
  right_join(share_ins) %>%
  # Impute zero origin students
  mutate(N_origin = if_else(is.na(N_origin), 0, N_origin))

# Missing shift-share values introduced at some point...

# Check first-stage again
felm(o_share ~ lag(shift_share) | factor(d_state) + factor(grad_y), data = share_ins) %>%
  summary(robust = T)

# IV estimates
felm(d_share ~ 0 | factor(o_state) + factor(y) | o_share ~ shift_share, data = share_ins)

### Verify that this shift-share actually increases enrollment at the OOS flagships too

### Really should use state-level young-adult population