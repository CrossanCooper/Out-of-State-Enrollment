#////////////////////////////////////////////////////////////////////////////////
# Filename: analysis_mlogit_iv.R
# Author: Ryan Haygood
# Date: 6/12/26
# Description: Runs a two-step IO-style multinomial logit / IV estimator for
# location spillovers from out-of-state origin shares.
#////////////////////////////////////////////////////////////////////////////////

# Setup -------------------------------------------------------------------------

source('C:/Users/ryanh/OneDrive/Documents/Grad School/Research/Out-of-State-Enrollment/code/setup.R')

paper_years <- sort(setdiff(2006:2023, 2020))

# Build data --------------------------------------------------------------------

# Read cleaned first-job data
dest <- readRDS(paste0(pathHome, 'revelio_data/first_spell_join.rds')) %>%
  filter(country == 'United States')

# Read origin data
origin <- read.csv(paste0(pathHome, 'data/linked_commencement_revelio_profile_data.csv'))

# Make state abbreviation/name crosswalk
states <- data.frame(originState = state.abb,
                     state = state.name) %>%
  bind_rows(data.frame(originState = 'DC', state = 'Washington, D.C.'))

# Clean up states of origin
origin <- origin %>%
  filter(!is.na(Year)) %>%
  mutate(originState = gsub(' ', '', originState)) %>%
  left_join(states, by = 'originState') %>%
  filter(!is.na(state)) %>%
  rename(grad_y = Year)

# Bring in IPEDS origin counts
iv <- readRDS(paste0(pathHome, 'data/selectivity_iv.rds')) %>%
  left_join(rename(states, STABBR = originState), by = 'STABBR') %>%
  mutate(grad_y = y + 4)

# Bring in Crossan's preferred leave-state-out market exposure instrument
market_iv <- read_csv(paste0(pathHome, 'data/market_iv/data/market_iv_panel.csv')) %>%
  transmute(originState = origin_state,
            grad_y = grad_year,
            z_lso = z_peer_non_alabama_count_growth_lso) %>%
  left_join(states, by = 'originState')

# Get other pull factors (unemployment rates, net migration rates) as controls
pull_factors <- readRDS(paste0(pathHome, 'data/pull_factors.rds')) %>%
  select(-c('pop', 'arrive', 'depart')) %>%
  mutate(state = if_else(state == 'District of Columbia', 'Washington, D.C.', state))

# Read in full Commencement records
comm <- read.csv(paste0(pathHome, 'data/all_alabama_data.csv')) %>%
  mutate(originState = gsub(' ', '', Origin.State)) %>%
  left_join(states, by = 'originState') %>%
  filter(!is.na(state)) %>%
  rename(grad_y = Year)

# Full-cohort origin shares for d_jc
shares <- comm %>%
  group_by(grad_y) %>%
  mutate(N_cohort = n()) %>%
  group_by(grad_y, state) %>%
  summarize(o_share = n() / mean(N_cohort) * 100,
            N_origin = n(),
            .groups = 'drop') %>%
  select(state, grad_y, o_share, N_origin)

# Attach destination shares from linked first-job data only to keep the same
# observed student sample and destination/cohort support as analysis_main.R.
shares <- dest %>%
  group_by(grad_y) %>%
  mutate(N_cohort = n()) %>%
  group_by(grad_y, state) %>%
  summarize(d_share = n() / mean(N_cohort),
            N_dest = n(),
            .groups = 'drop') %>%
  full_join(shares, by = c('grad_y', 'state')) %>%
  mutate(o_share = if_else(is.na(o_share), 0, o_share),
         d_share = if_else(is.na(d_share), 0, d_share)) %>%
  filter(d_share > 0,
         grad_y < 2024) %>%
  left_join(rename(pull_factors, grad_y = y), by = c('grad_y', 'state'))

# Join origin and destination data
join <- dest %>%
  select(-c('first_name', 'last_name', 'grad_y', 'fullname', 'field', 'user_location')) %>%
  rename(d_state = state) %>%
  inner_join(origin, by = 'user_id') %>%
  rename(o_state = state) %>%
  filter(grad_y < 2024) %>%
  ungroup()

# First-stage MNL data ----------------------------------------------------------

join_bal <- data.frame(user_id = rep(unique(join$user_id), times = nrow(states)),
                       alternative = rep(states$state, each = length(unique(join$user_id)))) %>%
  left_join(join, by = 'user_id') %>%
  filter(grad_y < 2024) %>%
  select(user_id, alternative, o_state, d_state, grad_y) %>%
  mutate(choice = d_state == alternative) %>%
  left_join(rename(shares, alternative = state), by = c('grad_y', 'alternative')) %>%
  select(-c('ur', 'in_rate', 'out_rate', 'net_rate')) %>%
  left_join(rename(pull_factors, grad_y = y, alternative = state),
            by = c('grad_y', 'alternative')) %>%
  mutate(o_share = if_else(is.na(o_share), 0, o_share),
         o_share = if_else(alternative == 'Alabama', 0, o_share),
         is_in_state = as.integer(o_state == 'Alabama'),
         is_oos = as.integer(o_state != 'Alabama'),
         dest_cohort_fe = if_else(alternative == 'Alabama',
                                  'AL_norm',
                                  paste0(alternative, '__', grad_y)),
         dest_cohort_fe = relevel(factor(dest_cohort_fe), ref = 'AL_norm'),
         o_share_outdiff = is_oos * o_share,
         home_state_oos = as.integer(is_oos == 1 & o_state == alternative),
         t_AL_outdiff = is_oos * (alternative == 'Alabama') * (grad_y - 2006)) %>%
  arrange(user_id, alternative)

stopifnot(any(join_bal$alternative == 'Alabama'))
stopifnot(all(levels(join_bal$dest_cohort_fe)[1] == 'AL_norm'))

# First-stage MNL ---------------------------------------------------------------

mod_twostep <- mlogit(
  choice ~ home_state_oos +
           o_share_outdiff +
           t_AL_outdiff +
           dest_cohort_fe | 0,
  data = join_bal
)

pi_out <- coef(mod_twostep)['o_share_outdiff']

# Extract destination-cohort fixed effects --------------------------------------

delta_coef <- coef(mod_twostep)

delta_second_stage <- data.frame(term = names(delta_coef),
                                 delta_hat_jc = unname(delta_coef)) %>%
  filter(str_detect(term, '^dest_cohort_fe')) %>%
  mutate(dest_cohort = str_remove(term, '^dest_cohort_fe'),
         d_state = str_remove(dest_cohort, '__[0-9]{4}$'),
         grad_y = as.numeric(str_extract(dest_cohort, '[0-9]{4}$'))) %>%
  select(d_state, grad_y, delta_hat_jc) %>%
  filter(d_state != 'Alabama') %>%
  left_join(shares %>%
              transmute(d_state = state,
                        grad_y,
                        o_share,
                        unemp = ur,
                        net_mig = net_rate) %>%
              distinct(d_state, grad_y, .keep_all = TRUE),
            by = c('d_state', 'grad_y')) %>%
  left_join(market_iv %>%
              transmute(d_state = state,
                        grad_y,
                        z_lso) %>%
              distinct(d_state, grad_y, .keep_all = TRUE),
            by = c('d_state', 'grad_y')) %>%
  filter(grad_y %in% paper_years)

stopifnot(all(delta_second_stage$d_state != 'Alabama'))

# Second-stage IV projection ----------------------------------------------------

second_stage_iv <- felm(
  delta_hat_jc ~ unemp + net_mig + grad_y | factor(d_state) |
    (o_share ~ z_lso) | d_state,
  data = delta_second_stage %>% filter(!is.na(z_lso))
)

gamma_in_name <- names(coef(second_stage_iv))[str_detect(names(coef(second_stage_iv)), '^o_share')]
stopifnot(length(gamma_in_name) == 1)

gamma_in <- coef(second_stage_iv)[gamma_in_name]
gamma_out <- gamma_in + pi_out

# Preliminary output ------------------------------------------------------------

summary(mod_twostep)
summary(second_stage_iv, robust = TRUE)

estimates_twostep <- data.frame(
  parameter = c('gamma_in', 'pi_out', 'gamma_out'),
  estimate = c(gamma_in, pi_out, gamma_out),
  row.names = NULL
)

estimates_twostep
