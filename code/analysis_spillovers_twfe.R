#////////////////////////////////////////////////////////////////////////////////
# Filename: analysis_spillovers_twfe.R
# Author: Ryan Haygood
# Date: 6/12/26
# Description: Runs TWFE OLS and IV count/share specifications for evaluating
# spillovers from out-of-state students to Alabama-origin out-migration.
#////////////////////////////////////////////////////////////////////////////////

# Setup
suppressPackageStartupMessages(
  suppressMessages(
    suppressWarnings(source('C:/Users/ryanh/OneDrive/Documents/Grad School/Research/Out-of-State-Enrollment/code/setup.R'))
  )
)

# Count specification source:
# - 'rescaled_ipeds': current setup; rescaled linked AL destinations and IPEDS origins.
# - 'linked_only': raw linked AL destinations and raw linked origins from linked profiles.
count_source <- 'rescaled_ipeds'
count_source <- match.arg(count_source, c('rescaled_ipeds', 'linked_only'))

# Origin share source for the log-share-difference specifications:
# - 'comm': Commencement graduate origin shares.
# - 'ipeds': IPEDS enrollment-by-residency shares.
origin_share_source <- 'ipeds'
origin_share_source <- match.arg(origin_share_source, c('comm', 'ipeds'))
origin_share_label <- if (origin_share_source == 'comm') {
  'Commencement-record origin share'
} else {
  'IPEDS enrollment-by-residency origin share'
}

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
  left_join(states) %>%
  filter(!is.na(state)) %>%
  rename(grad_y = Year)

# Bring in IPEDS origin counts
iv <- readRDS(paste0(pathHome, 'data/selectivity_iv.rds')) %>%
  left_join(rename(states, STABBR = originState)) %>%
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
  left_join(states) %>%
  filter(!is.na(state)) %>%
  rename(grad_y = Year)

# Get origin-state shares from full Commencement records
shares <- comm %>%
  group_by(grad_y) %>%
  mutate(N_cohort = n()) %>%
  group_by(grad_y, state) %>%
  summarize(o_share_comm = n() / mean(N_cohort) * 100,
            N_origin_comm = n()) %>%
  mutate(o_share = o_share_comm,
         N_origin = N_origin_comm) %>%
  select(state, grad_y, o_share, o_share_comm, N_origin, N_origin_comm)

# Add destination shares from linked first-job data
shares <- dest %>%
  group_by(grad_y) %>%
  mutate(N_cohort = n()) %>%
  group_by(grad_y, state) %>%
  summarize(d_share = n() / mean(N_cohort),
            N_dest = n()) %>%
  full_join(shares) %>%
  mutate(o_share_comm = if_else(is.na(o_share_comm), 0, o_share_comm),
         o_share = o_share_comm,
         N_origin_comm = if_else(is.na(N_origin_comm), 0L, N_origin_comm),
         N_origin = N_origin_comm,
         d_share = if_else(is.na(d_share), 0, d_share)) %>%
  filter(d_share > 0) %>%
  mutate(log_d_share = log(d_share)) %>%
  filter(grad_y < 2024)

# Get log Alabama shares (outside good)
shares <- shares %>%
  filter(state == 'Alabama') %>%
  select(log_d_share, grad_y) %>%
  rename(log_al_share = log_d_share) %>%
  right_join(shares) %>%
  mutate(diff_log_d_share = log_d_share - log_al_share)

# Attach unemployment rates (and migration rates)
shares <- shares %>%
  left_join(rename(pull_factors, grad_y = y))

# Join origin and destination data
join <- dest %>%
  select(-c('first_name', 'last_name', 'grad_y', 'fullname', 'field', 'user_location')) %>%
  rename(d_state = state) %>%
  inner_join(origin, by = 'user_id') %>%
  rename(o_state = state) %>%
  filter(grad_y < 2024) %>%
  ungroup()

# Build zero-inclusive in-state destination panel
full_cohorts <- comm %>%
  ungroup() %>%
  filter(grad_y < 2024) %>%
  group_by(grad_y) %>%
  summarize(N_full = n(),
            N_AL_full = sum(state == 'Alabama'))

comm_origins <- comm %>%
  ungroup() %>%
  filter(grad_y < 2024,
         state != 'Alabama') %>%
  count(grad_y, d_state = state, name = 'N_origin_comm') %>%
  left_join(full_cohorts %>% select(grad_y, N_full), by = 'grad_y') %>%
  mutate(o_share_comm = N_origin_comm / N_full * 100) %>%
  select(grad_y, d_state, N_origin_comm, o_share_comm)

comm_origins_offsets <- comm_origins %>%
  transmute(origin_grad_y = grad_y,
            d_state,
            N_origin_comm_offset = N_origin_comm,
            o_share_comm_offset = o_share_comm)

comm_origin_years <- sort(unique(comm_origins_offsets$origin_grad_y))

ipeds_cohorts <- iv %>%
  group_by(grad_y) %>%
  summarize(N_full_ipeds = sum(enroll, na.rm = T),
            .groups = 'drop')

ipeds_origins <- iv %>%
  filter(grad_y < 2024,
         state != 'Alabama') %>%
  left_join(ipeds_cohorts, by = 'grad_y') %>%
  transmute(grad_y,
            d_state = state,
            N_origin_ipeds = enroll,
            N_full_ipeds,
            o_share_ipeds = N_origin_ipeds / N_full_ipeds * 100)

ipeds_origins_offsets <- iv %>%
  filter(state != 'Alabama') %>%
  left_join(ipeds_cohorts, by = 'grad_y') %>%
  transmute(origin_grad_y = grad_y,
            d_state = state,
            N_origin_ipeds_offset = enroll,
            N_full_ipeds_offset = N_full_ipeds,
            o_share_ipeds_offset = N_origin_ipeds_offset / N_full_ipeds_offset * 100)

ipeds_origin_years <- sort(unique(ipeds_origins_offsets$origin_grad_y))

shares <- shares %>%
  left_join(iv %>%
              filter(grad_y < 2024) %>%
              left_join(ipeds_cohorts, by = 'grad_y') %>%
              transmute(grad_y,
                        state,
                        o_share_ipeds = enroll / N_full_ipeds * 100),
            by = c('grad_y', 'state'))

linked_origins <- origin %>%
  ungroup() %>%
  filter(state != 'Alabama',
         grad_y < 2024) %>%
  count(grad_y, d_state = state, name = 'N_origin_linked')

linked_origins_offsets <- linked_origins %>%
  transmute(origin_grad_y = grad_y,
            d_state,
            N_origin_linked_offset = N_origin_linked)

linked_origin_years <- sort(unique(linked_origins_offsets$origin_grad_y))

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
  select(d_state, grad_y, ur, net_rate) %>%
  distinct(d_state, grad_y, .keep_all = TRUE)

share_ins <- expand_grid(d_state = states$state[states$state != 'Alabama'],
                         grad_y = sort(unique(full_cohorts$grad_y))) %>%
  left_join(full_cohorts) %>%
  left_join(comm_origins) %>%
  left_join(ipeds_origins) %>%
  left_join(linked_origins) %>%
  left_join(obs_al_cohorts) %>%
  left_join(obs_al_dest) %>%
  left_join(obs_al_home) %>%
  left_join(pull_factors_dest) %>%
  mutate(N_origin_comm = if_else(is.na(N_origin_comm), 0L, N_origin_comm),
         N_origin_ipeds = if_else(is.na(N_origin_ipeds), 0, N_origin_ipeds),
         N_origin_linked = if_else(is.na(N_origin_linked), 0L, N_origin_linked),
         N_dest_obs = if_else(is.na(N_dest_obs), 0L, N_dest_obs),
         N_home_obs = if_else(is.na(N_home_obs), 0L, N_home_obs),
         ur = if_else(is.na(ur), 0, ur),
         net_rate = if_else(is.na(net_rate), 0, net_rate),
         p_dest = N_dest_obs / M_AL_obs,
         cond_leave_share = if_else(M_AL_obs > N_home_obs,
                                    N_dest_obs / (M_AL_obs - N_home_obs),
                                    NA_real_),
         d_share = p_dest,
         o_share_comm = if_else(is.na(o_share_comm), 0, o_share_comm),
         o_share = o_share_comm,
         o_share_ipeds = if_else(N_full_ipeds > 0, o_share_ipeds, NA_real_),
         log_d_share = if_else(p_dest > 0, log(p_dest), NA_real_),
         log_al_share = if_else(N_home_obs > 0, log(N_home_obs / M_AL_obs), NA_real_),
         diff_log_d_share = log_d_share - log_al_share,
         N_dest = p_dest * N_AL_full,
         N_origin = N_origin_ipeds,
         match_prob = M_AL_obs / N_AL_full,
         N_dest_count = if (count_source == 'linked_only') as.numeric(N_dest_obs) else N_dest,
         N_origin_count = if (count_source == 'linked_only') as.numeric(N_origin_linked) else as.numeric(N_origin_ipeds)) %>%
  filter(N_full > 0,
         M_AL_obs > 0) %>%
  left_join(comm_origins_offsets %>%
              transmute(d_state,
                        grad_y = origin_grad_y + 1,
                        N_origin_comm_lag = N_origin_comm_offset,
                        o_share_comm_lag = o_share_comm_offset)) %>%
  left_join(comm_origins_offsets %>%
              transmute(d_state,
                        grad_y = origin_grad_y + 2,
                        N_origin_comm_lag2 = N_origin_comm_offset,
                        o_share_comm_lag2 = o_share_comm_offset)) %>%
  left_join(comm_origins_offsets %>%
              transmute(d_state,
                        grad_y = origin_grad_y - 1,
                        N_origin_comm_lead = N_origin_comm_offset,
                        o_share_comm_lead = o_share_comm_offset)) %>%
  left_join(comm_origins_offsets %>%
              transmute(d_state,
                        grad_y = origin_grad_y - 2,
                        N_origin_comm_lead2 = N_origin_comm_offset,
                        o_share_comm_lead2 = o_share_comm_offset)) %>%
  left_join(ipeds_origins_offsets %>%
              transmute(d_state,
                        grad_y = origin_grad_y + 1,
                        N_origin_ipeds_lag = N_origin_ipeds_offset,
                        o_share_ipeds_lag = o_share_ipeds_offset)) %>%
  left_join(ipeds_origins_offsets %>%
              transmute(d_state,
                        grad_y = origin_grad_y + 2,
                        N_origin_ipeds_lag2 = N_origin_ipeds_offset,
                        o_share_ipeds_lag2 = o_share_ipeds_offset)) %>%
  left_join(ipeds_origins_offsets %>%
              transmute(d_state,
                        grad_y = origin_grad_y - 1,
                        N_origin_ipeds_lead = N_origin_ipeds_offset,
                        o_share_ipeds_lead = o_share_ipeds_offset)) %>%
  left_join(ipeds_origins_offsets %>%
              transmute(d_state,
                        grad_y = origin_grad_y - 2,
                        N_origin_ipeds_lead2 = N_origin_ipeds_offset,
                        o_share_ipeds_lead2 = o_share_ipeds_offset)) %>%
  left_join(linked_origins_offsets %>%
              transmute(d_state,
                        grad_y = origin_grad_y + 1,
                        N_origin_linked_lag = N_origin_linked_offset)) %>%
  left_join(linked_origins_offsets %>%
              transmute(d_state,
                        grad_y = origin_grad_y + 2,
                        N_origin_linked_lag2 = N_origin_linked_offset)) %>%
  left_join(linked_origins_offsets %>%
              transmute(d_state,
                        grad_y = origin_grad_y - 1,
                        N_origin_linked_lead = N_origin_linked_offset)) %>%
  left_join(linked_origins_offsets %>%
              transmute(d_state,
                        grad_y = origin_grad_y - 2,
                        N_origin_linked_lead2 = N_origin_linked_offset)) %>%
  mutate(N_origin_comm_lag = if_else(is.na(N_origin_comm_lag) & ((grad_y - 1) %in% comm_origin_years), 0L, N_origin_comm_lag),
         N_origin_comm_lag2 = if_else(is.na(N_origin_comm_lag2) & ((grad_y - 2) %in% comm_origin_years), 0L, N_origin_comm_lag2),
         N_origin_comm_lead = if_else(is.na(N_origin_comm_lead) & ((grad_y + 1) %in% comm_origin_years), 0L, N_origin_comm_lead),
         N_origin_comm_lead2 = if_else(is.na(N_origin_comm_lead2) & ((grad_y + 2) %in% comm_origin_years), 0L, N_origin_comm_lead2),
         N_origin_ipeds_lag = if_else(is.na(N_origin_ipeds_lag) & ((grad_y - 1) %in% ipeds_origin_years), 0, N_origin_ipeds_lag),
         N_origin_ipeds_lag2 = if_else(is.na(N_origin_ipeds_lag2) & ((grad_y - 2) %in% ipeds_origin_years), 0, N_origin_ipeds_lag2),
         N_origin_ipeds_lead = if_else(is.na(N_origin_ipeds_lead) & ((grad_y + 1) %in% ipeds_origin_years), 0, N_origin_ipeds_lead),
         N_origin_ipeds_lead2 = if_else(is.na(N_origin_ipeds_lead2) & ((grad_y + 2) %in% ipeds_origin_years), 0, N_origin_ipeds_lead2),
         N_origin_linked_lag = if_else(is.na(N_origin_linked_lag) & ((grad_y - 1) %in% linked_origin_years), 0L, N_origin_linked_lag),
         N_origin_linked_lag2 = if_else(is.na(N_origin_linked_lag2) & ((grad_y - 2) %in% linked_origin_years), 0L, N_origin_linked_lag2),
         N_origin_linked_lead = if_else(is.na(N_origin_linked_lead) & ((grad_y + 1) %in% linked_origin_years), 0L, N_origin_linked_lead),
         N_origin_linked_lead2 = if_else(is.na(N_origin_linked_lead2) & ((grad_y + 2) %in% linked_origin_years), 0L, N_origin_linked_lead2),
         o_share_comm_lag = if_else(is.na(o_share_comm_lag) & ((grad_y - 1) %in% comm_origin_years), 0, o_share_comm_lag),
         o_share_comm_lag2 = if_else(is.na(o_share_comm_lag2) & ((grad_y - 2) %in% comm_origin_years), 0, o_share_comm_lag2),
         o_share_comm_lead = if_else(is.na(o_share_comm_lead) & ((grad_y + 1) %in% comm_origin_years), 0, o_share_comm_lead),
         o_share_comm_lead2 = if_else(is.na(o_share_comm_lead2) & ((grad_y + 2) %in% comm_origin_years), 0, o_share_comm_lead2),
         N_origin_count_lag = if (count_source == 'linked_only') as.numeric(N_origin_linked_lag) else as.numeric(N_origin_ipeds_lag),
         N_origin_count_lag2 = if (count_source == 'linked_only') as.numeric(N_origin_linked_lag2) else as.numeric(N_origin_ipeds_lag2),
         N_origin_count_lead = if (count_source == 'linked_only') as.numeric(N_origin_linked_lead) else as.numeric(N_origin_ipeds_lead),
         N_origin_count_lead2 = if (count_source == 'linked_only') as.numeric(N_origin_linked_lead2) else as.numeric(N_origin_ipeds_lead2))

# Attach Crossan's leave-state-out market exposure instrument
shares <- market_iv %>%
  select(grad_y, state, z_lso) %>%
  right_join(shares, by = c('grad_y', 'state'))

share_ins <- market_iv %>%
  select(grad_y, state, z_lso) %>%
  rename(d_state = state) %>%
  right_join(share_ins, by = c('grad_y', 'd_state'))

share_ins_iv <- share_ins %>%
  filter(grad_y %in% paper_years,
         !is.na(z_lso))

if (count_source == 'linked_only') {
  stopifnot(nrow(share_ins_iv) == 850)
  stopifnot(n_distinct(share_ins_iv$d_state) == 50)
  stopifnot(sum(is.na(share_ins_iv$z_lso)) == 0)
}

# Log-share-difference TWFE -----------------------------------------------------

logit_data <- share_ins_iv %>%
  filter(N_dest_obs > 0,
         N_home_obs > 0) %>%
  left_join(full_cohorts %>%
              transmute(grad_y = grad_y + 1,
                        N_full_lag = N_full),
            by = 'grad_y') %>%
  left_join(full_cohorts %>%
              transmute(grad_y = grad_y - 1,
                        N_full_lead = N_full),
            by = 'grad_y') %>%
  left_join(ipeds_cohorts %>%
              transmute(grad_y = grad_y + 1,
                        N_full_ipeds_lag = N_full_ipeds),
            by = 'grad_y') %>%
  left_join(ipeds_cohorts %>%
              transmute(grad_y = grad_y - 1,
                        N_full_ipeds_lead = N_full_ipeds),
            by = 'grad_y') %>%
  mutate(o_share_model = if (origin_share_source == 'comm') o_share_comm else o_share_ipeds,
         o_share_model_lag = if (origin_share_source == 'comm') o_share_comm_lag else o_share_ipeds_lag,
         o_share_model_lead = if (origin_share_source == 'comm') o_share_comm_lead else o_share_ipeds_lead,
         N_share_model = if (origin_share_source == 'comm') N_full else N_full_ipeds,
         N_share_model_lag = if (origin_share_source == 'comm') N_full_lag else N_full_ipeds_lag,
         N_share_model_lead = if (origin_share_source == 'comm') N_full_lead else N_full_ipeds_lead)

logit_multiplier <- function(data) {
  weights <- data %>%
    group_by(d_state) %>%
    summarize(origin_weight = sum(o_share_model, na.rm = T),
              .groups = 'drop') %>%
    mutate(delta_students = 100 * origin_weight / sum(origin_weight))

  data %>%
    left_join(weights, by = 'd_state') %>%
    mutate(p_al = N_home_obs / M_AL_obs,
           delta_o_share = 100 * delta_students / N_share_model) %>%
    group_by(grad_y) %>%
    summarize(multiplier = first(N_AL_full) * first(p_al) * sum(p_dest * delta_o_share),
              .groups = 'drop') %>%
    summarize(multiplier = mean(multiplier)) %>%
    pull(multiplier)
}

logit_term_multiplier <- function(data, term) {
  denom <- if (grepl('lag', term)) {
    data$N_share_model_lag
  } else if (grepl('lead', term)) {
    data$N_share_model_lead
  } else {
    data$N_share_model
  }

  weights <- data %>%
    group_by(d_state) %>%
    summarize(origin_weight = sum(.data[[term]], na.rm = T),
              .groups = 'drop') %>%
    mutate(delta_students = 100 * origin_weight / sum(origin_weight))

  data %>%
    left_join(weights, by = 'd_state') %>%
    mutate(p_al = N_home_obs / M_AL_obs,
           delta_o_share = 100 * delta_students / denom) %>%
    group_by(grad_y) %>%
    summarize(multiplier = first(N_AL_full) * first(p_al) * sum(p_dest * delta_o_share),
              .groups = 'drop') %>%
    summarize(multiplier = mean(multiplier)) %>%
    pull(multiplier)
}

coef_row <- function(model, term, multiplier = 1) {
  coefs <- summary(model, robust = T)$coefficients
  hit <- rownames(coefs)[rownames(coefs) == term]
  if (length(hit) == 0) hit <- rownames(coefs)[grepl(term, rownames(coefs), fixed = TRUE)]
  data.frame(coef = coefs[hit[1], 'Estimate'],
             se = coefs[hit[1], 'Cluster s.e.'],
             ame = coefs[hit[1], 'Estimate'] * multiplier,
             ame_se = coefs[hit[1], 'Cluster s.e.'] * abs(multiplier))
}

stars <- function(est, se) {
  p <- 2 * pnorm(-abs(est / se))
  ifelse(p < 0.01, '$^{***}$',
         ifelse(p < 0.05, '$^{**}$',
                ifelse(p < 0.10, '$^{*}$', '')))
}

fmt_est <- function(est, se) {
  paste0(sprintf('%.2f', est), stars(est, se))
}

fmt_se <- function(se) {
  paste0('(', sprintf('%.2f', se), ')')
}

fmt_first_stage <- function(est, se) {
  paste0(sprintf('%.3f', est), stars(est, se))
}

fmt_num <- function(x) {
  sprintf('%.2f', x)
}

make_parts <- function(include_controls, trend_type) {
  rhs <- 'o_share_model'
  controls <- character(0)
  fe <- 'factor(d_state)'

  if (include_controls) controls <- c(controls, 'ur', 'net_rate')
  if (trend_type == 'linear') controls <- c(controls, 'grad_y')
  if (trend_type %in% c('cohort_fe', 'state_trends')) fe <- paste(fe, 'factor(grad_y)', sep = ' + ')
  if (trend_type == 'state_trends') fe <- paste(fe, 'factor(d_state):grad_y', sep = ' + ')

  list(rhs = paste(c(rhs, controls), collapse = ' + '),
       controls = paste(controls, collapse = ' + '),
       fe = fe)
}

fit_logit_ols <- function(data, include_controls = FALSE, trend_type = 'none') {
  parts <- make_parts(include_controls, trend_type)
  fml <- as.formula(paste0('diff_log_d_share ~ ', parts$rhs,
                           ' | ', parts$fe, ' | 0 | d_state'))
  felm(fml, data = data)
}

fit_logit_iv <- function(data, include_controls = FALSE, trend_type = 'none') {
  parts <- make_parts(include_controls, trend_type)
  exog <- ifelse(parts$controls == '', '0', parts$controls)
  fml <- as.formula(paste0('diff_log_d_share ~ ', exog,
                           ' | ', parts$fe,
                           ' | (o_share_model ~ z_lso) | d_state'))
  felm(fml, data = data)
}

fit_first_stage <- function(data, include_controls = FALSE, trend_type = 'none') {
  parts <- make_parts(include_controls, trend_type)
  rhs <- paste(c('z_lso', parts$controls[parts$controls != '']), collapse = ' + ')
  fml <- as.formula(paste0('o_share_model ~ ', rhs,
                           ' | ', parts$fe, ' | 0 | d_state'))
  felm(fml, data = data)
}

specs <- data.frame(
  spec = c('(1)', '(2)', '(3)', '(4)'),
  label = c('Linear trend',
            'Controls + linear trend',
            'Cohort FE',
            'Controls + cohort FE'),
  include_controls = c(FALSE, TRUE, FALSE, TRUE),
  trend_type = c('linear', 'linear', 'cohort_fe', 'cohort_fe'),
  stringsAsFactors = FALSE
)

estimate_one <- function(i) {
  s <- specs[i, ]
  ols <- fit_logit_ols(logit_data, s$include_controls, s$trend_type)
  iv <- fit_logit_iv(logit_data, s$include_controls, s$trend_type)
  fs <- fit_first_stage(logit_data, s$include_controls, s$trend_type)
  mult <- logit_multiplier(logit_data)
  ols_row <- coef_row(ols, 'o_share_model', mult)
  iv_row <- coef_row(iv, 'o_share_model', mult)
  fs_row <- coef_row(fs, 'z_lso')

  data.frame(spec = s$spec,
             label = s$label,
             ols_ame = ols_row$ame,
             ols_se = ols_row$ame_se,
             iv_ame = iv_row$ame,
             iv_se = iv_row$ame_se,
             first_stage = fs_row$coef,
             first_stage_se = fs_row$se,
             first_stage_f = (fs_row$coef / fs_row$se)^2,
             n = nobs(ols))
}

twfe_results <- bind_rows(lapply(seq_len(nrow(specs)), estimate_one))

print(twfe_results, row.names = FALSE)

latex_rows <- c(
  paste0('        OLS & ', paste(fmt_est(twfe_results$ols_ame, twfe_results$ols_se), collapse = ' & '), ' \\\\'),
  paste0('        & ', paste(fmt_se(twfe_results$ols_se), collapse = ' & '), ' \\\\'),
  paste0('        IV & ', paste(fmt_est(twfe_results$iv_ame, twfe_results$iv_se), collapse = ' & '), ' \\\\'),
  paste0('        & ', paste(fmt_se(twfe_results$iv_se), collapse = ' & '), ' \\\\'),
  paste0('        First stage & ', paste(fmt_first_stage(twfe_results$first_stage, twfe_results$first_stage_se), collapse = ' & '), ' \\\\'),
  paste0('        First-stage F-statistic & ', paste(fmt_num(twfe_results$first_stage_f), collapse = ' & '), ' \\\\'),
  paste0('        $N$ & ', paste(twfe_results$n, collapse = ' & '), ' \\\\')
)

latex_twfe_table <- paste(c(
  '\\begin{table}[!htbp]',
  '    \\centering',
  '    \\caption{Log-Share-Difference Spillover Estimates}',
  '    \\resizebox{\\textwidth}{!}{',
  '    \\begin{tabular}{lcccc}',
  '        \\toprule',
  '        & \\multicolumn{4}{c}{Average Marginal Effect of 100 OOS Students} \\\\',
  '        \\cmidrule(lr){2-5}',
  '        & \\multicolumn{2}{c}{Linear Cohort Trend} & \\multicolumn{2}{c}{Cohort Fixed Effects} \\\\',
  '        \\cmidrule(lr){2-3} \\cmidrule(lr){4-5}',
  paste0('        & ', paste(specs$spec, collapse = ' & '), ' \\\\'),
  '        \\midrule',
  latex_rows,
  '        \\midrule',
  paste0('        Controls & ', paste(ifelse(specs$include_controls, 'Yes', 'No'), collapse = ' & '), ' \\\\'),
  paste0('        Linear cohort trend & ', paste(ifelse(specs$trend_type == 'linear', 'Yes', 'No'), collapse = ' & '), ' \\\\'),
  paste0('        Cohort fixed effects & ', paste(ifelse(specs$trend_type %in% c('cohort_fe', 'state_trends'), 'Yes', 'No'), collapse = ' & '), ' \\\\'),
  '        \\midrule',
  paste0('        \\multicolumn{5}{l}{\\parbox{0.98\\textwidth}{Note: Entries in the OLS and IV rows are average marginal effects interpreted as in-state students leaving Alabama per 100 additional out-of-state students. The treatment is the ', origin_share_label, ' $d^o$. All specifications include destination-state fixed effects, omit 2020, and cluster standard errors at the state level. $^*$, $^{**}$, and $^{***}$ denote significance at the 10\\%, 5\\%, and 1\\% levels.}} \\\\'),
  '        \\bottomrule',
  '    \\end{tabular}}',
  '    \\label{tbl:spillovers_twfe_log_share}',
  '\\end{table}'
), collapse = '\n')

dir.create('tables', showWarnings = FALSE)
writeLines(latex_twfe_table, 'tables/analysis_spillovers_twfe_log_share.tex')
cat(latex_twfe_table, '\n')

# Log-share-difference OLS dynamics -------------------------------------------

dynamic_data <- logit_data %>%
  filter(!is.na(o_share_model),
         !is.na(o_share_model_lag),
         !is.na(o_share_model_lead))

fit_dynamic_ols <- function(data, terms, fe_type) {
  controls <- c('ur', 'net_rate')
  fe <- 'factor(d_state)'
  if (fe_type == 'linear') controls <- c(controls, 'grad_y')
  if (fe_type == 'cohort_fe') fe <- paste(fe, 'factor(grad_y)', sep = ' + ')
  rhs <- paste(c(terms, controls), collapse = ' + ')
  fml <- as.formula(paste0('diff_log_d_share ~ ', rhs,
                           ' | ', fe, ' | 0 | d_state'))
  felm(fml, data = data)
}

dynamic_specs <- data.frame(
  spec = c('(1)', '(2)', '(3)'),
  terms = I(list('o_share_model',
                 c('o_share_model', 'o_share_model_lag'),
                 c('o_share_model', 'o_share_model_lag', 'o_share_model_lead'))),
  stringsAsFactors = FALSE
)

dynamic_one <- function(fe_type, i) {
  s <- dynamic_specs[i, ]
  model <- fit_dynamic_ols(dynamic_data, unlist(s$terms), fe_type)
  bind_rows(lapply(c('o_share_model', 'o_share_model_lag', 'o_share_model_lead'), function(term) {
    if (!(term %in% unlist(s$terms))) {
      data.frame(fe_type = fe_type,
                 spec = s$spec,
                 term = term,
                 ame = NA_real_,
                 ame_se = NA_real_,
                 n = nobs(model))
    } else {
      mult <- logit_term_multiplier(dynamic_data, term)
      row <- coef_row(model, term, mult)
      data.frame(fe_type = fe_type,
                 spec = s$spec,
                 term = term,
                 ame = row$ame,
                 ame_se = row$ame_se,
                 n = nobs(model))
    }
  }))
}

dynamic_ols <- bind_rows(lapply(c('linear', 'cohort_fe'), function(fe_type) {
  bind_rows(lapply(seq_len(nrow(dynamic_specs)), function(i) dynamic_one(fe_type, i)))
}))

print(dynamic_ols, row.names = FALSE)

fmt_dyn_values <- function(data, fe_type, term, value) {
  sapply(dynamic_specs$spec, function(spec) {
    row <- data %>% filter(fe_type == !!fe_type, spec == !!spec, term == !!term)
    if (nrow(row) == 0) return('---')
    if (value == 'est') {
      if (is.na(row$ame)) return('---')
      fmt_est(row$ame, row$ame_se)
    } else {
      if (is.na(row$ame_se)) return('---')
      fmt_se(row$ame_se)
    }
  }) %>%
    paste(collapse = ' & ')
}

dynamic_rows <- function(data) {
  labels <- c('o_share_model' = '$d^o_{j,c}$',
              'o_share_model_lag' = '$d^o_{j,c-1}$',
              'o_share_model_lead' = '$d^o_{j,c+1}$')
  rows <- character(0)
  for (term in names(labels)) {
    rows <- c(rows,
              paste0('        ', labels[[term]], ' & ',
                     fmt_dyn_values(data, 'linear', term, 'est'), ' & ',
                     fmt_dyn_values(data, 'cohort_fe', term, 'est'), ' \\\\'),
              paste0('        & ',
                     fmt_dyn_values(data, 'linear', term, 'se'), ' & ',
                     fmt_dyn_values(data, 'cohort_fe', term, 'se'), ' \\\\'))
  }
  rows
}

dynamic_n_row <- function(data) {
  vals <- c(
    sapply(dynamic_specs$spec, function(spec) {
      data %>% filter(fe_type == 'linear', spec == !!spec) %>% pull(n) %>% first()
    }),
    sapply(dynamic_specs$spec, function(spec) {
      data %>% filter(fe_type == 'cohort_fe', spec == !!spec) %>% pull(n) %>% first()
    })
  )
  paste0('        $N$ & ', paste(vals, collapse = ' & '), ' \\\\')
}

latex_dynamic_table <- paste(c(
  '\\begin{table}[!htbp]',
  '    \\centering',
  '    \\caption{Log-Share-Difference Spillover Estimates with Adjacent Cohort Shares}',
  '    \\resizebox{\\textwidth}{!}{',
  '    \\begin{tabular}{lcccccc}',
  '        \\toprule',
  '        & \\multicolumn{6}{c}{Average Marginal Effect of 100 OOS Students} \\\\',
  '        \\cmidrule(lr){2-7}',
  '        & \\multicolumn{3}{c}{Linear Cohort Trend} & \\multicolumn{3}{c}{Cohort Fixed Effects} \\\\',
  '        \\cmidrule(lr){2-4} \\cmidrule(lr){5-7}',
  paste0('        & ', paste(dynamic_specs$spec, collapse = ' & '), ' & ',
         paste(dynamic_specs$spec, collapse = ' & '), ' \\\\'),
  '        \\midrule',
  dynamic_rows(dynamic_ols),
  '        \\midrule',
  dynamic_n_row(dynamic_ols),
  '        \\midrule',
  paste0('        \\multicolumn{7}{l}{\\parbox{0.98\\textwidth}{Note: Entries are OLS average marginal effects interpreted as in-state students leaving Alabama per 100 additional out-of-state students. All specifications use the common sample with nonmissing current, lagged, and lead ', origin_share_label, 's, include destination-state fixed effects, unemployment controls, net-migration controls, and omit 2020. Standard errors are clustered at the state level.}} \\\\'),
  '        \\bottomrule',
  '    \\end{tabular}}',
  '    \\label{tbl:spillovers_twfe_log_share_dynamics}',
  '\\end{table}'
), collapse = '\n')

writeLines(latex_dynamic_table, 'tables/analysis_spillovers_twfe_log_share_dynamics.tex')
cat(latex_dynamic_table, '\n')
