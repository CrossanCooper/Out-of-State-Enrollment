#////////////////////////////////////////////////////////////////////////////////
# Filename: analysis_main_iv.R
# Author: Ryan Haygood
# Date: 9/27/26
# Description: Consolidated structural-IV analysis. Estimates the preferred
# two-step destination-choice IV model, runs diagnostics and counterfactuals,
# optionally propagates first-step uncertainty with the multiplier bootstrap,
# and estimates major-specific preference heterogeneity.
#////////////////////////////////////////////////////////////////////////////////
# Preferred two-step destination-choice IV -----------------------------------

# Setup -------------------------------------------------------------------------

source('C:/Users/ryanh/OneDrive/Documents/Grad School/Research/Out-of-State-Enrollment/code/setup.R')

if (!requireNamespace('fixest', quietly = TRUE)) {
  stop('Package fixest is required for the grouped multinomial-Poisson models.')
}
if (!requireNamespace('ggrepel', quietly = TRUE)) {
  stop('Package ggrepel is required for the IV diagnostic figures.')
}
library(fixest)

paper_years <- sort(setdiff(2006:2023, 2020))
base_year <- min(paper_years)

# Helpers -----------------------------------------------------------------------

clustered_coef <- function(model, term_pattern) {
  coefs <- summary(model, robust = TRUE)$coefficients
  hit <- grep(term_pattern, rownames(coefs))
  if (length(hit) != 1) {
    stop('Expected one coefficient matching "', term_pattern,
         '"; found ', length(hit), '.')
  }
  data.frame(
    estimate = coefs[hit, 'Estimate'],
    se = coefs[hit, 'Cluster s.e.'],
    row.names = NULL
  )
}

unclustered_coef <- function(model, term_pattern) {
  coefs <- summary(model)$coefficients
  hit <- grep(term_pattern, rownames(coefs))
  if (length(hit) != 1) {
    stop('Expected one coefficient matching "', term_pattern,
         '"; found ', length(hit), '.')
  }
  data.frame(
    estimate = coefs[hit, 'Estimate'],
    se = coefs[hit, 'Std. Error'],
    row.names = NULL
  )
}

fixest_coef <- function(model, term) {
  tab <- coeftable(model)
  if (!(term %in% rownames(tab))) {
    stop('Coefficient "', term, '" not found in fixest model.')
  }
  data.frame(
    estimate = tab[term, 'Estimate'],
    se = tab[term, 'Std. Error'],
    row.names = NULL
  )
}

# Build individual origin-destination sample -----------------------------------

# Read cleaned first-job destinations. The saved RDS retains grouping metadata,
# so ungroup before any counting or joins.
dest <- readRDS(paste0(pathHome, 'revelio_data/first_spell_join.rds')) %>%
  ungroup() %>%
  filter(country == 'United States') %>%
  select(user_id, d_state = state)

# Make state abbreviation/name crosswalk.
states <- data.frame(
  originState = c(state.abb, 'DC'),
  state = c(state.name, 'Washington, D.C.')
)

# Read and clean states of origin.
origin <- read.csv(paste0(pathHome, 'data/linked_commencement_revelio_profile_data.csv')) %>%
  filter(!is.na(Year)) %>%
  mutate(originState = gsub(' ', '', originState)) %>%
  left_join(states, by = 'originState') %>%
  filter(!is.na(state)) %>%
  transmute(user_id,
            grad_y = Year,
            o_state = state)

# Match origin and destination records and use the same controlled-model sample
# as Column (6) of the current choice-model table.
join <- dest %>%
  inner_join(origin, by = 'user_id') %>%
  filter(grad_y %in% paper_years) %>%
  ungroup()

stopifnot(nrow(join) == n_distinct(join$user_id))
stopifnot(n_distinct(join$d_state) == nrow(states))

# Destination-cohort characteristics -------------------------------------------

# Construct Commencement-record origin shares, measured in percentage points.
comm <- read.csv(paste0(pathHome, 'data/all_alabama_data.csv')) %>%
  mutate(originState = gsub(' ', '', Origin.State)) %>%
  left_join(states, by = 'originState') %>%
  filter(!is.na(state), Year %in% paper_years) %>%
  rename(grad_y = Year) %>%
  ungroup()

cohort_sizes <- comm %>%
  group_by(grad_y) %>%
  summarize(N_cohort = n(),
            N_AL_cohort = sum(state == 'Alabama'),
            .groups = 'drop')

shares <- comm %>%
  count(grad_y, state, name = 'N_origin') %>%
  left_join(cohort_sizes, by = 'grad_y') %>%
  mutate(o_share = 100 * N_origin / N_cohort) %>%
  select(grad_y, state, o_share, N_origin, N_cohort, N_AL_cohort)

# Attach state labor-market controls.
pull_factors <- readRDS(paste0(pathHome, 'data/pull_factors.rds')) %>%
  ungroup() %>%
  mutate(state = if_else(state == 'District of Columbia',
                         'Washington, D.C.', state)) %>%
  transmute(state,
            grad_y = y,
            unemp = ur,
            net_mig = net_rate)

# Bring in the preferred leave-state-out shift-share instrument.
market_iv <- read_csv(
  paste0(pathHome, 'data/market_iv/data/market_iv_panel.csv'),
  show_col_types = FALSE
) %>%
  transmute(originState = origin_state,
            grad_y = grad_year,
            z_lso = z_peer_non_alabama_count_growth_lso) %>%
  left_join(states, by = 'originState') %>%
  transmute(d_state = state, grad_y, z_lso)

# Collapse the MNL likelihood ---------------------------------------------------

# The baseline model's covariates are constant within origin-state/cohort/
# destination cells. Collapsing to choice counts therefore preserves its exact
# multinomial likelihood while avoiding a roughly two-million-row long panel.
choice_counts <- join %>%
  count(grad_y, o_state, alternative = d_state, name = 'n_choice')

origin_cohorts <- join %>%
  distinct(grad_y, o_state)

# The current exogenous model has destination-state fixed effects, so all 51
# alternatives remain in every cohort's choice set, including state-years with
# zero realized choices.
choice_cells_full <- crossing(
  origin_cohorts,
  alternative = states$state
) %>%
  left_join(choice_counts,
            by = c('grad_y', 'o_state', 'alternative')) %>%
  mutate(n_choice = replace_na(n_choice, 0L))

# With saturated destination-by-cohort effects, an alternative that nobody chose
# in a cohort has an MLE of minus infinity. Retain all origin-cell zeroes, but
# restrict the destination support in each cohort to alternatives with at least
# one observed choice in the matched structural sample.
destination_support <- join %>%
  count(grad_y, alternative = d_state, name = 'N_dest')

choice_cells_saturated <- origin_cohorts %>%
  inner_join(destination_support, by = 'grad_y', relationship = 'many-to-many') %>%
  select(grad_y, o_state, alternative) %>%
  left_join(choice_counts,
            by = c('grad_y', 'o_state', 'alternative')) %>%
  mutate(n_choice = replace_na(n_choice, 0L))

# Attach regressors and construct both in-/out-of-state interactions.
add_choice_covariates <- function(data) {
  data %>%
    left_join(shares,
              by = c('grad_y', 'alternative' = 'state')) %>%
    left_join(pull_factors,
              by = c('grad_y', 'alternative' = 'state')) %>%
    mutate(o_share = replace_na(o_share, 0),
           o_share = if_else(alternative == 'Alabama', 0, o_share),
           is_oos = as.integer(o_state != 'Alabama'),
           is_in_state = 1L - is_oos,
           home_oos = as.integer(is_oos == 1 & o_state == alternative),
           home_ins = as.integer(is_in_state == 1 & alternative == 'Alabama'),
           o_share_oos = is_oos * o_share,
           o_share_ins = is_in_state * o_share,
           o_share_outdiff = is_oos * o_share,
           al_oos = as.integer(is_oos == 1 & alternative == 'Alabama'),
           al_ins = as.integer(is_in_state == 1 & alternative == 'Alabama'),
           origin_cohort = interaction(o_state, grad_y, drop = TRUE),
           dest_cohort = interaction(alternative, grad_y,
                                     drop = TRUE, sep = '__'))
}

choice_cells_full <- add_choice_covariates(choice_cells_full)
choice_cells_saturated <- add_choice_covariates(choice_cells_saturated)

stopifnot(sum(choice_cells_full$n_choice) == nrow(join))
stopifnot(sum(choice_cells_saturated$n_choice) == nrow(join))
stopifnot(!anyNA(choice_cells_full[c('unemp', 'net_mig')]))
stopifnot(!anyNA(choice_cells_saturated[c('unemp', 'net_mig')]))

# Current one-step estimator: origin shares treated as exogenous ----------------

# This is the grouped-Poisson equivalent of the current preferred MNL model:
# destination-state effects, in-/out-of-state home and origin-share parameters,
# flexible Alabama cohort utilities by in-state status, and pull-factor controls.
model_current_exogenous <- fepois(
  n_choice ~ home_oos +
    home_ins +
    o_share_oos +
    o_share_ins +
    unemp +
    net_mig +
    i(grad_y, al_oos, ref = base_year) +
    i(grad_y, al_ins, ref = base_year) |
    origin_cohort + alternative,
  data = choice_cells_full,
  vcov = ~o_state,
  notes = FALSE
)

# Saturated first step ----------------------------------------------------------

# Destination-by-cohort effects absorb the common (in-state baseline) response
# to origin shares. The estimable share coefficient is the out-of-state minus
# in-state response. Flexible OOS-by-Alabama cohort effects preserve the preferred
# model's type-specific outside-good utility.
model_saturated_first_step <- fepois(
  n_choice ~ home_oos +
    home_ins +
    o_share_outdiff +
    i(grad_y, al_oos, ref = base_year) |
    origin_cohort + dest_cohort,
  data = choice_cells_saturated,
  vcov = ~o_state,
  notes = FALSE
)

pi_out_minus_in <- fixest_coef(model_saturated_first_step,
                               'o_share_outdiff')

# Extract and normalize destination-by-cohort fixed effects ---------------------

dest_cohort_fe <- fixef(model_saturated_first_step,
                        notes = FALSE)[['dest_cohort']]

delta_second_stage <- data.frame(
  dest_cohort = names(dest_cohort_fe),
  delta_hat = unname(dest_cohort_fe),
  row.names = NULL
) %>%
  mutate(grad_y = as.integer(str_extract(dest_cohort, '[0-9]{4}$')),
         d_state = str_remove(dest_cohort, '__[0-9]{4}$')) %>%
  group_by(grad_y) %>%
  mutate(delta_rel = delta_hat - delta_hat[d_state == 'Alabama']) %>%
  ungroup() %>%
  filter(d_state != 'Alabama') %>%
  left_join(shares %>%
              transmute(d_state = state,
                        grad_y,
                        o_share),
            by = c('d_state', 'grad_y')) %>%
  mutate(o_share = replace_na(o_share, 0)) %>%
  left_join(pull_factors %>% rename(d_state = state),
            by = c('d_state', 'grad_y')) %>%
  left_join(market_iv,
            by = c('d_state', 'grad_y'))

stopifnot(all(delta_second_stage$grad_y %in% paper_years))
stopifnot(n_distinct(delta_second_stage$d_state) == 50)
stopifnot(!('N_dest' %in% names(delta_second_stage)))
stopifnot(!anyNA(delta_second_stage[c('delta_rel', 'o_share',
                                      'unemp', 'net_mig', 'z_lso')]))

# Second-step OLS and IV --------------------------------------------------------

# The OLS projection is the apples-to-apples non-IV counterpart to the IV
# projection. Both use the same destination-by-cohort observations, controls,
# and fixed effects. Each cell enters once: destination counts do not enter the
# structural moments or the regression weights.
model_second_step_ols <- felm(
  delta_rel ~ o_share + unemp + net_mig |
    factor(d_state) + factor(grad_y),
  data = delta_second_stage
)

model_second_step_iv <- felm(
  delta_rel ~ unemp + net_mig |
    factor(d_state) + factor(grad_y) |
    (o_share ~ z_lso),
  data = delta_second_stage
)

model_second_step_first_stage <- felm(
  o_share ~ z_lso + unemp + net_mig |
    factor(d_state) + factor(grad_y),
  data = delta_second_stage
)

stopifnot(nobs(model_second_step_ols) == nrow(delta_second_stage))
stopifnot(nobs(model_second_step_iv) == nrow(delta_second_stage))

# Estimate otherwise identical versions solely to obtain standard errors
# clustered by destination state, allowing serial dependence across cohorts.
model_second_step_ols_destination_clustered <- felm(
  delta_rel ~ o_share + unemp + net_mig |
    factor(d_state) + factor(grad_y) | 0 | d_state,
  data = delta_second_stage
)

model_second_step_iv_destination_clustered <- felm(
  delta_rel ~ unemp + net_mig |
    factor(d_state) + factor(grad_y) |
    (o_share ~ z_lso) | d_state,
  data = delta_second_stage
)

model_second_step_first_stage_destination_clustered <- felm(
  o_share ~ z_lso + unemp + net_mig |
    factor(d_state) + factor(grad_y) | 0 | d_state,
  data = delta_second_stage
)

gamma_in_current <- fixest_coef(model_current_exogenous, 'o_share_ins')
gamma_out_current <- fixest_coef(model_current_exogenous, 'o_share_oos')
gamma_in_ols_unclustered <- unclustered_coef(model_second_step_ols,
                                             '^o_share$')
gamma_in_iv_unclustered <- unclustered_coef(model_second_step_iv,
                                            'o_share\\(fit\\)')
gamma_in_ols_destination_clustered <- clustered_coef(
  model_second_step_ols_destination_clustered,
  '^o_share$'
)
gamma_in_iv_destination_clustered <- clustered_coef(
  model_second_step_iv_destination_clustered,
  'o_share\\(fit\\)'
)
first_stage_z_unclustered <- unclustered_coef(model_second_step_first_stage,
                                              '^z_lso$')
first_stage_z_destination_clustered <- clustered_coef(
  model_second_step_first_stage_destination_clustered,
  '^z_lso$'
)

stopifnot(isTRUE(all.equal(gamma_in_ols_unclustered$estimate,
                           gamma_in_ols_destination_clustered$estimate)))
stopifnot(isTRUE(all.equal(gamma_in_iv_unclustered$estimate,
                           gamma_in_iv_destination_clustered$estimate)))

first_stage_f_unclustered <- (
  first_stage_z_unclustered$estimate / first_stage_z_unclustered$se
)^2
first_stage_f_destination_clustered <- (
  first_stage_z_destination_clustered$estimate /
    first_stage_z_destination_clustered$se
)^2

# Average marginal effects ------------------------------------------------------

# Allocate a one-percentage-point increase in the total OOS origin share across
# states according to their share of OOS students in the linked estimation sample,
# as in analysis_main.R.
origin_weights <- join %>%
  filter(o_state != 'Alabama') %>%
  count(alternative = o_state, name = 'N_origin_linked') %>%
  mutate(origin_weight = N_origin_linked / sum(N_origin_linked)) %>%
  select(alternative, origin_weight)

in_state_cohort_weights <- join %>%
  filter(o_state == 'Alabama') %>%
  count(grad_y, name = 'N_in_state_linked') %>%
  mutate(cohort_weight = N_in_state_linked / sum(N_in_state_linked)) %>%
  select(grad_y, cohort_weight)

# Return the probability multiplier for a one-percentage-point increase and the
# count multiplier for 100 additional OOS students. Multiplying either by gamma_in
# gives the corresponding marginal effect.
ame_multipliers <- function(model, data) {
  pred <- data %>%
    mutate(mu_hat = as.numeric(predict(model, newdata = data,
                                       type = 'response'))) %>%
    filter(o_state == 'Alabama') %>%
    group_by(grad_y) %>%
    mutate(prob = mu_hat / sum(mu_hat)) %>%
    ungroup() %>%
    left_join(origin_weights, by = 'alternative') %>%
    mutate(origin_weight = replace_na(origin_weight, 0))

  by_cohort <- pred %>%
    group_by(grad_y) %>%
    summarize(p_al = prob[alternative == 'Alabama'],
              weighted_destination_prob = sum(
                prob[alternative != 'Alabama'] *
                  origin_weight[alternative != 'Alabama']
              ),
              .groups = 'drop') %>%
    left_join(in_state_cohort_weights, by = 'grad_y') %>%
    left_join(cohort_sizes, by = 'grad_y') %>%
    mutate(one_pp_multiplier =
             100 * p_al * weighted_destination_prob,
           per_100_multiplier =
             N_AL_cohort * p_al * weighted_destination_prob *
             (10000 / N_cohort))

  c(
    one_pp = weighted.mean(by_cohort$one_pp_multiplier,
                           by_cohort$cohort_weight),
    per_100 = mean(by_cohort$per_100_multiplier)
  )
}

current_multipliers <- ame_multipliers(model_current_exogenous,
                                       choice_cells_full)
saturated_multipliers <- ame_multipliers(model_saturated_first_step,
                                         choice_cells_saturated)

# Comparison output -------------------------------------------------------------

comparison_results <- data.frame(
  estimator = c('Current one-step MNL (exogenous shares)',
                'Two-step OLS projection (unweighted cells)',
                'Two-step IV projection (unweighted cells)'),
  gamma_in = c(gamma_in_current$estimate,
               gamma_in_ols_unclustered$estimate,
               gamma_in_iv_unclustered$estimate),
  gamma_out = c(gamma_out_current$estimate,
                gamma_in_ols_unclustered$estimate +
                  pi_out_minus_in$estimate,
                gamma_in_iv_unclustered$estimate +
                  pi_out_minus_in$estimate),
  pi_out_minus_in = c(gamma_out_current$estimate - gamma_in_current$estimate,
                      pi_out_minus_in$estimate,
                      pi_out_minus_in$estimate),
  in_state_ame_1pp = c(gamma_in_current$estimate *
                         current_multipliers[['one_pp']],
                       gamma_in_ols_unclustered$estimate *
                         saturated_multipliers[['one_pp']],
                       gamma_in_iv_unclustered$estimate *
                         saturated_multipliers[['one_pp']]),
  linearized_in_state_leavers_per_100_oos = c(
    gamma_in_current$estimate * current_multipliers[['per_100']],
    gamma_in_ols_unclustered$estimate * saturated_multipliers[['per_100']],
    gamma_in_iv_unclustered$estimate * saturated_multipliers[['per_100']]
  ),
  row.names = NULL
)

second_step_inference <- data.frame(
  estimator = c('Two-step OLS projection',
                'Two-step IV projection'),
  gamma_in = c(gamma_in_ols_unclustered$estimate,
               gamma_in_iv_unclustered$estimate),
  unclustered_se = c(gamma_in_ols_unclustered$se,
                     gamma_in_iv_unclustered$se),
  destination_state_clustered_se = c(
    gamma_in_ols_destination_clustered$se,
    gamma_in_iv_destination_clustered$se
  ),
  row.names = NULL
)

one_step_benchmark_inference <- data.frame(
  estimator = 'Current one-step MNL (exogenous shares)',
  gamma_in = gamma_in_current$estimate,
  origin_state_clustered_se = gamma_in_current$se,
  row.names = NULL
)

estimator_differences <- data.frame(
  comparison = c('Two-step IV minus two-step OLS',
                 'Two-step IV minus current one-step MNL'),
  gamma_in_difference = c(
    gamma_in_iv_unclustered$estimate - gamma_in_ols_unclustered$estimate,
    gamma_in_iv_unclustered$estimate - gamma_in_current$estimate
  ),
  in_state_ame_1pp_difference = c(
    (gamma_in_iv_unclustered$estimate - gamma_in_ols_unclustered$estimate) *
      saturated_multipliers[['one_pp']],
    gamma_in_iv_unclustered$estimate * saturated_multipliers[['one_pp']] -
      gamma_in_current$estimate * current_multipliers[['one_pp']]
  ),
  row.names = NULL
)

first_stage_diagnostics <- data.frame(
  instrument = 'z_peer_non_alabama_count_growth_lso',
  coefficient = first_stage_z_unclustered$estimate,
  unclustered_se = first_stage_z_unclustered$se,
  unclustered_f = first_stage_f_unclustered,
  destination_state_clustered_se = first_stage_z_destination_clustered$se,
  destination_state_clustered_f = first_stage_f_destination_clustered,
  observations = nrow(delta_second_stage),
  destination_clusters = n_distinct(delta_second_stage$d_state),
  row.names = NULL
)

# Residualized IV scatterplots --------------------------------------------------

# Partial y, x, and z separately with respect to exactly the controls and fixed
# effects in the preferred unweighted second-stage IV specification. Regressing
# the resulting residuals through the origin implements the Frisch-Waugh-Lovell
# representation of the first stage and reduced form.
residualize_second_step_variable <- function(variable, data) {
  residual_model <- felm(
    as.formula(paste0(
      variable,
      ' ~ unemp + net_mig | factor(d_state) + factor(grad_y)'
    )),
    data = data
  )
  as.numeric(residuals(residual_model))
}

residualized_iv_data <- delta_second_stage %>%
  transmute(
    d_state,
    grad_y,
    y_tilde = residualize_second_step_variable('delta_rel',
                                                delta_second_stage),
    x_tilde = residualize_second_step_variable('o_share',
                                                delta_second_stage),
    z_tilde = residualize_second_step_variable('z_lso',
                                                delta_second_stage)
  )

residualized_first_stage <- lm(
  x_tilde ~ 0 + z_tilde,
  data = residualized_iv_data
)
residualized_reduced_form <- lm(
  y_tilde ~ 0 + z_tilde,
  data = residualized_iv_data
)

residualized_first_stage_slope <- unname(
  coef(residualized_first_stage)[['z_tilde']]
)
residualized_reduced_form_slope <- unname(
  coef(residualized_reduced_form)[['z_tilde']]
)
residualized_iv_ratio <- (
  residualized_reduced_form_slope / residualized_first_stage_slope
)

# The plotted slopes must reproduce the corresponding partialled-out regression
# coefficients and, because the model is exactly identified, their ratio must
# reproduce the preferred IV point estimate.
stopifnot(
  isTRUE(all.equal(residualized_first_stage_slope,
                   first_stage_z_unclustered$estimate,
                   tolerance = 1e-8)),
  isTRUE(all.equal(residualized_iv_ratio,
                   gamma_in_iv_unclustered$estimate,
                   tolerance = 1e-8))
)

residualized_slope_diagnostics <- data.frame(
  first_stage_coefficient = residualized_first_stage_slope,
  reduced_form_coefficient = residualized_reduced_form_slope,
  reduced_form_over_first_stage = residualized_iv_ratio,
  iv_coefficient = gamma_in_iv_unclustered$estimate,
  observations = nrow(residualized_iv_data),
  row.names = NULL
)

# Label the union of the five highest-leverage cells and the five largest
# studentized-residual cells in each scatterplot. This targets both horizontal
# leverage and visually unusual vertical deviations without changing the fit.
add_diagnostic_plot_statistics <- function(data, model) {
  data %>%
    mutate(
      leverage = as.numeric(hatvalues(model)),
      studentized_residual = as.numeric(rstudent(model)),
      leverage_rank = rank(-leverage, ties.method = 'first'),
      residual_rank = rank(-abs(studentized_residual), ties.method = 'first'),
      diagnostic_label = if_else(
        leverage_rank <= 5L | residual_rank <= 5L,
        paste0(d_state, ', ', grad_y),
        NA_character_
      )
    )
}

first_stage_plot_data <- add_diagnostic_plot_statistics(
  residualized_iv_data,
  residualized_first_stage
)
reduced_form_plot_data <- add_diagnostic_plot_statistics(
  residualized_iv_data,
  residualized_reduced_form
)

iv_diagnostic_figure_directory <- file.path(
  'figures',
  'analysis_mlogit_iv'
)
dir.create(iv_diagnostic_figure_directory,
           recursive = TRUE, showWarnings = FALSE)

first_stage_residualized_plot <- ggplot(
  first_stage_plot_data,
  aes(x = z_tilde, y = x_tilde)
) +
  geom_hline(yintercept = 0, linewidth = 0.25, color = 'grey80') +
  geom_vline(xintercept = 0, linewidth = 0.25, color = 'grey80') +
  geom_point(alpha = 0.55, size = 1.5, color = '#2C5D86') +
  geom_abline(
    intercept = 0,
    slope = residualized_first_stage_slope,
    linewidth = 0.8,
    color = '#B24745'
  ) +
  ggrepel::geom_text_repel(
    aes(label = diagnostic_label),
    na.rm = TRUE,
    seed = 9252026,
    size = 2.7,
    min.segment.length = 0,
    box.padding = 0.25,
    max.overlaps = Inf
  ) +
  labs(
    title = 'Residualized first stage',
    subtitle = sprintf('Slope = %.4f; unweighted destination-cohort cells',
                       residualized_first_stage_slope),
    x = 'Residualized shift-share instrument',
    y = 'Residualized OOS-origin share (percentage points)'
  ) +
  theme_bw(base_size = 11)

reduced_form_residualized_plot <- ggplot(
  reduced_form_plot_data,
  aes(x = z_tilde, y = y_tilde)
) +
  geom_hline(yintercept = 0, linewidth = 0.25, color = 'grey80') +
  geom_vline(xintercept = 0, linewidth = 0.25, color = 'grey80') +
  geom_point(alpha = 0.55, size = 1.5, color = '#2C5D86') +
  geom_abline(
    intercept = 0,
    slope = residualized_reduced_form_slope,
    linewidth = 0.8,
    color = '#B24745'
  ) +
  ggrepel::geom_text_repel(
    aes(label = diagnostic_label),
    na.rm = TRUE,
    seed = 9252026,
    size = 2.7,
    min.segment.length = 0,
    box.padding = 0.25,
    max.overlaps = Inf
  ) +
  labs(
    title = 'Residualized reduced form',
    subtitle = sprintf('Slope = %.4f; IV ratio = %.4f',
                       residualized_reduced_form_slope,
                       residualized_iv_ratio),
    x = 'Residualized shift-share instrument',
    y = 'Residualized recovered mean utility'
  ) +
  theme_bw(base_size = 11)

ggsave(
  file.path(iv_diagnostic_figure_directory,
            'residualized_first_stage.png'),
  first_stage_residualized_plot,
  width = 7.5, height = 5.5, dpi = 300
)
ggsave(
  file.path(iv_diagnostic_figure_directory,
            'residualized_reduced_form.png'),
  reduced_form_residualized_plot,
  width = 7.5, height = 5.5, dpi = 300
)

# Leave-one-destination-state-out IV analysis ----------------------------------

estimate_leave_one_destination_out <- function(omitted_state) {
  leave_one_out_data <- delta_second_stage %>%
    filter(d_state != omitted_state)

  # Reestimate the full IV equation, first stage, reduced form, controls, and
  # both fixed-effect sets after removing every cohort for one destination.
  iv_model <- felm(
    delta_rel ~ unemp + net_mig |
      factor(d_state) + factor(grad_y) |
      (o_share ~ z_lso) | d_state,
    data = leave_one_out_data
  )
  first_stage_model <- felm(
    o_share ~ z_lso + unemp + net_mig |
      factor(d_state) + factor(grad_y) | 0 | d_state,
    data = leave_one_out_data
  )
  reduced_form_model <- felm(
    delta_rel ~ z_lso + unemp + net_mig |
      factor(d_state) + factor(grad_y) | 0 | d_state,
    data = leave_one_out_data
  )

  iv_result <- clustered_coef(iv_model, 'o_share\\(fit\\)')
  first_stage_result <- clustered_coef(first_stage_model, '^z_lso$')
  reduced_form_result <- clustered_coef(reduced_form_model, '^z_lso$')
  ratio_result <- (
    reduced_form_result$estimate / first_stage_result$estimate
  )

  stopifnot(isTRUE(all.equal(iv_result$estimate, ratio_result,
                             tolerance = 1e-8)))

  cluster_degrees_of_freedom <- n_distinct(leave_one_out_data$d_state) - 1L
  confidence_critical_value <- qt(0.975, df = cluster_degrees_of_freedom)

  data.frame(
    omitted_state = omitted_state,
    observations = nrow(leave_one_out_data),
    destination_states = n_distinct(leave_one_out_data$d_state),
    gamma_iv = iv_result$estimate,
    delta_gamma = iv_result$estimate - gamma_in_iv_unclustered$estimate,
    in_state_ame_1pp = iv_result$estimate *
      saturated_multipliers[['one_pp']],
    first_stage_coefficient = first_stage_result$estimate,
    reduced_form_coefficient = reduced_form_result$estimate,
    first_stage_f_destination_clustered = (
      first_stage_result$estimate / first_stage_result$se
    )^2,
    iv_se_destination_clustered = iv_result$se,
    ci_lower = iv_result$estimate -
      confidence_critical_value * iv_result$se,
    ci_upper = iv_result$estimate +
      confidence_critical_value * iv_result$se,
    row.names = NULL
  )
}

leave_one_destination_out_results <- bind_rows(lapply(
  sort(unique(delta_second_stage$d_state)),
  estimate_leave_one_destination_out
))

stopifnot(
  nrow(leave_one_destination_out_results) ==
    n_distinct(delta_second_stage$d_state),
  !anyNA(leave_one_destination_out_results)
)

leave_one_destination_out_most_influential <-
  leave_one_destination_out_results %>%
  arrange(desc(abs(delta_gamma))) %>%
  slice_head(n = 10)

leave_one_out_plot_data <- leave_one_destination_out_results %>%
  arrange(gamma_iv) %>%
  mutate(omitted_state = factor(omitted_state, levels = omitted_state))

leave_one_destination_out_plot <- ggplot(
  leave_one_out_plot_data,
  aes(y = omitted_state, x = gamma_iv)
) +
  geom_vline(
    xintercept = gamma_in_iv_unclustered$estimate,
    linetype = 'dashed',
    linewidth = 0.7,
    color = '#B24745'
  ) +
  geom_errorbar(
    aes(xmin = ci_lower, xmax = ci_upper),
    orientation = 'y',
    width = 0.25,
    linewidth = 0.4,
    color = 'grey45'
  ) +
  geom_point(size = 1.8, color = '#2C5D86') +
  labs(
    title = 'Leave-one-destination-state-out IV estimates',
    subtitle = paste(
      'Destination-state-clustered 95% confidence intervals;',
      'dashed line is the full-sample estimate'
    ),
    x = expression(hat(gamma)[-j]),
    y = 'Omitted destination state'
  ) +
  theme_bw(base_size = 10) +
  theme(panel.grid.major.y = element_blank())

ggsave(
  file.path(iv_diagnostic_figure_directory,
            'leave_one_destination_state_out.png'),
  leave_one_destination_out_plot,
  width = 7.5, height = 10, dpi = 300
)

# Second-step sample and trimming sensitivity ----------------------------------

# The log-share reduced-form specification requires a positive Alabama-origin
# outflow to destination j in cohort c. By contrast, the saturated MNL can
# recover delta_jc whenever at least one linked graduate (from any origin) chose
# that destination. Reconstruct the reduced-form support using this script's
# matched sample so that the difference is purely a cell-support comparison.
alabama_origin_destination_counts <- join %>%
  filter(o_state == 'Alabama', d_state != 'Alabama') %>%
  count(d_state, grad_y, name = 'N_dest_al_origin')

alabama_home_cohorts <- join %>%
  filter(o_state == 'Alabama', d_state == 'Alabama') %>%
  distinct(grad_y)

stopifnot(setequal(alabama_home_cohorts$grad_y, paper_years))

# Keep N_dest out of the preferred structural data object. It is joined here
# only to define explicitly requested trimming/sampling sensitivity exercises;
# it never enters as a regression weight.
second_step_sensitivity_data <- delta_second_stage %>%
  left_join(destination_support %>%
              transmute(d_state = alternative,
                        grad_y,
                        N_dest_all_origins = N_dest),
            by = c('d_state', 'grad_y')) %>%
  left_join(alabama_origin_destination_counts,
            by = c('d_state', 'grad_y')) %>%
  mutate(N_dest_al_origin = replace_na(N_dest_al_origin, 0L),
         in_reduced_form_log_share_sample = N_dest_al_origin > 0)

stopifnot(nrow(second_step_sensitivity_data) == nrow(delta_second_stage))
stopifnot(!anyNA(second_step_sensitivity_data$N_dest_all_origins))

reduced_form_cells_not_in_structural <- anti_join(
  alabama_origin_destination_counts,
  delta_second_stage,
  by = c('d_state', 'grad_y')
)
stopifnot(nrow(reduced_form_cells_not_in_structural) == 0)

structural_only_cells <- second_step_sensitivity_data %>%
  filter(!in_reduced_form_log_share_sample)

second_step_sample_comparison <- data.frame(
  sample = c('Possible balanced non-Alabama destination-cohort panel',
             'Structural second step: positive all-origin destination count',
             'Reduced-form log-share support: positive Alabama-origin outflow'),
  cells = c(50L * length(paper_years),
            nrow(second_step_sensitivity_data),
            sum(second_step_sensitivity_data$in_reduced_form_log_share_sample)),
  row.names = NULL
)

structural_only_cell_diagnostics <- structural_only_cells %>%
  summarize(
    cells = n(),
    min_all_origin_count = min(N_dest_all_origins),
    p25_all_origin_count = quantile(N_dest_all_origins, 0.25,
                                    names = FALSE),
    median_all_origin_count = median(N_dest_all_origins),
    mean_all_origin_count = mean(N_dest_all_origins),
    p75_all_origin_count = quantile(N_dest_all_origins, 0.75,
                                    names = FALSE),
    max_all_origin_count = max(N_dest_all_origins),
    share_count_le_5 = mean(N_dest_all_origins <= 5)
  )

estimate_second_step_sensitivity <- function(data, sample_label) {
  ols_unclustered <- felm(
    delta_rel ~ o_share + unemp + net_mig |
      factor(d_state) + factor(grad_y),
    data = data
  )
  ols_destination_clustered <- felm(
    delta_rel ~ o_share + unemp + net_mig |
      factor(d_state) + factor(grad_y) | 0 | d_state,
    data = data
  )
  iv_unclustered <- felm(
    delta_rel ~ unemp + net_mig |
      factor(d_state) + factor(grad_y) |
      (o_share ~ z_lso),
    data = data
  )
  iv_destination_clustered <- felm(
    delta_rel ~ unemp + net_mig |
      factor(d_state) + factor(grad_y) |
      (o_share ~ z_lso) | d_state,
    data = data
  )
  first_stage_destination_clustered <- felm(
    o_share ~ z_lso + unemp + net_mig |
      factor(d_state) + factor(grad_y) | 0 | d_state,
    data = data
  )

  ols_u <- unclustered_coef(ols_unclustered, '^o_share$')
  ols_c <- clustered_coef(ols_destination_clustered, '^o_share$')
  iv_u <- unclustered_coef(iv_unclustered, 'o_share\\(fit\\)')
  iv_c <- clustered_coef(iv_destination_clustered, 'o_share\\(fit\\)')
  fs_c <- clustered_coef(first_stage_destination_clustered, '^z_lso$')

  data.frame(
    sample = sample_label,
    observations = nobs(ols_unclustered),
    destination_states = n_distinct(data$d_state),
    cohorts = n_distinct(data$grad_y),
    ols_gamma_in = ols_u$estimate,
    ols_unclustered_se = ols_u$se,
    ols_destination_clustered_se = ols_c$se,
    iv_gamma_in = iv_u$estimate,
    iv_unclustered_se = iv_u$se,
    iv_destination_clustered_se = iv_c$se,
    first_stage_destination_clustered_f = (fs_c$estimate / fs_c$se)^2,
    row.names = NULL
  )
}

# These are sample restrictions, not weights: the retained cells continue to
# enter the ordinary unweighted IV moments once each. The first-step MNL and its
# recovered fixed effects are held fixed across all rows.
minimum_destination_counts <- c(1L, 2L, 3L, 5L, 10L, 20L)

second_step_count_trim_results <- bind_rows(lapply(
  minimum_destination_counts,
  function(min_count) {
    estimate_second_step_sensitivity(
      second_step_sensitivity_data %>%
        filter(N_dest_all_origins >= min_count),
      paste0('All-origin destination count >= ', min_count)
    )
  }
))

second_step_reduced_form_support_result <- estimate_second_step_sensitivity(
  second_step_sensitivity_data %>%
    filter(in_reduced_form_log_share_sample),
  'Exact reduced-form log-share support'
)

second_step_sample_sensitivity <- bind_rows(
  second_step_count_trim_results,
  second_step_reduced_form_support_result
)

# N_dest is used only to describe the likely precision of the generated first-
# step fixed effects. It is deliberately absent from delta_second_stage and all
# structural second-step moments.
first_step_cell_size_diagnostics <- destination_support %>%
  filter(alternative != 'Alabama') %>%
  summarize(
    destination_cohort_cells = n(),
    min_N_dest = min(N_dest),
    p10_N_dest = quantile(N_dest, 0.10, names = FALSE),
    median_N_dest = median(N_dest),
    mean_N_dest = mean(N_dest),
    p90_N_dest = quantile(N_dest, 0.90, names = FALSE),
    max_N_dest = max(N_dest),
    cells_N_dest_le_5 = sum(N_dest <= 5),
    share_cells_N_dest_le_5 = mean(N_dest <= 5),
    cells_N_dest_le_10 = sum(N_dest <= 10),
    share_cells_N_dest_le_10 = mean(N_dest <= 10)
  )

cat('\nChoice-model IV comparison\n')
cat('--------------------------\n')
print(comparison_results, digits = 4, row.names = FALSE)

cat('\nSecond-step inference\n')
cat('---------------------\n')
print(second_step_inference, digits = 4, row.names = FALSE)

cat('\nOne-step benchmark inference\n')
cat('----------------------------\n')
print(one_step_benchmark_inference, digits = 4, row.names = FALSE)

cat('\nDifferences across estimators\n')
cat('-----------------------------\n')
print(estimator_differences, digits = 4, row.names = FALSE)

cat('\nSecond-stage first-stage diagnostics\n')
cat('------------------------------------\n')
print(first_stage_diagnostics, digits = 4, row.names = FALSE)

cat('\nResidualized first-stage and reduced-form slopes\n')
cat('------------------------------------------------\n')
print(residualized_slope_diagnostics, digits = 5, row.names = FALSE)

cat('\nMost influential leave-one-destination-state-out estimates\n')
cat('----------------------------------------------------------\n')
print(leave_one_destination_out_most_influential,
      digits = 4, row.names = FALSE)

cat('\nStructural versus reduced-form cell samples\n')
cat('-------------------------------------------\n')
print(second_step_sample_comparison, row.names = FALSE)

cat('\nCells present only in the structural second step\n')
cat('------------------------------------------------\n')
print(as.data.frame(structural_only_cell_diagnostics),
      digits = 4, row.names = FALSE)

cat('\nUnweighted second-step sample-trimming sensitivity\n')
cat('-------------------------------------------------\n')
print(second_step_sample_sensitivity, digits = 4, row.names = FALSE)

cat('\nFirst-step fixed-effect precision diagnostics\n')
cat('---------------------------------------------\n')
print(as.data.frame(first_step_cell_size_diagnostics),
      digits = 4, row.names = FALSE)

cat('\nSample and computation diagnostics\n')
cat('----------------------------------\n')
cat('Individual choices:', nrow(join), '\n')
cat('Full-grid grouped cells:', nrow(choice_cells_full), '\n')
cat('Saturated-model grouped cells:', nrow(choice_cells_saturated), '\n')
cat('Destination-by-cohort fixed effects:',
    n_distinct(choice_cells_saturated$dest_cohort), '\n')
cat('Non-Alabama second-stage cells:', nrow(delta_second_stage), '\n')
cat('Second step is exactly identified: one endogenous regressor and one',
    'excluded instrument after controls and fixed effects.\n')
cat('No destination counts enter the structural second-step moments.\n')

cat('\nIV diagnostic note\n')
cat('------------------\n')
cat(paste(
  'The scatterplots partial out unemployment, net migration, destination-state',
  'fixed effects, and cohort fixed effects separately from delta_hat, the OOS',
  'origin share, and the instrument using the full unweighted second-step sample.',
  'Each leave-one-out row reestimates the IV equation, first stage, reduced form,',
  'controls, and fixed effects after deleting every cohort for the named',
  'destination. Its IV standard error and first-stage F-statistic use',
  'destination-state clustering. The AME holds the full-sample reference',
  'population and probability multiplier fixed. No observations are trimmed',
  'from the preferred estimator.\n'
))

cat('\nSample-trimming note\n')
cat('--------------------\n')
cat(paste(
  'The 648-cell reduced-form support requires a positive Alabama-origin',
  'outflow because its dependent variable is a log destination share. The',
  'structural first step instead requires a positive destination count among',
  'all linked graduates, producing 755 cells. Count trimming above changes only',
  'the second-step sample; it does not re-estimate the first-step MNL or weight',
  'the retained moments. Because destination counts are realized outcomes of',
  'utility, these outcome-based sample restrictions are sensitivity exercises,',
  'not alternative preferred causal estimators. The exact reduced-form-support',
  'row continues to use the structural model\'s Commencement origin share, so it',
  'isolates cell support rather than reproducing the reduced-form specification,',
  'which currently uses the IPEDS origin share.\n'
))

cat('\nMarginal-effect note\n')
cat('--------------------\n')
cat(paste(
  'in_state_ame_1pp is the percentage-point change in in-state out-migration',
  'from a one-percentage-point increase in the total OOS origin share, allocated',
  'across states using linked-sample origin weights.',
  'linearized_in_state_leavers_per_100_oos applies the same local derivative',
  'to 100 OOS students while holding the cohort denominator fixed.\n'
))

cat('\nInference note\n')
cat('--------------\n')
cat(paste(
  'The second-step table reports conventional standard errors and sandwich',
  'standard errors clustered by destination state. The one-step benchmark',
  'retains its origin-state-clustered first-step standard error and is reported',
  'separately. These in-script standard errors do not propagate estimation error',
  'in the generated destination-by-cohort fixed effects. The fixed-support',
  'optional multiplier-bootstrap section below reports that first-step',
  'uncertainty separately, including for gamma_out and the marginal effects.',
  'The N_dest distribution above is a precision diagnostic only; it is not used',
  'as a structural weight.\n'
))

# Fixed-support multiplier bootstrap (optional) -------------------------------

run_multiplier_bootstrap <- identical(
  Sys.getenv('MLOGIT_IV_RUN_BOOTSTRAP', unset = '0'),
  '1'
)

if (run_multiplier_bootstrap) {
# Bootstrap settings ------------------------------------------------------------

bootstrap_reps <- as.integer(Sys.getenv(
  'MLOGIT_IV_BOOT_REPS',
  unset = '2000'
))
bootstrap_seed <- 9262026L
checkpoint_every <- 100L

stopifnot(length(bootstrap_reps) == 1L,
          !is.na(bootstrap_reps),
          bootstrap_reps > 1L)

checkpoint_directory <- 'tmp'
dir.create(checkpoint_directory, showWarnings = FALSE, recursive = TRUE)
checkpoint_path <- file.path(
  checkpoint_directory,
  'analysis_mlogit_iv_bootstrap_checkpoint.rds'
)
final_rds_path <- file.path(
  checkpoint_directory,
  'analysis_mlogit_iv_bootstrap_results.rds'
)

# Validate and pre-index the multiplier cells ----------------------------------

# Each row is an origin x destination x cohort cell. Students in one such cell
# make identical contributions to the conditional MNL likelihood. The sum of
# iid Exp(1) student weights in a cell with n students is Gamma(n, 1).
stopifnot(
  nrow(choice_cells_saturated) ==
    nrow(distinct(choice_cells_saturated,
                  o_state, alternative, grad_y))
)

base_choice_counts <- choice_cells_saturated$n_choice
positive_choice_cell <- base_choice_counts > 0

# origin_cohort is precisely the origin x cohort bootstrap stratum. Normalizing
# Gamma draws within it holds N_kc fixed in every replication.
origin_cohort_id <- as.integer(factor(
  choice_cells_saturated$origin_cohort
))
origin_cohort_totals <- as.numeric(rowsum(
  base_choice_counts,
  origin_cohort_id,
  reorder = TRUE
))

stopifnot(all(origin_cohort_totals > 0))

# Index the recovered destination-cohort effects once. Name-based indexing makes
# the normalization invariant to the ordering chosen internally by fixest.
target_dest_cohort <- delta_second_stage$dest_cohort
target_alabama_dest_cohort <- paste0(
  'Alabama__',
  delta_second_stage$grad_y
)
second_step_template <- delta_second_stage

stopifnot(length(target_dest_cohort) == 755L)
stopifnot(all(target_dest_cohort %in% names(dest_cohort_fe)))
stopifnot(all(target_alabama_dest_cohort %in% names(dest_cohort_fe)))

# Original point estimates and conditional second-step inference ---------------

original_estimates <- c(
  gamma_in_ols = gamma_in_ols_unclustered$estimate,
  gamma_in_iv = gamma_in_iv_unclustered$estimate,
  gamma_out_ols = gamma_in_ols_unclustered$estimate +
    pi_out_minus_in$estimate,
  gamma_out_iv = gamma_in_iv_unclustered$estimate +
    pi_out_minus_in$estimate,
  in_state_ame_1pp_ols = gamma_in_ols_unclustered$estimate *
    saturated_multipliers[['one_pp']],
  in_state_ame_1pp_iv = gamma_in_iv_unclustered$estimate *
    saturated_multipliers[['one_pp']],
  leavers_per_100_oos_ols = gamma_in_ols_unclustered$estimate *
    saturated_multipliers[['per_100']],
  leavers_per_100_oos_iv = gamma_in_iv_unclustered$estimate *
    saturated_multipliers[['per_100']]
)

conditional_second_step_se <- c(
  gamma_in_ols = gamma_in_ols_destination_clustered$se,
  gamma_in_iv = gamma_in_iv_destination_clustered$se,
  gamma_out_ols = NA_real_,
  gamma_out_iv = NA_real_,
  in_state_ame_1pp_ols = NA_real_,
  in_state_ame_1pp_iv = NA_real_,
  leavers_per_100_oos_ols = NA_real_,
  leavers_per_100_oos_iv = NA_real_
)

# One fixed-support bootstrap replication --------------------------------------

bootstrap_one <- function(replication) {
  tryCatch({
    # This draw occurs independently at the finest grouped likelihood cell:
    # origin state x destination state x graduation cohort.
    gamma_counts <- numeric(length(base_choice_counts))
    gamma_counts[positive_choice_cell] <- rgamma(
      sum(positive_choice_cell),
      shape = base_choice_counts[positive_choice_cell],
      rate = 1
    )

    gamma_stratum_totals <- as.numeric(rowsum(
      gamma_counts,
      origin_cohort_id,
      reorder = TRUE
    ))

    bootstrap_choice_cells <- choice_cells_saturated
    bootstrap_choice_cells$n_choice_boot <- gamma_counts *
      origin_cohort_totals[origin_cohort_id] /
      gamma_stratum_totals[origin_cohort_id]

    # Verify the requested origin x cohort normalization. No redraw, pseudocount,
    # or support trimming is used if a numerical failure occurs.
    normalized_totals <- as.numeric(rowsum(
      bootstrap_choice_cells$n_choice_boot,
      origin_cohort_id,
      reorder = TRUE
    ))
    if (max(abs(normalized_totals - origin_cohort_totals)) > 1e-8) {
      stop('Origin-cohort multiplier normalization failed.')
    }

    bootstrap_first_step <- fepois(
      n_choice_boot ~ home_oos +
        home_ins +
        o_share_outdiff +
        i(grad_y, al_oos, ref = base_year) |
        origin_cohort + dest_cohort,
      data = bootstrap_choice_cells,
      notes = FALSE
    )

    bootstrap_fe <- fixef(
      bootstrap_first_step,
      notes = FALSE
    )[['dest_cohort']]

    if (!all(target_dest_cohort %in% names(bootstrap_fe)) ||
        !all(target_alabama_dest_cohort %in% names(bootstrap_fe))) {
      stop('A destination-cohort fixed effect is missing from the bootstrap fit.')
    }

    bootstrap_second_step <- second_step_template
    bootstrap_second_step$delta_rel <- unname(
      bootstrap_fe[target_dest_cohort] -
        bootstrap_fe[target_alabama_dest_cohort]
    )

    if (nrow(bootstrap_second_step) != 755L ||
        anyNA(bootstrap_second_step$delta_rel)) {
      stop('Bootstrap second-step support differs from the preferred 755 cells.')
    }

    bootstrap_ols <- felm(
      delta_rel ~ o_share + unemp + net_mig |
        factor(d_state) + factor(grad_y),
      data = bootstrap_second_step
    )

    bootstrap_iv <- felm(
      delta_rel ~ unemp + net_mig |
        factor(d_state) + factor(grad_y) |
        (o_share ~ z_lso),
      data = bootstrap_second_step
    )

    gamma_in_ols_b <- unclustered_coef(
      bootstrap_ols,
      '^o_share$'
    )$estimate
    gamma_in_iv_b <- unclustered_coef(
      bootstrap_iv,
      'o_share\\(fit\\)'
    )$estimate
    pi_out_minus_in_b <- fixest_coef(
      bootstrap_first_step,
      'o_share_outdiff'
    )$estimate

    multiplier_b <- ame_multipliers(
      bootstrap_first_step,
      bootstrap_choice_cells
    )

    data.frame(
      replication = replication,
      success = TRUE,
      error = NA_character_,
      gamma_in_ols = gamma_in_ols_b,
      gamma_in_iv = gamma_in_iv_b,
      gamma_out_ols = gamma_in_ols_b + pi_out_minus_in_b,
      gamma_out_iv = gamma_in_iv_b + pi_out_minus_in_b,
      in_state_ame_1pp_ols = gamma_in_ols_b * multiplier_b[['one_pp']],
      in_state_ame_1pp_iv = gamma_in_iv_b * multiplier_b[['one_pp']],
      leavers_per_100_oos_ols = gamma_in_ols_b * multiplier_b[['per_100']],
      leavers_per_100_oos_iv = gamma_in_iv_b * multiplier_b[['per_100']],
      destination_cohort_effects = length(bootstrap_fe),
      second_step_cells = nrow(bootstrap_second_step),
      row.names = NULL
    )
  }, error = function(e) {
    data.frame(
      replication = replication,
      success = FALSE,
      error = conditionMessage(e),
      gamma_in_ols = NA_real_,
      gamma_in_iv = NA_real_,
      gamma_out_ols = NA_real_,
      gamma_out_iv = NA_real_,
      in_state_ame_1pp_ols = NA_real_,
      in_state_ame_1pp_iv = NA_real_,
      leavers_per_100_oos_ols = NA_real_,
      leavers_per_100_oos_iv = NA_real_,
      destination_cohort_effects = NA_integer_,
      second_step_cells = NA_integer_,
      row.names = NULL
    )
  })
}

# Run or resume the bootstrap ---------------------------------------------------

restart_requested <- identical(
  Sys.getenv('MLOGIT_IV_BOOT_RESTART', unset = '0'),
  '1'
)

bootstrap_draws <- data.frame()
first_replication <- 1L

if (file.exists(checkpoint_path) && !restart_requested) {
  checkpoint <- readRDS(checkpoint_path)
  compatible_checkpoint <-
    identical(checkpoint$bootstrap_reps, bootstrap_reps) &&
    identical(checkpoint$bootstrap_seed, bootstrap_seed)

  if (compatible_checkpoint) {
    bootstrap_draws <- checkpoint$draws
    first_replication <- nrow(bootstrap_draws) + 1L
    assign('.Random.seed', checkpoint$random_seed, envir = .GlobalEnv)
    cat('Resuming after replication', nrow(bootstrap_draws), '\n')
  }
}

if (first_replication == 1L) {
  RNGkind("L'Ecuyer-CMRG")
  set.seed(bootstrap_seed)
}

bootstrap_start_time <- Sys.time()

if (first_replication <= bootstrap_reps) {
  for (b in first_replication:bootstrap_reps) {
    bootstrap_draws <- bind_rows(
      bootstrap_draws,
      bootstrap_one(b)
    )

    if (b %% checkpoint_every == 0L || b == bootstrap_reps) {
      checkpoint <- list(
        bootstrap_reps = bootstrap_reps,
        bootstrap_seed = bootstrap_seed,
        draws = bootstrap_draws,
        random_seed = get('.Random.seed', envir = .GlobalEnv),
        updated_at = Sys.time()
      )
      saveRDS(checkpoint, checkpoint_path)

      elapsed_minutes <- as.numeric(
        difftime(Sys.time(), bootstrap_start_time, units = 'mins')
      )
      cat(sprintf(
        'Completed %d/%d replications (%.2f minutes this run; %d failures).\n',
        b,
        bootstrap_reps,
        elapsed_minutes,
        sum(!bootstrap_draws$success)
      ))
      flush.console()
    }
  }
}

# Summarize first-step multiplier uncertainty ----------------------------------

successful_draws <- bootstrap_draws %>%
  filter(success)

estimands <- names(original_estimates)

bootstrap_summary <- bind_rows(lapply(estimands, function(estimand) {
  draws <- successful_draws[[estimand]]
  estimate <- original_estimates[[estimand]]
  percentile_quantiles <- quantile(
    draws,
    probs = c(0.025, 0.975),
    names = FALSE,
    na.rm = TRUE
  )

  data.frame(
    estimand = estimand,
    original_estimate = estimate,
    bootstrap_mean = mean(draws, na.rm = TRUE),
    bootstrap_bias = mean(draws, na.rm = TRUE) - estimate,
    first_step_multiplier_bootstrap_se = sd(draws, na.rm = TRUE),
    bootstrap_se_monte_carlo_error =
      sd(draws, na.rm = TRUE) /
      sqrt(2 * (length(draws) - 1)),
    normal_ci_lower = estimate - qnorm(0.975) * sd(draws, na.rm = TRUE),
    normal_ci_upper = estimate + qnorm(0.975) * sd(draws, na.rm = TRUE),
    percentile_ci_lower = percentile_quantiles[1],
    percentile_ci_upper = percentile_quantiles[2],
    basic_ci_lower = 2 * estimate - percentile_quantiles[2],
    basic_ci_upper = 2 * estimate - percentile_quantiles[1],
    destination_clustered_second_step_se =
      conditional_second_step_se[[estimand]],
    successful_replications = length(draws),
    requested_replications = bootstrap_reps,
    row.names = NULL
  )
}))

bootstrap_metadata <- list(
  bootstrap_reps = bootstrap_reps,
  bootstrap_seed = bootstrap_seed,
  successful_replications = nrow(successful_draws),
  failed_replications = sum(!bootstrap_draws$success),
  cell_level = 'origin_state x destination_state x graduation_cohort',
  strata = 'origin_state x graduation_cohort',
  multiplier = 'iid student Exp(1), aggregated as Gamma(n_cell, 1)',
  normalization = 'weights sum to observed N_kc within origin x cohort',
  second_step = 'unweighted 755-cell OLS and exactly identified 2SLS',
  inference_scope = paste(
    'Bootstrap distribution propagates first-step MNL estimation uncertainty;',
    'destination-state-clustered second-step SE is reported separately.'
  ),
  completed_at = Sys.time()
)

saveRDS(
  list(
    metadata = bootstrap_metadata,
    summary = bootstrap_summary,
    draws = bootstrap_draws
  ),
  final_rds_path
)

cat('\nFixed-support multiplier bootstrap complete\n')
cat('-------------------------------------------\n')
print(bootstrap_summary, digits = 5, row.names = FALSE)
cat('\nSuccessful replications:', nrow(successful_draws), '/',
    bootstrap_reps, '\n')
cat('Full RDS:', final_rds_path, '\n')
} else {
  cat(paste(
    '\nSkipping the 2,000-replication multiplier bootstrap.',
    'Set MLOGIT_IV_RUN_BOOTSTRAP=1 to run or resume it.\n'
  ))
}

# Migration counterfactuals ---------------------------------------------------

main_bootstrap_results_path <- file.path(
  'tmp', 'analysis_mlogit_iv_bootstrap_results.rds'
)

if (file.exists(main_bootstrap_results_path)) {
# Counterfactual construction --------------------------------------------------

# The preferred second-stage IV coefficient is estimated without 2020 because
# its destination controls are unavailable. As in the paper's original Figure 4
# exercise, however, construct first-step utilities and predictions for every
# 2006-2023 cohort. The 2020 prediction therefore uses the common IV coefficient
# estimated on the valid second-stage sample and a separately recovered 2020
# destination-by-cohort utility.
counterfactual_years <- 2006:2023
attendance_calibration_year <- 2008L
attendance_effect_at_calibration <- 0.10

join_counterfactual <- dest %>%
  inner_join(origin, by = 'user_id') %>%
  filter(grad_y %in% counterfactual_years) %>%
  ungroup()

comm_counterfactual <- read.csv(
  paste0(pathHome, 'data/all_alabama_data.csv')
) %>%
  mutate(originState = gsub(' ', '', Origin.State)) %>%
  left_join(states, by = 'originState') %>%
  filter(!is.na(state), Year %in% counterfactual_years) %>%
  rename(grad_y = Year) %>%
  ungroup()

counterfactual_cohort_sizes <- comm_counterfactual %>%
  group_by(grad_y) %>%
  summarize(
    N_cohort = n(),
    N_AL_cohort = sum(state == 'Alabama'),
    .groups = 'drop'
  )

counterfactual_shares <- comm_counterfactual %>%
  count(grad_y, state, name = 'N_origin') %>%
  left_join(counterfactual_cohort_sizes, by = 'grad_y') %>%
  mutate(o_share = 100 * N_origin / N_cohort) %>%
  select(grad_y, state, o_share)

counterfactual_choice_counts <- join_counterfactual %>%
  count(grad_y, o_state, alternative = d_state, name = 'n_choice')

counterfactual_origin_cohorts <- join_counterfactual %>%
  distinct(grad_y, o_state)

counterfactual_destination_support <- join_counterfactual %>%
  count(grad_y, alternative = d_state, name = 'N_dest')

choice_cells_counterfactual <- counterfactual_origin_cohorts %>%
  inner_join(
    counterfactual_destination_support,
    by = 'grad_y',
    relationship = 'many-to-many'
  ) %>%
  select(grad_y, o_state, alternative) %>%
  left_join(
    counterfactual_choice_counts,
    by = c('grad_y', 'o_state', 'alternative')
  ) %>%
  mutate(n_choice = replace_na(n_choice, 0L)) %>%
  left_join(
    counterfactual_shares,
    by = c('grad_y', 'alternative' = 'state')
  ) %>%
  mutate(
    o_share = replace_na(o_share, 0),
    o_share = if_else(alternative == 'Alabama', 0, o_share),
    is_oos = as.integer(o_state != 'Alabama'),
    home_oos = as.integer(is_oos == 1 & o_state == alternative),
    home_ins = as.integer(is_oos == 0 & alternative == 'Alabama'),
    o_share_outdiff = is_oos * o_share,
    al_oos = as.integer(is_oos == 1 & alternative == 'Alabama'),
    origin_cohort = interaction(o_state, grad_y, drop = TRUE),
    dest_cohort = interaction(
      alternative,
      grad_y,
      drop = TRUE,
      sep = '__'
    )
  )

model_saturated_first_step_counterfactual <- fepois(
  n_choice ~ home_oos +
    home_ins +
    o_share_outdiff +
    i(grad_y, al_oos, ref = base_year) |
    origin_cohort + dest_cohort,
  data = choice_cells_counterfactual,
  vcov = ~o_state,
  notes = FALSE
)

stopifnot(
  nrow(join_counterfactual) == n_distinct(join_counterfactual$user_id),
  setequal(unique(choice_cells_counterfactual$grad_y),
           counterfactual_years),
  nobs(model_saturated_first_step_counterfactual) ==
    nrow(choice_cells_counterfactual)
)

# The saturated first-step destination-by-cohort effects contain the actual-share
# contribution gamma_in * d_jc as well as the structural destination-cohort shock.
# Starting from fitted actual utilities and multiplying the fitted choice index by
# exp(gamma * (d_j,2006 - d_jc)) changes only the origin-share component while
# holding the recovered shock, controls, home preference, and Alabama cohort
# utility fixed.
base_year_shares <- counterfactual_shares %>%
  filter(grad_y == base_year) %>%
  transmute(alternative = state,
            o_share_base_year = o_share)

fitted_count_index_all <- as.numeric(predict(
  model_saturated_first_step_counterfactual,
  newdata = choice_cells_counterfactual,
  type = 'response'
))

in_state_counterfactual_cells <- choice_cells_counterfactual %>%
  filter(o_state == 'Alabama') %>%
  mutate(
    fitted_count_index = fitted_count_index_all[
      choice_cells_counterfactual$o_state == 'Alabama'
    ]
  ) %>%
  left_join(base_year_shares, by = 'alternative') %>%
  mutate(
    o_share_base_year = replace_na(o_share_base_year, 0),
    o_share_base_year = if_else(alternative == 'Alabama',
                                0, o_share_base_year),
    share_change_to_base_year = o_share_base_year - o_share
  ) %>%
  arrange(grad_y, alternative)

stopifnot(
  !anyNA(in_state_counterfactual_cells[c(
    'grad_y',
    'alternative',
    'o_share',
    'o_share_base_year',
    'share_change_to_base_year',
    'fitted_count_index'
  )]),
  all(in_state_counterfactual_cells$fitted_count_index > 0),
  all(in_state_counterfactual_cells$share_change_to_base_year[
    in_state_counterfactual_cells$alternative == 'Alabama'
  ] == 0)
)

counterfactual_outmigration_by_gamma <- function(gamma) {
  in_state_counterfactual_cells %>%
    mutate(
      counterfactual_count_index = fitted_count_index * exp(
        gamma * share_change_to_base_year
      )
    ) %>%
    group_by(grad_y) %>%
    summarize(
      actual_outmigration_rate =
        sum(fitted_count_index[alternative != 'Alabama']) /
        sum(fitted_count_index),
      fixed_share_outmigration_rate =
        sum(counterfactual_count_index[alternative != 'Alabama']) /
        sum(counterfactual_count_index),
      .groups = 'drop'
    ) %>%
    mutate(
      induced_outmigration_rate = actual_outmigration_rate -
        fixed_share_outmigration_rate
    )
}

iv_counterfactual_by_cohort <- counterfactual_outmigration_by_gamma(
  gamma_in_iv_unclustered$estimate
) %>%
  rename(
    fixed_share_outmigration_rate_iv = fixed_share_outmigration_rate,
    induced_outmigration_rate_iv = induced_outmigration_rate
  )

# Apply both non-IV coefficients to the same fitted actual utilities and full
# 18-cohort prediction sample. The preferred non-IV comparison is the unweighted
# two-step OLS projection (gamma_in = 0.363). Retain the one-shot MNL coefficient
# (gamma_in = 0.204) only as the paper's previous benchmark.
two_step_ols_counterfactual_by_cohort <- counterfactual_outmigration_by_gamma(
  gamma_in_ols_unclustered$estimate
) %>%
  select(
    grad_y,
    fixed_share_outmigration_rate_two_step_ols =
      fixed_share_outmigration_rate,
    induced_outmigration_rate_two_step_ols = induced_outmigration_rate
  )

one_shot_counterfactual_by_cohort <- counterfactual_outmigration_by_gamma(
  gamma_in_current$estimate
) %>%
  select(
    grad_y,
    fixed_share_outmigration_rate_one_shot = fixed_share_outmigration_rate,
    induced_outmigration_rate_one_shot = induced_outmigration_rate
  )

counterfactual_in_state_by_cohort <- iv_counterfactual_by_cohort %>%
  left_join(two_step_ols_counterfactual_by_cohort, by = 'grad_y') %>%
  left_join(one_shot_counterfactual_by_cohort, by = 'grad_y')

# OOS counterfactual -----------------------------------------------------------

# For OOS students, the destination-share coefficient is gamma_in plus the
# OOS-minus-in-state difference identified in the saturated first step. Starting
# from each origin-state/cohort's fitted actual utilities, replace only the peer
# share vector. Normalize probabilities separately within every origin/cohort
# choice set before averaging, thereby preserving home-state preferences,
# cohort utilities, destination controls, and recovered destination-cohort
# structural shocks.
gamma_out_iv <- gamma_in_iv_unclustered$estimate +
  pi_out_minus_in$estimate

oos_counterfactual_cells <- choice_cells_counterfactual %>%
  mutate(fitted_count_index = fitted_count_index_all) %>%
  filter(o_state != 'Alabama') %>%
  left_join(base_year_shares, by = 'alternative') %>%
  mutate(
    o_share_base_year = replace_na(o_share_base_year, 0),
    o_share_base_year = if_else(alternative == 'Alabama',
                                0, o_share_base_year),
    share_change_to_base_year = o_share_base_year - o_share
  ) %>%
  arrange(grad_y, o_state, alternative)

stopifnot(
  !anyNA(oos_counterfactual_cells[c(
    'grad_y',
    'o_state',
    'alternative',
    'o_share',
    'o_share_base_year',
    'share_change_to_base_year',
    'fitted_count_index'
  )]),
  all(oos_counterfactual_cells$fitted_count_index > 0),
  all(oos_counterfactual_cells$share_change_to_base_year[
    oos_counterfactual_cells$alternative == 'Alabama'
  ] == 0)
)

oos_counterfactual_outmigration_by_gamma <- function(gamma) {
  oos_counterfactual_cells %>%
    mutate(
      counterfactual_count_index = fitted_count_index * exp(
        gamma * share_change_to_base_year
      )
    ) %>%
    group_by(grad_y, o_state) %>%
    mutate(
      origin_cohort_size = sum(fitted_count_index),
      actual_probability = fitted_count_index /
        sum(fitted_count_index),
      counterfactual_probability = counterfactual_count_index /
        sum(counterfactual_count_index)
    ) %>%
    ungroup() %>%
    group_by(grad_y) %>%
    summarize(
      actual_outmigration_rate = sum(
        origin_cohort_size[alternative != 'Alabama'] *
          actual_probability[alternative != 'Alabama']
      ) / sum(origin_cohort_size[alternative == 'Alabama']),
      fixed_share_outmigration_rate_iv = sum(
        origin_cohort_size[alternative != 'Alabama'] *
          counterfactual_probability[alternative != 'Alabama']
      ) / sum(origin_cohort_size[alternative == 'Alabama']),
      .groups = 'drop'
    ) %>%
    mutate(
      actual_retention_rate = 1 - actual_outmigration_rate,
      fixed_share_retention_rate_iv =
        1 - fixed_share_outmigration_rate_iv,
      peer_induced_outmigration_rate_iv = actual_outmigration_rate -
        fixed_share_outmigration_rate_iv
    )
}

counterfactual_oos_by_cohort <-
  oos_counterfactual_outmigration_by_gamma(gamma_out_iv)

# Calibrate one no-UA retention probability at the observed 2008 OOS cohort and
# hold it fixed across cohorts. The 2006-share counterfactual above remains a
# separate peer-composition exercise and is not the attendance-effect anchor.
fixed_no_ua_retention <- counterfactual_oos_by_cohort %>%
  filter(grad_y == attendance_calibration_year) %>%
  pull(actual_retention_rate) - attendance_effect_at_calibration

stopifnot(length(fixed_no_ua_retention) == 1L)

counterfactual_oos_by_cohort <- counterfactual_oos_by_cohort %>%
  mutate(
    calibrated_no_ua_retention_rate = fixed_no_ua_retention,
    attendance_effect_actual = actual_retention_rate -
      calibrated_no_ua_retention_rate
  )

# Verify that fitted actual Alabama-origin probabilities reproduce observed
# out-migration rates. Flexible type-specific Alabama cohort utilities make the
# difference negligible up to numerical estimation tolerance.
observed_in_state_outmigration <- join_counterfactual %>%
  filter(o_state == 'Alabama') %>%
  group_by(grad_y) %>%
  summarize(
    observed_outmigration_rate = mean(d_state != 'Alabama'),
    linked_in_state_graduates = n(),
    .groups = 'drop'
  )

counterfactual_in_state_by_cohort <- counterfactual_in_state_by_cohort %>%
  left_join(observed_in_state_outmigration, by = 'grad_y')

maximum_actual_fit_difference <- max(abs(
  counterfactual_in_state_by_cohort$actual_outmigration_rate -
    counterfactual_in_state_by_cohort$observed_outmigration_rate
))

observed_oos_outmigration <- join_counterfactual %>%
  filter(o_state != 'Alabama') %>%
  group_by(grad_y) %>%
  summarize(
    observed_outmigration_rate = mean(d_state != 'Alabama'),
    linked_oos_graduates = n(),
    .groups = 'drop'
  )

counterfactual_oos_by_cohort <- counterfactual_oos_by_cohort %>%
  left_join(observed_oos_outmigration, by = 'grad_y')

maximum_oos_actual_fit_difference <- max(abs(
  counterfactual_oos_by_cohort$actual_outmigration_rate -
    counterfactual_oos_by_cohort$observed_outmigration_rate
))

# Bootstrap uncertainty --------------------------------------------------------

# The completed multiplier bootstrap saved gamma_in for each draw but did not
# save every draw's destination-by-cohort utilities. The intervals below apply
# those gamma draws to the full-sample fitted actual utilities. They propagate
# uncertainty in the IV coefficient (including the first-step contribution to
# that coefficient) while conditioning on the full-sample reference choice
# probabilities. A fully joint band would require saving the counterfactual
# probabilities within every first-step bootstrap replication.
bootstrap_results_path <- file.path(
  'tmp',
  'analysis_mlogit_iv_bootstrap_results.rds'
)
if (!file.exists(bootstrap_results_path)) {
  stop('Set MLOGIT_IV_RUN_BOOTSTRAP=1 and rerun analysis_main_iv.R before constructing counterfactual confidence intervals.')
}

iv_gamma_bootstrap_data <- readRDS(bootstrap_results_path)$draws %>%
  filter(success, !is.na(gamma_in_iv), !is.na(gamma_out_iv))

iv_gamma_bootstrap_draws <- iv_gamma_bootstrap_data$gamma_in_iv
iv_gamma_out_bootstrap_draws <- iv_gamma_bootstrap_data$gamma_out_iv

stopifnot(
  length(iv_gamma_bootstrap_draws) > 1L,
  length(iv_gamma_bootstrap_draws) ==
    length(iv_gamma_out_bootstrap_draws)
)

# Vectorize the counterfactual probabilities over all bootstrap gamma draws.
counterfactual_index_bootstrap <-
  in_state_counterfactual_cells$fitted_count_index * exp(outer(
    in_state_counterfactual_cells$share_change_to_base_year,
    iv_gamma_bootstrap_draws
  ))

fixed_share_bootstrap_matrix <- do.call(rbind, lapply(
  counterfactual_years,
  function(cohort) {
    cohort_rows <- which(in_state_counterfactual_cells$grad_y == cohort)
    outside_rows <- cohort_rows[
      in_state_counterfactual_cells$alternative[cohort_rows] != 'Alabama'
    ]
    fixed_share_draws <- colSums(
      counterfactual_index_bootstrap[outside_rows, , drop = FALSE]
    ) / colSums(
      counterfactual_index_bootstrap[cohort_rows, , drop = FALSE]
    )

    fixed_share_draws
  }
))
rownames(fixed_share_bootstrap_matrix) <- counterfactual_years

bootstrap_counterfactual_by_cohort <- data.frame(
  grad_y = counterfactual_years,
  fixed_share_bootstrap_se = apply(
    fixed_share_bootstrap_matrix, 1, sd
  ),
  row.names = NULL
) %>%
  left_join(
    counterfactual_in_state_by_cohort %>%
      select(grad_y, fixed_share_outmigration_rate_iv),
    by = 'grad_y'
  ) %>%
  mutate(
    fixed_share_ci_lower = pmax(
      0,
      fixed_share_outmigration_rate_iv -
        qnorm(0.975) * fixed_share_bootstrap_se
    ),
    fixed_share_ci_upper = pmin(
      1,
      fixed_share_outmigration_rate_iv +
        qnorm(0.975) * fixed_share_bootstrap_se
    )
  ) %>%
  select(-fixed_share_outmigration_rate_iv)

counterfactual_in_state_by_cohort <- counterfactual_in_state_by_cohort %>%
  left_join(bootstrap_counterfactual_by_cohort, by = 'grad_y')

# Conditional OOS counterfactual uncertainty. As for the in-state band, this
# applies the completed bootstrap's gamma_out draws to the full-sample fitted
# actual choice indices and therefore conditions on those fitted indices.
oos_counterfactual_index_bootstrap <-
  oos_counterfactual_cells$fitted_count_index * exp(outer(
    oos_counterfactual_cells$share_change_to_base_year,
    iv_gamma_out_bootstrap_draws
  ))

oos_fixed_share_bootstrap_matrix <- do.call(rbind, lapply(
  counterfactual_years,
  function(cohort) {
    cohort_rows <- which(oos_counterfactual_cells$grad_y == cohort)
    cohort_origins <- unique(
      oos_counterfactual_cells$o_state[cohort_rows]
    )

    origin_expected_outmigration <- lapply(
      cohort_origins,
      function(origin_state) {
        origin_rows <- cohort_rows[
          oos_counterfactual_cells$o_state[cohort_rows] == origin_state
        ]
        outside_rows <- origin_rows[
          oos_counterfactual_cells$alternative[origin_rows] != 'Alabama'
        ]
        origin_size <- sum(
          oos_counterfactual_cells$fitted_count_index[origin_rows]
        )
        origin_size * colSums(
          oos_counterfactual_index_bootstrap[
            outside_rows, , drop = FALSE
          ]
        ) / colSums(
          oos_counterfactual_index_bootstrap[
            origin_rows, , drop = FALSE
          ]
        )
      }
    )

    Reduce(`+`, origin_expected_outmigration) /
      sum(oos_counterfactual_cells$fitted_count_index[cohort_rows])
  }
))
rownames(oos_fixed_share_bootstrap_matrix) <- counterfactual_years

bootstrap_oos_counterfactual_by_cohort <- data.frame(
  grad_y = counterfactual_years,
  fixed_share_bootstrap_se = apply(
    oos_fixed_share_bootstrap_matrix, 1, sd
  ),
  row.names = NULL
) %>%
  left_join(
    counterfactual_oos_by_cohort %>%
      select(grad_y, fixed_share_outmigration_rate_iv),
    by = 'grad_y'
  ) %>%
  mutate(
    fixed_share_ci_lower = pmax(
      0,
      fixed_share_outmigration_rate_iv -
        qnorm(0.975) * fixed_share_bootstrap_se
    ),
    fixed_share_ci_upper = pmin(
      1,
      fixed_share_outmigration_rate_iv +
        qnorm(0.975) * fixed_share_bootstrap_se
    )
  ) %>%
  select(-fixed_share_outmigration_rate_iv)

counterfactual_oos_by_cohort <- counterfactual_oos_by_cohort %>%
  left_join(
    bootstrap_oos_counterfactual_by_cohort,
    by = 'grad_y'
  )

# Translate probability effects into cohort flows and cumulative departures ----

in_state_enrollment_by_graduation_cohort <- readRDS(
  paste0(pathHome, 'data/ef_by_state_panel.rds')
) %>%
  ungroup() %>%
  filter(UNITID == 100751, LINE == 1) %>%
  transmute(
    grad_y = y + 4L,
    in_state_first_year_enrollment = EFRES01
  ) %>%
  filter(grad_y %in% counterfactual_years) %>%
  arrange(grad_y)

counterfactual_in_state_by_cohort <- counterfactual_in_state_by_cohort %>%
  left_join(in_state_enrollment_by_graduation_cohort, by = 'grad_y') %>%
  arrange(grad_y) %>%
  mutate(
    induced_leavers_iv = in_state_first_year_enrollment *
      induced_outmigration_rate_iv,
    induced_leavers_two_step_ols = in_state_first_year_enrollment *
      induced_outmigration_rate_two_step_ols,
    induced_leavers_one_shot = in_state_first_year_enrollment *
      induced_outmigration_rate_one_shot,
    cumulative_induced_leavers_iv = cumsum(induced_leavers_iv),
    cumulative_induced_leavers_two_step_ols =
      cumsum(induced_leavers_two_step_ols),
    cumulative_induced_leavers_one_shot =
      cumsum(induced_leavers_one_shot)
  )

stopifnot(!anyNA(counterfactual_in_state_by_cohort))

last_counterfactual_cohort <- max(counterfactual_in_state_by_cohort$grad_y)
last_cohort_results <- counterfactual_in_state_by_cohort %>%
  filter(grad_y == last_counterfactual_cohort)

actual_outmigration_bootstrap_matrix <- matrix(
  counterfactual_in_state_by_cohort$actual_outmigration_rate,
  nrow = length(counterfactual_years),
  ncol = length(iv_gamma_bootstrap_draws)
)
induced_outmigration_bootstrap_matrix <-
  actual_outmigration_bootstrap_matrix - fixed_share_bootstrap_matrix
induced_leavers_bootstrap_matrix <-
  counterfactual_in_state_by_cohort$in_state_first_year_enrollment *
  induced_outmigration_bootstrap_matrix

last_cohort_gap_bootstrap <- 100 *
  induced_outmigration_bootstrap_matrix[length(counterfactual_years), ]
average_gap_bootstrap <- 100 *
  colMeans(induced_outmigration_bootstrap_matrix)
cumulative_leavers_bootstrap <- colSums(induced_leavers_bootstrap_matrix)

normal_bootstrap_ci <- function(point_estimate, bootstrap_draws) {
  point_estimate + c(-1, 1) *
    qnorm(0.975) * sd(bootstrap_draws)
}

last_cohort_gap_bootstrap_ci <- normal_bootstrap_ci(
  100 * last_cohort_results$induced_outmigration_rate_iv,
  last_cohort_gap_bootstrap
)
average_gap_bootstrap_ci <- normal_bootstrap_ci(
  100 * mean(
    counterfactual_in_state_by_cohort$induced_outmigration_rate_iv
  ),
  average_gap_bootstrap
)
cumulative_leavers_bootstrap_ci <- normal_bootstrap_ci(
  sum(counterfactual_in_state_by_cohort$induced_leavers_iv),
  cumulative_leavers_bootstrap
)

counterfactual_in_state_summary <- data.frame(
  statistic = c(
    'IV gamma_in',
    'Two-step OLS gamma_in (exogeneity benchmark)',
    'One-shot MNL gamma_in (previous paper benchmark)',
    paste0(last_counterfactual_cohort,
           ' actual predicted out-migration rate'),
    paste0(last_counterfactual_cohort,
           ' fixed-2006-share out-migration rate: IV'),
    paste0(last_counterfactual_cohort,
           ' IV-induced out-migration gap (percentage points)'),
    paste0(last_counterfactual_cohort,
           ' IV-induced gap: bootstrap 95% CI lower'),
    paste0(last_counterfactual_cohort,
           ' IV-induced gap: bootstrap 95% CI upper'),
    paste0(last_counterfactual_cohort,
           ' IV-induced gap relative to actual rate (percent)'),
    paste0(last_counterfactual_cohort,
           ' two-step OLS gap (percentage points)'),
    paste0(last_counterfactual_cohort,
           ' one-shot MNL gap (percentage points)'),
    'Average IV-induced gap across cohorts (percentage points)',
    'Average IV-induced gap: bootstrap 95% CI lower',
    'Average IV-induced gap: bootstrap 95% CI upper',
    'Cumulative IV-induced in-state leavers',
    'Cumulative IV-induced leavers: bootstrap 95% CI lower',
    'Cumulative IV-induced leavers: bootstrap 95% CI upper',
    'Cumulative two-step OLS induced in-state leavers',
    'Cumulative one-shot MNL induced in-state leavers',
    'Maximum fitted-versus-observed actual rate difference'
  ),
  value = c(
    gamma_in_iv_unclustered$estimate,
    gamma_in_ols_unclustered$estimate,
    gamma_in_current$estimate,
    last_cohort_results$actual_outmigration_rate,
    last_cohort_results$fixed_share_outmigration_rate_iv,
    100 * last_cohort_results$induced_outmigration_rate_iv,
    last_cohort_gap_bootstrap_ci[1],
    last_cohort_gap_bootstrap_ci[2],
    100 * last_cohort_results$induced_outmigration_rate_iv /
      last_cohort_results$actual_outmigration_rate,
    100 * last_cohort_results$induced_outmigration_rate_two_step_ols,
    100 * last_cohort_results$induced_outmigration_rate_one_shot,
    100 * mean(counterfactual_in_state_by_cohort$
                 induced_outmigration_rate_iv),
    average_gap_bootstrap_ci[1],
    average_gap_bootstrap_ci[2],
    sum(counterfactual_in_state_by_cohort$induced_leavers_iv),
    cumulative_leavers_bootstrap_ci[1],
    cumulative_leavers_bootstrap_ci[2],
    sum(counterfactual_in_state_by_cohort$induced_leavers_two_step_ols),
    sum(counterfactual_in_state_by_cohort$induced_leavers_one_shot),
    maximum_actual_fit_difference
  ),
  row.names = NULL
)

stopifnot(!anyNA(counterfactual_oos_by_cohort))

fiscal_baseline_year <- 2019L
fiscal_baseline_oos_results <- counterfactual_oos_by_cohort %>%
  filter(grad_y == fiscal_baseline_year)
last_oos_cohort_results <- counterfactual_oos_by_cohort %>%
  filter(grad_y == last_counterfactual_cohort)

attendance_reference_oos_results <- counterfactual_oos_by_cohort %>%
  filter(grad_y == attendance_calibration_year)

counterfactual_oos_summary <- data.frame(
  statistic = c(
    'IV gamma_out',
    paste0('Attendance effect in ', attendance_calibration_year,
           ' calibration cohort (pp)'),
    paste0(attendance_calibration_year,
           ' conditional-UA actual retention rate'),
    'Fixed no-UA Alabama retention probability',
    paste0(fiscal_baseline_year,
           ' conditional-UA actual retention rate'),
    paste0(fiscal_baseline_year,
           ' conditional-UA fixed-2006-share retention rate'),
    paste0(fiscal_baseline_year,
           ' out-migration gap relative to fixed 2006 shares (pp)'),
    paste0(fiscal_baseline_year,
           ' attendance effect at actual peer vector (pp)'),
    paste0(last_counterfactual_cohort,
           ' conditional-UA actual out-migration rate'),
    paste0(last_counterfactual_cohort,
           ' conditional-UA fixed-2006-share out-migration rate'),
    'Maximum OOS fitted-versus-observed actual rate difference'
  ),
  value = c(
    gamma_out_iv,
    100 * attendance_effect_at_calibration,
    attendance_reference_oos_results$actual_retention_rate,
    fixed_no_ua_retention,
    fiscal_baseline_oos_results$actual_retention_rate,
    fiscal_baseline_oos_results$fixed_share_retention_rate_iv,
    100 * fiscal_baseline_oos_results$peer_induced_outmigration_rate_iv,
    100 * fiscal_baseline_oos_results$attendance_effect_actual,
    last_oos_cohort_results$actual_outmigration_rate,
    last_oos_cohort_results$fixed_share_outmigration_rate_iv,
    maximum_oos_actual_fit_difference
  ),
  row.names = NULL
)

# Figures and saved results -----------------------------------------------------

counterfactual_figure_directory <- file.path(
  'figures',
  'analysis_mlogit_iv'
)
dir.create(counterfactual_figure_directory,
           recursive = TRUE, showWarnings = FALSE)

counterfactual_main_plot_data <- counterfactual_in_state_by_cohort %>%
  select(
    grad_y,
    `Actual OOS Shares` = actual_outmigration_rate,
    `Fixed 2006 OOS Shares (IV)` = fixed_share_outmigration_rate_iv
  ) %>%
  pivot_longer(
    cols = -grad_y,
    names_to = 'scenario',
    values_to = 'outmigration_rate'
  ) %>%
  complete(
    grad_y = 2006:2023,
    scenario
  ) %>%
  mutate(
    scenario = factor(
      scenario,
      levels = c('Actual OOS Shares', 'Fixed 2006 OOS Shares (IV)')
    )
  )

counterfactual_band_data <- counterfactual_in_state_by_cohort %>%
  transmute(
    grad_y,
    ci_lower = 100 * fixed_share_ci_lower,
    ci_upper = 100 * fixed_share_ci_upper
  ) %>%
  complete(grad_y = 2006:2023)

counterfactual_in_state_plot <- ggplot(
  counterfactual_main_plot_data,
  aes(
    x = grad_y,
    y = 100 * outmigration_rate,
    color = scenario,
    group = scenario
  )
) +
  geom_ribbon(
    data = counterfactual_band_data,
    aes(x = grad_y, ymin = ci_lower, ymax = ci_upper),
    inherit.aes = FALSE,
    fill = '#21908CFF',
    alpha = 0.18,
    na.rm = TRUE
  ) +
  geom_line(linewidth = 1, na.rm = TRUE) +
  geom_point(size = 1.5, na.rm = TRUE) +
  scale_color_manual(values = c(
    'Actual OOS Shares' = '#440154FF',
    'Fixed 2006 OOS Shares (IV)' = '#21908CFF'
  )) +
  scale_x_continuous(breaks = seq(2006, 2023, by = 2)) +
  labs(
    x = 'Graduation Year',
    y = 'Share Out-Migrating (%)',
    color = NULL,
    subtitle = paste(
      'Preferred IV coefficient; first-step predictions include all',
      '2006-2023 cohorts'
    )
  ) +
  theme_classic(base_size = 11) +
  theme(
    panel.grid.major.y = element_line(
      color = 'gray80',
      linetype = 'dashed'
    ),
    legend.position = 'bottom'
  )

counterfactual_oos_plot_data <- counterfactual_oos_by_cohort %>%
  select(
    grad_y,
    `Actual OOS Shares` = actual_outmigration_rate,
    `Fixed 2006 OOS Shares (IV)` = fixed_share_outmigration_rate_iv
  ) %>%
  pivot_longer(
    cols = -grad_y,
    names_to = 'scenario',
    values_to = 'outmigration_rate'
  ) %>%
  complete(grad_y = 2006:2023, scenario) %>%
  mutate(
    scenario = factor(
      scenario,
      levels = c('Actual OOS Shares', 'Fixed 2006 OOS Shares (IV)')
    )
  )

counterfactual_oos_band_data <- counterfactual_oos_by_cohort %>%
  transmute(
    grad_y,
    ci_lower = 100 * fixed_share_ci_lower,
    ci_upper = 100 * fixed_share_ci_upper
  ) %>%
  complete(grad_y = 2006:2023)

counterfactual_oos_plot <- ggplot(
  counterfactual_oos_plot_data,
  aes(
    x = grad_y,
    y = 100 * outmigration_rate,
    color = scenario,
    group = scenario
  )
) +
  geom_ribbon(
    data = counterfactual_oos_band_data,
    aes(x = grad_y, ymin = ci_lower, ymax = ci_upper),
    inherit.aes = FALSE,
    fill = '#21908CFF',
    alpha = 0.18,
    na.rm = TRUE
  ) +
  geom_line(linewidth = 1, na.rm = TRUE) +
  geom_point(size = 1.5, na.rm = TRUE) +
  scale_color_manual(values = c(
    'Actual OOS Shares' = '#440154FF',
    'Fixed 2006 OOS Shares (IV)' = '#21908CFF'
  )) +
  scale_x_continuous(breaks = seq(2006, 2023, by = 2)) +
  labs(
    x = 'Graduation Year',
    y = 'OOS Student Share Out-Migrating (%)',
    color = NULL,
    subtitle = paste(
      'Conditional on attending UA; Groen calibration does not enter',
      'the plotted counterfactual'
    )
  ) +
  theme_classic(base_size = 11) +
  theme(
    panel.grid.major.y = element_line(
      color = 'gray80',
      linetype = 'dashed'
    ),
    legend.position = 'bottom'
  )

counterfactual_comparison_plot_data <-
  counterfactual_in_state_by_cohort %>%
  select(
    grad_y,
    `Actual OOS Shares` = actual_outmigration_rate,
    `Fixed shares: IV` = fixed_share_outmigration_rate_iv,
    `Fixed shares: two-step OLS` =
      fixed_share_outmigration_rate_two_step_ols,
    `Fixed shares: one-shot MNL` =
      fixed_share_outmigration_rate_one_shot
  ) %>%
  pivot_longer(
    cols = -grad_y,
    names_to = 'scenario',
    values_to = 'outmigration_rate'
  ) %>%
  complete(grad_y = 2006:2023, scenario)

counterfactual_in_state_comparison_plot <- ggplot(
  counterfactual_comparison_plot_data,
  aes(
    x = grad_y,
    y = 100 * outmigration_rate,
    color = scenario,
    linetype = scenario,
    group = scenario
  )
) +
  geom_line(linewidth = 0.9, na.rm = TRUE) +
  scale_color_manual(values = c(
    'Actual OOS Shares' = '#440154FF',
    'Fixed shares: IV' = '#21908CFF',
    'Fixed shares: two-step OLS' = '#F28E2B',
    'Fixed shares: one-shot MNL' = 'grey55'
  )) +
  scale_linetype_manual(values = c(
    'Actual OOS Shares' = 'solid',
    'Fixed shares: IV' = 'solid',
    'Fixed shares: two-step OLS' = 'dotdash',
    'Fixed shares: one-shot MNL' = 'dashed'
  )) +
  scale_x_continuous(breaks = seq(2006, 2023, by = 2)) +
  labs(
    x = 'Graduation Year',
    y = 'Share Out-Migrating (%)',
    color = NULL,
    linetype = NULL,
    subtitle = paste(
      'IV counterfactual versus two-step OLS and the previous',
      'one-shot MNL benchmark'
    )
  ) +
  theme_classic(base_size = 11) +
  theme(
    panel.grid.major.y = element_line(
      color = 'gray80',
      linetype = 'dashed'
    ),
    legend.position = 'bottom'
  )

ggsave(
  file.path(counterfactual_figure_directory,
            'counterfactual_in_state_outmigration_iv_all_cohorts.png'),
  counterfactual_in_state_plot,
  width = 7, height = 5, dpi = 300
)
ggsave(
  file.path(counterfactual_figure_directory,
            paste0('counterfactual_in_state_outmigration_',
                   'comparison_all_cohorts.png')),
  counterfactual_in_state_comparison_plot,
  width = 7.5, height = 5.2, dpi = 300
)
ggsave(
  file.path(counterfactual_figure_directory,
            'counterfactual_oos_outmigration_iv_all_cohorts.png'),
  counterfactual_oos_plot,
  width = 7, height = 5, dpi = 300
)

cat('\nIV counterfactual in-state out-migration results\n')
cat('------------------------------------------------\n')
print(counterfactual_in_state_summary, digits = 5, row.names = FALSE)

cat('\nIV counterfactual OOS out-migration results\n')
cat('-------------------------------------------\n')
print(counterfactual_oos_summary, digits = 5, row.names = FALSE)

cat('\nCounterfactual construction note\n')
cat('--------------------------------\n')
cat(paste(
  'The counterfactual holds each fitted destination-by-cohort utility and its',
  'structural shock fixed at the actual-share estimate, then replaces the',
  'origin-share component with its 2006 value using gamma_in from the preferred',
  'unweighted IV projection. First-step utilities are recovered for every',
  '2006-2023 cohort, including 2020; the IV coefficient continues to come from',
  'the preferred second-stage sample with valid controls. The two-step OLS and',
  'one-shot MNL benchmarks use the same fitted actual utilities, reference',
  'population, cohorts, and share paths; only gamma changes. The plotted',
  'pointwise bands are centered on the full-sample counterfactual and use the',
  'standard deviation across transformed bootstrap draws. They condition on the',
  'full-sample fitted actual probabilities because the bootstrap did not save',
  'a full set',
  'of destination-by-cohort utilities for every replication. The OOS',
  'counterfactual additionally normalizes within each origin-state/cohort choice',
  'set and uses gamma_out = gamma_in plus the OOS-minus-in-state first-step',
  'difference. Its plotted probabilities are conditional on attending UA and do',
  'not use the Groen calibration. The reported attendance effect uses one no-UA',
  'retention probability calibrated so that the attendance effect is 10',
  'percentage points for the observed 2008 OOS cohort, then holds that no-UA',
  'probability fixed across cohorts.\n'
))
} else {
  warning(paste(
    'Skipping migration counterfactual confidence intervals because',
    'tmp/analysis_mlogit_iv_bootstrap_results.rds is absent.',
    'Set MLOGIT_IV_RUN_BOOTSTRAP=1 to create it.'
  ))
}

# Shift-share placebo diagnostics --------------------------------------------

# Exact baseline exposure underlying the preferred IV -------------------------

# z_peer_non_alabama_count_growth_lso equals the fixed 2000 peer-flagship
# exposure below times leave-state-out UA OOS enrollment growth. Scale the
# exposure into percentage points solely to make event-study coefficients
# readable; this does not alter inference or the joint tests.
baseline_exposure <- readr::read_csv(
  paste0(
    pathHome,
    'data/market_iv/tables/market_exposure_by_state.csv'
  ),
  show_col_types = FALSE
) %>%
  transmute(
    originState = origin_state,
    baseline_exposure =
      peer_flagship_flow_share_pre_non_alabama,
    baseline_exposure_pp = 100 * baseline_exposure
  ) %>%
  left_join(states, by = 'originState') %>%
  transmute(
    d_state = state,
    baseline_exposure,
    baseline_exposure_pp
  )

# Population growth is available from the same Census population vintages used
# to construct the migration-rate denominator. Rebuild annual levels so growth
# is observed for every preferred cohort, including the 2006 reference year.
raw_state_names <- c(state.name, 'District of Columbia')

population_2000s <- read.csv(paste0(
  pathHome, 'data/state_pop/st-est00int-alldata.csv'
)) %>%
  filter(
    NAME %in% raw_state_names,
    SEX == 0,
    ORIGIN == 0,
    RACE == 0,
    AGEGRP == 0
  ) %>%
  select(NAME, starts_with('POPEST')) %>%
  pivot_longer(-NAME, names_to = 'series', values_to = 'population') %>%
  transmute(
    state = NAME,
    grad_y = as.integer(stringr::str_extract(series, '[0-9]{4}$')),
    population
  ) %>%
  filter(grad_y != 2010)

population_2010s <- read.csv(paste0(
  pathHome, 'data/state_pop/nst-est2020-alldata.csv'
)) %>%
  filter(NAME %in% raw_state_names) %>%
  select(NAME, starts_with('POPEST')) %>%
  pivot_longer(-NAME, names_to = 'series', values_to = 'population') %>%
  transmute(
    state = NAME,
    grad_y = as.integer(stringr::str_extract(series, '[0-9]{4}$')),
    population
  ) %>%
  filter(grad_y != 2020)

population_2020s <- read.csv(paste0(
  pathHome, 'data/state_pop/NST-EST2024-ALLDATA.csv'
)) %>%
  filter(NAME %in% raw_state_names) %>%
  select(NAME, starts_with('POPEST')) %>%
  pivot_longer(-NAME, names_to = 'series', values_to = 'population') %>%
  transmute(
    state = NAME,
    grad_y = as.integer(stringr::str_extract(series, '[0-9]{4}$')),
    population
  )

population_growth <- bind_rows(
  population_2000s,
  population_2010s,
  population_2020s
) %>%
  arrange(state, grad_y) %>%
  group_by(state) %>%
  mutate(
    population_growth = 100 * (population / lag(population) - 1)
  ) %>%
  ungroup() %>%
  filter(grad_y %in% paper_years) %>%
  mutate(
    state = if_else(
      state == 'District of Columbia',
      'Washington, D.C.',
      state
    )
  ) %>%
  transmute(
    d_state = state,
    grad_y,
    population_growth
  )

# Peer-enrollment controls -----------------------------------------------------

# These controls measure the change since 2000 in state j's share of pooled
# domestic first-time enrollment at fixed groups of UA peer institutions. Read
# the full audit panel rather than the compact 648-row Table 4 extract so the
# preferred structural-IV sample is not restricted to positive-flow cells.
peer_control_path_candidates <- c(
  paste0(
    pathHome,
    'data/peer_controls/audit/expanded_peer_groups_state_year_shares.csv'
  ),
  paste0(
    pathHome,
    paste0(
      'data/market_iv/peer_controls/audit/',
      'expanded_peer_groups_state_year_shares.csv'
    )
  )
)
peer_control_path <- peer_control_path_candidates[
  file.exists(peer_control_path_candidates)
][1]
if (is.na(peer_control_path)) {
  stop(
    'Could not find the full peer-group state-year share panel at either ',
    'supported shared-data location.'
  )
}

peer_control_groups <- c('core3', 'core6', 'core10', 'core15')
peer_control_names <- paste0('h_', peer_control_groups)

peer_controls <- readr::read_csv(
  peer_control_path,
  show_col_types = FALSE
) %>%
  filter(
    peer_group %in% peer_control_groups,
    mapped_grad_year %in% paper_years
  ) %>%
  transmute(
    originState = origin_state,
    grad_y = as.integer(mapped_grad_year),
    peer_control = paste0('h_', peer_group),
    peer_share_change_pp = peer_origin_share_change_pp
  ) %>%
  distinct() %>%
  pivot_wider(
    names_from = peer_control,
    values_from = peer_share_change_pp
  ) %>%
  left_join(states, by = 'originState') %>%
  transmute(
    d_state = state,
    grad_y,
    across(all_of(peer_control_names))
  ) %>%
  arrange(d_state, grad_y)

stopifnot(
  setequal(names(peer_controls)[-(1:2)], peer_control_names),
  setequal(peer_controls$grad_y, paper_years),
  !anyNA(peer_controls),
  !anyDuplicated(peer_controls[c('d_state', 'grad_y')])
)

placebo_panel <- delta_second_stage %>%
  select(d_state, grad_y, z_lso, unemp, net_mig) %>%
  left_join(baseline_exposure, by = 'd_state') %>%
  left_join(population_growth, by = c('d_state', 'grad_y')) %>%
  left_join(peer_controls, by = c('d_state', 'grad_y')) %>%
  arrange(d_state, grad_y)

stopifnot(
  nrow(placebo_panel) == nrow(delta_second_stage),
  setequal(placebo_panel$grad_y, paper_years),
  n_distinct(placebo_panel$d_state) ==
    n_distinct(delta_second_stage$d_state),
  !anyNA(placebo_panel),
  !anyDuplicated(placebo_panel[c('d_state', 'grad_y')])
)

placebo_outcomes <- data.frame(
  outcome = c('unemp', 'net_mig', 'population_growth'),
  outcome_label = c(
    'Destination-state unemployment rate',
    'Destination-state net migration rate',
    'Destination-state population growth'
  ),
  y_axis_label = c(
    'Unemployment-rate change (pp)',
    'Net-migration-rate change (pp)',
    'Population-growth change (pp)'
  ),
  stringsAsFactors = FALSE
)

# Report both raw and identifying (state/cohort residualized) variation in the
# instrument. The latter is the economically relevant standardization for a
# two-way fixed-effects placebo coefficient.
z_raw_standard_deviation <- sd(placebo_panel$z_lso)
z_within_standard_deviation <- sd(resid(fixest::feols(
  z_lso ~ 1 | d_state + grad_y,
  data = placebo_panel,
  notes = FALSE
)))

# 1. Actual-IV placebo regressions --------------------------------------------

placebo_peer_control_specs <- data.frame(
  peer_control = c('none', peer_control_names),
  peer_control_label = c(
    'None',
    '3-school peer group',
    '6-school peer group',
    '10-school peer group',
    '15-school peer group'
  ),
  stringsAsFactors = FALSE
)

estimate_actual_iv_placebo <- function(outcome, peer_control) {
  rhs <- if (peer_control == 'none') {
    'z_lso'
  } else {
    paste('z_lso +', peer_control)
  }

  model <- fixest::feols(
    as.formula(paste0(
      outcome,
      ' ~ ', rhs, ' | d_state + grad_y'
    )),
    data = placebo_panel,
    cluster = ~d_state,
    notes = FALSE
  )

  coefficient_table <- fixest::coeftable(model)
  stopifnot('z_lso' %in% rownames(coefficient_table))

  list(
    model = model,
    result = data.frame(
      outcome = outcome,
      peer_control = peer_control,
      theta = unname(coefficient_table['z_lso', 'Estimate']),
      cluster_se = unname(
        coefficient_table['z_lso', 'Std. Error']
      ),
      p_value = unname(
        coefficient_table['z_lso', 'Pr(>|t|)']
      ),
      z_raw_standard_deviation = z_raw_standard_deviation,
      effect_of_one_raw_sd_z = unname(
        coefficient_table['z_lso', 'Estimate']
      ) * z_raw_standard_deviation,
      z_within_standard_deviation = z_within_standard_deviation,
      effect_of_one_within_sd_z = unname(
        coefficient_table['z_lso', 'Estimate']
      ) * z_within_standard_deviation,
      N = nobs(model),
      destination_clusters = n_distinct(placebo_panel$d_state),
      stringsAsFactors = FALSE
    )
  )
}

actual_iv_placebo_specifications <- tidyr::crossing(
  outcome = placebo_outcomes$outcome,
  peer_control = placebo_peer_control_specs$peer_control
)

actual_iv_placebo_runs <- lapply(
  seq_len(nrow(actual_iv_placebo_specifications)),
  function(i) {
    estimate_actual_iv_placebo(
      actual_iv_placebo_specifications$outcome[i],
      actual_iv_placebo_specifications$peer_control[i]
    )
  }
)
names(actual_iv_placebo_runs) <- paste(
  actual_iv_placebo_specifications$outcome,
  actual_iv_placebo_specifications$peer_control,
  sep = '__'
)

actual_iv_placebo_results <- bind_rows(lapply(
  actual_iv_placebo_runs,
  function(run) run$result
)) %>%
  left_join(placebo_outcomes, by = 'outcome') %>%
  left_join(placebo_peer_control_specs, by = 'peer_control') %>%
  arrange(outcome, match(peer_control, placebo_peer_control_specs$peer_control)) %>%
  select(
    outcome, outcome_label, peer_control, peer_control_label,
    theta, cluster_se, p_value,
    z_raw_standard_deviation, effect_of_one_raw_sd_z,
    z_within_standard_deviation, effect_of_one_within_sd_z,
    N, destination_clusters
  )

# 2. Baseline-exposure-by-cohort diagnostics ---------------------------------

cluster_wald_test <- function(model, term_pattern, cluster_count) {
  terms <- grep(term_pattern, names(coef(model)), value = TRUE)
  if (length(terms) == 0L) {
    stop('No coefficients matched the requested joint-test pattern.')
  }

  estimates <- coef(model)[terms]
  covariance <- vcov(model)[terms, terms, drop = FALSE]
  wald_chisq <- as.numeric(
    crossprod(estimates, qr.solve(covariance, estimates))
  )
  df1 <- length(terms)
  df2 <- cluster_count - 1L
  f_statistic <- wald_chisq / df1

  data.frame(
    F = f_statistic,
    df1 = df1,
    df2 = df2,
    p_value = pf(f_statistic, df1, df2, lower.tail = FALSE),
    stringsAsFactors = FALSE
  )
}

estimate_exposure_event_study <- function(outcome) {
  model <- fixest::feols(
    as.formula(paste0(
      outcome,
      ' ~ i(grad_y, baseline_exposure_pp, ref = ',
      base_year,
      ') | d_state + grad_y'
    )),
    data = placebo_panel,
    cluster = ~d_state,
    notes = FALSE
  )

  coefficient_table <- fixest::coeftable(model)
  event_terms <- grep(
    '^grad_y::[0-9]{4}:baseline_exposure_pp$',
    rownames(coefficient_table),
    value = TRUE
  )
  if (length(event_terms) != length(paper_years) - 1L) {
    stop(
      'Expected ', length(paper_years) - 1L,
      ' exposure-by-cohort coefficients; found ',
      length(event_terms), '.'
    )
  }

  cluster_count <- n_distinct(placebo_panel$d_state)
  critical_value <- qt(0.975, df = cluster_count - 1L)

  coefficients <- data.frame(
    outcome = outcome,
    grad_y = as.integer(stringr::str_extract(event_terms, '[0-9]{4}')),
    estimate = unname(coefficient_table[event_terms, 'Estimate']),
    cluster_se = unname(coefficient_table[event_terms, 'Std. Error']),
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      ci_lower = estimate - critical_value * cluster_se,
      ci_upper = estimate + critical_value * cluster_se
    ) %>%
    bind_rows(data.frame(
      outcome = outcome,
      grad_y = base_year,
      estimate = 0,
      cluster_se = NA_real_,
      ci_lower = 0,
      ci_upper = 0,
      stringsAsFactors = FALSE
    )) %>%
    arrange(grad_y)

  joint_test <- cluster_wald_test(
    model,
    '^grad_y::[0-9]{4}:baseline_exposure_pp$',
    cluster_count
  ) %>%
    mutate(
      outcome = outcome,
      N = nobs(model),
      destination_clusters = cluster_count,
      .before = 1
    )

  list(
    model = model,
    coefficients = coefficients,
    joint_test = joint_test
  )
}

exposure_event_study_runs <- lapply(
  placebo_outcomes$outcome,
  estimate_exposure_event_study
)
names(exposure_event_study_runs) <- placebo_outcomes$outcome

exposure_event_study_coefficients <- bind_rows(lapply(
  exposure_event_study_runs,
  function(run) run$coefficients
)) %>%
  left_join(placebo_outcomes, by = 'outcome') %>%
  mutate(
    y_axis_label = factor(
      y_axis_label,
      levels = placebo_outcomes$y_axis_label
    )
  )

exposure_event_study_joint_tests <- bind_rows(lapply(
  exposure_event_study_runs,
  function(run) run$joint_test
)) %>%
  left_join(
    placebo_outcomes %>% select(outcome, outcome_label),
    by = 'outcome'
  ) %>%
  select(
    outcome, outcome_label, F, df1, df2, p_value,
    N, destination_clusters
  )

# Event-study coefficient figure ---------------------------------------------

exposure_event_study_plot <- ggplot(
  exposure_event_study_coefficients,
  aes(x = grad_y, y = estimate)
) +
  geom_hline(yintercept = 0, color = 'gray45', linewidth = 0.4) +
  geom_vline(
    xintercept = base_year,
    color = 'gray65',
    linetype = 'dashed',
    linewidth = 0.4
  ) +
  geom_line(color = '#440154FF', linewidth = 0.7, na.rm = TRUE) +
  geom_errorbar(
    aes(ymin = ci_lower, ymax = ci_upper),
    color = '#440154FF',
    width = 0.25,
    linewidth = 0.55,
    na.rm = TRUE
  ) +
  geom_point(color = '#440154FF', size = 2, na.rm = TRUE) +
  facet_wrap(
    ~y_axis_label,
    ncol = 1,
    scales = 'free_y'
  ) +
  scale_x_continuous(
    breaks = c(seq(2006, 2018, by = 2), 2019, 2021, 2023),
    limits = range(paper_years)
  ) +
  labs(
    x = 'Graduation cohort',
    y = 'Coefficient on 2000 baseline exposure (per 1 pp)'
  ) +
  theme_classic(base_size = 11) +
  theme(
    panel.grid.major.y = element_line(
      color = 'gray85',
      linetype = 'dashed'
    ),
    strip.background = element_blank(),
    strip.text = element_text(face = 'bold')
  )

figure_directory <- file.path('figures', 'analysis_mlogit_iv')
dir.create(figure_directory, recursive = TRUE, showWarnings = FALSE)
event_study_figure_path <- file.path(
  figure_directory,
  'shift_share_baseline_exposure_placebos.png'
)

ggsave(
  event_study_figure_path,
  exposure_event_study_plot,
  width = 7,
  height = 9,
  dpi = 300
)

# Console results only; do not save tables to disk. ---------------------------

cat('\nActual shift-share IV placebo regressions\n')
cat('------------------------------------------\n')
print(
  actual_iv_placebo_results,
  digits = 6,
  row.names = FALSE
)

cat('\nBaseline exposure x cohort joint tests\n')
cat('--------------------------------------\n')
print(
  exposure_event_study_joint_tests,
  digits = 6,
  row.names = FALSE
)

cat('\nConstruction checks\n')
cat('-------------------\n')
cat('Second-stage cells:', nrow(placebo_panel), '\n')
cat('Destination states:', n_distinct(placebo_panel$d_state), '\n')
cat('Cohorts:', paste(paper_years, collapse = ', '), '\n')
cat('Peer controls:', paste(peer_control_names, collapse = ', '), '\n')
cat('Peer-control panel:', peer_control_path, '\n')
cat('Omitted event-study cohort:', base_year, '\n')
cat('Event-study figure:', event_study_figure_path, '\n')

# Major-specific preference heterogeneity ------------------------------------

major_levels <- c(
  'Business', 'Engineering', 'Marketing', 'Finance', 'Accounting',
  'Nursing', 'Economics', 'Education', 'Other STEM', 'All other'
)
major_nonbase <- setdiff(major_levels, 'All other')
major_keys <- setNames(
  c('business', 'engineering', 'marketing', 'finance', 'accounting',
    'nursing', 'economics', 'education', 'other_stem', 'all_other'),
  major_levels
)

classify_major <- function(field) {
  case_when(
    field %in% c('Business', 'Marketing', 'Finance', 'Nursing',
                 'Economics', 'Education', 'Accounting',
                 'Engineering') ~ field,
    field %in% c('Biology', 'Mathematics', 'Chemistry', 'Statistics',
                 'Physics', 'Medicine') ~ 'Other STEM',
    TRUE ~ 'All other'
  )
}

fixest_estimate <- function(model, term) {
  estimates <- coef(model)
  if (!(term %in% names(estimates))) {
    stop('Coefficient "', term, '" not found in fixest model.')
  }
  unname(estimates[[term]])
}

felm_estimate <- function(model, term_pattern) {
  tab <- summary(model)$coefficients
  hit <- grep(term_pattern, rownames(tab))
  if (length(hit) != 1L) {
    stop('Expected one coefficient matching "', term_pattern,
         '"; found ', length(hit), '.')
  }
  unname(tab[hit, 'Estimate'])
}

# Population major distributions ----------------------------------------------

# Anchor the aggregate distribution to the 2023 UA Interactive Factbook counts
# already used in the paper. Split it by residency using major-specific OOS odds
# from all linked cohorts, controlling flexibly for cohort. A common intercept
# shift makes the implied 2023 OOS share match the full Commencement records.
factbook_major_counts <- data.frame(
  major = major_levels,
  factbook_count = c(649, 723, 510, 497, 207, 374, 69, 363, 340, 2773),
  stringsAsFactors = FALSE
) %>%
  mutate(factbook_share = factbook_count / sum(factbook_count))

states <- data.frame(
  originState = c(state.abb, 'DC'),
  state = c(state.name, 'Washington, D.C.')
)

linked_profiles_raw <- read.csv(
  paste0(pathHome, 'data/linked_commencement_revelio_profile_data.csv')
)

linked_major_residency <- linked_profiles_raw %>%
  filter(!is.na(Year), Year %in% 2006:2023) %>%
  mutate(originState = gsub(' ', '', originState)) %>%
  filter(originState %in% states$originState) %>%
  transmute(
    user_id,
    grad_y = as.integer(Year),
    major = factor(classify_major(field), levels = major_levels),
    is_oos = as.integer(originState != 'AL')
  )

major_residency_model <- glm(
  is_oos ~ major + factor(grad_y),
  family = binomial(),
  data = linked_major_residency
)

comm_all <- read.csv(paste0(pathHome, 'data/all_alabama_data.csv'))
target_2023_residency <- comm_all %>%
  filter(Year == 2023) %>%
  mutate(originState = gsub(' ', '', Origin.State)) %>%
  filter(originState %in% states$originState) %>%
  summarize(
    target_oos_share = mean(originState != 'AL'),
    N_domestic = n(),
    .groups = 'drop'
  )

major_prediction_grid <- data.frame(
  major = factor(major_levels, levels = major_levels),
  grad_y = 2023L
)
major_log_odds_2023 <- as.numeric(predict(
  major_residency_model,
  newdata = major_prediction_grid,
  type = 'link'
))

target_oos_share_2023 <- target_2023_residency$target_oos_share[[1]]
factbook_share_vector <- factbook_major_counts$factbook_share

residency_intercept_shift <- uniroot(
  function(shift) {
    sum(factbook_share_vector * plogis(major_log_odds_2023 + shift)) -
      target_oos_share_2023
  },
  interval = c(-20, 20),
  tol = 1e-12
)$root

major_distribution_by_residency <- factbook_major_counts %>%
  mutate(
    probability_oos = plogis(
      major_log_odds_2023 + residency_intercept_shift
    ),
    in_state_share = factbook_share * (1 - probability_oos) /
      (1 - target_oos_share_2023),
    oos_share = factbook_share * probability_oos /
      target_oos_share_2023
  )

stopifnot(
  abs(sum(major_distribution_by_residency$in_state_share) - 1) < 1e-8,
  abs(sum(major_distribution_by_residency$oos_share) - 1) < 1e-8,
  max(abs(
    (1 - target_oos_share_2023) *
      major_distribution_by_residency$in_state_share +
      target_oos_share_2023 * major_distribution_by_residency$oos_share -
      major_distribution_by_residency$factbook_share
  )) < 1e-8
)

in_major_weights <- setNames(
  major_distribution_by_residency$in_state_share,
  major_distribution_by_residency$major
)
out_major_weights <- setNames(
  major_distribution_by_residency$oos_share,
  major_distribution_by_residency$major
)

# Choice-model sample -----------------------------------------------------------

dest <- readRDS(paste0(pathHome, 'revelio_data/first_spell_join.rds')) %>%
  ungroup() %>%
  filter(country == 'United States') %>%
  select(user_id, d_state = state)

origin <- linked_profiles_raw %>%
  filter(!is.na(Year)) %>%
  mutate(originState = gsub(' ', '', originState)) %>%
  left_join(states, by = 'originState') %>%
  filter(!is.na(state)) %>%
  transmute(
    user_id,
    grad_y = as.integer(Year),
    o_state = state,
    major = factor(classify_major(field), levels = major_levels)
  )

join_major <- dest %>%
  inner_join(origin, by = 'user_id') %>%
  filter(grad_y %in% paper_years) %>%
  ungroup()

stopifnot(
  nrow(join_major) == n_distinct(join_major$user_id),
  !anyNA(join_major$major),
  n_distinct(join_major$d_state) == nrow(states)
)

# Destination-cohort regressors ------------------------------------------------

comm <- comm_all %>%
  mutate(originState = gsub(' ', '', Origin.State)) %>%
  left_join(states, by = 'originState') %>%
  filter(!is.na(state), Year %in% paper_years) %>%
  rename(grad_y = Year) %>%
  ungroup()

cohort_sizes <- comm %>%
  group_by(grad_y) %>%
  summarize(
    N_cohort = n(),
    N_AL_cohort = sum(state == 'Alabama'),
    .groups = 'drop'
  )

shares <- comm %>%
  count(grad_y, state, name = 'N_origin') %>%
  left_join(cohort_sizes, by = 'grad_y') %>%
  mutate(o_share = 100 * N_origin / N_cohort) %>%
  select(grad_y, state, o_share, N_origin, N_cohort, N_AL_cohort)

pull_factors <- readRDS(paste0(pathHome, 'data/pull_factors.rds')) %>%
  ungroup() %>%
  mutate(state = if_else(state == 'District of Columbia',
                         'Washington, D.C.', state)) %>%
  transmute(state, grad_y = y, unemp = ur, net_mig = net_rate)

market_iv <- read_csv(
  paste0(pathHome, 'data/market_iv/data/market_iv_panel.csv'),
  show_col_types = FALSE
) %>%
  transmute(
    originState = origin_state,
    grad_y = grad_year,
    z_lso = z_peer_non_alabama_count_growth_lso
  ) %>%
  left_join(states, by = 'originState') %>%
  transmute(d_state = state, grad_y, z_lso)

# Grouped multinomial likelihood -----------------------------------------------

choice_counts_major <- join_major %>%
  count(grad_y, o_state, major, alternative = d_state, name = 'n_choice')

origin_cohort_majors <- join_major %>%
  distinct(grad_y, o_state, major)

destination_support <- join_major %>%
  count(grad_y, alternative = d_state, name = 'N_dest')

choice_cells_full_major <- crossing(
  origin_cohort_majors,
  alternative = states$state
) %>%
  left_join(
    choice_counts_major,
    by = c('grad_y', 'o_state', 'major', 'alternative')
  ) %>%
  mutate(n_choice = replace_na(n_choice, 0L))

choice_cells_saturated_major <- origin_cohort_majors %>%
  inner_join(destination_support, by = 'grad_y', relationship = 'many-to-many') %>%
  select(grad_y, o_state, major, alternative) %>%
  left_join(
    choice_counts_major,
    by = c('grad_y', 'o_state', 'major', 'alternative')
  ) %>%
  mutate(n_choice = replace_na(n_choice, 0L))

add_major_choice_covariates <- function(data) {
  result <- data %>%
    left_join(shares, by = c('grad_y', 'alternative' = 'state')) %>%
    left_join(pull_factors,
              by = c('grad_y', 'alternative' = 'state')) %>%
    mutate(
      o_share = replace_na(o_share, 0),
      o_share = if_else(alternative == 'Alabama', 0, o_share),
      is_oos = as.integer(o_state != 'Alabama'),
      is_in_state = 1L - is_oos,
      home_oos = as.integer(is_oos == 1 & o_state == alternative),
      home_ins = as.integer(is_in_state == 1 & alternative == 'Alabama'),
      o_share_oos = is_oos * o_share,
      o_share_ins = is_in_state * o_share,
      o_share_outdiff = is_oos * o_share,
      al_oos = as.integer(is_oos == 1 & alternative == 'Alabama'),
      al_ins = as.integer(is_in_state == 1 & alternative == 'Alabama'),
      origin_cohort_major = interaction(
        o_state, grad_y, major, drop = TRUE, sep = '__'
      ),
      dest_cohort = interaction(
        alternative, grad_y, drop = TRUE, sep = '__'
      )
    )

  # Weighted effect coding makes the omitted common coefficient the relevant
  # residency-specific population mean. Only H-1 columns are required; the
  # All-other deviation follows from the weighted-zero restriction.
  for (major_name in major_nonbase) {
    key <- major_keys[[major_name]]
    result[[paste0('share_dev_in_', key)]] <-
      result$is_in_state * result$o_share *
      (as.integer(result$major == major_name) -
         in_major_weights[[major_name]])
    result[[paste0('share_dev_out_', key)]] <-
      result$is_oos * result$o_share *
      (as.integer(result$major == major_name) -
         out_major_weights[[major_name]])
  }
  result
}

choice_cells_full_major <- add_major_choice_covariates(
  choice_cells_full_major
)
choice_cells_saturated_major <- add_major_choice_covariates(
  choice_cells_saturated_major
)

share_dev_in_terms <- paste0(
  'share_dev_in_', major_keys[major_nonbase]
)
share_dev_out_terms <- paste0(
  'share_dev_out_', major_keys[major_nonbase]
)
share_deviation_terms <- c(share_dev_in_terms, share_dev_out_terms)

stopifnot(
  sum(choice_cells_full_major$n_choice) == nrow(join_major),
  sum(choice_cells_saturated_major$n_choice) == nrow(join_major),
  !anyNA(choice_cells_full_major[c('unemp', 'net_mig')]),
  !anyNA(choice_cells_saturated_major[c('unemp', 'net_mig')])
)

# One-step exogeneity benchmark with the same heterogeneity --------------------

one_step_formula <- as.formula(paste(
  'n_choice ~',
  paste(
    c(
      'home_oos', 'home_ins', 'o_share_oos', 'o_share_ins',
      share_deviation_terms,
      'unemp', 'net_mig',
      sprintf('i(grad_y, al_oos, ref = %d)', base_year),
      sprintf('i(grad_y, al_ins, ref = %d)', base_year)
    ),
    collapse = ' + '
  ),
  '| origin_cohort_major + alternative'
))

model_major_current_exogenous <- fepois(
  one_step_formula,
  data = choice_cells_full_major,
  vcov = ~o_state,
  notes = FALSE
)

# Saturated first step ----------------------------------------------------------

saturated_formula <- as.formula(paste(
  'n_choice ~',
  paste(
    c(
      'home_oos', 'home_ins', 'o_share_outdiff',
      share_deviation_terms,
      sprintf('i(grad_y, al_oos, ref = %d)', base_year)
    ),
    collapse = ' + '
  ),
  '| origin_cohort_major + dest_cohort'
))

model_major_saturated_first_step <- fepois(
  saturated_formula,
  data = choice_cells_saturated_major,
  vcov = ~o_state,
  notes = FALSE
)

pi_out_minus_in_major_mean <- fixest_estimate(
  model_major_saturated_first_step,
  'o_share_outdiff'
)

# Common unweighted second step -------------------------------------------------

dest_cohort_fe_major <- fixef(
  model_major_saturated_first_step,
  notes = FALSE
)[['dest_cohort']]

delta_second_stage_major <- data.frame(
  dest_cohort = names(dest_cohort_fe_major),
  delta_hat = unname(dest_cohort_fe_major),
  row.names = NULL
) %>%
  mutate(
    grad_y = as.integer(str_extract(dest_cohort, '[0-9]{4}$')),
    d_state = str_remove(dest_cohort, '__[0-9]{4}$')
  ) %>%
  group_by(grad_y) %>%
  mutate(delta_rel = delta_hat - delta_hat[d_state == 'Alabama']) %>%
  ungroup() %>%
  filter(d_state != 'Alabama') %>%
  left_join(
    shares %>% transmute(d_state = state, grad_y, o_share),
    by = c('d_state', 'grad_y')
  ) %>%
  mutate(o_share = replace_na(o_share, 0)) %>%
  left_join(
    pull_factors %>% rename(d_state = state),
    by = c('d_state', 'grad_y')
  ) %>%
  left_join(market_iv, by = c('d_state', 'grad_y'))

stopifnot(
  nrow(delta_second_stage_major) == 755L,
  !anyNA(delta_second_stage_major[c(
    'delta_rel', 'o_share', 'unemp', 'net_mig', 'z_lso'
  )]),
  !('N_dest' %in% names(delta_second_stage_major))
)

model_major_second_step_ols <- felm(
  delta_rel ~ o_share + unemp + net_mig |
    factor(d_state) + factor(grad_y),
  data = delta_second_stage_major
)

model_major_second_step_iv <- felm(
  delta_rel ~ unemp + net_mig |
    factor(d_state) + factor(grad_y) |
    (o_share ~ z_lso),
  data = delta_second_stage_major
)

gamma_in_major_mean_ols <- felm_estimate(
  model_major_second_step_ols,
  '^o_share$'
)
gamma_in_major_mean_iv <- felm_estimate(
  model_major_second_step_iv,
  'o_share\\(fit\\)'
)
gamma_out_major_mean_ols <- gamma_in_major_mean_ols +
  pi_out_minus_in_major_mean
gamma_out_major_mean_iv <- gamma_in_major_mean_iv +
  pi_out_minus_in_major_mean

# Recover major-specific deviations and total coefficients ---------------------

recover_major_deviations <- function(model, residency, weights) {
  term_names <- paste0(
    'share_dev_', residency, '_', major_keys[major_nonbase]
  )
  theta <- coef(model)[term_names]
  if (anyNA(theta)) {
    stop('One or more major-deviation coefficients were not estimated.')
  }
  names(theta) <- major_nonbase
  weighted_offset <- sum(theta * weights[major_nonbase])
  deviations <- c(
    theta - weighted_offset,
    'All other' = -weighted_offset
  )
  deviations[major_levels]
}

eta_in_iv <- recover_major_deviations(
  model_major_saturated_first_step,
  'in',
  in_major_weights
)
eta_out_iv <- recover_major_deviations(
  model_major_saturated_first_step,
  'out',
  out_major_weights
)
eta_in_exogenous <- recover_major_deviations(
  model_major_current_exogenous,
  'in',
  in_major_weights
)
eta_out_exogenous <- recover_major_deviations(
  model_major_current_exogenous,
  'out',
  out_major_weights
)

gamma_in_mean_exogenous <- fixest_estimate(
  model_major_current_exogenous,
  'o_share_ins'
)
gamma_out_mean_exogenous <- fixest_estimate(
  model_major_current_exogenous,
  'o_share_oos'
)

stopifnot(
  abs(sum(in_major_weights * eta_in_iv)) < 1e-10,
  abs(sum(out_major_weights * eta_out_iv)) < 1e-10,
  abs(sum(in_major_weights * eta_in_exogenous)) < 1e-10,
  abs(sum(out_major_weights * eta_out_exogenous)) < 1e-10
)

major_choice_coefficients <- bind_rows(
  data.frame(
    estimator = 'One-step MNL (exogenous shares)',
    major = major_levels,
    gamma_in = gamma_in_mean_exogenous + eta_in_exogenous,
    gamma_out = gamma_out_mean_exogenous + eta_out_exogenous,
    eta_in = eta_in_exogenous,
    eta_out = eta_out_exogenous,
    row.names = NULL
  ),
  data.frame(
    estimator = 'Two-step OLS (unweighted cells)',
    major = major_levels,
    gamma_in = gamma_in_major_mean_ols + eta_in_iv,
    gamma_out = gamma_out_major_mean_ols + eta_out_iv,
    eta_in = eta_in_iv,
    eta_out = eta_out_iv,
    row.names = NULL
  ),
  data.frame(
    estimator = 'Two-step IV (unweighted cells)',
    major = major_levels,
    gamma_in = gamma_in_major_mean_iv + eta_in_iv,
    gamma_out = gamma_out_major_mean_iv + eta_out_iv,
    eta_in = eta_in_iv,
    eta_out = eta_out_iv,
    row.names = NULL
  )
) %>%
  mutate(
    in_major_share = in_major_weights[major],
    out_major_share = out_major_weights[major]
  )

major_mean_coefficients <- data.frame(
  estimator = c(
    'One-step MNL (exogenous shares)',
    'Two-step OLS (unweighted cells)',
    'Two-step IV (unweighted cells)'
  ),
  gamma_in_mean = c(
    gamma_in_mean_exogenous,
    gamma_in_major_mean_ols,
    gamma_in_major_mean_iv
  ),
  gamma_out_mean = c(
    gamma_out_mean_exogenous,
    gamma_out_major_mean_ols,
    gamma_out_major_mean_iv
  ),
  out_minus_in_mean = c(
    gamma_out_mean_exogenous - gamma_in_mean_exogenous,
    pi_out_minus_in_major_mean,
    pi_out_minus_in_major_mean
  ),
  row.names = NULL
)

# Major-specific average marginal effects --------------------------------------

origin_weights <- join_major %>%
  filter(o_state != 'Alabama') %>%
  count(alternative = o_state, name = 'N_origin_linked') %>%
  mutate(origin_weight = N_origin_linked / sum(N_origin_linked)) %>%
  select(alternative, origin_weight)

major_ame_multipliers <- function(model, data, sample) {
  predicted <- data %>%
    mutate(mu_hat = as.numeric(predict(
      model, newdata = data, type = 'response'
    ))) %>%
    group_by(origin_cohort_major) %>%
    mutate(prob = mu_hat / sum(mu_hat)) %>%
    ungroup() %>%
    left_join(origin_weights, by = 'alternative') %>%
    mutate(origin_weight = replace_na(origin_weight, 0))

  group_totals <- sample %>%
    count(grad_y, o_state, major, name = 'N_group')

  predicted %>%
    group_by(grad_y, o_state, major) %>%
    summarize(
      p_al = prob[alternative == 'Alabama'],
      weighted_destination_prob = sum(
        prob[alternative != 'Alabama'] *
          origin_weight[alternative != 'Alabama']
      ),
      .groups = 'drop'
    ) %>%
    left_join(group_totals, by = c('grad_y', 'o_state', 'major')) %>%
    mutate(probability_multiplier =
             100 * p_al * weighted_destination_prob) %>%
    group_by(
      residency = if_else(o_state == 'Alabama', 'In-state', 'OOS'),
      major
    ) %>%
    summarize(
      ame_multiplier = weighted.mean(probability_multiplier, N_group),
      students = sum(N_group),
      .groups = 'drop'
    )
}

ame_multiplier_exogenous <- major_ame_multipliers(
  model_major_current_exogenous,
  choice_cells_full_major,
  join_major
)
ame_multiplier_iv <- major_ame_multipliers(
  model_major_saturated_first_step,
  choice_cells_saturated_major,
  join_major
)

major_ame_results <- bind_rows(
  major_choice_coefficients %>%
    filter(estimator == 'One-step MNL (exogenous shares)') %>%
    select(estimator, major, gamma_in, gamma_out) %>%
    pivot_longer(
      cols = c(gamma_in, gamma_out),
      names_to = 'residency',
      values_to = 'gamma'
    ) %>%
    mutate(residency = recode(
      residency, gamma_in = 'In-state', gamma_out = 'OOS'
    )) %>%
    left_join(
      ame_multiplier_exogenous,
      by = c('major', 'residency')
    ),
  major_choice_coefficients %>%
    filter(estimator == 'Two-step IV (unweighted cells)') %>%
    select(estimator, major, gamma_in, gamma_out) %>%
    pivot_longer(
      cols = c(gamma_in, gamma_out),
      names_to = 'residency',
      values_to = 'gamma'
    ) %>%
    mutate(residency = recode(
      residency, gamma_in = 'In-state', gamma_out = 'OOS'
    )) %>%
    left_join(
      ame_multiplier_iv,
      by = c('major', 'residency')
    )
) %>%
  mutate(ame_pp_for_1pp_oos_share = gamma * ame_multiplier) %>%
  arrange(estimator, residency, match(major, major_levels))

# Output ------------------------------------------------------------------------

cat('\nMajor distributions used for effect coding and fiscal aggregation\n')
cat('------------------------------------------------------------------\n')
print(
  major_distribution_by_residency %>%
    select(major, factbook_share, in_state_share, oos_share),
  digits = 4,
  row.names = FALSE
)

cat('\nResidency-specific mean peer-share coefficients\n')
cat('------------------------------------------------\n')
print(major_mean_coefficients, digits = 4, row.names = FALSE)

cat('\nMajor-specific peer-share coefficients\n')
cat('--------------------------------------\n')
print(
  major_choice_coefficients %>%
    select(estimator, major, gamma_in, gamma_out),
  digits = 4,
  row.names = FALSE
)

cat('\nMajor-specific average marginal effects\n')
cat('---------------------------------------\n')
cat('Percentage-point increase in out-migration from a 1pp OOS-share increase.\n')
print(
  major_ame_results %>%
    select(estimator, residency, major, gamma,
           ame_pp_for_1pp_oos_share, students),
  digits = 4,
  row.names = FALSE
)

cat('\nSpecification note\n')
cat('------------------\n')
cat(paste(
  'The first step retains common destination-by-cohort fixed effects and adds',
  'only major-by-origin-share interactions separately for in-state and OOS',
  'students. No major-by-destination fixed effects are included. The interaction',
  'terms use population-weighted effect coding, so the second-step IV coefficient',
  'is the corrected in-state-major-distribution mean. The OOS mean equals that',
  'IV coefficient plus the first-step mean OOS-minus-in-state differential.',
  'The second step remains unweighted and uses exactly one observation per',
  'destination-cohort cell.\n'
))
