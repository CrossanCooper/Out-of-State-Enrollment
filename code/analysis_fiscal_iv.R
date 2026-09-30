#////////////////////////////////////////////////////////////////////////////////
# Filename: analysis_fiscal_iv.R
# Author: Ryan Haygood
# Date: 9/26/26
# Description: Recomputes the preferred IV fiscal effects using major-specific
# peer-share responses for in-state and OOS students, corrected residency-
# specific major distributions, and major-specific Alabama earnings profiles.
#////////////////////////////////////////////////////////////////////////////////

invisible(capture.output(
  suppressMessages(
    suppressWarnings(
      source(file.path('code', 'analysis_main_iv.R'))
    )
  )
))

# Parameters -------------------------------------------------------------------

tax_rate <- 0.05
standard_deduction <- 3000
personal_exemption <- 1500
federal_tax_deduction_rate <- 0.10
discount_rate <- 0.02
discount_factor <- 1 / (1 + discount_rate)
retirement_t <- 48
annual_leave_rate <- 0.02
grad_rate <- 0.74
enrollment_survival <- c(1, 0.86, 0.81, grad_rate)
fiscal_baseline_year <- 2019L
groen_calibration_year <- 2008L
groen_effect_at_reference <- 0.10

# Tuition margin ---------------------------------------------------------------

tuition <- read.csv(paste0(
  pathHome, 'data/ipeds_tuition/ic', fiscal_baseline_year, '_ay.csv'
)) %>%
  rename_with(toupper) %>%
  filter(UNITID == 100751)

posted_is_tuition <- as.numeric(tuition$TUITION1)
posted_oos_tuition <- as.numeric(tuition$TUITION3)
required_fees <- if_else(
  'FEE3' %in% names(tuition), as.numeric(tuition$FEE3), 0
)
posted_oos_tuition_fees <- posted_oos_tuition + required_fees

posted_oos_tuition_2023 <- 32400
tuition_scale_2019_from_2023 <-
  posted_oos_tuition / posted_oos_tuition_2023
avg_oos_auto_merit_2023 <- 8618.605
avg_oos_residual_tuition_grant_2023 <- 3089.222
avg_oos_residual_tuition_grant <-
  avg_oos_residual_tuition_grant_2023 * tuition_scale_2019_from_2023
avg_oos_tuition_grant <-
  (avg_oos_auto_merit_2023 + avg_oos_residual_tuition_grant_2023) *
  tuition_scale_2019_from_2023
oos_net_tuition_fees <- posted_oos_tuition_fees - avg_oos_tuition_grant

fin <- read.csv(paste0(
  pathHome, 'data/ipeds_finance/f',
  substr(fiscal_baseline_year, 3, 4),
  substr(fiscal_baseline_year + 1, 3, 4),
  '_f1a_rv.csv'
)) %>%
  rename_with(toupper) %>%
  filter(UNITID == 100751)

fte <- read_xlsx(
  paste0(pathHome, 'data/factbook/fte_by_college_in_out.xlsx')
) %>%
  rename(
    origin = Origin,
    college = `By College/School`,
    y = STYEAR,
    fte = sum_fte
  ) %>%
  mutate(y = as.numeric(y), fte = as.numeric(fte))

average_cost <- (fin$F1C011 + fin$F1C051 + fin$F1C061) /
  sum(fte$fte[fte$y == fiscal_baseline_year], na.rm = TRUE)
annual_oos_margin <- oos_net_tuition_fees - average_cost
discounted_oos_margin <- annual_oos_margin *
  sum(enrollment_survival * discount_factor^(1:4))

# Major-specific earnings and tax profiles ------------------------------------

major_inputs <- data.frame(
  major = major_levels,
  webber_category = c(
    'Business', 'STEM', 'Business', 'Business', 'Business',
    'STEM', 'Social', 'Arts / Humanities', 'STEM',
    'Arts / Humanities'
  ),
  initial_earnings = c(
    63835, 72152, 60452, 71114, 54407,
    65377, 63458, 45345, 54978, 54273
  ),
  stringsAsFactors = FALSE
) %>%
  left_join(
    major_distribution_by_residency %>%
      select(major, factbook_share, in_state_share, oos_share),
    by = 'major'
  )

earnings_anchors <- data.frame(
  webber_category = c('STEM', 'Business', 'Social', 'Arts / Humanities'),
  log_earn_age_26 = c(
    9.88 + 0.818, 9.88 + 0.738, 9.88 + 0.545, 9.88 + 0.294
  ),
  log_earn_age_41 = c(
    10.28 + 0.876, 10.28 + 0.850, 10.28 + 0.755, 10.28 + 0.550
  ),
  log_earn_age_61 = c(
    10.17 + 0.692, 10.17 + 0.597, 10.17 + 0.467, 10.17 + 0.324
  ),
  stringsAsFactors = FALSE
)

earnings_anchors$quadratic <- (
  ((earnings_anchors$log_earn_age_61 -
      earnings_anchors$log_earn_age_41) / (61 - 41)) -
    ((earnings_anchors$log_earn_age_41 -
        earnings_anchors$log_earn_age_26) / (41 - 26))
) / (61 - 26)
earnings_anchors$linear <- (
  (earnings_anchors$log_earn_age_41 -
     earnings_anchors$log_earn_age_26) / (41 - 26)
) - earnings_anchors$quadratic * (41 + 26)

major_inputs <- major_inputs %>%
  left_join(earnings_anchors, by = 'webber_category')

al_taxable_income <- function(earnings) {
  pmax(
    earnings * (1 - federal_tax_deduction_rate) -
      standard_deduction - personal_exemption,
    0
  )
}

annual_taxable_income <- function(initial_earnings, linear, quadratic,
                                  T = retirement_t) {
  t <- 5:T
  age <- 17 + t
  earnings <- initial_earnings * exp(
    linear * (age - 22) + quadratic * (age^2 - 22^2)
  )
  al_taxable_income(earnings)
}

earnings_paths <- mapply(
  annual_taxable_income,
  major_inputs$initial_earnings,
  major_inputs$linear,
  major_inputs$quadratic
)

t_postgrad <- 5:retirement_t
conditional_postgrad_survival <-
  (1 - annual_leave_rate)^(t_postgrad - 5)
major_inputs$conditional_pdv_tax <- tax_rate * colSums(
  earnings_paths *
    conditional_postgrad_survival *
    discount_factor^t_postgrad
)

stopifnot(
  !anyNA(major_inputs),
  abs(sum(major_inputs$in_state_share) - 1) < 1e-8,
  abs(sum(major_inputs$oos_share) - 1) < 1e-8,
  all(major_inputs$conditional_pdv_tax > 0)
)

# Rake the 2019 origin-by-major population -------------------------------------

baseline_origin_counts <- comm %>%
  filter(grad_y == fiscal_baseline_year) %>%
  count(state, name = 'N_origin_full')

baseline_total_graduates <- sum(baseline_origin_counts$N_origin_full)
baseline_in_state_graduates <- sum(
  baseline_origin_counts$N_origin_full[
    baseline_origin_counts$state == 'Alabama'
  ]
)
baseline_oos_graduates <- baseline_total_graduates -
  baseline_in_state_graduates

baseline_linked_groups <- join_major %>%
  filter(grad_y == fiscal_baseline_year) %>%
  count(o_state, major, name = 'N_linked')

represented_origins <- unique(baseline_linked_groups$o_state)
missing_linked_origins <- setdiff(
  baseline_origin_counts$state,
  represented_origins
)

origin_targets <- baseline_origin_counts %>%
  filter(state %in% represented_origins) %>%
  mutate(
    residency_group = if_else(state == 'Alabama', 'In-state', 'OOS')
  ) %>%
  group_by(residency_group) %>%
  mutate(
    full_residency_population = if_else(
      residency_group == 'In-state',
      baseline_in_state_graduates,
      baseline_oos_graduates
    ),
    target_population = N_origin_full *
      full_residency_population / sum(N_origin_full)
  ) %>%
  ungroup()

rake_origin_major <- function(residency_group) {
  if (residency_group == 'In-state') {
    origins <- 'Alabama'
    target_total <- baseline_in_state_graduates
    target_major_shares <- setNames(
      major_inputs$in_state_share, major_inputs$major
    )
  } else {
    origins <- origin_targets %>%
      filter(residency_group == 'OOS') %>%
      pull(state)
    target_total <- baseline_oos_graduates
    target_major_shares <- setNames(
      major_inputs$oos_share, major_inputs$major
    )
  }

  seed_long <- crossing(
    o_state = origins,
    major = factor(major_levels, levels = major_levels)
  ) %>%
    left_join(
      baseline_linked_groups,
      by = c('o_state', 'major')
    ) %>%
    mutate(N_linked = replace_na(N_linked, 0))

  seed <- xtabs(N_linked ~ o_state + major, data = seed_long)
  seed <- seed[origins, major_levels, drop = FALSE]

  target_rows <- origin_targets %>%
    filter(state %in% origins) %>%
    arrange(match(state, origins)) %>%
    pull(target_population)
  names(target_rows) <- origins
  target_cols <- target_total * target_major_shares[major_levels]

  if (any(rowSums(seed) == 0) || any(colSums(seed) == 0)) {
    stop('Raking seed has an empty origin or major margin for ',
         residency_group, '.')
  }

  fitted <- seed
  converged <- FALSE
  for (iteration in seq_len(10000L)) {
    fitted <- sweep(
      sweep(fitted, 1, rowSums(fitted), '/'),
      1, target_rows, '*'
    )
    fitted <- sweep(
      sweep(fitted, 2, colSums(fitted), '/'),
      2, target_cols, '*'
    )
    error <- max(
      abs(rowSums(fitted) - target_rows),
      abs(colSums(fitted) - target_cols)
    )
    if (error < 1e-8) {
      converged <- TRUE
      break
    }
  }
  if (!converged) {
    stop('Origin-by-major raking did not converge for ', residency_group, '.')
  }

  as.data.frame(as.table(fitted), responseName = 'population_count') %>%
    transmute(
      o_state = as.character(o_state),
      major = as.character(major),
      population_count,
      residency_group
    ) %>%
    filter(population_count > 0)
}

baseline_population_cells <- bind_rows(
  rake_origin_major('In-state'),
  rake_origin_major('OOS')
)

population_margin_checks <- bind_rows(
  baseline_population_cells %>%
    group_by(residency_group, major) %>%
    summarize(actual = sum(population_count), .groups = 'drop') %>%
    left_join(
      bind_rows(
        major_inputs %>%
          transmute(
            residency_group = 'In-state', major,
            target = baseline_in_state_graduates * in_state_share
          ),
        major_inputs %>%
          transmute(
            residency_group = 'OOS', major,
            target = baseline_oos_graduates * oos_share
          )
      ),
      by = c('residency_group', 'major')
    ),
  baseline_population_cells %>%
    group_by(residency_group, o_state) %>%
    summarize(actual = sum(population_count), .groups = 'drop') %>%
    left_join(
      origin_targets %>%
        transmute(
          residency_group, o_state = state,
          target = target_population
        ),
      by = c('residency_group', 'o_state')
    )
)

stopifnot(
  abs(sum(baseline_population_cells$population_count) -
        baseline_total_graduates) < 1e-6,
  max(abs(population_margin_checks$actual -
            population_margin_checks$target)) < 1e-6
)

# Baseline fitted choice probabilities ----------------------------------------

fitted_count_index_all <- as.numeric(predict(
  model_major_saturated_first_step,
  newdata = choice_cells_saturated_major,
  type = 'response'
))

choice_cells_major_fitted <- choice_cells_saturated_major %>%
  mutate(fitted_count_index = fitted_count_index_all)

baseline_choice_cells <- choice_cells_major_fitted %>%
  filter(grad_y == fiscal_baseline_year) %>%
  arrange(o_state, major, alternative)

baseline_share_check <- baseline_choice_cells %>%
  distinct(alternative, o_share) %>%
  left_join(
    baseline_origin_counts %>%
      transmute(
        alternative = state,
        expected_o_share = if_else(
          state == 'Alabama',
          0,
          100 * N_origin_full / baseline_total_graduates
        )
      ),
    by = 'alternative'
  ) %>%
  mutate(expected_o_share = replace_na(expected_o_share, 0))

stopifnot(max(abs(
  baseline_share_check$o_share - baseline_share_check$expected_o_share
)) < 1e-10)

marginal_oos_origin_distribution <- baseline_origin_counts %>%
  filter(state != 'Alabama') %>%
  transmute(
    state,
    marginal_origin_weight = N_origin_full / sum(N_origin_full)
  )

iv_major_gammas <- major_choice_coefficients %>%
  filter(estimator == 'Two-step IV (unweighted cells)') %>%
  select(major, gamma_in, gamma_out)

# Explicit finite-difference peer experiment ----------------------------------

choice_response_for_increment <- function(entrant_increment) {
  added_graduates <- marginal_oos_origin_distribution %>%
    mutate(
      added_graduates = grad_rate * entrant_increment *
        marginal_origin_weight
    ) %>%
    select(state, added_graduates)

  perturbed_shares <- baseline_origin_counts %>%
    left_join(added_graduates, by = 'state') %>%
    mutate(
      added_graduates = replace_na(added_graduates, 0),
      N_origin_perturbed = N_origin_full + added_graduates,
      o_share_perturbed = 100 * N_origin_perturbed /
        (baseline_total_graduates + grad_rate * entrant_increment),
      o_share_perturbed = if_else(
        state == 'Alabama', 0, o_share_perturbed
      )
    ) %>%
    select(alternative = state, o_share_perturbed)

  cell_probabilities <- baseline_choice_cells %>%
    mutate(major = as.character(major)) %>%
    left_join(iv_major_gammas, by = 'major') %>%
    left_join(perturbed_shares, by = 'alternative') %>%
    mutate(
      o_share_perturbed = replace_na(o_share_perturbed, 0),
      gamma_type = if_else(o_state == 'Alabama', gamma_in, gamma_out),
      perturbed_count_index = fitted_count_index * exp(
        gamma_type * (o_share_perturbed - o_share)
      )
    ) %>%
    group_by(o_state, major) %>%
    mutate(
      probability_actual = fitted_count_index /
        sum(fitted_count_index),
      probability_perturbed = perturbed_count_index /
        sum(perturbed_count_index)
    ) %>%
    ungroup()

  response_by_group <- cell_probabilities %>%
    filter(alternative == 'Alabama') %>%
    transmute(
      o_state,
      major,
      probability_alabama_actual = probability_actual,
      probability_alabama_perturbed = probability_perturbed,
      delta_probability_alabama =
        probability_alabama_perturbed - probability_alabama_actual
    )

  response_by_population_cell <- baseline_population_cells %>%
    left_join(response_by_group, by = c('o_state', 'major')) %>%
    left_join(
      major_inputs %>% select(major, conditional_pdv_tax),
      by = 'major'
    ) %>%
    mutate(
      entrant_increment = entrant_increment,
      resident_change = population_count * delta_probability_alabama,
      resident_derivative = resident_change / entrant_increment,
      fiscal_change = resident_change * conditional_pdv_tax,
      fiscal_derivative = fiscal_change / entrant_increment,
      residency = if_else(
        o_state == 'Alabama',
        'In-state incumbents',
        'OOS incumbents'
      )
    )

  stopifnot(!anyNA(response_by_population_cell[c(
    'delta_probability_alabama',
    'resident_derivative',
    'fiscal_derivative'
  )]))

  list(
    entrant_increment = entrant_increment,
    perturbed_shares = perturbed_shares,
    cell_probabilities = cell_probabilities,
    response_by_group = response_by_group,
    response_by_population_cell = response_by_population_cell
  )
}

perturbation_sizes <- c(0.001, 0.01, 0.1, 1)
finite_difference_runs <- lapply(
  perturbation_sizes,
  choice_response_for_increment
)
names(finite_difference_runs) <- as.character(perturbation_sizes)
preferred_perturbation <- 0.01
preferred_response <- finite_difference_runs[[
  as.character(preferred_perturbation)
]]

finite_difference_stability <- lapply(
  finite_difference_runs,
  function(run) {
    run$response_by_population_cell %>%
      group_by(residency) %>%
      summarize(
        resident_derivative = sum(resident_derivative),
        fiscal_derivative = sum(fiscal_derivative),
        .groups = 'drop'
      ) %>%
      mutate(entrant_increment = run$entrant_increment)
  }
) %>%
  bind_rows() %>%
  select(
    entrant_increment, residency,
    resident_derivative, fiscal_derivative
  )

finite_difference_stability_total <- finite_difference_stability %>%
  group_by(entrant_increment) %>%
  summarize(
    resident_derivative = sum(resident_derivative),
    fiscal_derivative = sum(fiscal_derivative),
    .groups = 'drop'
  )

stopifnot(
  diff(range(
    finite_difference_stability_total$resident_derivative
  )) < 1e-4,
  diff(range(
    finite_difference_stability_total$fiscal_derivative
  )) < 10
)

# Fixed-p0 attendance calibration by major ------------------------------------

conditional_oos_retention_by_major <- choice_cells_major_fitted %>%
  filter(
    grad_y %in% c(groen_calibration_year, fiscal_baseline_year),
    o_state != 'Alabama'
  ) %>%
  group_by(grad_y, o_state, major) %>%
  summarize(
    origin_major_size = sum(fitted_count_index),
    alabama_count = sum(
      fitted_count_index[alternative == 'Alabama']
    ),
    .groups = 'drop'
  ) %>%
  group_by(grad_y, major) %>%
  summarize(
    conditional_ua_retention = sum(alabama_count) /
      sum(origin_major_size),
    linked_students = sum(origin_major_size),
    .groups = 'drop'
  ) %>%
  mutate(major = as.character(major)) %>%
  left_join(
    major_inputs %>% select(major, oos_share),
    by = 'major'
  )

reference_retention <- conditional_oos_retention_by_major %>%
  filter(grad_y == groen_calibration_year) %>%
  summarize(
    retention = sum(oos_share * conditional_ua_retention),
    .groups = 'drop'
  ) %>%
  pull(retention)

fixed_no_ua_retention <- reference_retention - groen_effect_at_reference

fiscal_year_retention <- conditional_oos_retention_by_major %>%
  filter(grad_y == fiscal_baseline_year) %>%
  summarize(
    retention = sum(oos_share * conditional_ua_retention),
    .groups = 'drop'
  ) %>%
  pull(retention)

attendance_effect_fiscal_year <- fiscal_year_retention -
  fixed_no_ua_retention

# Preserve the representative-marginal-OOS-student assumption for the direct
# attendance channel. Major heterogeneity enters both incumbent peer channels;
# the one aggregate attendance effect is allocated across the corrected OOS
# major distribution solely to apply the appropriate earnings profiles.
direct_attendance_by_major <- major_inputs %>%
  transmute(
    channel = 'Direct OOS attendance effect',
    residency = 'Marginal OOS entrant',
    major,
    attendance_effect = attendance_effect_fiscal_year,
    resident_derivative = grad_rate * oos_share * attendance_effect,
    fiscal_derivative = resident_derivative * conditional_pdv_tax
  )

attendance_calibration_summary <- data.frame(
  statistic = c(
    '2008 conditional-UA retention, OOS-major weighted',
    'Fixed no-UA retention probability',
    '2008 attendance effect',
    '2019 attendance effect, OOS-major weighted'
  ),
  value = c(
    reference_retention,
    fixed_no_ua_retention,
    groen_effect_at_reference,
    attendance_effect_fiscal_year
  ),
  row.names = NULL
)

stopifnot(abs(
  attendance_calibration_summary$value[
    attendance_calibration_summary$statistic == '2008 attendance effect'
  ] - groen_effect_at_reference
) < 1e-12)

# Three-channel fiscal decomposition ------------------------------------------

peer_derivatives_by_major <-
  preferred_response$response_by_population_cell %>%
  group_by(residency, major) %>%
  summarize(
    resident_derivative = sum(resident_derivative),
    fiscal_derivative = sum(fiscal_derivative),
    .groups = 'drop'
  ) %>%
  mutate(
    channel = if_else(
      residency == 'OOS incumbents',
      'OOS incumbent peer effect',
      'In-state incumbent peer effect'
    ),
    attendance_effect = NA_real_
  )

resident_derivatives_by_major <- bind_rows(
  direct_attendance_by_major,
  peer_derivatives_by_major
) %>%
  select(
    channel, residency, major, attendance_effect,
    resident_derivative, fiscal_derivative
  )

migration_channels <- resident_derivatives_by_major %>%
  group_by(channel) %>%
  summarize(
    alabama_residents_per_oos_entrant = sum(resident_derivative),
    discounted_tax_revenue_per_oos_entrant = sum(fiscal_derivative),
    .groups = 'drop'
  )

net_migration_residents <- sum(
  migration_channels$alabama_residents_per_oos_entrant
)
net_migration_revenue <- sum(
  migration_channels$discounted_tax_revenue_per_oos_entrant
)
net_fiscal_effect <- discounted_oos_margin + net_migration_revenue

fiscal_results_iv <- bind_rows(
  migration_channels %>%
    transmute(
      component = channel,
      resident_effect_per_oos_entrant =
        alabama_residents_per_oos_entrant,
      fiscal_effect_per_oos_entrant =
        discounted_tax_revenue_per_oos_entrant
    ),
  data.frame(
    component = c(
      'Net migration effect',
      'Discounted OOS tuition margin before migration',
      'Total marginal state PDV'
    ),
    resident_effect_per_oos_entrant = c(
      net_migration_residents,
      NA_real_,
      net_migration_residents
    ),
    fiscal_effect_per_oos_entrant = c(
      net_migration_revenue,
      discounted_oos_margin,
      net_fiscal_effect
    )
  )
)

# Verify that annual major-specific cash flows reproduce the tax PDV -----------

total_resident_derivative_by_major <- resident_derivatives_by_major %>%
  group_by(major) %>%
  summarize(
    resident_derivative = sum(resident_derivative),
    .groups = 'drop'
  ) %>%
  right_join(major_inputs %>% select(major), by = 'major') %>%
  mutate(resident_derivative = replace_na(resident_derivative, 0)) %>%
  arrange(match(major, major_levels))

annual_migration_revenue <- conditional_postgrad_survival * tax_rate *
  as.numeric(
    earnings_paths %*%
      total_resident_derivative_by_major$resident_derivative
  )

discounted_annual_migration_revenue <- sum(
  annual_migration_revenue * discount_factor^t_postgrad
)

stopifnot(abs(
  discounted_annual_migration_revenue - net_migration_revenue
) < 1e-6)

# ACT-score PDV figure ----------------------------------------------------------

previous_no_iv_in_state_count <- 3401
previous_no_iv_chain_rule_adjustment <- 1 / (206 * 100)
previous_no_iv_oos_attendance_effect <- 0.10
previous_no_iv_in_state_marginal_effect <- c(
  -0.41, -0.30, -0.49, -0.48, -0.66,
  -0.17, -0.29, -0.09, -0.29, -0.28
)

previous_no_iv_in_state_peer_pdv <- grad_rate *
  previous_no_iv_in_state_count *
  previous_no_iv_chain_rule_adjustment *
  sum(
    previous_no_iv_in_state_marginal_effect *
      major_inputs$conditional_pdv_tax *
      major_inputs$factbook_share
  )
previous_no_iv_direct_attendance_pdv <- grad_rate *
  previous_no_iv_oos_attendance_effect *
  sum(
    major_inputs$conditional_pdv_tax * major_inputs$factbook_share
  )
previous_no_iv_migration_pdv <-
  previous_no_iv_in_state_peer_pdv +
  previous_no_iv_direct_attendance_pdv

award_oos_act <- function(act, gpa = 3.50) {
  aid <- numeric(length(act))
  gpa_300_349 <- gpa >= 3.00 & gpa < 3.50
  gpa_350_up <- gpa >= 3.50
  aid[gpa_300_349 & act >= 27 & act < 28] <- 6000
  aid[gpa_300_349 & act >= 28 & act < 30] <- 8000
  aid[gpa_300_349 & act >= 30] <- 15000
  aid[gpa_350_up & act >= 25 & act < 27] <- 6000
  aid[gpa_350_up & act >= 27 & act < 28] <- 8000
  aid[gpa_350_up & act >= 28 & act < 29] <- 10000
  aid[gpa_350_up & act >= 29 & act < 30] <- 15000
  aid[gpa_350_up & act >= 30 & act < 32] <- 24000
  aid[gpa_350_up & act >= 32] <- 28000
  aid * tuition_scale_2019_from_2023
}

oos_net_tuition_fees_by_act <- function(act, gpa = 3.50) {
  pmax(
    posted_oos_tuition_fees -
      award_oos_act(act, gpa = gpa) -
      avg_oos_residual_tuition_grant,
    0
  )
}

pdv_by_act_iv <- data.frame(act = 18:36) %>%
  mutate(
    net_oos_tuition_fees = oos_net_tuition_fees_by_act(act),
    discounted_tuition_margin =
      (net_oos_tuition_fees - average_cost) *
      sum(enrollment_survival * discount_factor^(1:4)),
    migration_pdv = net_migration_revenue,
    state_pdv = migration_pdv + discounted_tuition_margin,
    previous_no_iv_state_pdv = discounted_tuition_margin +
      previous_no_iv_migration_pdv
  )

pdv_by_act_plot_data <- pdv_by_act_iv %>%
  select(
    act,
    `State Budget` = state_pdv,
    `University Budget` = discounted_tuition_margin
  ) %>%
  pivot_longer(-act, names_to = 'pdv_concept', values_to = 'pdv') %>%
  mutate(
    pdv_concept = factor(
      pdv_concept,
      levels = c(
        'State Budget',
        'University Budget'
      )
    )
  )

pdv_by_act_plot <- ggplot(
  pdv_by_act_plot_data,
  aes(x = act, y = pdv, color = pdv_concept)
) +
  geom_hline(yintercept = 0, color = 'gray40') +
  geom_line(linewidth = 1) +
  scale_x_continuous(breaks = seq(18, 36, by = 3)) +
  scale_y_continuous(labels = scales::dollar) +
  scale_color_manual(
    values = c(
      'State Budget' = '#440154FF',
      'University Budget' = '#21908CFF'
    ),
    name = NULL
  ) +
  labs(
    x = 'ACT score',
    y = 'PDV (2% discount rate)'
  ) +
  theme_classic() +
  theme(
    panel.grid.major.y = element_line(
      color = 'gray80', linetype = 'dashed'
    ),
    legend.position = 'bottom'
  )

fiscal_figure_directory <- file.path(pathFigures, 'analysis_mlogit_iv')
dir.create(fiscal_figure_directory, recursive = TRUE, showWarnings = FALSE)
ggsave(
  file.path(
    fiscal_figure_directory,
    'fiscal_iv_pdv_by_act_score.png'
  ),
  pdv_by_act_plot,
  width = 7,
  height = 5,
  dpi = 300
)

# Output ------------------------------------------------------------------------

cat('\nFixed-p0 attendance calibration\n')
cat('-------------------------------\n')
print(attendance_calibration_summary, digits = 6, row.names = FALSE)

cat('\nDirect attendance allocation across OOS majors in 2019\n')
cat('------------------------------------------------------\n')
print(
  direct_attendance_by_major %>%
    select(major, attendance_effect,
           resident_derivative, fiscal_derivative),
  digits = 5,
  row.names = FALSE
)

cat('\nMajor-specific peer effects\n')
cat('---------------------------\n')
print(
  peer_derivatives_by_major %>%
    select(residency, major,
           resident_derivative, fiscal_derivative),
  digits = 5,
  row.names = FALSE
)

cat('\nThree-channel IV fiscal decomposition with major heterogeneity\n')
cat('-------------------------------------------------------------\n')
print(fiscal_results_iv, digits = 6, row.names = FALSE)

cat('\nFinite-difference stability\n')
cat('---------------------------\n')
print(finite_difference_stability, digits = 6, row.names = FALSE)

cat('\nConstruction note\n')
cat('-----------------\n')
cat(paste(
  'Incumbent choice probabilities are perturbed separately for every',
  'origin-state-by-major group using the corresponding in-state or OOS',
  'major-specific IV coefficient. The 2019 linked origin-by-major table is',
  'raked to the full Commencement origin counts and the corrected Factbook',
  'major distributions. The direct attendance channel holds one no-UA',
  'retention probability fixed, calibrates the OOS-major-weighted 2008',
  'attendance effect to 10 percentage points, and allocates the resulting',
  'representative 2019 attendance effect across the corrected OOS major',
  'distribution for the earnings calculation.',
  'The entrant-to-graduate conversion is applied exactly once.\n'
))

