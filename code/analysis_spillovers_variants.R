#////////////////////////////////////////////////////////////////////////////////
# Filename: analysis_spillovers_variants.R
# Author: Ryan Haygood
# Date: 6/12/26
# Description: Estimates alternative location-spillover specifications and reports
# AMEs as in-state Alabama leavers per 100 additional OOS students.
#////////////////////////////////////////////////////////////////////////////////

# Import the build section from analysis_spillovers_twfe.R ----------------------

options(width = 220)

quiet_source <- function(file) {
  tmp <- tempfile()
  con <- file(tmp, open = 'wt')
  sink(con)
  sink(con, type = 'message')
  on.exit({
    sink(type = 'message')
    sink()
    close(con)
    unlink(tmp)
  }, add = TRUE)
  suppressPackageStartupMessages(
    suppressMessages(
      suppressWarnings(source(file))
    )
  )
}

quiet_source('C:/Users/ryanh/OneDrive/Documents/Grad School/Research/Out-of-State-Enrollment/code/setup.R')

twfe_file <- 'C:/Users/ryanh/OneDrive/Documents/Grad School/Research/Out-of-State-Enrollment/code/analysis_spillovers_twfe.R'
twfe_code <- readLines(twfe_file)
twfe_code <- twfe_code[!grepl('^source\\(.*setup\\.R.*\\)', twfe_code)]
build_end <- grep('^# Count regressions', twfe_code)[1] - 1
suppressPackageStartupMessages(
  suppressMessages(
    suppressWarnings(eval(parse(text = twfe_code[seq_len(build_end)])))
  )
)

# Origin share source for specs 3-4:
# - 'comm': Commencement graduate origin shares.
# - 'ipeds': IPEDS enrollment-by-residency shares.
origin_share_source <- 'ipeds'
origin_share_source <- match.arg(origin_share_source, c('comm', 'ipeds'))

if (!requireNamespace('fixest', quietly = TRUE)) {
  stop('Package fixest is required for the PPML specification.')
}

# Estimation helpers ------------------------------------------------------------

est_data <- share_ins %>%
  ungroup() %>%
  filter(grad_y %in% paper_years,
         !is.na(z_lso)) %>%
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
         N_share_model_lead = if (origin_share_source == 'comm') N_full_lead else N_full_ipeds_lead,
         ppml_y = N_dest_obs,
         ppml_x = o_share_model,
         ppml_x_lag = o_share_model_lag,
         ppml_x_lead = o_share_model_lead)

coef_row <- function(model, term, multiplier = 1) {
  coefs <- summary(model, robust = T)$coefficients
  hit <- rownames(coefs)[rownames(coefs) == term]
  if (length(hit) == 0) hit <- rownames(coefs)[grepl(term, rownames(coefs), fixed = TRUE)]
  data.frame(coef = coefs[hit[1], 'Estimate'],
             se = coefs[hit[1], 'Cluster s.e.'],
             ame = coefs[hit[1], 'Estimate'] * multiplier,
             ame_se = coefs[hit[1], 'Cluster s.e.'] * abs(multiplier))
}

fit_lm <- function(data, y, x, iv = FALSE, lag = NULL, lead = NULL, fe_type = 'cohort FE') {
  rhs <- paste(c(x, lag, lead, 'ur', 'net_rate'), collapse = ' + ')
  controls <- if (fe_type == 'cohort FE') 'ur + net_rate' else 'ur + net_rate + grad_y'
  fe <- if (fe_type == 'cohort FE') 'factor(d_state) + factor(grad_y)' else 'factor(d_state)'
  if (iv) {
    fml <- as.formula(paste0(y, ' ~ ', controls, ' | ', fe, ' | (',
                             x, ' ~ z_lso) | d_state'))
  } else {
    if (fe_type == 'linear trend') rhs <- paste(c(x, lag, lead, 'ur', 'net_rate', 'grad_y'), collapse = ' + ')
    fml <- as.formula(paste0(y, ' ~ ', rhs,
                             ' | ', fe, ' | 0 | d_state'))
  }
  felm(fml, data = data)
}

origin_weights <- function(data, x) {
  data %>%
    group_by(d_state) %>%
    summarize(origin_total = sum(.data[[x]], na.rm = T), .groups = 'drop') %>%
    mutate(delta_students = 100 * origin_total / sum(origin_total)) %>%
    select(d_state, delta_students)
}

add_delta_x <- function(data, x, regressor = 'count') {
  out <- data %>% left_join(origin_weights(data, x), by = 'd_state')
  if (regressor == 'share') {
    denom <- if (grepl('lag', x) && 'N_share_model_lag' %in% names(out)) {
      out$N_share_model_lag
    } else if (grepl('lead', x) && 'N_share_model_lead' %in% names(out)) {
      out$N_share_model_lead
    } else if ('N_share_model' %in% names(out)) {
      out$N_share_model
    } else if (grepl('lag', x) && 'N_full_lag' %in% names(out)) {
      out$N_full_lag
    } else if (grepl('lead', x) && 'N_full_lead' %in% names(out)) {
      out$N_full_lead
    } else {
      out$N_full
    }
    out <- out %>% mutate(delta_x = 100 * delta_students / denom)
  } else {
    out <- out %>% mutate(delta_x = delta_students)
  }
  out
}

count_multiplier <- function(data, x, regressor = 'count') {
  sum(origin_weights(data, x)$delta_students)
}

share_multiplier <- function(data, x, regressor = 'count') {
  data %>%
    add_delta_x(x, regressor) %>%
    group_by(grad_y) %>%
    summarize(multiplier = first(N_AL_full) * sum(delta_x), .groups = 'drop') %>%
    summarize(multiplier = mean(multiplier)) %>%
    pull(multiplier)
}

logit_multiplier <- function(data, x, regressor = 'count') {
  data %>%
    add_delta_x(x, regressor) %>%
    mutate(p_al = N_home_obs / M_AL_obs) %>%
    group_by(grad_y) %>%
    summarize(multiplier = first(N_AL_full) * first(p_al) * sum(p_dest * delta_x),
              .groups = 'drop') %>%
    summarize(multiplier = mean(multiplier)) %>%
    pull(multiplier)
}

ppml_data <- est_data %>%
  select(d_state, grad_y, ppml_y, ppml_x, ppml_x_lag, ppml_x_lead, ur, net_rate,
         match_prob, N_full, N_full_lag, N_full_lead, N_origin_count,
         N_origin_linked_lag, N_share_model, N_share_model_lag,
         N_share_model_lead) %>%
  bind_rows(
    est_data %>%
      distinct(grad_y, N_home_obs, M_AL_obs, N_AL_full) %>%
      left_join(pull_factors %>%
                  rename(d_state = state, grad_y = y) %>%
                  filter(d_state == 'Alabama') %>%
                  select(d_state, grad_y, ur, net_rate),
                by = 'grad_y') %>%
      transmute(d_state = 'Alabama',
                grad_y,
                ppml_y = N_home_obs,
                ppml_x = 0,
                ppml_x_lag = 0,
                ppml_x_lead = 0,
                ur,
                net_rate,
                match_prob = M_AL_obs / N_AL_full,
                N_full = NA_real_,
                N_full_lag = NA_real_,
                N_full_lead = NA_real_,
                N_share_model = NA_real_,
                N_share_model_lag = NA_real_,
                N_share_model_lead = NA_real_,
                N_origin_count = 0,
                N_origin_linked_lag = NA_real_)
  )

ppml_ame <- function(model, data, x, regressor = 'count') {
  non_al <- data %>%
    filter(d_state != 'Alabama') %>%
    add_delta_x(x, regressor)
  b <- coef(model)[x]
  se_b <- fixest::se(model, cluster = ~ d_state)[x]
  mu <- predict(model, newdata = non_al, type = 'response')
  by_cohort <- non_al %>%
    mutate(diff_full = mu * (exp(b * delta_x) - 1) / match_prob,
           deriv_full = mu * delta_x * exp(b * delta_x) / match_prob) %>%
    group_by(grad_y) %>%
    summarize(ame = sum(diff_full),
              deriv = sum(deriv_full),
              .groups = 'drop')
  data.frame(coef = b,
             se = se_b,
             ame = mean(by_cohort$ame),
             ame_se = abs(mean(by_cohort$deriv)) * se_b)
}

specs <- list(
  list(name = '1. Cohort-scaled count',
       y = 'N_dest_count', x = 'N_origin_count', lag = 'N_origin_count_lag',
       lead = 'N_origin_count_lead',
       regressor = 'count', data = est_data, multiplier = count_multiplier),
  list(name = '2. Linked-sample count',
       y = 'N_dest_obs', x = 'N_origin_linked', lag = 'N_origin_linked_lag',
       lead = 'N_origin_linked_lead',
       regressor = 'count', data = est_data, multiplier = count_multiplier),
  list(name = '3. Log-share difference',
       y = 'diff_log_d_share', x = 'o_share_model', lag = 'o_share_model_lag',
       lead = 'o_share_model_lead',
       regressor = 'share', data = est_data %>% filter(N_dest_obs > 0, N_home_obs > 0),
       multiplier = logit_multiplier)
)

arrange_results <- function(x) {
  x %>%
    mutate(fe_type = factor(fe_type, levels = c('cohort FE', 'linear trend'))) %>%
    arrange(fe_type, spec) %>%
    mutate(fe_type = as.character(fe_type))
}

# AMEs --------------------------------------------------------------------------

linear_ames <- bind_rows(lapply(c('cohort FE', 'linear trend'), function(fe_type) {
  bind_rows(lapply(specs, function(s) {
    ols <- fit_lm(s$data, s$y, s$x, fe_type = fe_type)
    iv <- fit_lm(s$data, s$y, s$x, iv = TRUE, fe_type = fe_type)
    mult <- s$multiplier(s$data, s$x, s$regressor)
    bind_rows(
      coef_row(ols, s$x, mult) %>% mutate(spec = s$name, regressor = s$regressor,
                                          fe_type = fe_type, estimator = 'OLS'),
      coef_row(iv, s$x, mult) %>% mutate(spec = s$name, regressor = s$regressor,
                                         fe_type = fe_type, estimator = 'IV')
    )
  }))
}))

ppml_fit_fe <- fixest::fepois(ppml_y ~ ppml_x + ur + net_rate | d_state + grad_y,
                              data = ppml_data,
                              cluster = ~ d_state)
ppml_fit_trend <- fixest::fepois(ppml_y ~ ppml_x + ur + net_rate + grad_y | d_state,
                                 data = ppml_data,
                                 cluster = ~ d_state)

ame_table <- linear_ames %>%
  bind_rows(
    ppml_ame(ppml_fit_fe, ppml_data, 'ppml_x', 'share') %>%
      mutate(spec = '4. PPML grouped choice', regressor = 'share',
             fe_type = 'cohort FE', estimator = 'OLS'),
    ppml_ame(ppml_fit_trend, ppml_data, 'ppml_x', 'share') %>%
      mutate(spec = '4. PPML grouped choice', regressor = 'share',
             fe_type = 'linear trend', estimator = 'OLS')
  ) %>%
  select(spec, regressor, fe_type, estimator, ame, ame_se, coef, se) %>%
  arrange_results()
rownames(ame_table) <- NULL

print(ame_table, row.names = FALSE)

# LaTeX AME table ---------------------------------------------------------------

stars <- function(est, se) {
  p <- 2 * pnorm(-abs(est / se))
  ifelse(p < 0.01, '$^{***}$',
         ifelse(p < 0.05, '$^{**}$',
                ifelse(p < 0.10, '$^{*}$', '')))
}

fmt_est <- function(est, se) {
  if (length(est) == 0 || is.na(est)) return('---')
  paste0(sprintf('%.3f', est), stars(est, se))
}

fmt_se <- function(se) {
  if (length(se) == 0 || is.na(se)) return('---')
  paste0('(', sprintf('%.3f', se), ')')
}

spec_label <- function(spec, regressor) {
  spec_clean <- sub('^[0-9]+\\.\\s+', '', spec)
  suffix <- ifelse(regressor == 'count', ' ($N^o$)', ' ($d^o$)')
  paste0(spec_clean, suffix)
}

make_ame_latex <- function(data) {
  specs <- data %>% distinct(spec, regressor) %>% arrange(spec)
  get_entry <- function(spec, fe_type, estimator, value) {
    row <- data %>% filter(spec == !!spec, fe_type == !!fe_type, estimator == !!estimator)
    if (nrow(row) == 0) return('---')
    if (value == 'est') fmt_est(row$ame, row$ame_se) else fmt_se(row$ame_se)
  }
  body <- c()
  for (i in seq_len(nrow(specs))) {
    s <- specs$spec[i]
    r <- specs$regressor[i]
    body <- c(body,
              paste0('        ', spec_label(s, r), ' & ',
                     get_entry(s, 'linear trend', 'OLS', 'est'), ' & ',
                     get_entry(s, 'linear trend', 'IV', 'est'), ' & ',
                     get_entry(s, 'cohort FE', 'OLS', 'est'), ' & ',
                     get_entry(s, 'cohort FE', 'IV', 'est'), ' \\\\'),
              paste0('        & ',
                     get_entry(s, 'linear trend', 'OLS', 'se'), ' & ',
                     get_entry(s, 'linear trend', 'IV', 'se'), ' & ',
                     get_entry(s, 'cohort FE', 'OLS', 'se'), ' & ',
                     get_entry(s, 'cohort FE', 'IV', 'se'), ' \\\\'))
  }
  lines <- c(
    '\\begin{table}[!htbp]',
    '    \\centering',
    '    \\caption{Location Spillover Estimates Across Specifications}',
    '    \\resizebox{0.86\\textwidth}{!}{',
    '    \\begin{tabular}{lcccc}',
    '        \\toprule',
    '        & \\multicolumn{4}{c}{Outcome: Alabama-Origin Outmigration per 100 OOS Students} \\\\',
    '        \\cmidrule(lr){2-5}',
    '        & \\multicolumn{2}{c}{Linear Cohort Trend} & \\multicolumn{2}{c}{Cohort Fixed Effects} \\\\',
    '        \\cmidrule(lr){2-3} \\cmidrule(lr){4-5}',
    '        Specification & OLS & IV & OLS & IV \\\\',
    '        \\midrule',
    body,
    '        \\midrule',
    '        \\multicolumn{5}{l}{\\parbox{0.84\\textwidth}{Note: Entries are average marginal effects interpreted as in-state students leaving Alabama per 100 additional out-of-state students.}} \\\\',
    '        \\multicolumn{5}{l}{\\parbox{0.84\\textwidth}{Standard errors are reported in parentheses and clustered at the state level. $^*$, $^{**}$, and $^{***}$ denote significance at the 10\\%, 5\\%, and 1\\% levels.}} \\\\',
    '        \\bottomrule',
    '    \\end{tabular}}',
    '    \\label{tbl:spillovers_variants_ame}',
    '\\end{table}'
  )
  paste(lines, collapse = '\n')
}

latex_ame_table <- make_ame_latex(ame_table)
dir.create('tables', showWarnings = FALSE)
writeLines(latex_ame_table, 'tables/analysis_spillovers_variants_ame.tex')
cat(latex_ame_table, '\n')

# OLS single-lag specifications ------------------------------------------------

single_lag_one <- function(s, data, fe_type) {
  lag_data <- data %>% filter(!is.na(.data[[s$lag]]))
  model <- fit_lm(lag_data, s$y, s$x, lag = s$lag, fe_type = fe_type)
  main_mult <- s$multiplier(lag_data, s$x, s$regressor)
  lag_mult <- s$multiplier(lag_data, s$lag, s$regressor)
  main_coef <- coef_row(model, s$x, main_mult)
  lag_coef <- coef_row(model, s$lag, lag_mult)
  data.frame(spec = s$name,
             regressor = s$regressor,
             fe_type = fe_type,
             main_ame = main_coef$ame,
             main_ame_se = main_coef$ame_se,
             lag_ame = lag_coef$ame,
             lag_ame_se = lag_coef$ame_se)
}

single_lag_ols <- bind_rows(lapply(c('cohort FE', 'linear trend'), function(fe_type) {
  bind_rows(lapply(specs, function(s) {
    single_lag_one(s, s$data, fe_type)
  }))
}))

ppml_single_lag_row <- function(fe_type) {
  ppml_lag_data <- ppml_data %>% filter(!is.na(ppml_x_lag))
  if (fe_type == 'cohort FE') {
    model <- fixest::fepois(ppml_y ~ ppml_x + ppml_x_lag + ur + net_rate | d_state + grad_y,
                            data = ppml_lag_data,
                            cluster = ~ d_state)
  } else {
    model <- fixest::fepois(ppml_y ~ ppml_x + ppml_x_lag + ur + net_rate + grad_y | d_state,
                            data = ppml_lag_data,
                            cluster = ~ d_state)
  }
  main_ame <- ppml_ame(model, ppml_lag_data, 'ppml_x', 'share')
  lag_ame <- ppml_ame(model, ppml_lag_data, 'ppml_x_lag', 'share')
  data.frame(spec = '4. PPML grouped choice',
             regressor = 'share',
             fe_type = fe_type,
             main_ame = main_ame$ame,
             main_ame_se = main_ame$ame_se,
             lag_ame = lag_ame$ame,
             lag_ame_se = lag_ame$ame_se)
}

single_lag_ols <- single_lag_ols %>%
  bind_rows(ppml_single_lag_row('cohort FE'),
            ppml_single_lag_row('linear trend')) %>%
  arrange_results()
rownames(single_lag_ols) <- NULL

print(single_lag_ols, row.names = FALSE)

# OLS lead/lag specifications --------------------------------------------------

lead_lag_one <- function(s, data, fe_type) {
  ll_data <- data %>%
    filter(!is.na(.data[[s$lag]]),
           !is.na(.data[[s$lead]]))
  model <- fit_lm(ll_data, s$y, s$x, lag = s$lag, lead = s$lead, fe_type = fe_type)
  main_mult <- s$multiplier(ll_data, s$x, s$regressor)
  lag_mult <- s$multiplier(ll_data, s$lag, s$regressor)
  lead_mult <- s$multiplier(ll_data, s$lead, s$regressor)
  main_coef <- coef_row(model, s$x, main_mult)
  lag_coef <- coef_row(model, s$lag, lag_mult)
  lead_coef <- coef_row(model, s$lead, lead_mult)
  data.frame(spec = s$name,
             regressor = s$regressor,
             fe_type = fe_type,
             main_ame = main_coef$ame,
             main_ame_se = main_coef$ame_se,
             lag_ame = lag_coef$ame,
             lag_ame_se = lag_coef$ame_se,
             lead_ame = lead_coef$ame,
             lead_ame_se = lead_coef$ame_se)
}

lead_lag_ols <- bind_rows(lapply(c('cohort FE', 'linear trend'), function(fe_type) {
  bind_rows(lapply(specs, function(s) {
    lead_lag_one(s, s$data, fe_type)
  }))
}))

ppml_lead_lag_row <- function(fe_type) {
  ppml_ll_data <- ppml_data %>%
    filter(!is.na(ppml_x_lag),
           !is.na(ppml_x_lead))
  if (fe_type == 'cohort FE') {
    model <- fixest::fepois(ppml_y ~ ppml_x + ppml_x_lag + ppml_x_lead + ur + net_rate | d_state + grad_y,
                            data = ppml_ll_data,
                            cluster = ~ d_state)
  } else {
    model <- fixest::fepois(ppml_y ~ ppml_x + ppml_x_lag + ppml_x_lead + ur + net_rate + grad_y | d_state,
                            data = ppml_ll_data,
                            cluster = ~ d_state)
  }
  main_ame <- ppml_ame(model, ppml_ll_data, 'ppml_x', 'share')
  lag_ame <- ppml_ame(model, ppml_ll_data, 'ppml_x_lag', 'share')
  lead_ame <- ppml_ame(model, ppml_ll_data, 'ppml_x_lead', 'share')
  data.frame(spec = '4. PPML grouped choice',
             regressor = 'share',
             fe_type = fe_type,
             main_ame = main_ame$ame,
             main_ame_se = main_ame$ame_se,
             lag_ame = lag_ame$ame,
             lag_ame_se = lag_ame$ame_se,
             lead_ame = lead_ame$ame,
             lead_ame_se = lead_ame$ame_se)
}

lead_lag_ols <- lead_lag_ols %>%
  bind_rows(ppml_lead_lag_row('cohort FE'),
            ppml_lead_lag_row('linear trend')) %>%
  arrange_results()
rownames(lead_lag_ols) <- NULL

print(lead_lag_ols, row.names = FALSE)

# LaTeX single-lag and lead/lag table ------------------------------------------

make_lag_latex <- function(single_lag, lead_lag) {
  specs <- single_lag %>%
    filter(fe_type == 'cohort FE') %>%
    distinct(spec, regressor) %>%
    arrange(spec)
  spec_headers <- paste0('        & ',
                         paste(spec_label(specs$spec, specs$regressor), collapse = ' & '),
                         ' \\\\')
  spec_numbers <- paste0('        & ',
                         paste(paste0('(', seq_len(nrow(specs)), ')'), collapse = ' & '),
                         ' \\\\')
  get_lag_entry <- function(data, term, value) {
    vals <- sapply(specs$spec, function(s) {
      row <- data %>% filter(fe_type == 'cohort FE', spec == !!s)
      est <- row[[paste0(term, '_ame')]]
      se <- row[[paste0(term, '_ame_se')]]
      if (value == 'est') fmt_est(est, se) else fmt_se(se)
    })
    paste(vals, collapse = ' & ')
  }
  panel_rows <- function(data, panel_title, terms) {
    out <- c(paste0('        \\textit{', panel_title, '} & \\multicolumn{4}{c}{} \\\\'),
             '        \\cmidrule(lr){1-1} \\cmidrule(lr){2-5}')
    for (term in terms) {
      label <- ifelse(term == 'main', 'Main effect',
                      ifelse(term == 'lag', 'Lagged effect', 'Lead effect'))
      out <- c(out,
               paste0('        ', label, ' & ', get_lag_entry(data, term, 'est'), ' \\\\'),
               paste0('        & ', get_lag_entry(data, term, 'se'), ' \\\\'))
    }
    out
  }
  lines <- c(
    '\\begin{table}[!htbp]',
    '    \\centering',
    '    \\caption{Lag and Lead Location Spillover Estimates}',
    '    \\resizebox{\\textwidth}{!}{',
    '    \\begin{tabular}{lcccc}',
    '        \\toprule',
    '        & \\multicolumn{4}{c}{Outcome: Alabama-Origin Outmigration per 100 OOS Students} \\\\',
    '        \\cmidrule(lr){2-5}',
    spec_headers,
    spec_numbers,
    '        \\midrule',
    panel_rows(single_lag, 'Panel A: One Lag', c('main', 'lag')),
    '        \\midrule',
    panel_rows(lead_lag, 'Panel B: One Lag and One Lead', c('main', 'lag', 'lead')),
    '        \\midrule',
    '        \\multicolumn{5}{l}{\\parbox{0.98\\textwidth}{Note: Entries are average marginal effects interpreted as in-state students leaving Alabama per 100 additional out-of-state students.}} \\\\',
    '        \\multicolumn{5}{l}{\\parbox{0.98\\textwidth}{All specifications use state and cohort fixed effects and include unemployment and net-migration controls. Standard errors are reported in parentheses and clustered at the state level. $^*$, $^{**}$, and $^{***}$ denote significance at the 10\\%, 5\\%, and 1\\% levels.}} \\\\',
    '        \\bottomrule',
    '    \\end{tabular}}',
    '    \\label{tbl:spillovers_variants_lags}',
    '\\end{table}'
  )
  paste(lines, collapse = '\n')
}

latex_lag_table <- make_lag_latex(single_lag_ols, lead_lag_ols)
writeLines(latex_lag_table, 'tables/analysis_spillovers_variants_lags.tex')
cat(latex_lag_table, '\n')
