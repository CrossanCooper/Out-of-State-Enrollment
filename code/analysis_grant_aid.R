############################################################
# UA automatic merit-aid apportionment by residency
# Fall 2023 entering first-time students
#
# Goal:
#   Estimate average merit aid for AL resident and nonresident
#   first-time students by integrating UA's published automatic
#   merit schedules over imputed GPA/test-score distributions.
#
# Main assumptions:
#   1. SAT and ACT submitters are mutually exclusive.
#   2. Score submission rates are equal across residency groups.
#   3. Non-submitters receive no automatic merit aid.
#   4. Test-score and GPA marginals are normal, fitted to
#      residency-specific 25th/75th percentiles.
#   5. GPA and test score are joined by a Gaussian copula.
#   6. GPA among score-submitters is shifted upward to match
#      the CDS fact that score-submitters have stronger GPAs.
############################################################

set.seed(123)

# Setup
source('C:/Users/ryanh/OneDrive/Documents/Grad School/Research/Out-of-State-Enrollment/code/setup.R')

# -----------------------------
# 1. Inputs
# -----------------------------

# Source URLs for hard-coded values below:
# UA CDS 2023-24:
#   https://oira.ua.edu/d/sites/all/files/reports24/CDS%202023-24%20FINAL.pdf
# UA Admissions FAQ archive:
#   https://web.archive.org/web/20250519205218/https://admissions.ua.edu/faqs/
# UA 2023 OOS scholarships archive:
#   https://web.archive.org/web/20220814215215/scholarships.ua.edu/freshman/out-of-state/
# UA 2023 in-state scholarships archive:
#   https://web.archive.org/web/20221129212646/https://scholarships.ua.edu/freshman/in-state/

# Fall 2023 first-time undergraduate enrolled counts by residency,
# read from UA Factbook admission_by_transfer_in_out.xlsx.
admissions_factbook <- read_xlsx(
  paste0(pathHome, 'data/factbook/admission_by_transfer_in_out.xlsx')
)

n_is_factbook <- admissions_factbook %>%
  filter(Type == "First-time undergraduate",
         Origin == "In-state",
         Fall == 2023) %>%
  pull(`# Enrolled`) %>%
  as.numeric()

n_oos_factbook <- admissions_factbook %>%
  filter(Type == "First-time undergraduate",
         Origin == "Out-of-state",
         Fall == 2023) %>%
  pull(`# Enrolled`) %>%
  as.numeric()

# IPEDS SFA is the target aid population: full-time, first-time
# degree/certificate-seeking undergraduates.
sfa_year <- "2023-24"
sfa <- read.csv(paste0(pathHome, 'data/ipeds_aid/sfa2324.csv')) %>%
  rename_with(toupper) %>%
  filter(UNITID == 100751)

ipeds_fy_ftft_students <- as.numeric(sfa$SCFA1N)
factbook_first_time_students <- n_is_factbook + n_oos_factbook

n_is <- ipeds_fy_ftft_students *
  n_is_factbook / factbook_first_time_students

n_oos <- ipeds_fy_ftft_students *
  n_oos_factbook / factbook_first_time_students

# CDS 2023-24 G1 and F1 charges/housing.
tuition_is  <- 11100
tuition_oos <- 32400
required_fees <- 800
food_housing <- 13316
p_oncampus_fy <- 0.941
p_oncampus_undergraduates <- 0.276

# CDS 2023-24 Fall 2023 test submission counts/rates.
# CDS reports 23% SAT submitters and 54% ACT submitters.
p_sat <- 0.23
p_act <- 0.54
p_submit <- p_sat + p_act

# Conditional test-type shares among score submitters
w_sat <- p_sat / p_submit
w_act <- p_act / p_submit

# Baseline equal score-submission assumption by residency
p_submit_is  <- p_submit
p_submit_oos <- p_submit

# Residency-specific IQRs from UA admissions FAQ archive.
# Test score ranges reflect students who opted test inclusive.
iqr <- list(
  is = list(
    ACT = c(q25 = 23,   q75 = 29),
    SAT = c(q25 = 1130, q75 = 1350),
    GPA = c(q25 = 3.47, q75 = 4.16)
  ),
  oos = list(
    ACT = c(q25 = 25,   q75 = 32),
    SAT = c(q25 = 1200, q75 = 1440),
    GPA = c(q25 = 3.57, q75 = 4.23)
  )
)

# Gaussian copula correlation between GPA and test score.
# This is a sensitivity parameter; 0.6 is the baseline.
rho <- 0.6

# Number of Monte Carlo draws per residency x test type.
# Increase for smoother estimates.
B <- 2e6

# -----------------------------
# 2. Helper functions
# -----------------------------

# Fit normal distribution from 25th and 75th percentiles.
fit_normal_from_iqr <- function(q25, q75) {
  q25 <- as.numeric(q25)
  q75 <- as.numeric(q75)
  z75 <- qnorm(0.75)
  mu <- (q25 + q75) / 2
  sigma <- (q75 - q25) / (2 * z75)
  list(mu = mu, sigma = sigma)
}

# Simulate two variables with normal marginals and Gaussian copula.
simulate_joint <- function(n, dist_x, dist_gpa, rho) {
  z1 <- rnorm(n)
  z2 <- rho * z1 + sqrt(1 - rho^2) * rnorm(n)
  
  x   <- dist_x$mu   + dist_x$sigma   * z1
  gpa <- dist_gpa$mu + dist_gpa$sigma * z2
  
  # UA reports weighted GPAs above 4.0, so allow GPA > 4.0.
  # Truncate only implausible lower tails.
  gpa <- pmax(gpa, 0)
  
  data.frame(score = x, gpa = gpa)
}

# Shift GPA distribution among score-submitters upward.
#
# CDS GPA table for Fall 2023:
#   Among score-submitters:
#     51% GPA = 4.0
#     18% GPA 3.75-3.99
#     15% GPA 3.50-3.74
#   So approximately 84% have GPA >= 3.50.
#
# We implement this by shifting the fitted GPA mean so that
# Pr(GPA >= 3.5) among score-submitters is approximately 0.84.
shift_gpa_to_match_submitters <- function(gpa_dist, target_p_ge_35 = 0.84) {
  sigma <- gpa_dist$sigma
  mu_new <- 3.5 - qnorm(1 - target_p_ge_35) * sigma
  list(mu = mu_new, sigma = sigma)
}

score_dists_from_iqr <- function(score_iqr, test) {
  list(
    is = fit_normal_from_iqr(score_iqr$is[[test]]["q25"],
                             score_iqr$is[[test]]["q75"]),
    oos = fit_normal_from_iqr(score_iqr$oos[[test]]["q25"],
                              score_iqr$oos[[test]]["q75"])
  )
}

normal_mixture_cdf <- function(x, dists, weights) {
  weights["is"] * pnorm(x, dists$is$mu, dists$is$sigma) +
    weights["oos"] * pnorm(x, dists$oos$mu, dists$oos$sigma)
}

normal_mixture_quantile <- function(p, dists, weights) {
  lower <- min(c(dists$is$mu - 8 * dists$is$sigma,
                 dists$oos$mu - 8 * dists$oos$sigma))
  upper <- max(c(dists$is$mu + 8 * dists$is$sigma,
                 dists$oos$mu + 8 * dists$oos$sigma))
  
  uniroot(
    function(x) normal_mixture_cdf(x, dists, weights) - p,
    lower = lower,
    upper = upper
  )$root
}

normal_mixture_mean <- function(dists, weights) {
  weights["is"] * dists$is$mu + weights["oos"] * dists$oos$mu
}

normal_mixture_band_share <- function(lower, upper, dists, weights) {
  weights["is"] *
    (pnorm(upper, dists$is$mu, dists$is$sigma) -
       pnorm(lower, dists$is$mu, dists$is$sigma)) +
    weights["oos"] *
    (pnorm(upper, dists$oos$mu, dists$oos$sigma) -
       pnorm(lower, dists$oos$mu, dists$oos$sigma))
}

normalize_band_shares <- function(bands) {
  bands$share <- bands$share / sum(bands$share)
  bands
}

summarize_score_mixture <- function(score_iqr,
                                    test,
                                    cds_percentiles,
                                    cds_bands,
                                    residency_weights,
                                    model) {
  dists <- score_dists_from_iqr(score_iqr, test)
  bands <- normalize_band_shares(cds_bands)
  
  percentile_rows <- data.frame(
    test = test,
    model = model,
    moment = c("q25", "q50", "q75", "mean"),
    target = c(cds_percentiles["q25"],
               cds_percentiles["q50"],
               cds_percentiles["q75"],
               cds_percentiles["mean"]),
    fitted = c(
      normal_mixture_quantile(0.25, dists, residency_weights),
      normal_mixture_quantile(0.50, dists, residency_weights),
      normal_mixture_quantile(0.75, dists, residency_weights),
      normal_mixture_mean(dists, residency_weights)
    )
  )
  
  band_rows <- data.frame(
    test = test,
    model = model,
    moment = paste0("band_", bands$band),
    target = bands$share,
    fitted = mapply(
      normal_mixture_band_share,
      bands$lower,
      bands$upper,
      MoreArgs = list(dists = dists, weights = residency_weights)
    )
  )
  
  bind_rows(percentile_rows, band_rows) %>%
    mutate(diff = fitted - target)
}

fit_gap_preserving_iqr <- function(test,
                                   cds_percentiles,
                                   cds_bands,
                                   residency_weights) {
  
  gap25 <- iqr$oos[[test]]["q25"] - iqr$is[[test]]["q25"]
  gap75 <- iqr$oos[[test]]["q75"] - iqr$is[[test]]["q75"]
  bands <- normalize_band_shares(cds_bands)
  
  score_scale <- if_else(test == "ACT", 1, 10)
  lower_bound <- if_else(test == "ACT", 1, 400)
  upper_bound <- if_else(test == "ACT", 36, 1600)
  
  make_trial_iqr <- function(par) {
    trial <- iqr
    trial$is[[test]] <- c(q25 = as.numeric(par[1]),
                          q75 = as.numeric(par[2]))
    trial$oos[[test]] <- c(q25 = as.numeric(par[1] + gap25),
                           q75 = as.numeric(par[2] + gap75))
    trial
  }
  
  objective <- function(par) {
    if (par[2] <= par[1] ||
        par[1] < lower_bound ||
        par[2] > upper_bound ||
        par[1] + gap25 < lower_bound ||
        par[2] + gap75 > upper_bound) {
      return(1e9 + sum(par^2))
    }
    
    loss <- tryCatch({
      trial <- make_trial_iqr(par)
      dists <- score_dists_from_iqr(trial, test)
      
      fitted_quantiles <- c(
        q25 = normal_mixture_quantile(0.25, dists, residency_weights),
        q50 = normal_mixture_quantile(0.50, dists, residency_weights),
        q75 = normal_mixture_quantile(0.75, dists, residency_weights),
        mean = normal_mixture_mean(dists, residency_weights)
      )
      fitted_quantiles <- setNames(
        as.numeric(fitted_quantiles),
        c("q25", "q50", "q75", "mean")
      )
      
      fitted_bands <- mapply(
        normal_mixture_band_share,
        bands$lower,
        bands$upper,
        MoreArgs = list(dists = dists, weights = residency_weights)
      )
      
      percentile_loss <- sum(
        ((fitted_quantiles -
            cds_percentiles[names(fitted_quantiles)]) / score_scale)^2
      )
      band_loss <- sum(((fitted_bands - bands$share) / 0.01)^2)
      
      percentile_loss + band_loss
    }, error = function(e) {
      1e9 + sum(par^2)
    })
    
    if (!is.finite(loss)) {
      loss <- 1e9 + sum(par^2)
    }
    
    loss
  }
  
  start <- c(
    cds_percentiles["q25"] - residency_weights["oos"] * gap25,
    cds_percentiles["q75"] - residency_weights["oos"] * gap75
  )
  
  fit <- optim(
    par = start,
    fn = objective,
    method = "L-BFGS-B",
    lower = c(lower_bound, lower_bound),
    upper = c(upper_bound, upper_bound)
  )
  
  list(
    fit = fit,
    score_iqr = make_trial_iqr(fit$par),
    gap25 = as.numeric(gap25),
    gap75 = as.numeric(gap75)
  )
}

# 2023 out-of-state automatic merit schedule from scholarship archive.
award_oos <- function(score, gpa, test = c("ACT", "SAT")) {
  test <- match.arg(test)
  aid <- numeric(length(score))
  
  gpa_300_349 <- gpa >= 3.00 & gpa < 3.50
  gpa_350_up  <- gpa >= 3.50
  
  if (test == "ACT") {
    # GPA 3.00-3.49
    aid[gpa_300_349 & score >= 27 & score < 28] <- 6000
    aid[gpa_300_349 & score >= 28 & score < 30] <- 8000
    aid[gpa_300_349 & score >= 30]              <- 15000
    
    # GPA 3.50+
    aid[gpa_350_up & score >= 25 & score < 27] <- 6000
    aid[gpa_350_up & score >= 27 & score < 28] <- 8000
    aid[gpa_350_up & score >= 28 & score < 29] <- 10000
    aid[gpa_350_up & score >= 29 & score < 30] <- 15000
    aid[gpa_350_up & score >= 30 & score < 32] <- 24000
    aid[gpa_350_up & score >= 32]              <- 28000
  }
  
  if (test == "SAT") {
    # GPA 3.00-3.49
    aid[gpa_300_349 & score >= 1260 & score < 1300] <- 6000
    aid[gpa_300_349 & score >= 1300 & score < 1360] <- 8000
    aid[gpa_300_349 & score >= 1360]                <- 15000
    
    # GPA 3.50+
    aid[gpa_350_up & score >= 1200 & score < 1260] <- 6000
    aid[gpa_350_up & score >= 1260 & score < 1300] <- 8000
    aid[gpa_350_up & score >= 1300 & score < 1330] <- 10000
    aid[gpa_350_up & score >= 1330 & score < 1360] <- 15000
    aid[gpa_350_up & score >= 1360 & score < 1420] <- 24000
    aid[gpa_350_up & score >= 1420]                <- 28000
  }
  
  aid
}

# 2023 in-state automatic merit schedule from scholarship archive.
#
# For in-state students, the schedule has two GPA bands:
#   GPA 3.00-3.49: higher test-score threshold, lower max award
#   GPA 3.50+    : slightly lower test-score threshold, Presidential = tuition
#
# CDS G1 reports 2023-24 in-state tuition = $11,100.
# Use this as the value of the in-state Presidential award.
award_is <- function(score, gpa, test = c("ACT", "SAT"),
                     tuition_is_award = tuition_is) {
  test <- match.arg(test)
  aid <- numeric(length(score))
  
  gpa_300_349 <- gpa >= 3.00 & gpa < 3.50
  gpa_350_up  <- gpa >= 3.50
  
  if (test == "ACT") {
    # GPA 3.00-3.49
    aid[gpa_300_349 & score >= 25 & score < 26] <- 4000
    aid[gpa_300_349 & score >= 26 & score < 27] <- 5000
    aid[gpa_300_349 & score >= 27 & score < 28] <- 6000
    aid[gpa_300_349 & score >= 28 & score < 29] <- 7000
    aid[gpa_300_349 & score >= 29 & score < 30] <- 8000
    aid[gpa_300_349 & score >= 30]              <- 9000
    
    # GPA 3.50+
    aid[gpa_350_up & score >= 21 & score < 25] <- 4000
    aid[gpa_350_up & score >= 25 & score < 26] <- 5000
    aid[gpa_350_up & score >= 26 & score < 27] <- 6000
    aid[gpa_350_up & score >= 27 & score < 28] <- 7000
    aid[gpa_350_up & score >= 28 & score < 29] <- 8000
    aid[gpa_350_up & score >= 29 & score < 30] <- 9000
    aid[gpa_350_up & score >= 30]              <- tuition_is_award
  }
  
  if (test == "SAT") {
    # GPA 3.00-3.49
    aid[gpa_300_349 & score >= 1200 & score < 1230] <- 4000
    aid[gpa_300_349 & score >= 1230 & score < 1260] <- 5000
    aid[gpa_300_349 & score >= 1260 & score < 1300] <- 6000
    aid[gpa_300_349 & score >= 1300 & score < 1330] <- 7000
    aid[gpa_300_349 & score >= 1330 & score < 1360] <- 8000
    aid[gpa_300_349 & score >= 1360]                <- 9000
    
    # GPA 3.50+
    aid[gpa_350_up & score >= 1060 & score < 1200] <- 4000
    aid[gpa_350_up & score >= 1200 & score < 1230] <- 5000
    aid[gpa_350_up & score >= 1230 & score < 1260] <- 6000
    aid[gpa_350_up & score >= 1260 & score < 1300] <- 7000
    aid[gpa_350_up & score >= 1300 & score < 1330] <- 8000
    aid[gpa_350_up & score >= 1330 & score < 1360] <- 9000
    aid[gpa_350_up & score >= 1360]                <- tuition_is_award
  }
  
  aid
}

# Estimate expected merit aid for a residency group.
estimate_group <- function(group = c("is", "oos"),
                           n_students,
                           p_submit_group,
                           rho = 0.6,
                           B = 2e6,
                           score_iqr = iqr,
                           target_p_gpa_ge_35_submitters = 0.84) {
  
  group <- match.arg(group)
  
  # Fit test-score distributions from residency-specific IQRs
  act_dist <- fit_normal_from_iqr(score_iqr[[group]]$ACT["q25"],
                                  score_iqr[[group]]$ACT["q75"])
  sat_dist <- fit_normal_from_iqr(score_iqr[[group]]$SAT["q25"],
                                  score_iqr[[group]]$SAT["q75"])
  
  # GPA remains fitted from the original residency-specific FAQ GPA IQRs.
  gpa_raw <- fit_normal_from_iqr(iqr[[group]]$GPA["q25"], iqr[[group]]$GPA["q75"])
  gpa_submitter <- shift_gpa_to_match_submitters(
    gpa_raw,
    target_p_ge_35 = target_p_gpa_ge_35_submitters
  )
  
  # Simulate ACT and SAT submitters separately
  sim_act <- simulate_joint(B, act_dist, gpa_submitter, rho)
  sim_sat <- simulate_joint(B, sat_dist, gpa_submitter, rho)
  
  if (group == "is") {
    aid_act <- award_is(sim_act$score, sim_act$gpa, test = "ACT")
    aid_sat <- award_is(sim_sat$score, sim_sat$gpa, test = "SAT")
  } else {
    aid_act <- award_oos(sim_act$score, sim_act$gpa, test = "ACT")
    aid_sat <- award_oos(sim_sat$score, sim_sat$gpa, test = "SAT")
  }
  
  # Expected aid among score-submitters
  mean_aid_submitter <- w_act * mean(aid_act) + w_sat * mean(aid_sat)
  
  # Recipient probability among score-submitters
  p_recipient_submitter <- w_act * mean(aid_act > 0) + w_sat * mean(aid_sat > 0)
  
  # Unconditional on all entering students
  mean_aid_all <- p_submit_group * mean_aid_submitter
  p_recipient_all <- p_submit_group * p_recipient_submitter
  
  total_aid <- n_students * mean_aid_all
  n_recipients <- n_students * p_recipient_all
  
  data.frame(
    group = group,
    n_students = n_students,
    p_submit = p_submit_group,
    mean_aid_all = mean_aid_all,
    p_recipient_all = p_recipient_all,
    n_recipients = n_recipients,
    mean_aid_recipients = total_aid / n_recipients,
    total_aid = total_aid
  )
}

# -----------------------------
# 3. Main score distribution: CDS gap-preserving normal mixtures
# -----------------------------

# CDS 2023-24 aggregate score levels appear more consistent with
# the CDS financial-aid totals than the FAQ residency-specific levels.
# However, the CDS aggregate ACT q25 = 22 is inconsistent with both
# FAQ residency q25 values being strictly higher. The main specification
# therefore fits aggregate ACT/SAT normal mixtures to CDS score targets
# while preserving the FAQ in-state vs. OOS q25/q75 gaps.
score_residency_weights <- c(
  is = n_is / (n_is + n_oos),
  oos = n_oos / (n_is + n_oos)
)

# CDS 2023-24 C9/C10 aggregate score targets for Fall 2023
# first-year score-submitters. Rounded band percentages are normalized
# before being used as fitting targets.
cds_act_percentiles <- c(q25 = 22, q50 = 26, q75 = 30, mean = 26)
cds_sat_percentiles <- c(q25 = 1120, q50 = 1230, q75 = 1370, mean = 1241)

cds_act_bands <- data.frame(
  band = c("30-36", "24-29", "18-23", "12-17", "6-11", "below_6"),
  lower = c(29.5, 23.5, 17.5, 11.5, 5.5, -Inf),
  upper = c(Inf, 29.5, 23.5, 17.5, 11.5, 5.5),
  share = c(0.31, 0.35, 0.28, 0.05, 0.00, 0.00)
)

cds_sat_bands <- data.frame(
  band = c("1400-1600", "1200-1399", "1000-1199",
           "800-999", "600-799", "400-599"),
  lower = c(1399.5, 1199.5, 999.5, 799.5, 599.5, -Inf),
  upper = c(Inf, 1399.5, 1199.5, 999.5, 799.5, 599.5),
  share = c(0.20, 0.40, 0.34, 0.06, 0.00, 0.00)
)

cds_gap_act_fit <- fit_gap_preserving_iqr(
  "ACT",
  cds_act_percentiles,
  cds_act_bands,
  score_residency_weights
)

cds_gap_sat_fit <- fit_gap_preserving_iqr(
  "SAT",
  cds_sat_percentiles,
  cds_sat_bands,
  score_residency_weights
)

cds_gap_iqr <- iqr
cds_gap_iqr$is$ACT <- cds_gap_act_fit$score_iqr$is$ACT
cds_gap_iqr$oos$ACT <- cds_gap_act_fit$score_iqr$oos$ACT
cds_gap_iqr$is$SAT <- cds_gap_sat_fit$score_iqr$is$SAT
cds_gap_iqr$oos$SAT <- cds_gap_sat_fit$score_iqr$oos$SAT

cds_gap_fitted_iqrs <- bind_rows(
  data.frame(test = "ACT", group = c("is", "oos"),
             q25 = c(cds_gap_iqr$is$ACT["q25"],
                     cds_gap_iqr$oos$ACT["q25"]),
             q75 = c(cds_gap_iqr$is$ACT["q75"],
                     cds_gap_iqr$oos$ACT["q75"])),
  data.frame(test = "SAT", group = c("is", "oos"),
             q25 = c(cds_gap_iqr$is$SAT["q25"],
                     cds_gap_iqr$oos$SAT["q25"]),
             q75 = c(cds_gap_iqr$is$SAT["q75"],
                     cds_gap_iqr$oos$SAT["q75"]))
)

cds_gap_preserved_gaps <- cds_gap_fitted_iqrs %>%
  group_by(test) %>%
  summarize(
    q25_gap = q25[group == "oos"] - q25[group == "is"],
    q75_gap = q75[group == "oos"] - q75[group == "is"],
    .groups = "drop"
  )

cds_gap_score_diagnostics <- bind_rows(
  summarize_score_mixture(iqr, "ACT", cds_act_percentiles,
                          cds_act_bands, score_residency_weights,
                          "FAQ IQR baseline"),
  summarize_score_mixture(cds_gap_iqr, "ACT", cds_act_percentiles,
                          cds_act_bands, score_residency_weights,
                          "CDS gap-preserving"),
  summarize_score_mixture(iqr, "SAT", cds_sat_percentiles,
                          cds_sat_bands, score_residency_weights,
                          "FAQ IQR baseline"),
  summarize_score_mixture(cds_gap_iqr, "SAT", cds_sat_percentiles,
                          cds_sat_bands, score_residency_weights,
                          "CDS gap-preserving")
)

print(cds_gap_fitted_iqrs)
print(cds_gap_preserved_gaps)
print(cds_gap_score_diagnostics)

set.seed(124)

res_is <- estimate_group(
  group = "is",
  n_students = n_is,
  p_submit_group = p_submit_is,
  rho = rho,
  B = B,
  score_iqr = cds_gap_iqr
)

res_oos <- estimate_group(
  group = "oos",
  n_students = n_oos,
  p_submit_group = p_submit_oos,
  rho = rho,
  B = B,
  score_iqr = cds_gap_iqr
)

baseline <- rbind(res_is, res_oos)
baseline$total_share <- baseline$total_aid / sum(baseline$total_aid)

print(baseline)

# -----------------------------
# 4. Sensitivity: FAQ-level score distributions
# -----------------------------

set.seed(123)

faq_iqr_res_is <- estimate_group(
  group = "is",
  n_students = n_is,
  p_submit_group = p_submit_is,
  rho = rho,
  B = B,
  score_iqr = iqr
)

faq_iqr_res_oos <- estimate_group(
  group = "oos",
  n_students = n_oos,
  p_submit_group = p_submit_oos,
  rho = rho,
  B = B,
  score_iqr = iqr
)

faq_iqr_sensitivity <- rbind(faq_iqr_res_is, faq_iqr_res_oos)
faq_iqr_sensitivity$total_share <-
  faq_iqr_sensitivity$total_aid / sum(faq_iqr_sensitivity$total_aid)

score_distribution_merit_comparison <- bind_rows(
  baseline %>%
    mutate(model = "CDS gap-preserving main"),
  faq_iqr_sensitivity %>%
    mutate(model = "FAQ-IQR sensitivity")
) %>%
  transmute(
    model,
    group,
    avg_auto_merit = mean_aid_all,
    total_auto_merit = total_aid,
    total_auto_merit_millions = total_aid / 1e6,
    total_share
  )

print(faq_iqr_sensitivity)
print(score_distribution_merit_comparison)

# -----------------------------
# 5. Sensitivity: GPA-test correlation
# -----------------------------

rho_grid <- c(0, 0.3, 0.6, 0.8)

rho_sens <- do.call(rbind, lapply(rho_grid, function(rr) {
  x_is <- estimate_group("is", n_is, p_submit_is, rho = rr,
                         B = B / 4, score_iqr = cds_gap_iqr)
  x_oos <- estimate_group("oos", n_oos, p_submit_oos, rho = rr,
                          B = B / 4, score_iqr = cds_gap_iqr)
  x <- rbind(x_is, x_oos)
  data.frame(
    rho = rr,
    avg_is = x$mean_aid_all[x$group == "is"],
    avg_oos = x$mean_aid_all[x$group == "oos"],
    oos_share = x$total_aid[x$group == "oos"] / sum(x$total_aid)
  )
}))

print(rho_sens)

# -----------------------------
# 5B. Sensitivity: differential score submission by residency
# -----------------------------

# Overall score-submission rate must remain 77%.
# Given assumed OOS submission rate, solve for IS submission rate:
#
#   p_submit_total = (N_IS p_IS + N_OOS p_OOS) / (N_IS + N_OOS)
#
# This preserves the CDS aggregate submission rate.
oos_submit_grid <- c(0.65, 0.77, 0.85, 0.90)

submit_sens <- do.call(rbind, lapply(oos_submit_grid, function(p_oos_assumed) {
  
  p_is_implied <- ((n_is + n_oos) * p_submit - n_oos * p_oos_assumed) / n_is
  
  x_is <- estimate_group("is", n_is, p_is_implied, rho = rho,
                         B = B / 4, score_iqr = cds_gap_iqr)
  x_oos <- estimate_group("oos", n_oos, p_oos_assumed, rho = rho,
                          B = B / 4, score_iqr = cds_gap_iqr)
  x <- rbind(x_is, x_oos)
  
  data.frame(
    p_submit_oos = p_oos_assumed,
    p_submit_is = p_is_implied,
    avg_is = x$mean_aid_all[x$group == "is"],
    avg_oos = x$mean_aid_all[x$group == "oos"],
    oos_share = x$total_aid[x$group == "oos"] / sum(x$total_aid)
  )
}))

print(submit_sens)

############################################################
# 6. Exact-match institutional tuition grants by residency
#
# This block starts from the automatic-merit estimates and
# allocates the residual needed to match a target for first-year
# institutional tuition grants.
#
# Preferred target:
#   IPEDS SFA total institutional grant/scholarship aid awarded
#   to full-time, first-time degree/certificate-seeking
#   undergraduate students, net of estimated first-year
#   auxiliary grants when the target is tuition-only.
#
# If unavailable:
#   use a CDS proxy based on:
#     - H2A first-year no-need institutional non-need grants
#     - H2 first-year all-source need grants
#     - H1 all-undergraduate institutional share of need grants
#
# Key issue:
#   CDS need-based aid can include non-need/merit aid used to
#   meet need. To avoid double-counting, subtract the amount by
#   which modeled automatic merit exceeds the clean H2A no-need
#   institutional non-need total.
############################################################

# -----------------------------
# 6A. Automatic merit totals
# -----------------------------

auto_is  <- baseline$total_aid[baseline$group == "is"]
auto_oos <- baseline$total_aid[baseline$group == "oos"]
auto_total <- auto_is + auto_oos

# -----------------------------
# 6B. Choose institutional grant target
# -----------------------------

# Option 1: IPEDS first-year institutional tuition-grant target.
#
# This is the preferred target:
#   Total institutional grant/scholarship aid to full-time,
#   first-time degree/certificate-seeking undergraduates,
#   less estimated first-year auxiliary institutional grants.
#
ipeds_fy_inst_grants <- as.numeric(sfa$IGRNT_T)
ipeds_fy_inst_grant_recipients <- as.numeric(sfa$IGRNT_N)
ipeds_fy_inst_grant_pct <- as.numeric(sfa$IGRNT_P)
ipeds_fy_inst_grant_avg <- as.numeric(sfa$IGRNT_A)

ipeds_avg_from_total_and_recipients <-
  ipeds_fy_inst_grants / ipeds_fy_inst_grant_recipients

ipeds_avg_including_zeroes <-
  ipeds_fy_inst_grants / ipeds_fy_ftft_students

ipeds_recipient_pct_from_counts <-
  100 * ipeds_fy_inst_grant_recipients / ipeds_fy_ftft_students

# Finance discount fields identify institutional discounts applied
# to auxiliary enterprises, but only for all students. Estimate the
# first-year share of auxiliary discounts using CDS Section F housing
# shares. This is preferable to assuming first-years have the same
# tuition-vs.-auxiliary discount split as all undergraduates, because
# 94.1% of first-time first-year students live in college-operated
# housing versus 27.6% of all undergraduates.
fin_2324 <- read.csv(paste0(pathHome, 'data/ipeds_finance/f2324_f1a.csv')) %>%
  rename_with(toupper) %>%
  filter(UNITID == 100751)

ipeds_undergraduates <- as.numeric(sfa$SCFA2)

endowment_gift_discounts_auxiliary <- as.numeric(fin_2324$F1E162)
other_institutional_discounts_auxiliary <- as.numeric(fin_2324$F1E172)

institutional_discounts_auxiliary <-
  endowment_gift_discounts_auxiliary +
  other_institutional_discounts_auxiliary

estimated_fy_share_of_auxiliary_discounts <-
  (p_oncampus_fy * ipeds_fy_ftft_students) /
  (p_oncampus_undergraduates * ipeds_undergraduates)

estimated_fy_share_of_auxiliary_discounts <-
  pmin(estimated_fy_share_of_auxiliary_discounts, 1)

estimated_fy_auxiliary_inst_grants <-
  institutional_discounts_auxiliary *
  estimated_fy_share_of_auxiliary_discounts

ipeds_fy_inst_tuition_grants <-
  ipeds_fy_inst_grants - estimated_fy_auxiliary_inst_grants

# Alternative target: CDS proxy for first-year institutional grant total.
#
# Source: UA CDS 2024-25, Section H, which reports 2023-24 final
# aid figures for H1, H2, H2A, and H6.

# H2A: no-need first-time full-time first-year students receiving
# institutional non-need scholarship/grant aid.
cds_no_need_inst_nonneed_n   <- 2942
cds_no_need_inst_nonneed_avg <- 19109

cds_no_need_inst_nonneed_total <-
  cds_no_need_inst_nonneed_n * cds_no_need_inst_nonneed_avg

# H2: first-time full-time first-year students receiving need-based
# scholarship/grant aid from all sources.
cds_need_grant_n   <- 3670
cds_need_grant_avg <- 19144

cds_need_grant_total_all_sources <-
  cds_need_grant_n * cds_need_grant_avg

# H1: all-undergraduate need-based grants by source.
# These are used only to estimate the institutional share of need grants.
cds_h1_need_federal       <-  39995937
cds_h1_need_state_other   <-   3141161
cds_h1_need_institutional <- 101548455
cds_h1_need_external      <-   3592459
# CDS 2024-25 H2 line A reports the H1/H2/H2A aid population.
cds_h2_ft_undergraduate_n <- 29956
cds_h2_lt_undergraduate_n <-  2367
cds_h2_undergraduate_n <- cds_h2_ft_undergraduate_n + cds_h2_lt_undergraduate_n
cds_h1_nonneed_institutional <- 213991536
cds_h2a_ft_undergrad_no_need_inst_nonneed_n <- 10928
cds_h2a_lt_undergrad_no_need_inst_nonneed_n <- 186
cds_h2a_all_undergrad_no_need_inst_nonneed_n <-
  cds_h2a_ft_undergrad_no_need_inst_nonneed_n +
  cds_h2a_lt_undergrad_no_need_inst_nonneed_n

cds_h1_need_total <-
  cds_h1_need_federal +
  cds_h1_need_state_other +
  cds_h1_need_institutional +
  cds_h1_need_external

institutional_share_of_need_grants <-
  cds_h1_need_institutional / cds_h1_need_total

# Gross CDS-imputed first-year institutional aid:
# clean no-need institutional non-need aid
# + imputed institutional share of need-based grants.
cds_fy_inst_grants_gross <-
  cds_no_need_inst_nonneed_total +
  institutional_share_of_need_grants * cds_need_grant_total_all_sources

cds_fy_inst_tuition_grants_gross <-
  cds_fy_inst_grants_gross - estimated_fy_auxiliary_inst_grants

# Diagnostic: compare modeled automatic merit to estimated first-year
# non-need institutional grant aid. H1 gives all-undergraduate dollars;
# H2A gives first-year and all-undergraduate recipient counts.
cds_h2a_first_year_share_of_nonneed_inst_recipients <-
  cds_no_need_inst_nonneed_n /
  cds_h2a_all_undergrad_no_need_inst_nonneed_n

cds_fy_nonneed_inst_grants_from_h1_recipient_share <-
  cds_h1_nonneed_institutional *
  cds_h2a_first_year_share_of_nonneed_inst_recipients

target_fy_inst_grants <- ipeds_fy_inst_tuition_grants

# -----------------------------
# 6B.1 IPEDS/CDS target diagnostics
# -----------------------------

target_diagnostics <- data.frame(
  source = c(
    "IPEDS SFA 2023-24 direct total",
    "IPEDS SFA 2023-24 tuition target",
    "CDS 2024-25 H proxy gross",
    "CDS 2024-25 H tuition proxy gross"
  ),
  total_inst_grants = c(
    ipeds_fy_inst_grants,
    ipeds_fy_inst_tuition_grants,
    cds_fy_inst_grants_gross,
    cds_fy_inst_tuition_grants_gross
  )
)

target_diagnostics$diff_from_ipeds_2023 <-
  target_diagnostics$total_inst_grants - ipeds_fy_inst_grants

target_diagnostics$pct_diff_from_ipeds_2023 <-
  target_diagnostics$diff_from_ipeds_2023 / ipeds_fy_inst_grants

print(target_diagnostics)

# -----------------------------
# 6C. Residual institutional grant aid to allocate
# -----------------------------

residual_inst_grants <- target_fy_inst_grants - auto_total

# If negative, the target is inconsistent with the automatic-merit model.
# In that case, scale down automatic merit proportionally rather than
# allocate a negative residual.
if (residual_inst_grants < 0) {
  warning("Institutional grant target is below modeled automatic merit total. Scaling merit down proportionally.")
  
  scale_factor <- target_fy_inst_grants / auto_total
  
  auto_is_adj  <- auto_is  * scale_factor
  auto_oos_adj <- auto_oos * scale_factor
  
  residual_inst_grants <- 0
} else {
  scale_factor <- 1
  auto_is_adj  <- auto_is
  auto_oos_adj <- auto_oos
}

# -----------------------------
# 6D. Alabama Advantage chunk
# -----------------------------

# Alabama Advantage is assigned 100% to in-state students.
# It is a residual tuition-filling program for Pell-eligible AL residents.
#
# These assumptions are transparent and should be sensitivity-tested.
pell_share_is <- 0.25
avg_alabama_advantage_topup <- 3500

alabama_advantage_total <-
  n_is * pell_share_is * avg_alabama_advantage_topup

# Cannot allocate more Alabama Advantage than the residual.
alabama_advantage_total <-
  min(alabama_advantage_total, residual_inst_grants)

# -----------------------------
# 6E. Allocate remaining residual institutional aid
# -----------------------------

other_residual_inst_grants <-
  residual_inst_grants - alabama_advantage_total

# Allocate the non-Alabama-Advantage residual by expected grant
# dollars per student, not by a literal split of dollars across
# residency groups. The baseline 1:1 ratio means the average
# residual grant is the same for in-state and OOS students.
residual_share_to_is <- function(is_per_student_weight,
                                 oos_per_student_weight) {
  n_is * is_per_student_weight /
    (n_is * is_per_student_weight +
       n_oos * oos_per_student_weight)
}

residual_per_student_weight_is <- 1
residual_per_student_weight_oos <- 1

share_other_residual_to_is <- residual_share_to_is(
  residual_per_student_weight_is,
  residual_per_student_weight_oos
)

resid_is <-
  alabama_advantage_total +
  share_other_residual_to_is * other_residual_inst_grants

resid_oos <-
  (1 - share_other_residual_to_is) * other_residual_inst_grants

# -----------------------------
# 6F. Final total institutional grant estimates
# -----------------------------

total_is  <- auto_is_adj  + resid_is
total_oos <- auto_oos_adj + resid_oos

exact_match_results <- data.frame(
  group = c("is", "oos"),
  n_students = c(n_is, n_oos),
  auto_merit_total = c(auto_is_adj, auto_oos_adj),
  residual_inst_grants = c(resid_is, resid_oos),
  total_inst_grants = c(total_is, total_oos)
)

exact_match_results$avg_auto_merit <-
  exact_match_results$auto_merit_total / exact_match_results$n_students

exact_match_results$avg_residual_inst_grants <-
  exact_match_results$residual_inst_grants / exact_match_results$n_students

exact_match_results$avg_total_inst_grants <-
  exact_match_results$total_inst_grants / exact_match_results$n_students

exact_match_results$share_total_inst_grants <-
  exact_match_results$total_inst_grants / sum(exact_match_results$total_inst_grants)

print(exact_match_results)

# Alternative exact-match allocation using the FAQ-IQR score-distribution
# sensitivity, with the same IPEDS tuition-grant target, Alabama Advantage
# assumptions, and 50/50 residual split.
build_exact_match_from_auto <- function(auto_results) {
  
  auto_is_s <- auto_results$total_aid[auto_results$group == "is"]
  auto_oos_s <- auto_results$total_aid[auto_results$group == "oos"]
  auto_total_s <- auto_is_s + auto_oos_s
  
  residual_s <- target_fy_inst_grants - auto_total_s
  
  if (residual_s < 0) {
    warning("Institutional grant target is below sensitivity automatic merit total. Scaling merit down proportionally.")
    
    scale_factor_s <- target_fy_inst_grants / auto_total_s
    auto_is_adj_s  <- auto_is_s  * scale_factor_s
    auto_oos_adj_s <- auto_oos_s * scale_factor_s
    residual_s <- 0
  } else {
    auto_is_adj_s  <- auto_is_s
    auto_oos_adj_s <- auto_oos_s
  }
  
  alabama_advantage_s <- min(
    n_is * pell_share_is * avg_alabama_advantage_topup,
    residual_s
  )
  
  other_residual_s <- residual_s - alabama_advantage_s
  
  resid_is_s <-
    alabama_advantage_s +
    share_other_residual_to_is * other_residual_s
  
  resid_oos_s <-
    (1 - share_other_residual_to_is) * other_residual_s
  
  out <- data.frame(
    group = c("is", "oos"),
    n_students = c(n_is, n_oos),
    auto_merit_total = c(auto_is_adj_s, auto_oos_adj_s),
    residual_inst_grants = c(resid_is_s, resid_oos_s),
    total_inst_grants = c(
      auto_is_adj_s + resid_is_s,
      auto_oos_adj_s + resid_oos_s
    )
  )
  
  out$avg_auto_merit <- out$auto_merit_total / out$n_students
  out$avg_residual_inst_grants <-
    out$residual_inst_grants / out$n_students
  out$avg_total_inst_grants <- out$total_inst_grants / out$n_students
  out$share_total_inst_grants <-
    out$total_inst_grants / sum(out$total_inst_grants)
  
  out
}

faq_iqr_exact_match_results <-
  build_exact_match_from_auto(faq_iqr_sensitivity)

print(faq_iqr_exact_match_results)

# -----------------------------
# 6G. Sensitivity over residual allocation
# -----------------------------

residual_split_sensitivity <- data.frame(
  residual_split = c("1:2 IS:OOS", "1:1 IS:OOS", "2:1 IS:OOS"),
  is_per_student_weight = c(1, 1, 2),
  oos_per_student_weight = c(2, 1, 1)
) %>%
  mutate(
    share_other_residual_to_is = residual_share_to_is(
      is_per_student_weight,
      oos_per_student_weight
    )
  )

residual_alloc_sens <- do.call(rbind, lapply(seq_len(nrow(residual_split_sensitivity)), function(i) {
  
  s_is <- residual_split_sensitivity$share_other_residual_to_is[i]
  
  resid_is_s <-
    alabama_advantage_total +
    s_is * other_residual_inst_grants
  
  resid_oos_s <-
    (1 - s_is) * other_residual_inst_grants
  
  total_is_s  <- auto_is_adj  + resid_is_s
  total_oos_s <- auto_oos_adj + resid_oos_s
  
  data.frame(
    residual_split = residual_split_sensitivity$residual_split[i],
    is_per_student_weight =
      residual_split_sensitivity$is_per_student_weight[i],
    oos_per_student_weight =
      residual_split_sensitivity$oos_per_student_weight[i],
    share_other_residual_to_is = s_is,
    avg_total_is = total_is_s / n_is,
    avg_total_oos = total_oos_s / n_oos,
    is_total_inst_grants = total_is_s,
    oos_total_inst_grants = total_oos_s,
    oos_share_total_inst_grants =
      total_oos_s / (total_is_s + total_oos_s)
  )
}))

print(residual_alloc_sens)

# -----------------------------
# 6H. IPEDS Finance/FTE cost measures
# -----------------------------

finance_year <- "2023-24"
fte_year <- 2024

fte_2324 <- read_xlsx(paste0(pathHome, 'data/factbook/fte_by_college_in_out.xlsx')) %>%
  rename(origin = Origin,
         college = `By College/School`,
         y = STYEAR,
         fte = sum_fte) %>%
  mutate(y = as.numeric(y), fte = as.numeric(fte)) %>%
  filter(y == fte_year)

total_fte_2324 <- sum(fte_2324$fte, na.rm = TRUE)

core_education_expenses <-
  as.numeric(fin_2324$F1C011) +
  as.numeric(fin_2324$F1C051) +
  as.numeric(fin_2324$F1C061)

auxiliary_expenses <- as.numeric(fin_2324$F1C111)

core_cost_per_fte <- core_education_expenses / total_fte_2324
auxiliary_cost_per_fte <- auxiliary_expenses / total_fte_2324

auxiliary_inclusive_cost_per_fte <-
  core_cost_per_fte + auxiliary_cost_per_fte

cost_summary <- data.frame(
  finance_year = finance_year,
  fte_year = fte_year,
  total_fte = total_fte_2324,
  core_education_expenses = core_education_expenses,
  auxiliary_expenses = auxiliary_expenses,
  core_cost_per_fte = core_cost_per_fte,
  auxiliary_cost_per_fte = auxiliary_cost_per_fte,
  auxiliary_inclusive_cost_per_fte =
    auxiliary_inclusive_cost_per_fte
)

print(cost_summary)

# -----------------------------
# 6I. ACT score distributions by residency
# -----------------------------

act_bin_weights <- function(group = c("is", "oos"),
                            act_grid,
                            score_iqr = cds_gap_iqr) {
  
  group <- match.arg(group)
  act_dist <- fit_normal_from_iqr(score_iqr[[group]]$ACT["q25"],
                                  score_iqr[[group]]$ACT["q75"])
  act_density <- dnorm(act_grid, act_dist$mu, act_dist$sigma)
  
  data.frame(
    act = act_grid,
    act_prob = act_density / sum(act_density)
  )
}

act_grid <- 1:36
act_distribution_plot_grid <- 12:36
act_merit_plot_grid <- 18:36
sat_merit_plot_grid <- seq(1000, 1600, by = 10)
gpa_plot <- 3.50

act_distribution_plot_data <- bind_rows(
  act_bin_weights("is", act_distribution_plot_grid) %>%
    mutate(group = "In-state"),
  act_bin_weights("oos", act_distribution_plot_grid) %>%
    mutate(group = "Out-of-state")
) %>%
  mutate(group = factor(group, levels = c("In-state", "Out-of-state")))

act_distribution_plot <- ggplot(
  act_distribution_plot_data,
  aes(x = act, y = act_prob, color = group)
) +
  geom_line(linewidth = 1) +
  scale_x_continuous(breaks = seq(12, 36, by = 3)) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(
    values = c("In-state" = "#440154FF",
               "Out-of-state" = "#21908CFF"),
    name = NULL
  ) +
  labs(x = "ACT score", y = "Share of ACT submitters") +
  theme_classic() +
  theme(
    panel.grid.major.y = element_line(color = "gray80", linetype = "dashed"),
    legend.position = "bottom"
  )

print(act_distribution_plot)

ggsave(
  paste0(pathFigures, "act_score_distribution.png"),
  act_distribution_plot,
  width = 6.5,
  height = 5
)

act_distribution_faq_plot_data <- bind_rows(
  act_bin_weights("is", act_distribution_plot_grid, iqr) %>%
    mutate(group = "In-state"),
  act_bin_weights("oos", act_distribution_plot_grid, iqr) %>%
    mutate(group = "Out-of-state")
) %>%
  mutate(group = factor(group, levels = c("In-state", "Out-of-state")))

act_distribution_faq_plot <- ggplot(
  act_distribution_faq_plot_data,
  aes(x = act, y = act_prob, color = group)
) +
  geom_line(linewidth = 1) +
  scale_x_continuous(breaks = seq(12, 36, by = 3)) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(
    values = c("In-state" = "#440154FF",
               "Out-of-state" = "#21908CFF"),
    name = NULL
  ) +
  labs(x = "ACT score", y = "Share of ACT submitters") +
  theme_classic() +
  theme(
    panel.grid.major.y = element_line(color = "gray80", linetype = "dashed"),
    legend.position = "bottom"
  )

print(act_distribution_faq_plot)

ggsave(
  paste0(pathFigures, "act_score_distribution_faq_sensitivity.png"),
  act_distribution_faq_plot,
  width = 6.5,
  height = 5
)

# Share of all OOS students who are either non-submitters or have
# ACT-equivalent scores at or below selected cutoffs.
oos_act_cutoff_shares <- data.frame(
  act_cutoff = c(24, 26, 28, 30)
) %>%
  mutate(
    share_oos_submitters_at_or_below =
      sapply(
        act_cutoff,
        function(x) sum(act_bin_weights("oos", act_grid)$act_prob[
          act_bin_weights("oos", act_grid)$act <= x
        ])
      ),
    share_all_oos_at_or_below_or_non_submitter =
      (1 - p_submit_oos) +
      p_submit_oos * share_oos_submitters_at_or_below
  )

print(oos_act_cutoff_shares)

# -----------------------------
# 6J. Plot merit aid and net tuition schedules by ACT score
# -----------------------------

merit_schedule_plot_data <- data.frame(
  group = rep(c("In-state", "Out-of-state"), each = length(act_merit_plot_grid)),
  act = rep(act_merit_plot_grid, times = 2)
) %>%
  left_join(
    exact_match_results %>%
      transmute(
        group = if_else(group == "is", "In-state", "Out-of-state"),
        avg_residual_tuition_grants = avg_residual_inst_grants
      ),
    by = "group"
  ) %>%
  mutate(
    gross_tuition_fees = if_else(
      group == "In-state",
      tuition_is + required_fees,
      tuition_oos + required_fees
    ),
    automatic_merit = if_else(
      group == "In-state",
      award_is(act, gpa_plot, test = "ACT"),
      award_oos(act, gpa_plot, test = "ACT")
    ),
    residual_tuition_grants =
      avg_residual_tuition_grants,
    total_tuition_grants =
      automatic_merit + residual_tuition_grants,
    net_tuition_fees = pmax(gross_tuition_fees - total_tuition_grants, 0),
    group = factor(group, levels = c("In-state", "Out-of-state"))
  )

sat_merit_schedule_plot_data <- data.frame(
  group = rep(c("In-state", "Out-of-state"), each = length(sat_merit_plot_grid)),
  score = rep(sat_merit_plot_grid, times = 2)
) %>%
  mutate(
    test = "SAT",
    automatic_merit = if_else(
      group == "In-state",
      award_is(score, gpa_plot, test = "SAT"),
      award_oos(score, gpa_plot, test = "SAT")
    ),
    group = factor(group, levels = c("In-state", "Out-of-state"))
  )

merit_schedule_by_test_plot_data <- bind_rows(
  merit_schedule_plot_data %>%
    transmute(
      test = "ACT",
      score = act,
      group,
      automatic_merit
    ),
  sat_merit_schedule_plot_data %>%
    select(test, score, group, automatic_merit)
) %>%
  mutate(test = factor(test, levels = c("ACT", "SAT")))

tuition_line_labels <- data.frame(
  test = factor(rep(c("ACT", "SAT"), each = 2), levels = c("ACT", "SAT")),
  score = c(36, 36, 1600, 1600),
  y = rep(c(tuition_is, tuition_oos), times = 2),
  label = rep(c("In-state tuition", "Out-of-state tuition"), times = 2)
)

merit_schedule_plot <- ggplot(
  merit_schedule_by_test_plot_data,
  aes(x = score, y = automatic_merit, color = group)
) +
  geom_hline(yintercept = tuition_is,
             color = "black",
             linetype = "solid") +
  geom_hline(yintercept = tuition_oos,
             color = "black",
             linetype = "solid") +
  geom_text(
    data = tuition_line_labels,
    aes(x = score, y = y + 600, label = label),
    inherit.aes = FALSE,
    hjust = 1,
    color = "black",
    size = 3.25
  ) +
  geom_line(linewidth = 1) +
  facet_wrap(~ test, scales = "free_x", nrow = 1) +
  scale_x_continuous(
    breaks = function(x) {
      if (max(x, na.rm = TRUE) <= 40) {
        seq(18, 36, by = 3)
      } else {
        seq(1000, 1600, by = 100)
      }
    }
  ) +
  scale_y_continuous(labels = scales::dollar) +
  scale_color_manual(
    values = c("In-state" = "#440154FF",
               "Out-of-state" = "#21908CFF"),
    name = NULL
  ) +
  labs(x = "Test score", y = "Automatic merit aid") +
  theme_classic() +
  theme(
    panel.grid.major.y = element_line(color = "gray80", linetype = "dashed"),
    legend.position = "bottom",
    strip.background = element_blank(),
    strip.text = element_text(face = "bold")
  )

print(merit_schedule_plot)

ggsave(
  paste0(pathFigures, "merit_aid_schedule.png"),
  merit_schedule_plot,
  width = 8,
  height = 5
)

net_tuition_schedule_plot <- ggplot(
  merit_schedule_plot_data,
  aes(x = act, y = net_tuition_fees, color = group)
) +
  geom_hline(yintercept = core_cost_per_fte,
             color = "black",
             linetype = "solid") +
  annotate(
    "text",
    x = 36,
    y = core_cost_per_fte + 800,
    label = "Average spending",
    hjust = 1,
    color = "black"
  ) +
  geom_line(linewidth = 1) +
  scale_x_continuous(breaks = seq(18, 36, by = 3)) +
  scale_y_continuous(labels = scales::dollar) +
  scale_color_manual(
    values = c("In-state" = "#440154FF",
               "Out-of-state" = "#21908CFF"),
    name = NULL
  ) +
  labs(x = "ACT score", y = "Net tuition and fees") +
  theme_classic() +
  theme(
    panel.grid.major.y = element_line(color = "gray80", linetype = "dashed"),
    legend.position = "bottom"
  )

print(net_tuition_schedule_plot)

ggsave(
  paste0(pathFigures, "net_tuition_schedule.png"),
  net_tuition_schedule_plot,
  width = 6.5,
  height = 5
)

# Full first-year institutional grant allocation for the expansive
# charge/cost analysis below. This keeps the room-and-board-inclusive
# analysis matched to full IPEDS SFA institutional grants, while the
# main Section 6 tuition analysis uses the tuition-only target.
full_grant_residual_inst_grants <- ipeds_fy_inst_grants - auto_total

if (full_grant_residual_inst_grants < 0) {
  warning("Full institutional grant target is below modeled automatic merit total. Scaling merit down proportionally.")
  
  full_grant_scale_factor <- ipeds_fy_inst_grants / auto_total
  full_grant_auto_is_adj  <- auto_is  * full_grant_scale_factor
  full_grant_auto_oos_adj <- auto_oos * full_grant_scale_factor
  full_grant_residual_inst_grants <- 0
} else {
  full_grant_auto_is_adj  <- auto_is
  full_grant_auto_oos_adj <- auto_oos
}

full_grant_alabama_advantage_total <-
  min(n_is * pell_share_is * avg_alabama_advantage_topup,
      full_grant_residual_inst_grants)

full_grant_other_residual_inst_grants <-
  full_grant_residual_inst_grants - full_grant_alabama_advantage_total

full_grant_resid_is <-
  full_grant_alabama_advantage_total +
  share_other_residual_to_is * full_grant_other_residual_inst_grants

full_grant_resid_oos <-
  (1 - share_other_residual_to_is) * full_grant_other_residual_inst_grants

full_grant_exact_match_results <- data.frame(
  group = c("is", "oos"),
  n_students = c(n_is, n_oos),
  total_inst_grants = c(
    full_grant_auto_is_adj + full_grant_resid_is,
    full_grant_auto_oos_adj + full_grant_resid_oos
  )
)

full_grant_exact_match_results$avg_total_inst_grants <-
  full_grant_exact_match_results$total_inst_grants /
  full_grant_exact_match_results$n_students

full_grant_residual_alloc_sens <- do.call(rbind, lapply(seq_len(nrow(residual_split_sensitivity)), function(i) {
  
  s_is <- residual_split_sensitivity$share_other_residual_to_is[i]
  
  resid_is_s <-
    full_grant_alabama_advantage_total +
    s_is * full_grant_other_residual_inst_grants
  
  resid_oos_s <-
    (1 - s_is) * full_grant_other_residual_inst_grants
  
  total_is_s  <- full_grant_auto_is_adj  + resid_is_s
  total_oos_s <- full_grant_auto_oos_adj + resid_oos_s
  
  data.frame(
    residual_split = residual_split_sensitivity$residual_split[i],
    is_per_student_weight =
      residual_split_sensitivity$is_per_student_weight[i],
    oos_per_student_weight =
      residual_split_sensitivity$oos_per_student_weight[i],
    share_other_residual_to_is = s_is,
    avg_total_is = total_is_s / n_is,
    avg_total_oos = total_oos_s / n_oos
  )
}))

############################################################
# 7. Supplemental expansive-cost analysis
#
# This block asks what in-state and out-of-state net payments
# look like if first-year student charges include expected
# university room and board, and if costs include auxiliary
# expenses from IPEDS Finance. The on-campus share is applied
# only to student payments; auxiliary cost per FTE already
# averages over students who do and do not use auxiliary services.
#
# Grant aid comes from the full exact-match IPEDS SFA target:
#   automatic merit by residency
#   + Alabama Advantage allocated to in-state students
#   + remaining residual institutional aid split 50/50
############################################################

# -----------------------------
# 7A. First-year charges
# -----------------------------

expected_food_housing_charge <- p_oncampus_fy * food_housing

expansive_charges <- data.frame(
  group = c("is", "oos"),
  tuition = c(tuition_is, tuition_oos),
  required_fees = required_fees,
  expected_food_housing_charge = expected_food_housing_charge
)

expansive_charges$total_expected_charges <-
  expansive_charges$tuition +
  expansive_charges$required_fees +
  expansive_charges$expected_food_housing_charge

# -----------------------------
# 7B. Main tuition/fee net payments and margins
# -----------------------------

build_tuition_margin_results <- function(grant_results, source) {
  grant_results %>%
    transmute(
      source = source,
      group,
      n_students,
      tuition = if_else(group == "is", tuition_is, tuition_oos),
      required_fees = required_fees,
      avg_tuition_grants = avg_total_inst_grants,
      gross_tuition_fees = tuition + required_fees,
      net_tuition_fees = gross_tuition_fees - avg_tuition_grants,
      margin_vs_core_cost = net_tuition_fees - core_cost_per_fte,
      margin_rate_vs_core_cost = margin_vs_core_cost / net_tuition_fees
    )
}

tuition_margin_results <- build_tuition_margin_results(
  exact_match_results,
  "IPEDS SFA tuition target"
)

print(tuition_margin_results)

faq_iqr_tuition_margin_results <- build_tuition_margin_results(
  faq_iqr_exact_match_results,
  "IPEDS SFA tuition target, FAQ-IQR scores"
)

score_sensitivity_margin_comparison <- bind_rows(
  tuition_margin_results %>%
    mutate(model = "CDS gap-preserving main"),
  faq_iqr_tuition_margin_results %>%
    mutate(model = "FAQ-IQR sensitivity")
) %>%
  left_join(
    bind_rows(
      baseline %>%
        transmute(model = "CDS gap-preserving main",
                  group,
                  avg_auto_merit = mean_aid_all),
      faq_iqr_sensitivity %>%
        transmute(model = "FAQ-IQR sensitivity",
                  group,
                  avg_auto_merit = mean_aid_all)
    ),
    by = c("model", "group")
  ) %>%
  select(
    model,
    group,
    avg_auto_merit,
    avg_total_tuition_grants = avg_tuition_grants,
    net_tuition_fees,
    margin_vs_core_cost
  )

print(score_sensitivity_margin_comparison)

# -----------------------------
# 7C. OOS cross-subsidy and loss-making high-achievement awards
# -----------------------------

is_tuition_margin <- tuition_margin_results %>%
  filter(group == "is")

oos_tuition_margin <- tuition_margin_results %>%
  filter(group == "oos")

is_subsidy_per_student <-
  core_cost_per_fte - is_tuition_margin$net_tuition_fees

oos_current_total_margin <-
  oos_tuition_margin$margin_vs_core_cost * oos_tuition_margin$n_students

avg_oos_margin <- oos_current_total_margin / oos_tuition_margin$n_students

avg_oos_residual_tuition_grants <- exact_match_results %>%
  filter(group == "oos") %>%
  pull(avg_residual_inst_grants)

# Treat the OOS ACT distribution as an ACT-equivalent distribution for
# score-submitters. Non-submitters are not assigned automatic merit aid, but
# they still receive the average residual/non-merit tuition grant.
oos_loss_by_act <- act_bin_weights("oos", act_grid) %>%
  mutate(
    n_students = n_oos * p_submit_oos * act_prob,
    gross_tuition_fees = tuition_oos + required_fees,
    automatic_merit = award_oos(act, gpa_plot, test = "ACT"),
    residual_tuition_grants = avg_oos_residual_tuition_grants,
    net_tuition_fees = pmax(
      gross_tuition_fees - automatic_merit - residual_tuition_grants,
      0
    ),
    margin_vs_core_cost = net_tuition_fees - core_cost_per_fte,
    loss_per_student = pmax(core_cost_per_fte - net_tuition_fees, 0),
    recoupable_loss = n_students * loss_per_student
  )

recoupable_oos_loss <- sum(oos_loss_by_act$recoupable_loss)

oos_non_submitter_n <- n_oos * (1 - p_submit_oos)
oos_non_submitter_net_tuition_fees <-
  tuition_oos + required_fees - avg_oos_residual_tuition_grants
oos_non_submitter_margin_vs_core_cost <-
  oos_non_submitter_net_tuition_fees - core_cost_per_fte
oos_non_submitter_gain <- oos_non_submitter_n *
  pmax(oos_non_submitter_margin_vs_core_cost, 0)

profitable_oos_gain <- sum(
  oos_loss_by_act$n_students *
    pmax(oos_loss_by_act$margin_vs_core_cost, 0)
) + oos_non_submitter_gain

unprofitable_oos_loss <- sum(
  oos_loss_by_act$n_students *
    pmax(-oos_loss_by_act$margin_vs_core_cost, 0)
)

profitable_oos_n <- sum(
  oos_loss_by_act$n_students[oos_loss_by_act$margin_vs_core_cost > 0]
) + if_else(oos_non_submitter_margin_vs_core_cost > 0,
            oos_non_submitter_n,
            0)

unprofitable_oos_n <- sum(
  oos_loss_by_act$n_students[oos_loss_by_act$margin_vs_core_cost < 0]
)

avg_profitable_oos_gain <- profitable_oos_gain / profitable_oos_n
avg_unprofitable_oos_loss <- unprofitable_oos_loss / unprofitable_oos_n

profitability_cutoff <- oos_loss_by_act %>%
  summarize(
    max_profitable_act = max(act[margin_vs_core_cost > 0]),
    min_unprofitable_act = min(act[margin_vs_core_cost < 0])
  )

is_total_loss <- is_subsidy_per_student * is_tuition_margin$n_students

cross_subsidy_summary <- data.frame(
  measure = c(
    "In-state subsidy per student",
    "Total losses on in-state students ($M)",
    "Total net gains on profitable OOS students ($M)",
    "  of which: OOS non-submitters ($M)",
    "Total net losses on unprofitable OOS students ($M)",
    "Average gain per profitable OOS student",
    "Average loss per unprofitable OOS student",
    "Average margin per OOS student",
    "In-state students funded by one profitable OOS student",
    "In-state students funded by one average OOS student",
    "High-score OOS students funded by one profitable OOS student",
    "Current total OOS margin ($M)",
    "Current in-state students financed by OOS margin",
    "Recoupable loss on unprofitable OOS students ($M)",
    "Additional in-state students financeable from OOS losses",
    "Potential in-state students financed from OOS losses"
  ),
  value = c(
    is_subsidy_per_student,
    is_total_loss / 1e6,
    profitable_oos_gain / 1e6,
    oos_non_submitter_gain / 1e6,
    unprofitable_oos_loss / 1e6,
    avg_profitable_oos_gain,
    avg_unprofitable_oos_loss,
    avg_oos_margin,
    avg_profitable_oos_gain / is_subsidy_per_student,
    avg_oos_margin / is_subsidy_per_student,
    avg_profitable_oos_gain / avg_unprofitable_oos_loss,
    oos_current_total_margin / 1e6,
    oos_current_total_margin / is_subsidy_per_student,
    recoupable_oos_loss / 1e6,
    recoupable_oos_loss / is_subsidy_per_student,
    (oos_current_total_margin + recoupable_oos_loss) /
      is_subsidy_per_student
  )
)

cross_subsidy_summary$value <- round(cross_subsidy_summary$value, 2)

print(oos_loss_by_act)
print(profitability_cutoff)
print(cross_subsidy_summary)

# -----------------------------
# 7D. Alternative CDS grant targets
# -----------------------------

allocate_grants_for_target <- function(target_total) {
  
  residual <- target_total - auto_total
  
  if (residual < 0) {
    scale_factor_target <- target_total / auto_total
    auto_is_target <- auto_is * scale_factor_target
    auto_oos_target <- auto_oos * scale_factor_target
    residual <- 0
  } else {
    auto_is_target <- auto_is
    auto_oos_target <- auto_oos
  }
  
  aa_target <- min(
    n_is * pell_share_is * avg_alabama_advantage_topup,
    residual
  )
  
  other_residual <- residual - aa_target
  
  total_is_target <-
    auto_is_target +
    aa_target +
    share_other_residual_to_is * other_residual
  
  total_oos_target <-
    auto_oos_target +
    (1 - share_other_residual_to_is) * other_residual
  
  data.frame(
    group = c("is", "oos"),
    n_students = c(n_is, n_oos),
    total_inst_grants = c(total_is_target, total_oos_target)
  ) %>%
    mutate(avg_total_inst_grants = total_inst_grants / n_students)
}

cds_tuition_margin_results <- bind_rows(
  build_tuition_margin_results(
    allocate_grants_for_target(cds_fy_inst_tuition_grants_gross),
    "CDS H2 gross tuition"
  )
)

print(cds_tuition_margin_results)

# -----------------------------
# 7E. Supplemental expansive-cost net payments and margins
# -----------------------------

expansive_residency_results <- expansive_charges %>%
  left_join(
    full_grant_exact_match_results %>%
      select(group, n_students, avg_total_inst_grants),
    by = "group"
  ) %>%
  mutate(
    avg_net_payment = total_expected_charges - avg_total_inst_grants,
    margin_vs_core_cost = avg_net_payment - core_cost_per_fte,
    margin_vs_auxiliary_inclusive_cost =
      avg_net_payment - auxiliary_inclusive_cost_per_fte,
    margin_rate_vs_core_cost = margin_vs_core_cost / avg_net_payment,
    margin_rate_vs_auxiliary_inclusive_cost =
      margin_vs_auxiliary_inclusive_cost / avg_net_payment
  )

print(expansive_residency_results)
