#////////////////////////////////////////////////////////////////////////////////
# Filename: analysis_crowd_out.R
# Author: Ryan Haygood
# Date: 5/12/25
# Description: Evaluates potential crowd-in/-out effects of OOS enrollment, using
# budget identities and a synthetic control.
#////////////////////////////////////////////////////////////////////////////////

# Setup
source('C:/Users/ryanh/OneDrive/Documents/Grad School/Research/Out-of-State-Enrollment/code/setup.R')

# Budget analysis ---------------------------------------------------------------

# Bring in data on UA revenues and spending, as well as in- vs. out-of-state tuition
# and enrollment. Estimate budget shortfall supposing out-of-state enrollment had
# remained at its 2006 level (assuming either in-state students replaced out-of-state
# students, or that those students never came in the first place, in which case we have
# to adjust UA expenditures)

# In- and out-of-state tuition
years <- 2000:2019
ic <- data.frame()
for (i in 1:length(years)) {
  
  if (years[i] %in% 2009:2012) {
    
    ic <- read_xlsx(paste0(pathHome, 'data/ipeds_tuition/ic', years[i], '_ay.xlsx')) %>%
      rename_with(toupper) %>%
      select(UNITID, CHG2AY3, CHG3AY3, TUITION2, TUITION3) %>%
      mutate_all(~as.numeric(as.character(.))) %>%
      mutate(y = years[i]) %>%
      bind_rows(ic)
    
  } else {
    
    ic <- read.csv(paste0(pathHome, 'data/ipeds_tuition/ic', years[i], '_ay.csv')) %>%
      rename_with(toupper) %>%
      select(UNITID, CHG2AY3, CHG3AY3, TUITION2, TUITION3) %>%
      mutate_all(~as.numeric(as.character(.))) %>%
      mutate(y = years[i]) %>%
      bind_rows(ic)
    
  }
  
}

# Financial aid data
years <- 2001:2019
sfa <- data.frame()
for (i in 1:length(years)) {
  
  if (years[i] == 2009) {
    
    sfa <- read_xlsx(paste0(pathHome, 'data/ipeds_aid/sfa', substr(years[i], 3, 4), if_else(years[i] < 2009, '0', ''), as.numeric(substr(years[i], 3, 4)) + 1, '.xlsx')) %>%
      rename_with(toupper) %>%
      select(any_of(c('UNITID', 'SCFA12N', 'SCFA13N', 'IGRNT_N', 'IGRNT_A', 'GIS4N12', 'GIS4N22', 'GIS4N32', 'GIS4N42', 'GIS4N52', 'GIS4T12', 'GIS4T22', 'GIS4T32', 'GIS4T42', 'GIS4T52', 'NPIST2', 'NPIS412', 'NPIS422', 'NPIS432', 'NPIS442', 'NPIS452'))) %>%
      mutate(y = years[i]) %>%
      bind_rows(sfa)
    
  } else {
    
    sfa <- read.csv(paste0(pathHome, 'data/ipeds_aid/sfa', substr(years[i], 3, 4), if_else(years[i] < 2009, '0', ''), as.numeric(substr(years[i], 3, 4)) + 1, '.csv')) %>%
      rename_with(toupper) %>%
      select(any_of(c('UNITID', 'SCFA12N', 'SCFA13N', 'IGRNT_N', 'IGRNT_A', 'GIS4N12', 'GIS4N22', 'GIS4N32', 'GIS4N42', 'GIS4N52', 'GIS4T12', 'GIS4T22', 'GIS4T32', 'GIS4T42', 'GIS4T52', 'NPIST2', 'NPIS412', 'NPIS422', 'NPIS432', 'NPIS442', 'NPIS452'))) %>%
      mutate(y = years[i]) %>%
      bind_rows(sfa)
    
  }
  
}
# Does average aid received include zeroes? If not sure, should use total aid disbursements from finance dataset

# Filter to UA series
ua_sfa <- sfa %>%
  rename(in_N = SCFA12N,
         out_N = SCFA13N,
         grant_N = IGRNT_N,
         grant_A = IGRNT_A,
         net_price_in = NPIST2,
         net_price_in_q5 = NPIS412,
         net_price_in_q4 = NPIS422,
         net_price_in_q3 = NPIS432,
         net_price_in_q2 = NPIS442,
         net_price_in_q1 = NPIS452) %>%
  filter(UNITID == 100751) %>%
  # Get total institutional grant aid (assuming average does not include zeroes)
  mutate(#total_grants = (in_N + out_N) * grant_A,
         total_grants = grant_N * grant_A)

# We have in- vs. out-of-state tuition and fees, but only total institutional aid
# Can try backing out average institutional aid for in- and out-of-state students
# by projecting total aid onto the number of in- and out-of-state students

# Probability of grant receipt
lm(grant_N ~ in_N + out_N, data = ua_sfa) %>%
  summary(robust = T)
ggplot(aes(x = out_N, y = grant_N), data = ua_sfa) +
  geom_point()
# Average grant amount
lm(total_grants ~ in_N + out_N, data = ua_sfa) %>%
  summary(robust = T)
ggplot(aes(x = out_N, y = total_grants), data = ua_sfa) +
  geom_point()
# This has some curvature, probably because tuition is increasing over time so average
# aid award is increasing over time too. If we think the share of tuition covered might
# be more constant over time, could scale by total tuition revenues.

# Bring in finance data
# F1B01: tuition and fees, after deducting discounts and allowances
# Total revenues and total expenses (F1D01 vs. F1D02) -- roughly equal each year?
# Split tuition revenues out of total revenues. Do total revenues include full sticker-
# price tuition, so that institutional grants are included in expenses? Or do revenues
# just include net tuition?
years <- 2001:2019
fin <- data.frame()
for (i in 1:length(years)) {
  
  fin <- read.csv(paste0(pathHome, 'data/ipeds_finance/f', substr(years[i], 3, 4), if_else(years[i] < 2009, '0', ''), as.numeric(substr(years[i], 3, 4)) + 1, '_f1a', if_else(years[i] > 2002, '_rv', ''), '.csv')) %>%
    rename_with(toupper) %>%
    select(any_of(c('UNITID', 'F1B01', 'F1B09', 'F1C151', 'F1D01', 'F1D02', 'F1D03', 'F1E08', 'F1H01'))) %>%
    mutate(y = years[i]) %>%
    bind_rows(fin)
  
}

# Get UA finance data
ua_fin <- fin %>%
  filter(UNITID == 100751)

# Get single UA dataset
ua <- ic %>%
  filter(UNITID == 100751 & y > 2000) %>%
  left_join(ua_fin) %>%
  left_join(ua_sfa)

# To calculate alternative revenues need to use total number of students, not just first-years
# But we only have residence status of first-year cohorts
# Need to use retention rates to impute something
ua <- ua %>%
  arrange(y) %>%
  mutate(in_N_tot = in_N + lag(in_N) + lag(in_N, n = 2) + lag(in_N, n = 3),
         out_N_tot = out_N + lag(out_N) + lag(out_N, n = 2) + lag(out_N, n = 3)) %>%
  filter(y >= 2004)

# Suppose first that costs would have remained the same (assuming in-state students
# took the spots of OOS students and have same per-student cost), but that revenues
# would have been lower simply due to lower net tuition per student. Calculate deficit.

# An estimate of average institutional grants per recipient
ua$discount <- ua$F1E08 / (ua$grant_N * 4)
# Share of tuition and fees which is given away in discounts
ua$F1E08 / (ua$F1E08 + ua$F1B01)

# Estimate gap between OOS and in-state net tuition
# Assume only OOS students receive institutional grants
ua$oos_gap <- (ua$TUITION3 - ua$discount) - ua$TUITION2

# Foregone revenues from replacing OOS with in-state
ua$excess_revenue <- ua$oos_gap * ua$out_N_tot
# Total foregone revenues
ua$oos_gap %*% ua$out_N_tot

# How much higher would in-state tuition have had to be to keep revenues constant?
# Spread the total foregone revenues over every student (assuming new in-state replace out-of-state)
(ua$oos_gap %*% ua$out_N_tot) / (sum(ua$in_N_tot) + sum(ua$out_N_tot))

# Dormitory capacity ------------------------------------------------------------

# Check UA's dormitory capacity relative to enrollment over the period
# Maybe they have some slack, in which case we would worry less about mechanical
# crowding out
# In- and out-of-state tuition
years <- 2000:2019
ic <- data.frame()
for (i in 1:length(years)) {
  
  ic <- read.csv(paste0(pathHome, 'data/ipeds_ic/ic', years[i], if_else(years[i] >= 2008, '_rv', ''), '.csv')) %>%
    rename_with(toupper) %>%
    select(UNITID, ROOMCAP) %>%
    mutate_all(~as.numeric(as.character(.))) %>%
    mutate(y = years[i]) %>%
    bind_rows(ic)
  
}

ic %>%
  filter(UNITID == 100751)

# Synthetic control -------------------------------------------------------------

# Idea is to find another state university (or combination of universities) which
# had similar state funding losses but were prevented from raising their OOS enrollment.
# E.g., UNC has a 18% OOS enrollment cap, but NC has had large funding cuts.


