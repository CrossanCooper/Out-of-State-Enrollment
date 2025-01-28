#////////////////////////////////////////////////////////////////////////////////
# Filename: dscrb_ua_trends.R
# Author: Ryan Haygood
# Date: 11/11/24
# Description: Describes trends in tuition, financial aid, revenues, and expenses
# at UA between 2001-2019.
#////////////////////////////////////////////////////////////////////////////////

# Setup
source('C:/Users/ryanh/Dropbox/admissions_project/code/setup.R')

# Build data --------------------------------------------------------------------

years <- 2000:2019

# HD
hd <- data.frame()

for (i in 1:length(years)) {
  
  hd <- read.csv(paste0(pathHome, 'data/ipeds_ins/hd', years[i], '.csv')) %>%
    rename_all(toupper) %>%
    mutate(y = years[i],
           UNITID = as.numeric(UNITID)) %>%
    select(any_of(c('y', 'UNITID', 'INSTNM', 'CITY', 'STABBR', 'FIPS', 'COUNTYCD', 'F1SYSNAM', 'CONTROL', 'ICLEVEL', 'INSTCAT'))) %>%
    bind_rows(hd)
  
}

# Enrollments by state-of-origin
ef <- data.frame()

for (i in 1:length(years)) {
  
  ef <- read.csv(paste0(pathHome, 'data/ipeds_fe/ef', years[i], 'c.csv')) %>%
    rename_all(toupper) %>%
    mutate(y = years[i],
           UNITID = as.numeric(UNITID)) %>%
    select(any_of(c('y', 'UNITID', 'LINE', 'EFRES01'))) %>%
    bind_rows(ef)
  
}

# Save panel of Fall enrollment by state-of-origin
saveRDS(ef, paste0(pathHome, 'data/ef_by_state_panel.rds'))

years <- 2001:2019

# Tuition
ic <- data.frame()

for (i in 1:length(years)) {
  
  # Weird CSV reading issue for years 2009-2012 -- use a .xlsx version
  if (years[i] %in% 2009:2012) {
    
    ic <- read_xlsx(paste0(pathHome, 'data/ipeds_tuition/ic', years[i], '_ay.xlsx')) %>%
      left_join(read.csv(paste0(pathHome, 'data/ipeds_ic/ic', years[i], if_else(years[i] <= 2007, '.csv', '_rv.csv')))) %>%
      rename_all(toupper) %>%
      mutate(y = years[i],
             UNITID = as.numeric(UNITID)) %>%
      select(y, UNITID, TUITION1, FEE1, TUITION2, FEE2, TUITION3, FEE3, CHG1AY3, CHG2AY3, CHG3AY3, APPLCN, ADMSSN, SATVR25, SATVR75, SATMT25, SATMT75) %>%
      mutate_all(as.numeric) %>%
      bind_rows(ic)
    
  } else if (years[i] <= 2013) {
    
    ic <- read.csv(paste0(pathHome, 'data/ipeds_tuition/ic', years[i], '_ay.csv')) %>%
      left_join(read.csv(paste0(pathHome, 'data/ipeds_ic/ic', years[i], if_else(years[i] <= 2007, '.csv', '_rv.csv')))) %>%
      rename_all(toupper) %>%
      mutate(y = years[i],
             UNITID = as.numeric(UNITID)) %>%
      select(any_of(c('y', 'UNITID', 'TUITION1', 'FEE1', 'TUITION2', 'FEE2', 'TUITION3', 'FEE3', 'CHG1AY3', 'CHG2AY3', 'CHG3AY3', 'APPLCN', 'ADMSSN', 'APPLCNM', 'APPLCNW', 'ADMSSNM', 'ADMSSNW', 'SATVR25', 'SATVR75', 'SATMT25', 'SATMT75'))) %>%
      mutate_all(as.numeric) %>%
      bind_rows(ic)
    
  } else {
    
    ic <- read.csv(paste0(pathHome, 'data/ipeds_tuition/ic', years[i], '_ay.csv')) %>%
      left_join(read.csv(paste0(pathHome, 'data/ipeds_ic/adm', years[i], '_rv.csv'))) %>%
      rename_all(toupper) %>%
      mutate(y = years[i],
             UNITID = as.numeric(UNITID)) %>%
      select(any_of(c('y', 'UNITID', 'TUITION1', 'FEE1', 'TUITION2', 'FEE2', 'TUITION3', 'FEE3', 'CHG1AY3', 'CHG2AY3', 'CHG3AY3', 'APPLCN', 'ADMSSN', 'SATVR25', 'SATVR75', 'SATMT25', 'SATMT75'))) %>%
      mutate_all(as.numeric) %>%
      bind_rows(ic)
    
  }
  
}

# Get admissions rate
ic <- ic %>%
  mutate(ADMSSN = if_else(y == 2001, ADMSSNM + ADMSSNW, ADMSSN),
         APPLCN = if_else(y == 2001, APPLCNM + APPLCNW, APPLCN)) %>%
  mutate(adm_rate = ADMSSN / APPLCN)

# Aid
aid <- data.frame()

for (i in 1:length(years)) {
  
  # Weird CSV reading issue for year 2009 -- use a .xlsx version
  if (years[i] == 2009) {
    
    aid <- read_xlsx(paste0(pathHome, 'data/ipeds_aid/sfa', substr(years[i], 3, 4), substr(years[i] + 1, 3, 4), '.xlsx')) %>%
      rename_all(toupper) %>%
      mutate(y = years[i],
             UNITID = as.numeric(UNITID)) %>%
      select(any_of(c('y', 'UNITID', 'SCFA1N', 'SCFA1P', 'SCFA11P', 'SCFA12P', 'SCFA13P', 'SCFA14P',
                      # UAGRNTP, UAGRNTA, AIDFSIP, AGRNT_P, AGRNT_A, PGRNT_P, PGRNT_A
                      'ANYAIDP',
                      'FGRNT_P', 'FGRNT_A', 'SGRNT_P', 'SGRNT_A',
                      'IGRNT_P', 'IGRNT_A', 'LOAN_P', 'LOAN_A',
                      'GIS4N12', 'GIS4N22', 'GIS4N32', 'GIS4N42', 'GIS4N52',
                      'GIS4A12', 'GIS4A22', 'GIS4A32', 'GIS4A42', 'GIS4A52'))) %>%
      mutate_all(as.numeric) %>%
      bind_rows(aid)
    
  } else {
    
    aid <- read.csv(paste0(pathHome, 'data/ipeds_aid/sfa', substr(years[i], 3, 4), substr(years[i] + 1, 3, 4), '.csv')) %>%
      rename_all(toupper) %>%
      mutate(y = years[i],
             UNITID = as.numeric(UNITID)) %>%
      select(any_of(c('y', 'UNITID', 'SCFA1N', 'SCFA1P', 'SCFA11P', 'SCFA12P', 'SCFA13P', 'SCFA14P',
                      # UAGRNTP, UAGRNTA, AIDFSIP, AGRNT_P, AGRNT_A, PGRNT_P, PGRNT_A
                      'ANYAIDP',
                      'FGRNT_P', 'FGRNT_A', 'SGRNT_P', 'SGRNT_A',
                      'IGRNT_P', 'IGRNT_A', 'LOAN_P', 'LOAN_A',
                      'GIS4N12', 'GIS4N22', 'GIS4N32', 'GIS4N42', 'GIS4N52',
                      'GIS4A12', 'GIS4A22', 'GIS4A32', 'GIS4A42', 'GIS4A52'))) %>%
      mutate_all(as.numeric) %>%
      bind_rows(aid)
    
  }
  
}

# Financials
fin <- data.frame()

for (i in 1:length(years)) {
  
  fin <- read.csv(paste0(pathHome, 'data/ipeds_finance/f', substr(years[i], 3, 4), substr(years[i] + 1, 3, 4), if_else(years[i] <= 2002, '_f1a.csv', '_f1a_rv.csv'))) %>%
    rename_all(toupper) %>%
    mutate(y = years[i],
           UNITID = as.numeric(UNITID)) %>%
    select(any_of(c('y', 'UNITID', 'F1A18', 'F1B01', 'F1B02', 'F1B03', 'F1B04',
                    'F1B05', 'F1B06', 'F1B26', 'F1B07', 'F1B08',
                    'F1B09', 'F1B10', 'F1B11', 'F1B12', 'F1B13', 'F1B14', 'F1B15', 'F1B25',
                    'F1C011', 'F1C021', 'F1C031', 'F1C041', 'F1C051', 'F1C061', 'F1C071', 'F1C101', 'F1C111',
                    'F1D01', 'F1D02', 'F1E11', 'F1E07', 'F1H02'))) %>%
    mutate_all(as.numeric) %>%
    bind_rows(fin)
  
}

# Get state flagship list
flagships <- c(
  "University of Alabama", "University of Alaska Fairbanks", "University of Arizona", 
  "University of Arkansas Main Campus", "University of California-Berkeley", "University of Colorado at Boulder",
  "University of Connecticut", "University of Delaware", "University of Florida", 
  "University of Georgia", "University of Hawaii at Manoa", "University of Idaho", 
  "University of Illinois at Urbana-Champaign", "Indiana University-Bloomington", 
  "University of Iowa", "University of Kansas Main Campus", "University of Kentucky", 
  "Louisiana State Univ & Ag & Mech & Hebert Laws Ctr", 
  "University of Maine", "University of Maryland-College Park",
  "University of Massachusetts-Amherst", "University of Michigan-Ann Arbor", 
  "University of Minnesota-Twin Cities", "University of Mississippi Main Campus", "University of Missouri-Columbia",
  "The University of Montana-Missoula", "University of Nebraska at Lincoln", 
  "University of Nevada-Reno", "University of New Hampshire-Main Campus", "Rutgers University-New Brunswick",
  "University of New Mexico-Main Campus", "New Mexico State University-Main Campus",
  "SUNY at Buffalo", "University of North Carolina at Chapel Hill",
  "University of North Dakota-Main Campus", "Ohio State University-Main Campus", 
  "University of Oklahoma Norman Campus", 
  "University of Oregon", "Pennsylvania State University-Main Campus", 
  "Purdue University-Main Campus", "University of Rhode Island", 
  "University of South Carolina at Columbia", "University of South Dakota", "The University of Tennessee", 
  "The University of Texas at Austin", "Texas A & M University", "University of Utah", 
  "University of Vermont and State Agricultural Coll", 
  "University of Virginia-Main Campus", "University of Washington-Seattle Campus", "West Virginia University", 
  "University of Wisconsin-Madison", "University of Wyoming"
)
flagship_ids <- c(
  100751, 102614, 104179, 106397, 110635, 126614, 129020, 130943, 134130, 139959, 141574, 142285, 145637, 151351, 153658, 155317,
  157085, 159391, 161253, 163286, 166629, 170976, 174066, 176017, 178396, 180489, 181464, 182290, 183044, 186380, 187985, 188030,
  196088, 199120, 200280, 204796, 207500, 209551, 214777, 217484, 218663, 219471, 221759, 228723, 228778, 230764, 231174, 234076,
  236948, 238032, 240444, 240727, 243780
)
flagships <- toupper(flagships)

# Get total Fall enrollment
ef_tot <- ef %>%
  # Total across all origin states
  filter(LINE == 99) %>%
  mutate(enroll = EFRES01) %>%
  select(y, UNITID, enroll)

# Get panel of all colleges and their characteristics
all <- hd %>%
  # Remove administrative units
  filter(CONTROL != -3) %>%
  left_join(ic) %>%
  left_join(aid) %>%
  left_join(fin) %>%
  left_join(ef_tot) %>%
  # Identify flagships
  mutate(flagship = UNITID %in% flagship_ids)

# Save IPEDS panel
saveRDS(all, paste0(pathHome, 'data/ipeds_panel.rds'))

# Get U of A separately
ua <- all %>%
  filter(UNITID == 100751) %>%
  filter(y >= 2001)

# In 2012, 10% of students have "unknown" residence -- treat as OOS
ua <- ua %>%
  mutate(SCFA13P = SCFA13P + SCFA14P)

# Before 2008, F1B26 (sales of education activities) was not reported -- impute zeroes
ua <- ua %>%
  mutate(F1B26 = if_else(is.na(F1B26), 0, F1B26))

# Describe UA trends ------------------------------------------------------------

# Get tuition rates over time, in- vs. out-of-state
ua %>%
  select(y, UNITID, CHG2AY3, CHG3AY3) %>%
  pivot_longer(cols = c('CHG2AY3', 'CHG3AY3')) %>%
  mutate(name = if_else(name == 'CHG2AY3', 'In-state', 'Out-of-state')) %>%
  ggplot(aes(x = y, y = value, col = name)) +
  geom_line(size = 1) +
  scale_color_manual(values = c('steelblue1', 'indianred1')) +
  ylim(0, NA) +
  labs(x = 'Year',
       y = 'Tuition & fees',
       col = NULL,
       title = 'UA tuition rates') +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = 'gray80', linetype = 'dashed'),
        legend.position = 'right',
        plot.title = element_text(hjust = 0.5))
ggsave(paste0(pathFigures, 'ipeds/ua_trends/tuition_ins_oos.png'), width = 7, height = 5)

# Get out-of-state share over time
ggplot(ua, aes(x = y, y = SCFA13P)) +
  geom_line(size = 1, col = 'indianred1') +
  labs(x = 'Year',
       col = NULL,
       title = 'UA out-of-state share (%)') +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = 'gray80', linetype = 'dashed'),
        legend.position = 'right',
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5))
ggsave(paste0(pathFigures, 'ipeds/ua_trends/oos_share.png'), width = 5, height = 4)

# Financial aid statistics

# Percent receiving different forms of aid
ua %>%
  select(y, UNITID, ANYAIDP, FGRNT_P, SGRNT_P, IGRNT_P, LOAN_P) %>%
  pivot_longer(cols = c('ANYAIDP', 'FGRNT_P', 'SGRNT_P', 'IGRNT_P', 'LOAN_P')) %>%
  mutate(name = case_when(name == 'ANYAIDP' ~ 'Any',
                          name == 'FGRNT_P' ~ 'Federal',
                          name == 'SGRNT_P' ~ 'State',
                          name == 'IGRNT_P' ~ 'Institution',
                          name == 'LOAN_P' ~ 'Loans')) %>%
  # Weird jumps in 2019
  filter(y <= 2018) %>%
  ggplot(aes(x = y, y = value, col = name)) +
  geom_line(size = 1) +
  scale_color_manual(values = c('grey30', 'hotpink', 'purple', 'steelblue1', 'pink')) +
  labs(x = 'Year',
       col = NULL,
       title = 'Percent receiving aid at UA') +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = 'gray80', linetype = 'dashed'),
        legend.position = 'right',
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5))
ggsave(paste0(pathFigures, 'ipeds/ua_trends/pct_aid_by_type.png'), width = 7, height = 5)

# Average amounts received
ua %>%
  select(y, UNITID, FGRNT_A, IGRNT_A, LOAN_A) %>%
  pivot_longer(cols = c('FGRNT_A', 'IGRNT_A', 'LOAN_A')) %>%
  mutate(name = case_when(name == 'FGRNT_A' ~ 'Federal',
                          name == 'IGRNT_A' ~ 'Institution',
                          name == 'LOAN_A' ~ 'Loans')) %>%
  ggplot(aes(x = y, y = value, col = name)) +
  geom_line(size = 1) +
  ylim(0, NA) +
  scale_color_manual(values = c('hotpink', 'purple', 'steelblue1')) +
  labs(x = 'Year',
       col = NULL,
       title = 'Average aid amount received at UA') +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = 'gray80', linetype = 'dashed'),
        legend.position = 'right',
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5))
ggsave(paste0(pathFigures, 'ipeds/ua_trends/avg_aid_by_type.png'), width = 7, height = 5)

# Institution financial characteristics

# Net assets
ggplot(ua, aes(x = y, y = F1A18)) +
  geom_line() +
  ylim(0, NA)

# Operating revenue sources
# Group together the negligible categores of F1B06 and F1B07 (actually zero), F1B26, F1B04, maybe F1B03
ua %>%
  mutate(other = F1B06 + F1B26 + F1B04 + F1B03 + F1B08) %>%
  select(y, UNITID, F1B01, F1B02, F1B05, F1B09, other) %>%
  pivot_longer(cols = c('F1B01', 'F1B02', 'F1B05', 'F1B09', 'other')) %>%
  mutate(name = case_when(name == 'F1B01' ~ 'Tuition',
                          name == 'F1B02' ~ 'Federal operating grants',
                          name == 'F1B05' ~ 'Auxiliary enterprises (athletics)',
                          name == 'F1B09' ~ 'Total',
                          name == 'other' ~ 'Other')) %>%
  ggplot(aes(x = y, y = value / 100000000, col = name)) +
  geom_line(size = 1) +
  ylim(0, NA) +
  scale_color_manual(values = c('indianred1', 'pink', 'steelblue3', 'grey30', 'steelblue1')) +
  labs(x = 'Year',
       y = '$100Ms',
       col = NULL,
       title = 'Operating revenue sources at UA') +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = 'gray80', linetype = 'dashed'),
        legend.position = 'right',
        plot.title = element_text(hjust = 0.5))
ggsave(paste0(pathFigures, 'ipeds/ua_trends/op_rev_by_source.png'), width = 8, height = 5)

# Tuition share of operating revenues, and of all revenues
ua %>%
  mutate(tuition_share = F1B01 / F1B09,
         tuition_share_all = F1B01 / F1B25) %>%
  pivot_longer(cols = c('tuition_share', 'tuition_share_all')) %>%
  mutate(name = if_else(name == 'tuition_share', 'Share of operating revenues', 'Share of all revenues')) %>%
  ggplot(aes(x = y, y = value, lty = name)) +
  geom_line(size = 1) +
  scale_linetype_manual(values = c(2, 1)) +
  labs(x = 'Year',
       lty = NULL,
       title = 'Tuition share of revenues at UA') +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = 'gray80', linetype = 'dashed'),
        legend.position = 'right',
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5))
ggsave(paste0(pathFigures, 'ipeds/ua_trends/tuition_share.png'), width = 7, height = 5)

# Tuition and athletics and other revenue shares
# Out of operating revenues alone
ua %>%
  mutate(tuition_share = F1B01 / F1B09,
         athletics_share = F1B05 / F1B09,
         other_share = 1 - ((F1B01 + F1B05) / F1B09)) %>%
  pivot_longer(cols = c('tuition_share', 'athletics_share', 'other_share')) %>%
  mutate(name = case_when(name == 'tuition_share' ~ 'Tuition',
                          name == 'athletics_share' ~ 'Auxiliary enterprises (athletics)',
                          name == 'other_share' ~ 'Other')) %>%
  ggplot(aes(x = y, y = value, col = name)) +
  geom_line(size = 1) +
  scale_color_manual(values = c('indianred1', 'grey30', 'steelblue1')) +
  labs(x = 'Year',
       col = NULL,
       title = 'UA revenue sources (shares of operating revenues)') +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = 'gray80', linetype = 'dashed'),
        legend.position = 'right',
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5))
ggsave(paste0(pathFigures, 'ipeds/ua_trends/op_rev_share_by_source.png'), width = 8, height = 5)

# Tuition and athletics and other revenue shares
# Out of all revenues
ua %>%
  mutate(tuition_share = F1B01 / F1B25,
         athletics_share = F1B05 / F1B25,
         other_share = 1 - ((F1B01 + F1B05) / F1B25)) %>%
  pivot_longer(cols = c('tuition_share', 'athletics_share', 'other_share')) %>%
  mutate(name = case_when(name == 'tuition_share' ~ 'Tuition',
                          name == 'athletics_share' ~ 'Auxiliary enterprises (athletics)',
                          name == 'other_share' ~ 'Other')) %>%
  ggplot(aes(x = y, y = value, col = name)) +
  geom_line(size = 1) +
  scale_color_manual(values = c('indianred1', 'grey30', 'steelblue1')) +
  labs(x = 'Year',
       col = NULL,
       title = 'UA revenue sources (shares of all revenues)') +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = 'gray80', linetype = 'dashed'),
        legend.position = 'right',
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5))
ggsave(paste0(pathFigures, 'ipeds/ua_trends/tot_rev_share_by_source.png'), width = 8, height = 5)

# Including nonoperating grants too (all appropriations)
ua %>%
  select(y, UNITID, F1B10, F1B11, F1B12, F1B13, F1B14, F1B15) %>%
  pivot_longer(cols = c('F1B10', 'F1B11', 'F1B12', 'F1B13', 'F1B14', 'F1B15')) %>%
  ggplot(aes(x = y, y = value, col = name)) +
  geom_line() +
  ylim(0, NA)

# Non-negligible sources of revenue
ua %>%
  select(y, UNITID, F1B01, F1B02, F1B03, F1B11, F1B13, F1B25) %>%
  pivot_longer(cols = c('F1B01', 'F1B02', 'F1B03', 'F1B11', 'F1B13', 'F1B25')) %>%
  ggplot(aes(x = y, y = value, col = name)) +
  geom_line() +
  ylim(0, NA)

# Operating revenues, tuition, and state appropriations
ua %>%
  select(y, UNITID, F1B01, F1B11, F1B25) %>%
  pivot_longer(cols = c('F1B01', 'F1B11', 'F1B25')) %>%
  mutate(name = case_when(name == 'F1B01' ~ 'Tuition',
                          name == 'F1B11' ~ 'State appropriations',
                          name == 'F1B25' ~ 'Total revenues')) %>%
  ggplot(aes(x = y, y = value / 100000000, col = name)) +
  geom_line(size = 1) +
  scale_color_manual(values = c('indianred1', 'grey30', 'steelblue1')) +
  labs(x = 'Year',
       y = '$100Ms',
       col = NULL,
       title = 'UA revenue sources') +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = 'gray80', linetype = 'dashed'),
        legend.position = 'right',
        plot.title = element_text(hjust = 0.5))
ggsave(paste0(pathFigures, 'ipeds/ua_trends/approp_vs_tuition.png'), width = 8, height = 5)

# Spending sources
# Omit categories F1C101 for net grants/fellowships and F1C031 for public service
ua %>%
  # Group academic support and student services
  mutate(support = F1C051 + F1C061) %>%
  select(y, UNITID, F1C011, F1C021, support, F1C071, F1C111) %>%
  pivot_longer(cols = c('F1C011', 'F1C021', 'support', 'F1C071', 'F1C111')) %>%
  mutate(name = case_when(name == 'F1C011' ~ 'Instructional',
                          name == 'F1C021' ~ 'Research',
                          name == 'support' ~ 'Student services',
                          name == 'F1C071' ~ 'Administrative',
                          name == 'F1C111' ~ 'Auxiliary enterprises (athletics)')) %>%
  ggplot(aes(x = y, y = value / 100000000, col = name)) +
  geom_line(size = 1) +
  scale_color_manual(values = c('grey30', 'indianred1', 'steelblue1', 'pink', 'purple')) +
  ylim(0, NA) +
  labs(x = 'Year',
       y = '$100Ms',
       col = NULL,
       title = 'UA spending') +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = 'gray80', linetype = 'dashed'),
        legend.position = 'right',
        plot.title = element_text(hjust = 0.5))
ggsave(paste0(pathFigures, 'ipeds/ua_trends/spend_by_source.png'), width = 8, height = 5)

# Shares of spending
ua %>%
  mutate(instructional_share = F1C011 / F1D02,
         research_share = F1C021 / F1D02,
         athletics_share = F1C111 / F1D02,
         admin_share = F1C071 / F1D02,
         # Group academic support and student services
         support_share = (F1C051 + F1C061) / F1D02) %>%
  pivot_longer(cols = c('instructional_share', 'research_share', 'athletics_share', 'admin_share', 'support_share')) %>%
  mutate(name = case_when(name == 'instructional_share' ~ 'Instructional',
                          name == 'research_share' ~ 'Research',
                          name == 'support_share' ~ 'Student services',
                          name == 'admin_share' ~ 'Administrative',
                          name == 'athletics_share' ~ 'Auxiliary enterprises (athletics)')) %>%
  ggplot(aes(x = y, y = value, col = name)) +
  geom_line(size = 1) +
  scale_color_manual(values = c('grey30', 'indianred1', 'steelblue1', 'pink', 'purple')) +
  ylim(0, NA) +
  labs(x = 'Year',
       col = NULL,
       title = 'UA spending (shares of all expenses)') +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = 'gray80', linetype = 'dashed'),
        legend.position = 'right',
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5))
ggsave(paste0(pathFigures, 'ipeds/ua_trends/spend_share_by_source.png'), width = 8, height = 5)

# Spending vs. revenues
ua %>%
  select(y, UNITID, F1D01, F1D02) %>%
  pivot_longer(cols = c('F1D01', 'F1D02')) %>%
  mutate(name = if_else(name == 'F1D01', 'Total revenues', 'Total expenses')) %>%
  ggplot(aes(x = y, y = value / 100000000, lty = name)) +
  geom_line(size = 1) +
  ylim(0, NA) +
  labs(x = 'Year',
       y = '$100Ms',
       lty = NULL,
       title = 'UA budget') +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = 'gray80', linetype = 'dashed'),
        legend.position = 'right',
        plot.title = element_text(hjust = 0.5))
ggsave(paste0(pathFigures, 'ipeds/ua_trends/rev_vs_spend.png'), width = 7, height = 5)

# Evolution of admissions rate vs. SAT scores
ua %>%
  filter(y >= 2004) %>%
  mutate(`SAT math Q25` = SATMT25 / 800,
         `SAT math Q75` = SATMT75 / 800,
         `Admission rate` = adm_rate) %>%
  select(y, `Admission rate`, `SAT math Q25`, `SAT math Q75`) %>%
  pivot_longer(cols = c('Admission rate', 'SAT math Q25', 'SAT math Q75')) %>%
  ggplot(aes(x = y, y = value, col = name)) +
  geom_line(size = 1) +
  scale_color_manual(values = c('indianred1', 'steelblue1', 'steelblue4')) +
  labs(x = 'Year',
       col = NULL,
       title = 'UA admissions') +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = 'gray80', linetype = 'dashed'),
        legend.position = 'right',
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5))
ggsave(paste0(pathFigures, 'ipeds/ua_trends/admissions.png'), width = 7, height = 5)

# Compare to flagships ----------------------------------------------------------

# Define flagship sample
flag <- all %>%
  filter(flagship) %>%
  mutate(group = if_else(STABBR == 'AL', 'UA', 'Other')) %>%
  # Get tuition share of revenues
  mutate(tuition_share = F1B01 / F1B25,
         appropriations_share = F1B11 / F1B25,
         appropriations_per_capita = F1B11 / enroll,
         oos_share = SCFA13P / 100)

# Compare characteristics in latest cross-section
flag %>%
  filter(y %in% c(2001, 2019)) %>%
  group_by(group, y) %>%
  summarize(Year = mean(y),
            `Admission rate` = mean(adm_rate, na.rm = T),
            `SAT math Q25` = mean(SATMT25, na.rm = T),
            `SAT math Q75` = mean(SATMT75, na.rm = T),
            `SAT verbal Q25` = mean(SATVR25, na.rm = T),
            `SAT verbal Q75` = mean(SATVR75, na.rm = T),
            `OOS share` = mean(SCFA13P, na.rm = T),
            `In-state tuition` = mean(CHG2AY3, na.rm = T),
            `OOS tuition` = mean(CHG3AY3, na.rm = T),
            `OOS premium` = mean(CHG3AY3 / CHG2AY3, na.rm = T),
            `% school grant` = mean(IGRNT_P, na.rm = T),
            `Avg school grant` = mean(IGRNT_A, na.rm = T),
            `% federal grant` = mean(FGRNT_P, na.rm = T),
            `Avg federal grant` = mean(FGRNT_A, na.rm = T),
            `Tuition rev share` = mean(F1B01 / F1B25, na.rm = T),
            `Athletics rev share` = mean(F1B05 / F1B25, na.rm = T),
            `Appropriations rev share` = mean(F1B11 / F1B25, na.rm = T),
            `Instruction spend share` = mean(F1C011 / F1D02, na.rm = T),
            `Research spend share` = mean(F1C021 / F1D02, na.rm = T),
            `Athletics spend share` = mean(F1C111 / F1D02, na.rm = T),
            `Admin spend share` = mean(F1C071 / F1D02, na.rm = T),
            `Support spend share` = mean((F1C051 + F1C061) / F1D02, na.rm = T)) %>%
  arrange(y, group) %>%
  select(-y) %>%
  t() %>%
  data.frame() %>%
  mutate(X1 = round(as.numeric(X1), 2),
         X2 = round(as.numeric(X2), 2),
         X3 = round(as.numeric(X3), 2),
         X4 = round(as.numeric(X4), 2)) %>%
  rename('Other flagships' = X1,
         'U of A' = X2,
         ' Other flagships ' = X3,
         ' U of A ' = X4) %>%
  filter(row_number() != 1) %>%
  stargazer(summary = F, type = 'text', title = 'Mean college characteristics', digits = 2, out = paste0(pathHome, 'tables/ua_vs_flag_char.txt'))

# Or could plot distributions of these things and represent UA as a vertical line
# Or could make a scatterplot of two variables (like ins/oos_tuition, and note where
# UA falls)

# E.g., plot OOS share vs. tuition share of revenues
ggplot(filter(flag, y == 2019), aes(x = SCFA13P, y = adm_rate, col = group)) +
  geom_point()

# Does tuition share tend to comove with OOS share?
felm(tuition_share ~ oos_share | factor(UNITID) + factor(y), data = flag) %>%
  summary(robust = T)

# Does student academic quality tend to comove with OOS share?
# I.e., colleges relax academic entrance requirements to draw in OOS students
felm(SATMT25 ~ oos_share | factor(UNITID) + factor(y), data = flag) %>%
  summary(robust = T)
felm(SATVR25 ~ oos_share | factor(UNITID) + factor(y), data = flag) %>%
  summary(robust = T)

# Does institutional grant aid tend to comove with OOS share?
felm(IGRNT_P ~ oos_share | factor(UNITID) + factor(y), data = flag) %>%
  summary(robust = T)
felm(IGRNT_A ~ oos_share | factor(UNITID) + factor(y), data = flag) %>%
  summary(robust = T)

# Does admissions rate rise with OOS share?
felm(adm_rate ~ oos_share | factor(UNITID) + factor(y), data = flag) %>%
  summary(robust = T)

# Do state appropriations tend to predict OOS share?
felm(oos_share ~ lag(appropriations_per_capita) | factor(UNITID) + factor(y), data = flag %>% group_by(STABBR) %>% arrange(STABBR, y)) %>%
  summary(robust = T)

# Trends in overall Alabama state colleges --------------------------------------

# Spending per student at Alabama flagship vs. CCs (supposedly much higher at CCs)


# Alabama college composition by control/level
all %>%
  filter(STABBR == 'AL') %>%
  group_by(CONTROL, ICLEVEL, y) %>%
  summarize(N = n()) %>%
  ggplot(aes(x = y, y = N, col = factor(CONTROL), lty = factor(ICLEVEL))) +
  geom_line()

# Other Alabama publics (CCs)
cc_id <- hd %>%
  filter(STABBR == 'AL' & CONTROL == 1 & ICLEVEL %in% 2:3) %>%
  pull(UNITID) %>%
  unique()
