#////////////////////////////////////////////////////////////////////////////////
# Filename: analysis_placebo.R
# Author: Ryan Haygood
# Date: 3/2/25
# Description: Regresses various placebo outcomes on OOS shares and IVs.
#////////////////////////////////////////////////////////////////////////////////

# Setup
source('C:/Users/ryanh/OneDrive/Documents/Grad School/Research/Out-of-State-Enrollment/code/setup.R')

# Build data --------------------------------------------------------------------

# Bring in cleaned OOS shares from Commencement records
shares <- readRDS(paste0(pathHome, 'data/shares.rds')) %>%
  # Rename DC
  mutate(state = if_else(state == 'Washington, D.C.', 'District of Columbia', state))

# Get state dictionary
state_dict <- data.frame(STABBR = c(state.abb, 'DC'),
                         state = c(state.name, 'District of Columbia')) %>%
  arrange(state)

# Bring in OOS flagship selectivity IV

# Read in cleaned IPEDS panel
ipeds <- readRDS(paste0(pathHome, 'data/ipeds_panel.rds'))

# Get each state's flagship selectivity over time
flag <- ipeds %>%
  filter(flagship) %>%
  group_by(STABBR, y) %>%
  summarize(adm_rate = 100 * mean(adm_rate, na.rm = T)) %>%
  # Move year forward by 4 to match the graduation year (compare the push to the
  # pull 4 years later)
  mutate(y = y + 4)

# Attach state name
flag <- flag %>%
  left_join(state_dict) %>%
  ungroup() %>%
  select(-STABBR)

# State-level unemployment rates

# Read in unemployment rates by state from BLS, 2004-2023
# Derived from FRED: https://fredaccount.stlouisfed.org/datalists/250706
bls <- read_xls(paste0(pathHome, 'data/BLS/State_Unemployment_Rates.xls'), sheet = 2)

# At monthly level; get annual average
bls <- bls %>%
  pivot_longer(cols = -any_of('DATE')) %>%
  mutate(STABBR = substr(name, 1, 2),
         ur = value,
         y = as.numeric(substr(DATE, 1, 4))) %>%
  group_by(STABBR, y) %>%
  summarize(ur = mean(ur)) %>%
  filter(y %in% 2006:2023)

# Attach state name
bls <- bls %>%
  left_join(state_dict) %>%
  ungroup() %>%
  select(-STABBR)

# State-level migration

# Get numbers arriving and departing, as well as population
# Need other data for population before 2010

# Get state names
states <- sort(c(state.name, 'District of Columbia'))

# Prepare panel dataframe
mig_bal <- data.frame()

# From 2006 to 2023
years <- c(2006:2019, 2021:2023)

for (i in 1:length(years)) {
  
  # Read in table
  if (years[i] < 2022) {
    
    mig <- read_xls(paste0(pathHome, 'data/state_migration_flows/',
                           if_else(years[i] < 2014, 'state_to_state_migrations_table_', 'State_to_State_Migrations_Table_'),
                           years[i],
                           '.xls'),
                    skip = 5)
    
  } else {
    
    mig <- read_xlsx(paste0(pathHome, 'data/state_migration_flows/',
                            if_else(years[i] < 2014, 'state_to_state_migrations_table_', 'State_to_State_Migrations_Table_'),
                            years[i],
                            '.xlsx'),
                     skip = 5)
    
  }
  
  # Clean up
  mig <- mig %>%
    # Remove extraneous columns
    select(1, which(mig[1, ] %in% states)) %>%
    rename(state = 1) %>%
    # Remove extraneous rows
    filter(state %in% states) %>%
    select(-state) %>%
    mutate_all(as.numeric) %>%
    as.matrix()
  colnames(mig) <- NULL
  diag(mig) <- 0
  
  # Get number staying, leaving, and arriving for each state
  mig_bal <- data.frame(arrive = rowSums(mig),
                        depart = colSums(mig),
                        y = years[i],
                        state = states) %>%
    bind_rows(mig_bal)
  
}

# Get state populations over time
pop00 <- read.csv(paste0(pathHome, 'data/state_pop/st-est00int-alldata.csv')) %>%
  filter(NAME %in% states & SEX == 0 & ORIGIN == 0 & RACE == 0 & AGEGRP == 0) %>%
  select(NAME, starts_with('POPEST')) %>%
  pivot_longer(cols = -NAME) %>%
  rename(state = NAME,
         pop = value) %>%
  mutate(y = as.numeric(substr(name, nchar(name) - 3, nchar(name)))) %>%
  select(-name) %>%
  # Use next dataset for 2010
  filter(y != 2010)
pop10 <- read.csv(paste0(pathHome, 'data/state_pop/nst-est2020-alldata.csv')) %>%
  filter(NAME %in% states) %>%
  select(NAME, starts_with('POPEST')) %>%
  pivot_longer(cols = -NAME) %>%
  rename(state = NAME,
         pop = value) %>%
  mutate(y = as.numeric(substr(name, nchar(name) - 3, nchar(name)))) %>%
  select(-name) %>%
  # Use next dataset for 2020
  filter(y != 2020)
pop20 <- read.csv(paste0(pathHome, 'data/state_pop/NST-EST2024-ALLDATA.csv')) %>%
  filter(NAME %in% states) %>%
  select(NAME, starts_with('POPEST')) %>%
  pivot_longer(cols = -NAME) %>%
  rename(state = NAME,
         pop = value) %>%
  mutate(y = as.numeric(substr(name, nchar(name) - 3, nchar(name)))) %>%
  select(-name)

# Join together
pop <- pop00 %>%
  bind_rows(pop10) %>%
  bind_rows(pop20)

# Join onto migration data
mig <- pop %>%
  # We'll use lagged population as the migration rate denominator
  mutate(y = y + 1) %>%
  right_join(mig_bal)

# Get in-migration, out-migration, and net-migration rates
# Should we use lagged population?
mig <- mig %>%
  mutate(in_rate = arrive / pop * 100,
         out_rate = depart / pop * 100,
         net_rate = (arrive - depart) / pop * 100)

# Save migration and unemployment rate data
bls %>%
  left_join(mig) %>%
  saveRDS(paste0(pathHome, 'data/pull_factors.rds'))

# Join datasets
join <- bls %>%
  left_join(mig) %>%
  left_join(flag) %>%
  rename(grad_y = y) %>%
  left_join(shares) %>%
  # Impute zero shares
  mutate(o_share = if_else(is.na(o_share), 0, o_share),
         N_origin = if_else(is.na(N_origin), 0, N_origin)) %>%
  # Remove Alabama
  filter(state != 'Alabama')

# Save to Stata
write_dta(join, paste0(pathHome, 'data/placebo.dta'))

# Regressions -------------------------------------------------------------------

# Are origin shares orthogonal to pull factors 4 years later?
# Is flagship selectivity orthogonal to pull factors 4 years later?

# Unemployment rate

# Origin share itself
mod0 <- felm(ur ~ o_share | factor(state) | 0 | state, data = join)
mod1 <- felm(ur ~ o_share | factor(state) + factor(grad_y) | 0 | state, data = join)

# Selectivity
mod2 <- felm(ur ~ adm_rate | factor(state) | 0 | state, data = join)
mod3 <- felm(ur ~ adm_rate | factor(state) + factor(grad_y) | 0 | state, data = join)

# Net migration rate

# Origin share itself
mod4 <- felm(net_rate ~ o_share | factor(state) | 0 | state, data = join)
mod5 <- felm(net_rate ~ o_share | factor(state) + factor(grad_y) | 0 | state, data = join)

# Selectivity
mod6 <- felm(net_rate ~ adm_rate | factor(state) | 0 | state, data = join)
mod7 <- felm(net_rate ~ adm_rate | factor(state) + factor(grad_y) | 0 | state, data = join)

# Output tables
# First row for origin shares
stargazer(mod0, mod1, mod4, mod5)
# Second row for flagship selectivity
stargazer(mod2, mod3, mod6, mod7)

# Or just the TWFE models
stargazer(mod1, mod3, mod5, mod7)

# Origin shares being positively correlated with unemployment rates suggests our
# estimates would be even larger if we conditioned on labor market conditions
