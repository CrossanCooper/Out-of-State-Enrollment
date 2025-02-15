#////////////////////////////////////////////////////////////////////////////////
# Filename: analysis_msa.R
# Author: Ryan Haygood
# Date: 2/11/25
# Description: Constructs crosswalk from towns to Revelio MSAs and replicates
# main OOS pull effect estimates at the MSA level.
#////////////////////////////////////////////////////////////////////////////////

# Setup
source('C:/Users/ryanh/OneDrive/Documents/Grad School/Research/Out-of-State-Enrollment/code/setup.R')

# Metro areas -------------------------------------------------------------------

# Make state abbreviation/name crosswalk
states <- data.frame(originState = state.abb,
                     state = state.name) %>%
  bind_rows(data.frame(originState = 'DC', state = 'Washington, D.C.'))

# Read in cleaned Revelio data (first post-college job)
rv <- read.csv(paste0(pathHome, 'revelio_data/job_spells_data_ua.csv'))

# Revelio has a list of nonmetropolitan areas, just one for each state
unique(rv$metro_area[str_detect(rv$metro_area, 'nonmetro') & rv$country == 'United States'])
# As well as metro areas
unique(rv$metro_area[!str_detect(rv$metro_area, 'nonmetro') & rv$country == 'United States'])
# Note some of these are listed as spanning two to three adjacent states

# Metro areas are all just a city name followed by "metropolitan area" for the most part
# So let's try to find the city name in the official MSA names listed in the crosswalk
# Basically, we'll construct a crosswalk from Revelio pseudo-MSAs to real MSAs

# Get MSA crosswalk
# This also has the constituent counties of each metro area, which we can use to
# map Commencement towns -> counties -> metros
msa <- read_xlsx(paste0(pathHome, 'data/area_definitions_m2023.xlsx'))

# Get real MSA name
msa <- msa %>%
  mutate(name = tolower(`May 2022 MSA name`))

# Fix MSA state name for DC
msa <- msa %>%
  mutate(state = if_else(State == 'District of Columbia', 'Washington, D.C.', State))

# Get list of Revelio metro areas
rv_metros <- rv %>%
  # Extract just the city name, removing the "metropolitan area" suffix (will use this to match)
  mutate(city = sub("(.*?)(\\s?)(metro.*)", "\\1", metro_area)) %>%
  # Restrict to US metro areas
  filter(country == 'United States' & !str_detect(metro_area, 'nonmetro') & metro_area != '') %>%
  select(state, metro_area, city) %>%
  distinct()

# Need to do this within each state ideally
for (i in 1:nrow(msa)) {
  
  msa_city <- rv_metros$city[rv_metros$state == msa$state[i]]
  
  msa$city[i] <- msa_city[which(str_detect(msa$name[i], msa_city))][1]
  
}

# Remove "Savannah" metro city name from the upper/lower Savannah nonmetro area
msa <- msa %>%
  mutate(city = if_else(str_detect(name, 'upper savannah') | str_detect(name, 'lower savannah'), NA, city))

# Get map of Revelio metro area names to CBSA metros
msa_map <- msa %>%
  filter(!is.na(city)) %>%
  select(city, state, name) %>%
  distinct() %>%
  right_join(rv_metros)

# Manually match the remaining cities that didn't
# Most cases are because the reported state in Revelio is adjacent to the state of the metro area...
msa_map <- msa_map %>%
  mutate(name = case_when(city == 'reno' ~ 'reno, nv',
                          city == 'new york city' ~ 'new york-newark-jersey city, ny-nj-pa',
                          city == 'dallas' ~ 'dallas-fort worth-arlington, tx',
                          city == 'anaheim' ~ 'los angeles-long beach-anaheim, ca',
                          city == 'fort lauderdale' ~ 'miami-fort lauderdale-west palm beach, fl',
                          city == 'albany' ~ 'albany-schenectady-troy, ny',
                          city == 'st louis' ~ 'st. louis, mo-il',
                          city == 'long beach' ~ 'los angeles-long beach-anaheim, ca',
                          city == 'pittsburgh' ~ 'pittsburgh, pa',
                          city == 'miami' ~ 'miami-fort lauderdale-west palm beach, fl',
                          city == 'savannah' ~ 'savannah, ga',
                          city == 'dayton' ~ 'dayton, oh',
                          city == 'jacksonville' ~ 'jacksonville, fl',
                          city == 'mobile' ~ 'mobile, al',
                          city == 'baltimore' ~ 'baltimore-columbia-towson, md',
                          city == 'milwaukee' ~ 'milwaukee-waukesha-west allis, wi',
                          city == 'chattanooga' ~ 'chattanooga, tn-ga',
                          city == 'fort collins' ~ 'fort collins, co',
                          city == 'el paso' ~ 'el paso, tx',
                          city == 'huntsville' ~ 'huntsville, al',
                          city == 'providence' ~ 'providence-warwick, ri-ma',
                          city == 'hartford' ~ 'hartford-west hartford-east hartford, ct',
                          city == 'spokane' ~ 'spokane-spokane valley, wa',
                          city == 'tallahassee' ~ 'tallahassee, fl',
                          city == 'toledo' ~ 'toledo, oh',
                          city == 'greensboro' ~ 'greensboro-high point, nc',
                          city == 'nashville' ~ 'nashville-davidson--murfreesboro--franklin, tn',
                          city == 'fort wayne' ~ 'fort wayne, in',
                          .default = name)) %>%
  left_join(distinct(select(msa, c('name', 'May 2022 MSA name'))))

# Now this MSA map relates all the metro areas listed in Revelio to a unique CBSA metro area
# Some of them link to the same CBSA metro area (where CBSAs have multiple constituent cities listed)

# It remains to match Revelio's non-metro areas to groups of CBSA non-metro areas
# Let's assume each state's Revelio non-metro area should map to all of the real
# non-metro areas in that state

# Get list of Revelio non-metro areas (these are all actually just state names)
rv_nonmetros <- rv %>%
  # Restrict to US metro areas
  filter(country == 'United States' & str_detect(metro_area, 'nonmetro')) %>%
  # Extract just the state name, removing the "nonmetropolitan area" suffix (will use this to match)
  mutate(loc = sub("(.*?)(\\s?)(nonmetro.*)", "\\1", metro_area)) %>%
  select(state, metro_area, loc) %>%
  distinct()

# Map the real non-metro areas to each state's non-metro area in Revelio
msa_map_nonmetro <- msa %>%
  filter(str_detect(name, 'nonmetro')) %>%
  select(state, name, `May 2022 MSA name`) %>%
  distinct() %>%
  right_join(rv_nonmetros) %>%
  select(-loc)

# Although Revelio lists non-metro areas in New Jersey, Delaware, and Rhode Island,
# these don't actually exist in the real MSA data

# Combine into a single crosswalk
msa_map <- msa_map %>%
  select(-city) %>%
  mutate(metro = T) %>%
  bind_rows(msa_map_nonmetro) %>%
  mutate(metro = !is.na(metro)) %>%
  rename(rl_metro_area = metro_area,
         census_metro_area = `May 2022 MSA name`)

# Fix MSA non-metro names too
msa_map_nonmetro <- msa_map_nonmetro %>%
  rename(rl_metro_area = metro_area,
         census_metro_area = `May 2022 MSA name`)

# Now we can attach the real MSAs to Revelio destination data
# Among people in metros, each metro_area/state combination gives a single crosswalk row
# But really we won't want to distinguish states with the same metro area
# For non-metro people, we'll need to aggregate the real non-metro areas together within states

# Get version of MSA map which collapses non-metro areas into a single one for each state
# Can recover this later using msa_map_nonmetro to identify average wages
msa_map_grouped <- msa_map %>%
  mutate(census_metro_area = if_else(metro, census_metro_area, rl_metro_area)) %>%
  select(census_metro_area, rl_metro_area, metro, state) %>%
  distinct()

# Read cleaned first-job data
dest <- readRDS(paste0(pathHome, 'revelio_data/first_spell_join.rds'))

# Attach MSA to Revelio destinations
dest <- dest %>%
  filter(country == 'United States') %>%
  rename(rl_metro_area = metro_area) %>%
  left_join(msa_map_grouped)

# And now we can map origin towns to MSAs and group them in the same way (aggregating
# non-metros within states). To make the origin MSAs consistent with destination MSAs
# we'll also need to pair origin MSAs that don't show up in the Revelio data with the nearest
# Revelio MSA. Or we could just leave them out of the origin-share calculations (or spread
# them evenly among the Revelio MSAs in the state or something).

# Link towns to MSAs

# Get Census Places -> County crosswalk
town_county <- read.delim(paste0(pathHome, 'data/place_county_crosswalk.txt'),
                          header = T,
                          sep = '|') %>%
  # Remove missing place names
  filter(PLACEFP != 0) %>%
  # This county name actually lists multiple counties in some cases
  # For now take the first one only
  #mutate(county_name = trimws(sub(",.*$", "", iconv(COUNTY, to = "UTF-8"))))
  # Now try getting all the counties for each town
  mutate(county_name = strsplit(COUNTY, ',\\s*')) %>%
  unnest(cols = c(county_name))

# Get County -> MSA crosswalk
county_msa <- msa %>%
  mutate(county = as.numeric(paste0(`FIPS code`, `County code`)),
         census_metro_area = `May 2022 MSA name`) %>%
  # Also join on Revelio MSAs
  left_join(msa_map) %>%
  # And select unique row for each county/MSA pair
  select(county, census_metro_area, rl_metro_area, metro) %>%
  distinct()

# Also bring in county FIPS codes
county <- read.delim(paste0(pathHome, 'data/county_fips.txt'),
                     header = F,
                     sep = ',') %>%
  rename(state = V1,
         county_name = V4) %>%
  mutate(county = V2 * 1000 + V3) %>%
  select(county_name, county, state)

# Attach county FIPS to places
town_county <- town_county %>%
  rename(state = STATE) %>%
  left_join(county)

# Bring in UA hometown data
comm <- read.csv(paste0(pathHome, 'data/all_alabama_data.csv'))

# Clean Commencement hometown names
comm <- comm %>%
  mutate(originState = gsub(' ', '', Origin.State)) %>%
  # State will be NA for origins outside the US
  left_join(states) %>%
  # Filter to US origins for now
  filter(!is.na(state)) %>%
  select(-state) %>%
  rename(state = originState) %>%
  # Standardize city names
  mutate(city = trimws(sub("\\s*\\b(city|town|cdp|village|township|borough)\\b", "", tolower(sub("\\s*,?\\s*(?:[A-Z]{2})?\\s*$", "", iconv(Origin.Town, to = "UTF-8"))), ignore.case = TRUE)))

# Standardize place names
town_county <- town_county %>%
  mutate(city = trimws(sub("\\s*\\b(city|town|cdp|village|township|borough)\\b\\s*$", "", tolower(iconv(PLACENAME, to = "UTF-8")), ignore.case = TRUE))) %>%
  select(city, state, county_name, county) %>%
  distinct()

# Join counties onto Commencement records by city/state
comm <- comm %>%
  left_join(town_county) %>%
  left_join(county_msa)

# This dataframe has multiple MSA matches for some people
# Preserve this multiplicity for now

# How many of the Commencement-record hometowns do we match to a county? To MSAs?
mean(!is.na(comm$county))
mean(!is.na(comm$census_metro_area))
mean(!is.na(comm$rl_metro_area))

# Also get standalone crosswalk from Census places to MSAs
town_msa <- town_county %>%
  left_join(county_msa)

# In cases of multiple MSAs, first preference metro areas
comm <- comm %>%
  # Remove students without any matching metro areas (8.25% loss)
  filter(!is.na(census_metro_area)) %>%
  # Redefine metro indicator
  mutate(metro = !str_detect(census_metro_area, 'nonmetro')) %>%
  # For each person, remove non-metro areas if there exists a metro area
  group_by(Name, Year, Origin.Town) %>%
  filter((sum(metro) == 0) | (metro == T)) %>%
  # When multiple counties match to the same MSA, remove those extra rows
  select(-c('county', 'county_name')) %>%
  distinct() %>%
  # Now when someone has a valid Revelio MSA, remove all missing Revelio MSAs
  group_by(Name, Year, Origin.Town) %>%
  filter((sum(!is.na(rl_metro_area)) == 0) | !is.na(rl_metro_area))

# Still need to consolidate some RL metros into a single true metro (e.g., LA + Anaheim)
# They currently appear as multiple observations; collapse them
comm <- comm %>%
  group_by(census_metro_area) %>%
  mutate(mult_rl_metros = length(unique(rl_metro_area)) - (sum(is.na(rl_metro_area)) > 0) > 1) %>%
  ungroup() %>%
  mutate(rl_metro_area = if_else(mult_rl_metros, census_metro_area, rl_metro_area))

# Bring in Conzelmann et al. CBSA crosswalk
cbsa <- read.csv(paste0(pathHome, 'data/CBSACrosswalk.csv'))

# Can try to assign missing Revelio metros in cases where a Revelio metro is adjacent
# to the MSA, based on Conzelmann crosswalk
# Only 40% of our metro names show up in theirs though
cbsa <- cbsa %>%
  rename(census_metro_area = CBSATitle,
         neighbor_metro = cbsa_nearest_name) %>%
  select(census_metro_area, neighbor_metro) %>%
  filter(neighbor_metro != '')
# Get reverse of these neighbor matches too
cbsa <- cbsa %>%
  rename(census_metro_area = neighbor_metro,
         neighbor_metro = census_metro_area) %>%
  bind_rows(cbsa)
# Join on the Revelio metros
cbsa <- cbsa %>%
  left_join(msa_map) %>%
  # Now make the "neighbor" metro correspond to the Revelio metro
  rename(census_metro_area = neighbor_metro,
         neighbor_metro = census_metro_area,
         rl_neighbor = rl_metro_area) %>%
  select(census_metro_area, neighbor_metro, rl_neighbor) %>%
  distinct() %>%
  filter(!is.na(rl_neighbor))

# Now join this onto Commencement towns
comm <- comm %>%
  left_join(cbsa) %>%
  # Impute a neighboring Revelio metro to unmatched Census metros
  mutate(rl_metro_area = if_else(is.na(rl_metro_area), rl_neighbor, rl_metro_area))
# This only actually fixed 0.13% of sample...

# Now get minimal set of variables and filter to distinct rows
comm <- comm %>%
  select(Name, Origin.Town, Year, census_metro_area, rl_metro_area, state) %>%
  distinct()

# Define origin shares
# For now, just remove missing Revelio metros and filter to a unique metro
# In another version, we could impute the others evenly across the state
shares <- comm %>%
  group_by(Name, Origin.Town, Year) %>%
  # Filter to a unique metro area arbitrarily
  filter(row_number() == 1) %>%
  # First get the cohort size before filtering out missing metros
  rename(grad_y = Year) %>%
  group_by(grad_y) %>%
  mutate(N_cohort = n()) %>%
  # Now remove missing Revelio metros
  filter(!is.na(rl_metro_area)) %>%
  # And now get the origin shares
  group_by(grad_y, rl_metro_area, state) %>%
  summarize(o_share = n() / mean(N_cohort) * 100,
            N_origin = n())

# Some MSAs correspond to more than one state
# Let's collapse these states together for now
shares <- shares %>%
  group_by(grad_y, rl_metro_area) %>%
  # Aggregate but keep one of the state names
  mutate(o_share = sum(o_share),
         N_origin = sum(N_origin)) %>%
  filter(row_number() == 1)

# Consolidate the couple cases of multiple Revelio metros into one as with origins
dest <- dest %>%
  group_by(census_metro_area) %>%
  mutate(mult_rl_metros = length(unique(rl_metro_area)) > 1) %>%
  ungroup() %>%
  mutate(rl_metro_area = if_else(mult_rl_metros, census_metro_area, rl_metro_area))

# Save town-MSA crosswalk
saveRDS(town_msa, paste0(pathHome, 'data/town_msa_crosswalk.rds'))

# Analysis at MSA level ---------------------------------------------------------

# Read origin data
origin <- read.csv(paste0(pathHome, 'data/linked_commencement_revelio_profile_data.csv'))

# Clean up states of origin
origin <- origin %>%
  # Remove students with missing graduation years
  filter(!is.na(Year)) %>%
  mutate(originState = gsub(' ', '', originState)) %>%
  # State will be NA for origins outside the US
  left_join(states) %>%
  # Filter to US origins for now
  filter(!is.na(state)) %>%
  rename(grad_y = Year)

# Join origin and destination data
# Used for conditioning on origin, as well as full conditional logit model
join <- dest %>%
  select(-c('first_name', 'last_name', 'grad_y', 'fullname', 'field', 'user_location')) %>%
  # Get destination MSA
  rename(d_msa = rl_metro_area,
         d_state = state) %>%
  # Only keep those showing up in both
  inner_join(origin) %>%
  # Get origin state
  rename(o_state = state) %>%
  filter(grad_y < 2024)

# Run reduced-form regressions for in-state students

# Build in-state student share data
# Get destination shares of in-state students
share_ins <- join %>%
  filter(o_state == 'Alabama') %>%
  group_by(grad_y) %>%
  mutate(N_cohort = n()) %>%
  group_by(grad_y, d_msa, N_cohort) %>%
  summarize(d_share = n() / mean(N_cohort),
            N_dest = n()) %>%
  # Remove zero-share destinations (can't take log)
  filter(d_share > 0) %>%
  # Get log destination shares
  mutate(log_d_share = log(d_share)) %>%
  # Remove 2024 (incomplete data)
  filter(grad_y < 2024)

# Attach on the old origin shares (want to use full sample here)
share_ins <- shares %>%
  filter(state != 'AL') %>%
  rename(d_msa = rl_metro_area) %>%
  select(d_msa, grad_y, o_share, N_origin) %>%
  right_join(share_ins) %>%
  # Impute zero origin students
  mutate(N_origin = if_else(is.na(N_origin), 0, N_origin))

# Simple spec: raw # of students to/from the MSA
felm(N_dest ~ N_origin | factor(d_msa) | 0 | d_msa, data = share_ins) %>%
  summary(robust = T)
# Now including cohort effects
felm(N_dest ~ N_origin | factor(d_msa) + factor(grad_y) | 0 | d_msa, data = share_ins) %>%
  summary(robust = T)

# Now the full multinomial logit

# First get map from MSA to states for defining home-state indicators
msa_state <- join %>%
  ungroup() %>%
  select(d_msa, d_state) %>%
  distinct() %>%
  group_by(d_msa) %>%
  mutate(N = n())
# When multiple states match, choose one
mult_msa <- unique(msa_state$d_msa[msa_state$N > 1]) %>% sort()
mult_msa <- data.frame(d_msa = mult_msa,
                       state = c('New York', 'Massachusetts', 'North Carolina', 'Tennessee', 'Illinois',
                                 'Ohio', 'Texas', 'Colorado', 'Indiana', 'North Carolina',
                                 'South Carolina', 'Connecticut', 'Alabama', 'Florida', 'Missouri',
                                 'Kentucky', 'Tennessee', 'Wisconsin', 'Alabama', 'New York',
                                 'Pennsylvania', 'Oregon', 'Rhode Island', 'Nevada', 'Georgia',
                                 'Washington', 'Missouri', 'Florida', 'Ohio', 'Washington, D.C.'))
# Filter to these states
msa_state <- msa_state %>%
  left_join(mult_msa) %>%
  filter(is.na(state) | (d_state == state)) %>%
  select(d_msa, d_state) %>%
  rename(alternative = d_msa,
         alternative_state = d_state)

# Get individual/product-level dataframe
join_bal <- data.frame(user_id = rep(unique(join$user_id), n = length(unique(join$d_msa))),
                       alternative = rep(unique(join$d_msa), each = length(unique(join$user_id)))) %>%
  left_join(join) %>%
  # Get the alternative's associated state
  left_join(msa_state) %>%
  # Remove 2024
  filter(grad_y < 2024) %>%
  # Restrict to necessary variables
  select(user_id, alternative, alternative_state, o_state, d_msa, grad_y) %>%
  # Generate indicator for whether the destination is your home state
  mutate(home_state = o_state == alternative_state,
         # Generate choice indicator
         choice = d_msa == alternative,
         # Generate out-of-state indicator
         oos = o_state != 'Alabama') %>%
  # Join on origin-share data
  left_join(rename(shares, alternative = rl_metro_area)) %>%
  # Impute zero origin shares where missing
  mutate(o_share = if_else(is.na(o_share), 0, o_share)) %>%
  # Fix the Alabama origin shares to zero
  mutate(o_share = if_else(alternative_state == 'Alabama', 0, o_share)) %>%
  # Manually generate interactions with OOS indicator
  mutate(home_state_oos = home_state * oos,
         home_state_ins = home_state * !oos,
         o_share_oos = o_share * oos,
         o_share_ins = o_share * !oos) %>%
  # Get grad_y interacted with Alabama (for a trend in Alabama's mean utility)
  mutate(t_AL = (grad_y - 2006) * (alternative_state == 'Alabama')) %>%
  # Interact this with OOS
  mutate(t_AL_oos = t_AL * oos,
         t_AL_ins = t_AL * !oos) %>%
  # Arrange dataframe
  arrange(user_id, alternative)

# Multinomial logit, no heterogeneity by in-state
mod0 <- mlogit(choice ~ home_state + o_share, data = join_bal)

# Multinomial logit, no heterogeneity by in-state, with a trend
mod1 <- mlogit(choice ~ home_state + o_share + t_AL, data = join_bal)

# Multinomial logit, heterogeneity but no trend in Alabama utility
mod2 <- mlogit(choice ~ home_state_oos + home_state_ins + o_share_oos + o_share_ins, data = join_bal)

# Multinomial logit, heterogeneity and trends in Alabama utilities
mod3 <- mlogit(choice ~ home_state_oos + home_state_ins + o_share_oos + o_share_ins + t_AL_oos + t_AL_ins, data = join_bal)

# Multinomial logit, heterogeneity and cohort effects on Alabama utilities
mod4 <- mlogit(choice ~ home_state_oos + home_state_ins + o_share_oos + o_share_ins + t_AL_oos + t_AL_ins, data = join_bal %>% mutate(t_AL_oos = factor(t_AL_oos), t_AL_ins = factor(t_AL_ins)))

# Get marginal effect of a 1pp increase in the out-of-state share
# Get weights
weights <- shares %>%
  group_by(rl_metro_area) %>%
  filter(state != 'AL') %>%
  summarize(N = sum(N_origin))
# Get average marginal effects
effects0 <- effects(mod0, covariate = 'o_share')
effects1 <- effects(mod1, covariate = 'o_share')
effects2 <- effects(mod2, covariate = 'o_share_ins')
effects3 <- effects(mod3, covariate = 'o_share_ins')
effects4 <- effects(mod4, covariate = 'o_share_ins')

# Get sum of effects on the 4 Alabama MSAs from raising the OOS shares of all non-Alabama
# metros in proportion to their overall OOS shares
AL_metros <- shares %>%
  filter(state == 'AL') %>%
  pull(rl_metro_area) %>%
  unique()
me0 <- (weights$N %*% colSums(effects0[AL_metros, weights$rl_metro_area])) / sum(weights$N)
me1 <- (weights$N %*% colSums(effects1[AL_metros, weights$rl_metro_area])) / sum(weights$N)
me2 <- (weights$N %*% colSums(effects2[AL_metros, weights$rl_metro_area])) / sum(weights$N)
me3 <- (weights$N %*% colSums(effects3[AL_metros, weights$rl_metro_area])) / sum(weights$N)
me4 <- (weights$N %*% colSums(effects4[AL_metros, weights$rl_metro_area])) / sum(weights$N)
