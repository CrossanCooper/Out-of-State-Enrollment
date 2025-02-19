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
  # Now remove missing Revelio metros before defining cohort size
  filter(!is.na(rl_metro_area)) %>%
  # Get the cohort size of observed students
  rename(grad_y = Year) %>%
  group_by(grad_y) %>%
  mutate(N_cohort = n()) %>%
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

# Fix which state they correspond to
# Choosing the state that the city resides in
shares <- shares %>%
  mutate(state = case_when(rl_metro_area == 'cincinnati metropolitan area' ~ 'OH',
                           rl_metro_area == 'memphis metropolitan area' ~ 'TN',
                           rl_metro_area == 'philadelphia metropolitan area' ~ 'PA',
                           rl_metro_area == 'washington metropolitan area' ~ 'DC',
                           rl_metro_area == 'new york city metropolitan area' ~ 'NY',
                           .default = state))

# For each metro, also get the share coming from other metros in the same state
shares <- shares %>%
  group_by(grad_y, state) %>%
  mutate(o_share_other = sum(o_share) - o_share)

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
         alternative_state = d_state) %>%
  # Also get metro indicator
  mutate(alternative_metro = !(str_detect(alternative, 'nonmetro')))

# Get individual/product-level dataframe
join_bal <- data.frame(user_id = rep(unique(join$user_id), n = length(unique(join$d_msa))),
                       alternative = rep(unique(join$d_msa), each = length(unique(join$user_id)))) %>%
  left_join(join) %>%
  # Get the alternative's associated state
  left_join(msa_state) %>%
  # Remove 2024
  filter(grad_y < 2024) %>%
  # Restrict to necessary variables
  select(user_id, alternative, alternative_state, alternative_metro, o_state, d_msa, grad_y) %>%
  # Generate indicator for whether the destination is your home state
  mutate(home_state = o_state == alternative_state,
         # Generate choice indicator
         choice = d_msa == alternative,
         # Generate out-of-state indicator
         oos = o_state != 'Alabama') %>%
  # Join on origin-share data
  left_join(rename(shares, alternative = rl_metro_area)) %>%
  # Impute zero origin shares where missing
  mutate(o_share = if_else(is.na(o_share), 0, o_share),
         o_share_other = if_else(is.na(o_share_other), 0, o_share_other)) %>%
  # Fix the Alabama origin shares to zero
  mutate(o_share = if_else(alternative_state == 'Alabama', 0, o_share),
         o_share_other = if_else(alternative_state == 'Alabama', 0, o_share_other)) %>%
  # Manually generate interactions with OOS indicator
  mutate(home_state_oos = home_state * oos,
         home_state_ins = home_state * !oos,
         o_share_oos = o_share * oos,
         o_share_ins = o_share * !oos,
         o_share_other_oos = o_share_other * oos,
         o_share_other_ins = o_share_other * !oos) %>%
  # Get grad_y interacted with Alabama (for a trend in Alabama's mean utility)
  mutate(t_AL = (grad_y - 2006) * (alternative_state == 'Alabama')) %>%
  # Interact this with OOS
  mutate(t_AL_oos = t_AL * oos,
         t_AL_ins = t_AL * !oos) %>%
  # Arrange dataframe
  arrange(user_id, alternative)

# Multinomial logit, no heterogeneity by in-state
mod0 <- mlogit(choice ~ home_state + o_share, data = join_bal, heterosc = T)

# Multinomial logit, no heterogeneity by in-state, with a trend
mod1 <- mlogit(choice ~ home_state + o_share + t_AL, data = join_bal)

# Including share of other MSAs in the state
mod2 <- mlogit(choice ~ home_state + o_share + o_share_other + t_AL, data = join_bal)

# Multinomial logit, heterogeneity but no trend in Alabama utility
mod3 <- mlogit(choice ~ home_state_oos + home_state_ins + o_share_oos + o_share_ins, data = join_bal)

# Multinomial logit, heterogeneity but no trend in Alabama utility
mod4 <- mlogit(choice ~ home_state_oos + home_state_ins + o_share_oos + o_share_ins + o_share_other_oos + o_share_other_ins, data = join_bal)

# Multinomial logit, heterogeneity and trends in Alabama utilities
mod5 <- mlogit(choice ~ home_state_oos + home_state_ins + o_share_oos + o_share_ins + t_AL_oos + t_AL_ins, data = join_bal)

# Including share of other MSAs in the state
mod6 <- mlogit(choice ~ home_state_oos + home_state_ins + o_share_oos + o_share_ins + o_share_other_oos + o_share_other_ins + t_AL_oos + t_AL_ins, data = join_bal)

# Multinomial logit, heterogeneity and cohort effects on Alabama utilities
mod7 <- mlogit(choice ~ home_state_oos + home_state_ins + o_share_oos + o_share_ins + t_AL_oos + t_AL_ins, data = join_bal %>% mutate(t_AL_oos = factor(t_AL_oos), t_AL_ins = factor(t_AL_ins)))

# Including share of other MSAs in the state
mod8 <- mlogit(choice ~ home_state_oos + home_state_ins + o_share_oos + o_share_ins + o_share_other_oos + o_share_other_ins + t_AL_oos + t_AL_ins, data = join_bal %>% mutate(t_AL_oos = factor(t_AL_oos), t_AL_ins = factor(t_AL_ins)))

# Get robust SEs
se4 <- sqrt(diag(vcovCL(mod4, type = 'HC1')))
se6 <- sqrt(diag(vcovCL(mod6, type = 'HC1')))
se8 <- sqrt(diag(vcovCL(mod8, type = 'HC1')))
# Can also cluster by origin state if we wish
# This isn't working at the moment
sqrt(diag(vcovCL(mod4, cluster = ~o_state, type = 'HC1')))

# Print regression table
stargazer(mod4, mod6, mod8, omit = 'Intercept', se = list(se4, se6, se8))

# Get AMEs ----------------------------------------------------------------------

# Get sum of effects on the 4 Alabama MSAs from raising the OOS shares of all non-Alabama
# metros in proportion to their overall OOS shares
AL_metros <- shares %>%
  filter(state == 'AL') %>%
  pull(rl_metro_area) %>%
  unique()

# Get weights on each state (their overall share of OOS students at UA over the whole period)
weights <- shares %>%
  rename(originState = state) %>%
  left_join(states) %>%
  rename(state_old = state,
         d_msa = rl_metro_area) %>%
  # Get the same state as above for each MSA with multiple states
  left_join(mult_msa) %>%
  mutate(state = if_else(!is.na(state), state, state_old)) %>%
  # Manually fix Omaha and Virginia Beach
  mutate(state = case_when(d_msa == 'omaha metropolitan area' ~ 'Nebraska', d_msa == 'virginia beach metropolitan area' ~ 'Virginia', .default = state)) %>%
  # Convert names back
  rename(rl_metro_area = d_msa) %>%
  group_by(rl_metro_area, state) %>%
  filter(state != 'Alabama') %>%
  summarize(N = sum(N_origin))

# Need to add in the pull effect to other MSAs in the state for models 2, 4, 6, 8
# For each non-AL MSA, get the portion of the 1pp increase in OOS share accounted for
# by the other MSAs in their state (a 1pp increase in OOS share is a >1pp total increase in
# this variable)
weights <- weights %>%
  ungroup() %>%
  # Get actual fraction of the 1pp increase in OOS share in each MSA
  mutate(msa_pp = N / sum(N)) %>%
  # Get pp increase in other MSAs' share
  group_by(state) %>%
  mutate(other_msa_pp = sum(msa_pp) - msa_pp)

# Get dataframe for prediction
# In-state student predictions are identical up to cohorts, so just get one per cohort
# and then weight according to cohort sizes for ease of computation
pred <- join %>%
  # Filter to in-state students
  filter(o_state == 'Alabama') %>%
  # Get each cohort's weight for average of marginal effects
  group_by(grad_y) %>%
  mutate(N = n()) %>%
  ungroup() %>%
  mutate(pred_weight = N / n()) %>%
  # Get a single representative from each cohort
  group_by(grad_y) %>%
  filter(row_number() == 1) %>%
  select(user_id, grad_y, pred_weight)

# Get dataframe for prediction (works for models without cohort FE)
pred_bal <- join_bal %>%
  filter(user_id %in% pred$user_id)

# Function giving average marginal effect of 1pp increase in OOS share in terms of
# model coefficients; x can be 'o_share_ins' or 'o_share_other_ins'
ame <- function(coefs, x, mod) {
  
  # Replace model coefficients with coefs
  mod$coefficients <- coefs
  
  # Compute AME
  pred$pred_weight %*% t(unlist(weights[, if_else(x == 'o_share_ins', 'msa_pp', 'other_msa_pp')]) %*% t(-mod$coefficients[x] * rowSums(predict(mod, pred_bal)[, AL_metros]) * predict(mod, pred_bal)[, weights$rl_metro_area]))
  
}

# Point estimate
ame(mod4$coefficients, x = 'o_share_ins', mod = mod4)
ame(mod4$coefficients, x = 'o_share_other_ins', mod = mod4)

# Get Jacobian
jac4 <- jacobian(function(coefs) {ame(coefs, 'o_share_ins', mod = mod4)}, mod4$coefficients)
jac4_other <- jacobian(function(coefs) {ame(coefs, 'o_share_other_ins', mod = mod4)}, mod4$coefficients)

# Standard error via Delta method
ame4_se <- sqrt(jac4 %*% vcovCL(mod4, type = 'HC1') %*% t(jac4))
ame4_se_other <- sqrt(jac4_other %*% vcovCL(mod4, type = 'HC1') %*% t(jac4_other))

# Point estimate
ame(mod6$coefficients, x = 'o_share_ins', mod = mod6)
ame(mod6$coefficients, x = 'o_share_other_ins', mod = mod6)

# Get Jacobian
jac6 <- jacobian(function(coefs) {ame(coefs, 'o_share_ins', mod = mod6)}, mod6$coefficients)
jac6_other <- jacobian(function(coefs) {ame(coefs, 'o_share_other_ins', mod = mod6)}, mod6$coefficients)

# Standard error via Delta method
ame6_se <- sqrt(jac6 %*% vcovCL(mod6, type = 'HC1') %*% t(jac6))
ame6_se_other <- sqrt(jac6_other %*% vcovCL(mod6, type = 'HC1') %*% t(jac6_other))

# Get dataframe for prediction
# This is for the model with non-parametric trends
pred_bal <- join_bal %>%
  mutate(t_AL_oos = factor(t_AL_oos),
         t_AL_ins = factor(t_AL_ins)) %>%
  filter(user_id %in% pred$user_id)

# Function giving average marginal effect of 1pp increase in OOS share in terms of
# model coefficients; x can be 'o_share_ins' or 'o_share_other_ins'
ame <- function(coefs, x, mod) {
  
  # Replace its coefficients with coefs
  mod$coefficients <- coefs
  
  # Compute AME
  pred$pred_weight %*% t(unlist(weights[, if_else(x == 'o_share_ins', 'msa_pp', 'other_msa_pp')]) %*% t(-mod$coefficients[x] * rowSums(predict(mod, pred_bal)[, AL_metros]) * predict(mod, pred_bal)[, weights$rl_metro_area]))
  
}

# Point estimate
ame(mod8$coefficients, x = 'o_share_ins', mod = mod8)
ame(mod8$coefficients, x = 'o_share_other_ins', mod = mod8)

# This takes ages to run -- are all coefficients needed in the AME function?
# I could exclude the ones that apply to OOS students, but the main dimensionality
# is the fixed effects, which we need.
# Alternatively, I could try finding the analytic gradient...
jac8 <- jacobian(function(coefs) {ame(coefs, 'o_share_ins', mod = mod8)}, mod8$coefficients)
jac8_other <- jacobian(function(coefs) {ame(coefs, 'o_share_other_ins', mod = mod8)}, mod8$coefficients)

# Standard error via Delta method
ame8_se <- sqrt(jac8 %*% vcovCL(mod8, type = 'HC1') %*% t(jac8))
ame8_se_other <- sqrt(jac8_other %*% vcovCL(mod8, type = 'HC1') %*% t(jac8_other))

# Save panel for use in Stata
write_dta(join_bal, paste0(pathHome, 'data/join_bal_msa.dta'))