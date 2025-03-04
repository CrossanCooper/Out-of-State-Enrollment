#////////////////////////////////////////////////////////////////////////////////
# Filename: analysis_msa.R
# Author: Ryan Haygood
# Date: 2/11/25
# Description: Constructs crosswalk from towns to Revelio MSAs and replicates
# main OOS pull effect estimates at the MSA level.
#////////////////////////////////////////////////////////////////////////////////

# Setup
source('C:/Users/ryanh/OneDrive/Documents/Grad School/Research/Out-of-State-Enrollment/code/setup.R')

# Alternative MSA map -----------------------------------------------------------

# Make state abbreviation/name crosswalk
states <- data.frame(originState = state.abb,
                     state = state.name) %>%
  bind_rows(data.frame(originState = 'DC', state = 'Washington, D.C.'))

# Also bring in county FIPS codes
county <- read.delim(paste0(pathHome, 'data/county_fips.txt'),
                     header = F,
                     sep = ',') %>%
  rename(state = V1,
         county_name = V4) %>%
  mutate(county = V2 * 1000 + V3) %>%
  select(county_name, county, state)

# Directly map cities to MSAs
city_msa <- read_xlsx(paste0(pathHome, 'data/City to MSA Crosswalk.xlsx'), skip = 2) %>%
  rename(fips = `FIPS State Code`) %>%
  mutate(fips = as.numeric(fips))

# Convert FIPS code to state abbreviation
city_msa <- county %>%
  mutate(fips = trunc(county / 1000)) %>%
  select(fips, state) %>%
  distinct() %>%
  right_join(city_msa)

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

# Clean crosswalk city names too
city_msa <- city_msa %>%
  mutate(city = trimws(sub("\\s*\\b(city|town|cdp|village|township|borough)\\b\\s*$", "", tolower(iconv(`Principal City Name`, to = "UTF-8")), ignore.case = TRUE))) %>%
  select(state, city, `CBSA Code`, `CBSA Title`)

# Join counties onto Commencement records by city/state
comm %>%
  left_join(city_msa) %>%
  pull(`CBSA Title`) %>%
  is.na() %>%
  mean()

# Instead of all this, try just mapping place locations to CBSAs using shapefiles

# Get place coordinates
place_loc <- read.delim(paste0(pathHome, 'data/place_locations_2023.txt')) %>%
  rename(state = USPS,
         lat = INTPTLAT,
         lon = INTPTLONG) %>%
  group_by(state, NAME) %>%
  # A few rare cases of more than one place -- just take the one with larger land area
  filter((n() == 1) | (ALAND == max(ALAND))) %>%
  mutate(city = trimws(sub("\\s*\\b(city|town|cdp|village|township|borough)\\b\\s*$", "", tolower(iconv(NAME, to = "UTF-8")), ignore.case = TRUE))) %>%
  ungroup() %>%
  select(city, state, lat, lon)

# Get CBSA shapefiles
cbsa <- st_read(paste0(pathHome, 'data/cbsa_shapefiles_2023/tl_2023_us_cbsa.shp'))

# Map places to CBSAs
place_sf <- st_as_sf(place_loc, coords = c('lon', 'lat'), crs = 4326)

# Convert to same system
place_sf <- st_transform(place_sf, crs = st_crs(cbsa))

# Spatial join
city_msa <- st_join(place_sf, cbsa) %>%
  data.frame() %>%
  ungroup() %>%
  select(city, state, CBSAFP, NAME, LSAD) %>%
  rename(metro_code = CBSAFP,
         metro_name = NAME) %>%
  # Metro/micro indicator
  mutate(metro = LSAD == 'M1') %>%
  select(-LSAD)

# What fraction of Commencement records have associated coordinates?
comm %>%
  left_join(place_loc) %>%
  group_by(Name, Year, Origin.Town) %>%
  summarize(nonmissing = sum(!is.na(lat)) > 0) %>%
  pull(nonmissing) %>%
  mean()

# What fraction have an associated CBSA?
comm %>%
  left_join(city_msa) %>%
  group_by(Name, Year, Origin.Town) %>%
  summarize(nonmissing = sum(!is.na(metro_name)) > 0) %>%
  pull(nonmissing) %>%
  mean()

# Join MSAs to Commencement cities
comm <- comm %>%
  left_join(city_msa)

# 88% of hometowns match to a MSA this way
comm %>%
  group_by(Name, Year, Origin.Town) %>%
  summarize(missing = sum(is.na(metro_name)) == n()) %>%
  pull(missing) %>%
  mean()
# We should expect some small fraction like this to come from non-metro areas,
# so we'll assume that's the reason for not matching and assign these students
# to the aggregate "nonmetropolitan area" of their state

# Try supplementing with the City -> County -> MSA method

# 2020 city to county map
# Some cities belong to multiple counties (keep it this way for now)
city_county <- read.delim(paste0(pathHome, 'data/place_county_crosswalk_2020.txt'),
                          header = T,
                          sep = '|') %>%
  mutate(county = STATEFP * 1000 + COUNTYFP,
         city = trimws(sub("\\s*\\b(city|town|cdp|village|township|borough)\\b\\s*$", "", tolower(iconv(PLACENAME, to = "UTF-8")), ignore.case = TRUE))) %>%
  rename(state = STATE) %>%
  select(city, county, state) %>%
  distinct()

# 2023 county to MSA map
county_msa <- read_xlsx(paste0(pathHome, 'data/County to MSA Crosswalk.xlsx'), skip = 2) %>%
  rename(metro_code_aux = `CBSA Code`,
         metro_name_aux = `CBSA Title`) %>%
  mutate(county = as.numeric(paste0(`FIPS State Code`, `FIPS County Code`))) %>%
  filter(!is.na(county)) %>%
  select(county, metro_code_aux, metro_name_aux)

# Attach these to Commencement hometowns
comm %>%
  left_join(city_county) %>%
  left_join(county_msa) %>%
  group_by(Name, Year, Origin.Town) %>%
  summarize(metro = sum(!is.na(metro_name)) > 0, metro_aux = sum(!is.na(metro_name_aux)) > 0) %>%
  ungroup() %>%
  select(metro, metro_aux) %>%
  table()
# This hardly adds any extra students -- let's just go by lat/lon coordinates

# Read in cleaned Revelio data (first post-college job)
rv <- read.csv(paste0(pathHome, 'revelio_data/job_spells_data_ua.csv'))

# Revelio has a list of nonmetropolitan areas, just one for each state
unique(rv$metro_area[str_detect(rv$metro_area, 'nonmetro') & rv$country == 'United States'])
# As well as metro areas
unique(rv$metro_area[!str_detect(rv$metro_area, 'nonmetro') & rv$country == 'United States'])
# Note some of these are listed as spanning two to three adjacent states

# Get list of Revelio metro areas
rv_metros <- rv %>%
  # Extract just the city name, removing the "metropolitan area" suffix (will use this to match)
  mutate(city = sub("(.*?)(\\s?)(metro.*)", "\\1", metro_area)) %>%
  # Restrict to US metro areas
  filter(country == 'United States' & !str_detect(metro_area, 'nonmetro') & metro_area != '') %>%
  select(state, metro_area, city) %>%
  distinct() %>%
  # Get state abbreviation
  left_join(states) %>%
  rename(state_name = state) %>%
  rename(state = originState)

# Prepare MSA <-> pseudo-MSA dictionary
msa_map <- city_msa %>%
  filter(!is.na(metro_name)) %>%
  mutate(name = tolower(metro_name)) %>%
  select(state, metro_code, metro_name, name, metro) %>%
  distinct()

# Match these MSAs to Revelio MSAs

# Need to do this within each state
for (i in 1:nrow(msa_map)) {
  
  city_temp <- rv_metros$city[rv_metros$state == msa_map$state[i]]
  
  msa_map$city[i] <- city_temp[which(str_detect(msa_map$name[i], city_temp))][1]
  
}

# Get dataframe of just the Revelio MSAs matched to counterpart true MSAs
rv_metros <- msa_map %>%
  right_join(rv_metros)

# Manually match the remaining cities that didn't
# Most cases are because the reported state in Revelio is adjacent to the state of the metro area...
rv_metros <- rv_metros %>%
  mutate(name = case_when(city == 'reno' ~ 'reno, nv',
                          city == 'new york city' ~ 'new york-newark-jersey city, ny-nj',
                          city == 'dallas' ~ 'dallas-fort worth-arlington, tx',
                          city == 'anaheim' ~ 'los angeles-long beach-anaheim, ca',
                          city == 'fort lauderdale' ~ 'miami-fort lauderdale-west palm beach, fl',
                          city == 'albany' ~ 'albany-schenectady-troy, ny',
                          city == 'st louis' ~ 'st. louis, mo-il',
                          city == 'long beach' ~ 'los angeles-long beach-anaheim, ca',
                          city == 'pittsburgh' ~ 'pittsburgh, pa',
                          city == 'miami' ~ 'miami-fort lauderdale-west palm beach, fl',
                          city == 'savannah' ~ 'savannah, ga',
                          city == 'dayton' ~ 'dayton-kettering-beavercreek, oh',
                          city == 'jacksonville' ~ 'jacksonville, fl',
                          city == 'mobile' ~ 'mobile, al',
                          city == 'baltimore' ~ 'baltimore-columbia-towson, md',
                          city == 'milwaukee' ~ 'milwaukee-waukesha, wi',
                          city == 'chattanooga' ~ 'chattanooga, tn-ga',
                          city == 'fort collins' ~ 'fort collins-loveland, co',
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
  select(-c('metro_name', 'metro', 'metro_code')) %>%
  left_join(distinct(select(msa_map, c('name', 'metro_name', 'metro_code', 'metro'))))

# Consolidate three RL pseudo-MSAs that are too granular
rv_metros <- rv_metros %>%
  mutate(metro_area = if_else(metro_name %in% c('Los Angeles-Long Beach-Anaheim, CA', 'Miami-Fort Lauderdale-West Palm Beach, FL', 'Dallas-Fort Worth-Arlington, TX'),
                              metro_name, metro_area)) %>%
  # Select just the true MSA and pseudo-MSA information (city names no longer relevant)
  select(state, metro_area, metro_name) %>%
  rename(rl_metro_area = metro_area,
         census_metro_area = metro_name) %>%
  distinct()

# Get list of Revelio non-metro areas (these are all actually just state names)
rv_nonmetros <- rv %>%
  # Restrict to US metro areas
  filter(country == 'United States' & str_detect(metro_area, 'nonmetro')) %>%
  # Extract just the state name, removing the "nonmetropolitan area" suffix (will use this to match)
  mutate(loc = sub("(.*?)(\\s?)(nonmetro.*)", "\\1", metro_area)) %>%
  select(state, metro_area, loc) %>%
  distinct()

# How many origin cities map directly to a Revelio pseudo-MSA?
comm %>%
  rename(census_metro_area = metro_name) %>%
  left_join(rv_metros) %>%
  pull(rl_metro_area) %>%
  is.na() %>%
  mean()

# Join on Revelio metro names to all MSAs too
msa_map <- msa_map %>%
  left_join(rv_metros)

# What share of US population lives in the CBSAs matching to RL metro areas?

# Get MSA-level population estimates
msa_pop <- read_xlsx(paste0(pathHome, 'data/cbsa-met-est2023-pop.xlsx'), skip = 3) %>%
  rename(census_metro_area = ...1,
         pop = ...2) %>%
  select(census_metro_area, pop)
# Get total US population, 2020
total_pop <- msa_pop$pop[1]

# Filter to metro areas and clean up names
msa_pop <- msa_pop %>%
  filter(str_detect(census_metro_area, 'Metro Area')) %>%
  mutate(census_metro_area = gsub(' Metro.*', '', substr(census_metro_area, 2, nchar(census_metro_area))))

# Get population share
sum(msa_pop$pop[msa_pop$census_metro_area %in% rv_metros$census_metro_area]) / total_pop

# For students who match to an MSA not linked to a Revelio MSA:
# If their origin city is within 30 miles of the closest Revelio MSA, impute them to be there
# Or impute them to the nearest one if they're in a metro area? Because it doesn't make
# much sense to classify them as in the nonmmetropolitan area in that case...
# Otherwise, they fall under the state's "nonmetropolitan area"

# Fundamental issue: do we believe students from cities that don't match to a
# Revelio-linked CBSA all really come from non-metro areas? Or do we think that
# some of those students, especially those from MSAs, actually come from the nearest

# Do we believe that students never actually go to the MSAs not matching to Revelio
# locations? Either the Revelio-linked MSAs actually include other MSA destinations,
# in which case we should consider students from non-RL MSA cities as coming from some
# nearby RL MSA
# Or the Revelio "nonmetropolitan areas" include the other MSA destinations, in which
# case we should consider students from non-RL MSA cities as coming from those catch-all
# RL "nonmetropolitan areas"
# To evaluate this, we should see what fraction of RL destinations are such a "nonmetropolitan
# area" -- because we know the share actually going to nonmetro areas can't be that high
# This is about 29% of first job destinations for UA graduates... 
# If we think the share should mirror the population share in nonmetro areas, this is
# way too high -- about 80% or more of the population lives in metropolitan areas
# And it turns out that 65% of the US population lives in our 98 RL metro areas!
# That's great -- it probably means that we have roughly accurate MSA data for this
# part of the sample, and we should think of the nonmetro areas as including all the
# other metro areas too.

# Still, only 53% of the students' hometowns are in a RL MSA

# Attach RL metro areas to Commencement data
comm <- comm %>%
  rename(census_metro_area = metro_name) %>%
  left_join(rv_metros %>% select(census_metro_area, rl_metro_area) %>% distinct()) %>%
  # Now when someone has a valid Revelio MSA, remove all missing Revelio MSAs
  group_by(Name, Year, Origin.Town) %>%
  filter((sum(!is.na(rl_metro_area)) == 0) | !is.na(rl_metro_area))

# Now impute nonmetropolitan areas for students without a matching RL metro area
# First attach full state name
comm <- states %>%
  rename(state_name = state) %>%
  rename(state = originState) %>%
  right_join(comm)
# Now attach RL nonmetropolitan area names by state
comm <- rv_nonmetros %>%
  rename(state_name = state,
         nonmetro_area = metro_area) %>%
  right_join(comm) %>%
  mutate(rl_metro_area = if_else(is.na(rl_metro_area), nonmetro_area, rl_metro_area))

# Now some students appear more than once in Commencement records due primarily to
# multiple Degree, Honors, or Commencement listings -- filter to just one per student
comm <- comm %>%
  select(Name, Origin.Town, Year, census_metro_area, rl_metro_area, state) %>%
  distinct()
# 40 students' towns match to more than one metro area (due to multiple cities
# of that name in the same state in the Census records)
# Filter to the RL MSA with more students
comm <- comm %>%
  group_by(rl_metro_area) %>%
  mutate(N_rl_metro = n()) %>%
  group_by(Name, Origin.Town, Year) %>%
  arrange(Name, Origin.Town, Year, desc(N_rl_metro)) %>%
  filter(row_number() == 1)

# Let's split the choice set into combinations of RL metros and states, since some
# metros span multiple states, and we need to be able to define the shares residing
# in the same state in other RL metros correctly

# Define origin shares using RL metro X state combinations
shares <- comm %>%
  # Get the cohort size
  rename(grad_y = Year) %>%
  group_by(grad_y) %>%
  mutate(N_cohort = n()) %>%
  # And now get the origin shares
  group_by(grad_y, rl_metro_area, state) %>%
  summarize(o_share = n() / mean(N_cohort) * 100,
            N_origin = n())

# For each metro, also get the share coming from other metros in the same state
shares <- shares %>%
  group_by(grad_y, state) %>%
  mutate(o_share_other = sum(o_share) - o_share)

# Now read cleaned first-job data
dest <- readRDS(paste0(pathHome, 'revelio_data/first_spell_join.rds'))

# Filter to US destinations
# Consolidate three RL pseudo-MSAs that are too granular
dest <- dest %>%
  filter(country == 'United States') %>%
  rename(rl_metro_area = metro_area) %>%
  mutate(rl_metro_area = case_when(rl_metro_area %in% c('los angeles metropolitan area', 'anaheim metropolitan area', 'long beach metropolitan area') ~ 'Los Angeles-Long Beach-Anaheim, CA',
                                   rl_metro_area %in% c('west palm beach metropolitan area', 'fort lauderdale metropolitan area', 'miami metropolitan area') ~ 'Miami-Fort Lauderdale-West Palm Beach, FL',
                                   rl_metro_area %in% c('dallas metropolitan area', 'fort worth metropolitan area') ~ 'Dallas-Fort Worth-Arlington, TX',
                                   .default = rl_metro_area))

# Attach state names
dest <- dest %>%
  left_join(states) %>%
  rename(state_name = state) %>%
  rename(state = originState)

# Now identify RL-MSA/state combinations that don't show up in origin RL-MSA/state list
dest_msas <- unique(paste0(dest$rl_metro_area, dest$state))
comm_msas <- unique(paste0(shares$rl_metro_area, shares$state))
unobserved_msas <- dest_msas[which(!(dest_msas %in% comm_msas))]

# How many students go to one such MSA?
dest %>%
  mutate(area = paste0(rl_metro_area, state)) %>%
  mutate(unobserved = area %in% unobserved_msas) %>%
  pull(unobserved) %>%
  table()
# Only 238 -- these states may be misreported, so group these students under the
# state corresponding to that RL-MSA with the largest share
main_state <- dest %>%
  group_by(rl_metro_area) %>%
  mutate(N_rl_metro = n()) %>%
  group_by(rl_metro_area, state) %>%
  summarize(state_share = n() / mean(N_rl_metro)) %>%
  group_by(rl_metro_area) %>%
  filter(state_share == max(state_share)) %>%
  rename(main_state = state)
dest <- dest %>%
  left_join(main_state) %>%
  mutate(state = if_else(paste0(rl_metro_area, state) %in% unobserved_msas, main_state, state))

# Now define these RL-MSA/state geographic units
shares <- shares %>%
  mutate(region = paste0(rl_metro_area, ' - ', state))
dest <- dest %>%
  mutate(region = paste0(rl_metro_area, ' - ', state))

# What share of students' destinations are in a RL metro area?
dest %>%
  mutate(metro = !str_detect(rl_metro_area, 'nonmetro')) %>%
  pull(metro) %>%
  mean()

# Get other pull factors (unemployment rates, net migration rates) as controls
pull_factors <- readRDS(paste0(pathHome, 'data/pull_factors.rds')) %>%
  select(-c('pop', 'arrive', 'depart')) %>%
  # Rename DC
  mutate(state = if_else(state == 'District of Columbia', 'Washington, D.C.', state)) %>%
  # Get state abbreviations for later join
  left_join(states) %>%
  select(-state) %>%
  rename(state = originState)

# Save city-MSA crosswalk
saveRDS(city_msa, paste0(pathHome, 'data/city_msa_crosswalk.rds'))

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
         d_state = state,
         d_region = region) %>%
  # Only keep those showing up in both
  inner_join(origin) %>%
  select(-state) %>%
  rename(o_state = originState) %>%
  filter(grad_y < 2024)

# Run reduced-form regressions for in-state students

# Build in-state student share data
# Get destination shares of in-state students
share_ins <- join %>%
  filter(o_state == 'AL') %>%
  group_by(grad_y) %>%
  mutate(N_cohort = n()) %>%
  group_by(grad_y, d_region, N_cohort) %>%
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
  rename(d_region = region) %>%
  select(d_region, grad_y, o_share, N_origin) %>%
  right_join(share_ins) %>%
  # Impute zero origin students
  mutate(N_origin = if_else(is.na(N_origin), 0, N_origin))

# Simple spec: raw # of students to/from the MSA
felm(N_dest ~ N_origin | factor(d_region) | 0 | d_region, data = share_ins) %>%
  summary(robust = T)
# Now including cohort effects
felm(N_dest ~ N_origin | factor(d_region) + factor(grad_y) | 0 | d_region, data = share_ins) %>%
  summary(robust = T)

# Now the full multinomial logit

# Get individual/product-level dataframe
join_bal <- data.frame(user_id = rep(unique(join$user_id), n = length(unique(join$d_region))),
                       alternative = rep(unique(join$d_region), each = length(unique(join$user_id)))) %>%
  left_join(join) %>%
  # Get the alternative's associated state
  mutate(alternative_state = substr(alternative, nchar(alternative) - 1, nchar(alternative)),
         alternative_metro = !str_detect(alternative, 'nonmetro')) %>%
  # Remove 2024
  filter(grad_y < 2024) %>%
  # Restrict to necessary variables
  select(user_id, alternative, alternative_state, alternative_metro, o_state, d_region, grad_y) %>%
  # Generate indicator for whether the destination is your home state
  mutate(home_state = o_state == alternative_state,
         # Generate choice indicator
         choice = d_region == alternative,
         # Generate out-of-state indicator
         oos = o_state != 'AL') %>%
  # Join on origin-share data
  left_join(rename(shares, alternative = region)) %>%
  # Join on other pull factors as control variables (at state level)
  left_join(rename(pull_factors, alternative_state = state, grad_y = y)) %>%
  # Remove extraneous variables
  select(-c('rl_metro_area', 'state', 'N_origin')) %>%
  # Impute zero origin shares where missing
  mutate(o_share = if_else(is.na(o_share), 0, o_share),
         o_share_other = if_else(is.na(o_share_other), 0, o_share_other)) %>%
  # Fix the Alabama origin shares to zero
  mutate(o_share = if_else(alternative_state == 'AL', 0, o_share),
         o_share_other = if_else(alternative_state == 'AL', 0, o_share_other)) %>%
  # Manually generate interactions with OOS indicator
  mutate(home_state_oos = home_state * oos,
         home_state_ins = home_state * !oos,
         o_share_oos = o_share * oos,
         o_share_ins = o_share * !oos,
         o_share_other_oos = o_share_other * oos,
         o_share_other_ins = o_share_other * !oos,
         metro_oos = alternative_metro * oos,
         metro_ins = alternative_metro * !oos) %>%
  # Get grad_y interacted with Alabama (for a trend in Alabama's mean utility)
  mutate(t_AL = (grad_y - 2006) * (alternative_state == 'AL')) %>%
  # Interact this with OOS
  mutate(t_AL_oos = t_AL * oos,
         t_AL_ins = t_AL * !oos) %>%
  # Arrange dataframe
  arrange(user_id, alternative)

# Convert alternatives to simpler names for Stata
alternative_dict <- data.frame(alternative = unique(join_bal$alternative),
                               code = paste0('alt', c(paste0('00', 1:9), paste0('0', 10:99), 100:length(unique(join_bal$alternative)))))
# Attach new alternative names
join_bal <- join_bal %>%
  left_join(alternative_dict)

# Multinomial logit, no heterogeneity by in-state
mod0 <- mlogit(choice ~ home_state + o_share, data = join_bal)

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

# Controlling for unemployment rate and net migration rate
mod9 <- mlogit(choice ~ home_state_oos + home_state_ins + o_share_oos + o_share_ins + o_share_other_oos + o_share_other_ins + ur + net_rate + t_AL_oos + t_AL_ins, data = join_bal %>% filter(grad_y != 2020) %>% mutate(t_AL_oos = factor(t_AL_oos), t_AL_ins = factor(t_AL_ins)))

# Get robust SEs
se4 <- sqrt(diag(vcovCL(mod4, type = 'HC1')))
se6 <- sqrt(diag(vcovCL(mod6, type = 'HC1')))
se8 <- sqrt(diag(vcovCL(mod8, type = 'HC1')))
se9 <- sqrt(diag(vcovCL(mod9, type = 'HC1')))

# Print regression table
stargazer(mod4, mod6, mod8, omit = 'Intercept', se = list(se4, se6, se8))

# Bring in variance-covariance matrices from Stata for clustered SEs
vcov4 <- read.csv(paste0(pathHome, 'data/vcov_mod4_msa.csv'))
vcov6 <- read.csv(paste0(pathHome, 'data/vcov_mod6_msa.csv'))
vcov8 <- read.csv(paste0(pathHome, 'data/vcov_mod8_msa.csv'))
vcov9 <- read.csv(paste0(pathHome, 'data/vcov_mod9_msa.csv'))

# Get function to clean Stata SEs
clean_stata_se <- function(vcov, mod) {
  
  # Clean up to match R version
  vcov <- data.frame(lapply(vcov, function(x) gsub("=", "", x)), stringsAsFactors = FALSE) %>%
    mutate(X. = case_when(X. == '_cons' ~ lag(X.),
                          str_detect(X., 'alt') ~ NA,
                          .default = X.)) %>%
    filter(!is.na(X.) & X. != 'code')
  
  # Get stata and R variable names (in their original orders)
  stata_vars <- vcov$X.
  R_vars <- gsub('\\(Intercept\\):|TRUE|FALSE|alternative', '', colnames(vcov(mod)))
  
  # Replace Stata variable names with R variable names
  stata_vars <- if_else(stata_vars %in% alternative_dict$code, 
                        alternative_dict$alternative[match(stata_vars, alternative_dict$code)], 
                        stata_vars)
  
  # Replace cohort FE names
  stata_cohort <- if_else(grepl("^\\d", stata_vars), sub("^([0-9]+).*", "\\1", stata_vars), '')
  stata_vars <- paste0(sub("^\\d+\\.", "", stata_vars), stata_cohort)
  # Remove variable-name column
  vcov <- vcov %>%
    select(-X.) %>%
    # Turn into numeric
    mutate_all(as.numeric)
  # Assign cleaned names
  rownames(vcov) <- colnames(vcov) <- stata_vars
  # Remove cohort zero
  vcov <- vcov[!str_detect(rownames(vcov), '0b'), !str_detect(colnames(vcov), '0b')]
  # Reorder matrix to match R variable ordering
  vcov <- vcov[R_vars, R_vars]
  
  as.matrix(vcov)
  
}

# Clean cluster-robust SEs from Stata to match R formatting
vcov4 <- clean_stata_se(vcov4, mod4)
vcov6 <- clean_stata_se(vcov6, mod6)
vcov8 <- clean_stata_se(vcov8, mod8)
vcov9 <- clean_stata_se(vcov9, mod9)

# Now can get SEs from diagonal and apply Delta method using these v-cov matrices
se4 <- sqrt(diag(vcov4))
se6 <- sqrt(diag(vcov6))
se8 <- sqrt(diag(vcov8))
se9 <- sqrt(diag(vcov9))

# Output table with cluster-robust SEs
stargazer(mod4, mod6, mod8, mod9, omit = c('Intercept', 't_AL_ins', 't_AL_oos'), se = list(se4, se6, se8, se9))

# Get AMEs ----------------------------------------------------------------------

# Get sum of effects on the 4 Alabama MSAs from raising the OOS shares of all non-Alabama
# metros in proportion to their overall OOS shares
AL_metros <- shares %>%
  filter(substr(region, nchar(region) - 1, nchar(region)) == 'AL') %>%
  pull(region) %>%
  unique()

# Get weights on each region (their overall share of OOS students at UA over the whole period)
# Need to add in the pull effect to other MSAs in the state for models 2, 4, 6, 8
# For each non-AL region, get the portion of the 1pp increase in OOS share accounted for
# by the other regions in their state (a 1pp increase in OOS share is a >1pp total increase in
# this variable)
weights <- shares %>%
  group_by(region, state) %>%
  summarize(N = sum(N_origin)) %>%
  filter(state != 'AL') %>%
  ungroup() %>%
  # Get actual fraction of the 1pp increase in OOS share in each MSA
  mutate(region_pp = N / sum(N)) %>%
  # Get pp increase in other MSAs' share
  group_by(state) %>%
  mutate(other_region_pp = sum(region_pp) - region_pp) %>%
  # Remove metros that have zero destination shares
  filter(region %in% join$d_region)

# Get dataframe for prediction
# In-state student predictions are identical up to cohorts, so just get one per cohort
# and then weight according to cohort sizes for ease of computation
pred <- join %>%
  # Filter to in-state students
  filter(o_state == 'AL') %>%
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
  pred$pred_weight %*% t(unlist(weights[, if_else(x == 'o_share_ins', 'region_pp', 'other_region_pp')]) %*% t(-mod$coefficients[x] * rowSums(predict(mod, pred_bal)[, AL_metros]) * predict(mod, pred_bal)[, weights$region]))
  
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

# Cluster-robust standard errors
ame4_se_cl <- sqrt(jac4 %*% vcov4 %*% t(jac4))
ame4_se_other_cl <- sqrt(jac4_other %*% vcov4 %*% t(jac4_other))

# Point estimate
ame(mod6$coefficients, x = 'o_share_ins', mod = mod6)
ame(mod6$coefficients, x = 'o_share_other_ins', mod = mod6)

# Get Jacobian
jac6 <- jacobian(function(coefs) {ame(coefs, 'o_share_ins', mod = mod6)}, mod6$coefficients)
jac6_other <- jacobian(function(coefs) {ame(coefs, 'o_share_other_ins', mod = mod6)}, mod6$coefficients)

# Standard error via Delta method
ame6_se <- sqrt(jac6 %*% vcovCL(mod6, type = 'HC1') %*% t(jac6))
ame6_se_other <- sqrt(jac6_other %*% vcovCL(mod6, type = 'HC1') %*% t(jac6_other))

# Cluster-robust standard errors
ame6_se_cl <- sqrt(jac6 %*% vcov6 %*% t(jac6))
ame6_se_other_cl <- sqrt(jac6_other %*% vcov6 %*% t(jac6_other))

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
  pred$pred_weight %*% t(unlist(weights[, if_else(x == 'o_share_ins', 'region_pp', 'other_region_pp')]) %*% t(-mod$coefficients[x] * rowSums(predict(mod, pred_bal)[, AL_metros]) * predict(mod, pred_bal)[, weights$region]))
  
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

# Cluster-robust standard errors
ame8_se_cl <- sqrt(jac8 %*% vcov8 %*% t(jac8))
ame8_se_other_cl <- sqrt(jac8_other %*% vcov8 %*% t(jac8_other))

# Save panel for use in Stata
write_dta(join_bal, paste0(pathHome, 'data/join_bal_msa.dta'))