#=====================================================================
## Created by: Crossan Cooper
## Last Modified: 10-29-24

## query HUD API and link town data to ACS data
# Step 1: Town-ZIP from HUD (@crossan: ERRORS HERE)
# Step 2: ZIP-ZCTA from HRSA
# Step 3: ACS data at ZCTA level from tidycensus()
# Step 4: ACS ZCTA - Town
# Step 5: Aggregate up ACS data so one row per Town
#=====================================================================

#=====================================================================
# 0 - clean environment and load libraries
#=====================================================================

# default list of packages and cleaning command
rm(list=ls())
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,data.table,ggplot2,skimr,
               dplyr,fixest,ggmap,stargazer,sjmisc,httr,
               Hmisc,tseries,DescTools,here,censusapi,
               tidycensus,ggiplot,educationdata,foreach,
               doParallel,readxl,did,ggExtra,stringi)

#=====================================================================
# 1 - read and edit hud data (uses 2020 Census geographies)
#=====================================================================

key <- "eyJ0eXAiOiJKV1QiLCJhbGciOiJSUzI1NiJ9.eyJhdWQiOiI2IiwianRpIjoiNWNhMzg4NjUzMWJmNGM4NDdjNDE4MTY5YzJjN2FjYjliYjc5M2EzMGE5ZWEyMWFkOTU3NDhkOWMxNjIwZDgzZjBjNTAxNTFkODMyOWU3ZTAiLCJpYXQiOjE3MzAxMzg0ODYuMzY4MTM0LCJuYmYiOjE3MzAxMzg0ODYuMzY4MTM2LCJleHAiOjIwNDU2NzEyODYuMzY0MTEzLCJzdWIiOiI3MzY1OCIsInNjb3BlcyI6W119.gM6LUsRyWZ_wSgR2OPCcPiouMfNEuRPxGIAgEHqVMd7dpP0bgZwE88_YODbZkViPtHEDD5v4o7JUjoQPNnzoSw"
url <- "https://www.huduser.gov/hudapi/public/usps"

# Note that type is set to 1 which will return values for the ZIP to Tract file and query is set to VA which will return Zip Codes in Virginia
response <- httr::GET(url, query = list(type = 1, query = "All"), add_headers(Authorization = paste("Bearer", key)))

#check for errors
httr::http_error(response)

#access the output
output <- httr::content(response)

output_dt <- as.data.table(rbindlist(output$data$results, fill = TRUE)) # fill=TRUE handles missing elements

output_unique_dt <- unique(output_dt, by = c("zip","city","state"))
output_unique_dt <- output_unique_dt[,c("zip","city","state")]

# write the file to output

fwrite(output_unique_dt, file = here("data","hud_city_zip_crosswalk.csv"))

#=====================================================================
# 2 - read in zip level census data to describe cities / towns
#=====================================================================

### i. acs stuff

census_api_key("22814bd2350480eb533edaa7ad3c403f63c96335", install = TRUE, overwrite=TRUE)

# B19013_001 == Median HH income
# B17001_002 == Count HHs under poverty line
# B17001_001 == Total HH count
# B02001_003 == Count black individuals
# B02001_001 == Count all (total population)
# B23025_004 == Count in labor force
# B23025_007 == Count unemployed
acs_2022 <- get_acs(geography = "zcta", variables = c("B19013_001",
                                                      "B17001_002",
                                                      "B17001_001",
                                                      "B02001_003",
                                                      "B02001_001",
                                                      "B23025_004",
                                                      "B23025_007"),
                    year = 2022, survey = "acs5")

acs_2022_dt <- setDT(acs_2022)

### ii. merge with zip - zcta crosswalk

zip_zcta_crosswalk_dt <- fread(here("data","ZIP Code to ZCTA Crosswalk.csv"))

### iii. merge with zip - town crosswalk

zip_town_crosswalk_dt <- fread(here("data","hud_city_zip_crosswalk.csv"))
setnames(zip_town_crosswalk_dt, c("zip"), c("ZIP_CODE"))

### iv(a). merge zip-town with zip-zcta

# this is the town linked with zcta
# but there are multiple rows with the same town and zcta (because they only differ by zip code)
town_zcta <- merge(zip_town_crosswalk_dt, zip_zcta_crosswalk_dt, by = "ZIP_CODE")

town_zcta_unique <- town_zcta[,-c("ZIP_CODE")]
town_zcta_unique <- unique(town_zcta_unique, by = c("city","state","zcta"))

### iv(b). merge the town-zcta link with the acs data
acs_2022_dt[, zcta := as.integer(GEOID)]

acs_town <- merge(town_zcta_unique, acs_2022_dt, by = "zcta") # 275k rows (33.7k zctas / 18.5k cities)

acs_town[, variable := fcase(variable == "B19013_001", "median_hh_income",
                             variable == "B17001_002", "hh_under_poverty_line",
                             variable == "B17001_001", "hh_count",
                             variable == "B02001_003", "individual_black_count",
                             variable == "B02001_001", "individual_count",
                             variable == "B23025_007", "unemployed_count",
                             variable == "B23025_004", "labor_force_count")]

### v. reshape acs - town data to have one row per zcta - city combination -- DONE

acs_data_wide <- dcast(
  acs_town,
  zcta + city + state + PO_NAME + STATE + ZIP_TYPE + zip_join_type + GEOID + NAME ~ variable,
  value.var = "estimate"
)

acs_data_wide <- setDT(acs_data_wide)

### vi. aggregate up to one row per city (MAKE SURE WEIGHTING DONE CORRECTLY)

acs_data_wide[, poverty_share := hh_under_poverty_line/hh_count]
acs_data_wide[, black_share := individual_black_count/individual_count]
acs_data_wide[, unemployment_rate := unemployed_count/labor_force_count]

# (a) weight median hh income and poverty share by hh count
columns_to_weight <- c("median_hh_income", "poverty_share")
weighted_income_poverty <- acs_data_wide[, lapply(.SD, function(x) weighted.mean(x, w = hh_count, na.rm = TRUE)), 
                                by = .(city, state),
                                .SDcols = columns_to_weight]

# (b) weight black share by individual count
columns_to_weight <- c("black_share")
weighted_black_share <- acs_data_wide[, lapply(.SD, function(x) weighted.mean(x, w = individual_count, na.rm = TRUE)), 
                                        by = .(city, state),
                                        .SDcols = columns_to_weight]

# (c) weight unemployment rate by labor force count
columns_to_weight <- c("unemployment_rate")
weighted_unemployment_rate <- acs_data_wide[, lapply(.SD, function(x) weighted.mean(x, w = labor_force_count, na.rm = TRUE)), 
                                      by = .(city, state),
                                      .SDcols = columns_to_weight]

# (c) weight unemployment rate by labor force count
columns_to_sum <- c("hh_count", "individual_count")
sum_population <- acs_data_wide[, lapply(.SD, function(x) sum(x, na.rm = TRUE)), 
                                            by = .(city, state),
                                            .SDcols = columns_to_sum]
# (e) combine the weighted averages
all_weighted_outputs <- cbind(weighted_income_poverty, weighted_black_share, weighted_unemployment_rate, sum_population)

all_weighted_outputs_clean <- all_weighted_outputs[, -c(5:6,8:9,11:12)]


# this is using the 2018-2022 ACS 
all_weighted_outputs_final <- all_weighted_outputs_clean[!is.na(median_hh_income)]

fwrite(all_weighted_outputs_final, here("data","acs_2018_2022_town_estimates.csv"))


#=====================================================================
# 3 - combine town-level ACS estimates with commencement hometowns
#=====================================================================

### i. merge commencement and ACS data

alabama_data <- fread(here("data","all_alabama_data.csv"))
acs_data <- fread(here("data","acs_2018_2022_town_estimates.csv"))

acs_data[, town := tolower(city)]
acs_data[, state := tolower(state)]

alabama_data[, town := trimws(tolower(gsub(",.*", "", `Origin Town`)))]
alabama_data[, state := tolower(`Origin State`)]

# merge 100851 into ACS data (91.5%)
merge_acs_alabama <- merge(alabama_data, acs_data, by = c("town","state"))

# explore not merged -- looks like there is obvious room for improvement with spacing, etc
left_merged <- merge(alabama_data, acs_data, by = c("town", "state"), all.x = TRUE)
unmatched_from_commencement <- left_merged[is.na(left_merged[,median_hh_income])]

# improvements? e.g. alexander city -- abbreviated or misspelled (i can work on this)
# vestavia hills and homewood are missing

### ii. descriptive analysis of in vs out trends (bar charts)

merge_acs_alabama[, OutOfState := fifelse(state %flike% "al", "In-State", "Out-of-State")]

average_income_by_year <- merge_acs_alabama[, .(
  avg_median_hh_income = mean(median_hh_income, na.rm = TRUE)
), by = .(Year, OutOfState)]


average_income_by_year[, relative_to_2006 := avg_median_hh_income - avg_median_hh_income[Year == 2006], by = OutOfState]

income_relative_to_2006_levels <- ggplot(average_income_by_year, aes(x = Year, y = relative_to_2006, fill = OutOfState)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8, color = "black") +
  labs(
    x = "Year",
    fill = "Student Type",
    y = "Change in Average HH Income"
  ) +
  theme_bw() + removeGridX() + 
  scale_fill_viridis_d() + 
  theme(legend.position = "bottom",
        legend.background = element_rect(size = 0.25, linetype = "solid", colour = "black")) + 
scale_y_continuous(limits = c(-5000,25000),
                     breaks = c(-5000,0,5000,10000,15000,20000,25000),
                     labels = c("-$5k","$0k","$5k","$10k","$15k","$20k","$25k"))

print(income_relative_to_2006_levels)

ggsave(here("figures","income_relative_to_2006_levels.png"),
       plot = income_relative_to_2006_levels,width = 8, height = 4.5)




average_income_by_year[, share_relative_to_2006 := ((avg_median_hh_income - avg_median_hh_income[Year == 2006])/avg_median_hh_income[Year == 2006])*100, by = OutOfState]

income_relative_to_2006_percent <- ggplot(average_income_by_year, aes(x = Year, y = share_relative_to_2006, fill = OutOfState)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8, color = "black") +
  labs(
    x = "Year",
    fill = "Student Type",
    y = "Change in Average HH Income"
  ) +
  theme_bw() + removeGridX() + 
  scale_fill_viridis_d() + 
  theme(legend.position = "bottom",
        legend.background = element_rect(size = 0.25, linetype = "solid", colour = "black")) + 
  scale_y_continuous(limits = c(-5,25),
                     breaks = c(-5,0,5,10,15,20,25),
                     labels = c("-5%","0%","5%","10%","15%","20%","25%"))

print(income_relative_to_2006_percent)

ggsave(here("figures","income_relative_to_2006_percent.png"),
       plot = income_relative_to_2006_percent,width = 8, height = 4.5)




### iii. within alabama figures

in_state_merge_acs <- merge_acs_alabama[(state %flike% "al")]
in_state_merge_acs[, total_count_by_year := .N, by = Year]
in_state_merge_acs[, share_in_state_students := .N / total_count_by_year, by = .(town, Year)]

plot_data <- in_state_merge_acs[Year %in% c(2006, 2023)]

ggplot(plot_data[median_hh_income <= 125000], aes(x = median_hh_income, fill = factor(Year))) +
  geom_histogram(position = "identity", alpha = 0.6, bins = 60, color = "black") +
  labs(
    x = "Origin Location Median HH Income",
    y = "Student Counts",
    fill = "Year"
  ) +
  theme_bw() + removeGridX() + 
  scale_fill_viridis_d() + 
  theme(legend.position = "bottom",
        legend.background = element_rect(size = 0.25, linetype = "solid", colour = "black"))

unique_plot <- unique(plot_data, by = c("town","Year"))
#  go wide across years
unique_plot_wide <- dcast(unique_plot[Year %in% c(2006, 2023)], 
                           town + median_hh_income ~ Year, 
                           value.var = "share_in_state_students")
# set shares to 0 if missing
unique_plot_wide[, `2006` := fifelse(is.na(`2006`), 0, `2006`)]
unique_plot_wide[, `2023` := fifelse(is.na(`2023`), 0, `2023`)]
# 
# unique_plot_wide[, change := 100*(`2023` - `2006`)]
# 
# ggplot(unique_plot_wide, aes(x = median_hh_income, y = change, size = `2006`)) +
#   geom_point(alpha = 0.6, color = "#440154FF") +
#   # geom_smooth(method = "lm", se = T, color = "#FDE725FF", linetype = "dashed") +  # Adds regression line
#   labs(
#     x = "Median Household Income",
#     y = "Change in Student Share (2006-2023)"
#   ) + theme_bw() + removeGridX() + 
#   scale_x_continuous(limits = c(0,125000),
#                      breaks = c(0,25000,50000,75000,100000,125000),
#                      labels = c("$0k","$25k","$50k","$75k","$100k","$125k")) 

  
  
  
  