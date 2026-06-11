#=====================================================================
## Created by: Crossan Cooper
## Last Modified: 2-27-25

## file use: extract visits from html
#=====================================================================

#=====================================================================
# 0 - clean environment and load libraries
#=====================================================================

# default list of packages and cleaning command
rm(list=ls())
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,data.table,ggplot2,skimr,rvest,
               dplyr,fixest,ggmap,stargazer,sjmisc,xml2,
               Hmisc,tseries,DescTools,here,censusapi,RSQLite,
               tidycensus,educationdata,foreach,binsreg,stringr,
               doParallel,readxl,did,ggExtra,DBI,arrow)

#=====================================================================
# 1 - load UA rds files and place into master file
#=====================================================================

### i. read in files and place into master file

## (a) define the directory where .rds files are stored
ua_chunks_dir <- "/Volumes/T7/ua_chunks"
## (b) list all .rds files in the folder
rds_files <- list.files(ua_chunks_dir, pattern = "\\.rds$", full.names = TRUE)
## (c) check if files are found
if (length(rds_files) == 0) {
  stop("No .rds files found in the directory!")
}
## (d) initialize a list to accumulate data tables
dt_list <- list()
## (e) loop over each .rds file, read and store in the list
for (i in seq_along(rds_files)) {
  dt <- tryCatch({
    readRDS(rds_files[i])
  }, error = function(e) {
    message(sprintf("Error reading file %s: %s", rds_files[i], e))
    return(NULL)
  })
  if (!is.null(dt)) {
    dt_list[[length(dt_list) + 1]] <- dt
  }
}
## (f) combine all data tables into one using rbindlist
combined_dt <- setDT(rbindlist(dt_list, fill = TRUE))
print(dim(combined_dt))

## (g) write to output
output_file <- "/Users/crossancooper/Dropbox/Professional/active-projects/admissions_project/data/scraped_ua_html.rds"
saveRDS(combined_dt, output_file)

## (h) explore HTML
# test <- combined_dt[11111,2]
# print(test)

#=====================================================================
# 2 - extract visit info
#=====================================================================

### i. define function

extractRecruitingInfoFromHTML <- function(html_string) {
  # parse the HTML string
  doc <- read_html(html_string)
  
  # try to find alert nodes that likely hold recruiting visit info
  alert_nodes <- xml_find_all(doc, "//div[contains(@class, 'alert') and contains(@class, 'alert-success')]")
  
  if (length(alert_nodes) > 0) {
    events <- lapply(alert_nodes, function(node) {
      # get the date from the <strong> tag
      date <- xml_find_first(node, ".//strong") %>% xml_text(trim = TRUE)
      
      # get all text nodes within the alert
      texts <- xml_find_all(node, ".//text()") %>% xml_text()
      texts <- trimws(texts)
      texts <- texts[texts != ""]
      
      # remove the date (if present) from the texts
      texts <- texts[texts != date]
      
      # we expect texts to follow this pattern:
      # [1] time-range (with a colon), [2] event name,
      # [3] school name (with trailing colon), [4] location
      event_name <- if (length(texts) >= 2) texts[2] else NA
      school_name <- if (length(texts) >= 3) sub(":$", "", texts[3]) else NA
      location   <- if (length(texts) >= 4) texts[4] else NA
      
      data.table(Date = date, EventName = event_name, SchoolName = school_name, Location = location)
    })
    dt <- rbindlist(events, fill = TRUE)
    return(dt)
    
  } else if (length(html_nodes(doc, "#register_date")) > 0) {
    # fall back: assume a registration page with IDs register_date and register_location
    date <- html_node(doc, "#register_date") %>% html_text(trim = TRUE)
    event_name <- html_node(doc, "h1") %>% html_text(trim = TRUE)
    location_text <- html_node(doc, "#register_location") %>% html_text(trim = TRUE)
    # assume first line is the school name and the rest is the location
    lines <- unlist(strsplit(location_text, "\n"))
    lines <- trimws(lines)
    lines <- lines[lines != ""]
    school_name <- if (length(lines) >= 1) lines[1] else NA
    location <- if (length(lines) >= 2) paste(lines[-1], collapse = ", ") else NA
    
    dt <- data.table(Date = date, EventName = event_name, SchoolName = school_name, Location = location)
    return(dt)
    
  } else {
    # no recruiting info found; return empty table with correct columns.
    return(data.table(Date = character(), EventName = character(), SchoolName = character(), Location = character()))
  }
}


### ii. call function

## (a) apply function to each row in combined_dt column 2
results_list <- lapply(seq_len(nrow(combined_dt)), function(i) {
  text <- as.character(combined_dt[i, 2])
  extractRecruitingInfoFromHTML(text)
})

## (b) format the call output
call_output_dt <- rbindlist(results_list, idcol = "row_index")
# dates
call_output_dt[, extracted_date := str_extract(Date, "[A-Za-z]+ \\d{1,2}, \\d{4}")][
  , extracted_date := format(mdy(extracted_date), "%m-%d-%Y")]
call_output_dt[, extracted_date := as.Date(extracted_date, format = "%m-%d-%Y")]
call_output_dt[, extracted_year := year(extracted_date)]
# extract location from event name
call_output_dt[, extracted_EventName := str_trim(str_extract(EventName, "^[^:]+"))]
call_output_dt[, edited_location := str_trim(sub(".*:\\s*", "", EventName))]
# extract location from school name
call_output_dt[, edited_school_location := str_trim(sub(".*:\\s*", "", SchoolName))]
call_output_dt[, cleaned_school_location := sub(".*?(\\d+)", "\\1", edited_school_location)]
call_output_dt[, cleaned_school_location := gsub("([a-z0-9])([A-Z])", "\\1 \\2", cleaned_school_location)]
# replace edited location with cleaned_school_location if missing
call_output_dt[, edited_location := fifelse(!str_detect(edited_location, ","), cleaned_school_location, edited_location)]
# make cleaned location variable
call_output_dt[, extracted_location := fcase(Location %flike% "Hamilton, Alabama","Hamilton, Alabama",
                                             Location %flike% "Phil Campbell, Alabama","Phil Campbell, Alabama",
                                             Location %flike% "Tuscaloosa, Alabama","Tuscaloosa, Alabama",
                                             Location %flike% "Fayette, Alabama","Fayette, Alabama",
                                             Location %flike% "Hanceville, Alabama","Hanceville, Alabama",
                                             Location %flike% "Columbiana, Alabama","Columbiana, Alabama",
                                             Location %flike% "Gadsden, Alabama","Gadsden, Alabama",
                                             Location %flike% "Chattanooga, Tennessee","Chattanooga, Tennessee",
                                             Location %flike% "Birmingham, Alabama","Birmingham, Alabama",
                                             default = edited_location)]
location_counts <- call_output_dt[,.N,.(extracted_location)]
check <- call_output_dt[is.na(extracted_location)]
# drop if missing location
call_output_final_dt <- call_output_dt[!is.na(extracted_location) & extracted_location != "," & extracted_location != "–"]
# 18,857 unique activities with locations
nrow(call_output_final_dt[,.N,.(EventName,Date)])

## (c) additional formatting before writing to output

# only keep main columns
clean_output_final_dt <- call_output_final_dt[,c(1,6:8,12)]
# clean up extracted_location
clean_output_final_dt[, cleaned_location := gsub("([A-Z])([A-Z])([a-z])", "\\1 \\2\\3", extracted_location)]
clean_output_final_dt[, cleaned_location_v2 := gsub("([A-Z])([A-Z])([a-z])", "\\1 \\2\\3", cleaned_location)]
clean_output_final_dt[, cleaned_location_v2 := str_trim(gsub("United States", "", gsub("([A-Z])([A-Z])([a-z])", "\\1 \\2\\3", cleaned_location)))]
clean_output_final_dt[, cleaned_location_v3 := fifelse(
  str_detect(cleaned_location_v2, "\\d"),  # check if the string contains numbers
  gsub("\\d", "", cleaned_location_v2),   # remove all numbers
  cleaned_location_v2  # otherwise, keep it unchanged
)]
# state abbreviation to full name mapping
state_map <- c(
  "AL" = "Alabama", "AK" = "Alaska", "AZ" = "Arizona", "AR" = "Arkansas", "CA" = "California",
  "CO" = "Colorado", "CT" = "Connecticut", "DE" = "Delaware", "FL" = "Florida", "GA" = "Georgia",
  "HI" = "Hawaii", "ID" = "Idaho", "IL" = "Illinois", "IN" = "Indiana", "IA" = "Iowa",
  "KS" = "Kansas", "KY" = "Kentucky", "LA" = "Louisiana", "ME" = "Maine", "MD" = "Maryland",
  "MA" = "Massachusetts", "MI" = "Michigan", "MN" = "Minnesota", "MS" = "Mississippi",
  "MO" = "Missouri", "MT" = "Montana", "NE" = "Nebraska", "NV" = "Nevada", "NH" = "New Hampshire",
  "NJ" = "New Jersey", "NM" = "New Mexico", "NY" = "New York", "NC" = "North Carolina",
  "ND" = "North Dakota", "OH" = "Ohio", "OK" = "Oklahoma", "OR" = "Oregon", "PA" = "Pennsylvania",
  "RI" = "Rhode Island", "SC" = "South Carolina", "SD" = "South Dakota", "TN" = "Tennessee",
  "TX" = "Texas", "UT" = "Utah", "VT" = "Vermont", "VA" = "Virginia", "WA" = "Washington",
  "WV" = "West Virginia", "WI" = "Wisconsin", "WY" = "Wyoming"
)
# function to replace state abbreviations after a comma
replace_state_abbrev <- function(location) {
  match <- str_match(location, ",\\s*([A-Z]{2})\\b")  # find the state abbreviation
  
  if (!is.na(match[1,1]) && match[1,2] %in% names(state_map)) {
    state_full <- state_map[match[1,2]]  # lookup full state name
    return(sub(match[1,1], paste0(", ", state_full), location))  # replace abbreviation
  }
  
  return(location)  # return unchanged if no match
}
# apply transformation to cleaned_location_v3
clean_output_final_dt[, cleaned_location_v4 := sapply(cleaned_location_v3, replace_state_abbrev)]
# postal abbreviations / locations for further cleaning
common_suffixes <- c(
  "Highway", "Hwy", "Rd", "Road", "Blvd", "Boulevard", "Ave", "Avenue", "GA-",
  "St", "Street", "Dr", "Drive", "Pl", "Place", "Ct", "Court", "Pkwy", "Parkway", "US-",
  "Ln", "Lane", "Ter", "Terrace", "Way", "Cir", "Circle", "Loop", "Trl", "Trail", "U.S.",
  "Expwy", "Expressway", "Tpke", "Turnpike", "Row", "Walk", "Cres", "Crescent", "Route",
  "Mews", "Alley", "Bypass","Pike", "Fm", "Trce", "Building", "Ih", "Xing", "Byp", "Plz"
)
# create regex pattern to match any common street suffix followed by a space
suffix_pattern <- paste0(".*\\b(", paste(common_suffixes, collapse = "|"), ")\\s+")

# function to clean up the location string
remove_street_part <- function(location) {
  return(sub(suffix_pattern, "", location))
}

# apply transformation to cleaned_location_v4
clean_output_final_dt[, cleaned_location_v5 := sapply(cleaned_location_v4, remove_street_part)]
# remove leading cardinal directions from cleaned_location_v5
# define regex pattern to match leading cardinal directions
direction_pattern <- "^(\\b(N|S|E|W|NW|NE|SW|SE|NN|SS|EE|WW|N\\s+NW|S\\s+NE|S\\s+SE|E\\s+NE|W\\s+SW)\\b\\s*)+"
# function to remove leading cardinal directions
remove_leading_directions <- function(location) {
  return(str_trim(sub(direction_pattern, "", location)))
}
# apply transformation to cleaned_location_v5
clean_output_final_dt[, cleaned_location_v6 := sapply(cleaned_location_v5, remove_leading_directions)]

# keep just cleaned_location_v6
to_save_output_final_dt <- clean_output_final_dt[,c(1:4,11)]
# manual name edits
to_save_output_final_dt[, final_location := fcase(cleaned_location_v6 %flike% "#, McKinney, Texas", "McKinney, Texas", 
                                                  cleaned_location_v6 %flike% "Greenville, South Carolina, South Carolina", "Greenville, South Carolina", 
                                                  cleaned_location_v6 %flike% "Willow Park, Fort Worth, Texas", "Fort Worth, Texas", 
                                                  cleaned_location_v6 %flike% "N  WW Lafayette, Indiana", "Lafayette, Indiana", 
                                                  cleaned_location_v6 %flike% "Bakersfield, California, California", "Bakersfield, California", 
                                                  cleaned_location_v6 %flike% "Beverly Hills High School, California", "Beverly Hills, California", 
                                                  cleaned_location_v6 %flike% "N  W Kokomo, Indiana", "Kokomo, Indiana", 
                                                  cleaned_location_v6 %flike% "Morehead, Kentucky", "Morehead, Kentucky", 
                                                  cleaned_location_v6 %flike% "Nicholasville, Kentucky", "Nicholasville, Kentucky", 
                                                  cleaned_location_v6 %flike% "Malvern, Pennsylvania", "Malvern, Pennsylvania", 
                                                  cleaned_location_v6 %flike% "Mississippi  Meridian, Mississippi", "Meridian, Mississippi", 
                                                  cleaned_location_v6 %flike% "Sugar Land, Texas", "Sugar Land, Texas", 
                                                  cleaned_location_v6 %flike% "Tampa, Florida", "Tampa, Florida", 
                                                  cleaned_location_v6 %flike% "Bluffton, South Carolina", "Bluffton, South Carolina", 
                                                  cleaned_location_v6 %flike% "Charlottesville, Virginia", "Charlottesville, Virginia", 
                                                  cleaned_location_v6 %flike% "#  Cordova, Tennessee", "Cordova, Tennessee", 
                                                  cleaned_location_v6 %flike% "#  Las Vegas, Nevada", "Las Vegas, Nevada", 
                                                  cleaned_location_v6 %flike% "#  Monroeville, Alabama", "Monroeville, Alabama", 
                                                  cleaned_location_v6 %flike% "Plymouth Mtng, Pennsylvania", "Plymouth Meeting, Pennsylvania", 
                                                  cleaned_location_v6 %flike% "E Baltimore Pike Kennett Sq, Pennsylvania", "Kennett Square, Pennsylvania", 
                                                  cleaned_location_v6 %flike% "W Chester Pike West Chester, Pennsylvania", "West Chester, Pennsylvania", 
                                                  cleaned_location_v6 %flike% "Bristol Pike Bensalem, Pennsylvania", "Bensalem, Pennsylvania", 
                                                  cleaned_location_v6 %flike% "Tysons, Virginia", "Tysons, Virginia", 
                                                  cleaned_location_v6 %flike% "Nashville, Tennessee", "Nashville, Tennessee", 
                                                  cleaned_location_v6 %flike% "Florence, Kentucky", "Florence, Kentucky", 
                                                  cleaned_location_v6 %flike% "Lancaster, Pennsylvania", "Lancaster, Pennsylvania", 
                                                  cleaned_location_v6 %flike% "Falls Church, Virginia", "Falls Church, Virginia", 
                                                  cleaned_location_v6 %flike% "Huntsville, Alabama", "Huntsville, Alabama", 
                                                  cleaned_location_v6 %flike% "Urbana, Maryland", "Urbana, Maryland", 
                                                  cleaned_location_v6 %flike% "Egg Hbr Twp, New Jersey", "Egg Harbor Township, New Jersey", 
                                                  cleaned_location_v6 %flike% "Collegeville, Pennsylvania", "Collegeville, Pennsylvania", 
                                                  cleaned_location_v6 %flike% "Mclean, Virginia", "Mclean, Virginia", 
                                                  cleaned_location_v6 %flike% "Gwynedd Valley, Pennsylvania", "Gwynedd Valley, Pennsylvania", 
                                                  cleaned_location_v6 %flike% "Conshohocken, Pennsylvania", "Conshohocken, Pennsylvania", 
                                                  cleaned_location_v6 %flike% "Brentwood, Tennessee", "Brentwood, Tennessee", 
                                                  cleaned_location_v6 %flike% "Nicholasville, Kentucky", "Nicholasville, Kentucky", 
                                                  cleaned_location_v6 %flike% "Malvern, Pennsylvania", "Malvern, Pennsylvania",
                                                  cleaned_location_v6 %flike% ", Massachusetts" & extracted_EventName %flike% "Boston College High School",
                                                  "Boston, Massachusetts",
                                                  cleaned_location_v6 %flike% ", Illinois" & extracted_EventName %flike% "Addison Trail HS",
                                                  "Addison, Illinois",
                                                  cleaned_location_v6 %flike% ", Alabama" & extracted_EventName %flike% "Central High School Clay County",
                                                  "Lineville, Alabama",
                                                  cleaned_location_v6 %flike% ", New York" & extracted_EventName %flike% "Commack High School",
                                                  "Commack, New York",
                                                  cleaned_location_v6 %flike% ", Texas" & extracted_EventName %flike% "Crowley ISD College Fair",
                                                  "Crowley, Texas",
                                                  cleaned_location_v6 %flike% ", Connecticut" & extracted_EventName %flike% "Danbury Mall",
                                                  "Danbury, Connecticut",
                                                  cleaned_location_v6 %flike% ", Texas" & extracted_EventName %flike% "Deer Park High School",
                                                  "Deer Park, Texas",
                                                  cleaned_location_v6 %flike% ", Illinois" & extracted_EventName %flike% "Downer's Grove North HS",
                                                  "Downer's Grove, Illinois",
                                                  cleaned_location_v6 %flike% ", New York" & extracted_EventName %flike% "Garden City High School",
                                                  "Garden City, New York",
                                                  cleaned_location_v6 %flike% ", Illinois" & extracted_EventName %flike% "Geneva HS",
                                                  "Geneva, Illinois",
                                                  cleaned_location_v6 %flike% ", Illinois" & extracted_EventName %flike% "Glenbard East HS",
                                                  "Lombard, Illinois",
                                                  cleaned_location_v6 %flike% ", Illinois" & extracted_EventName %flike% "Glenbard South HS",
                                                  "Glen Ellyn, Illinois",
                                                  cleaned_location_v6 %flike% ", Illinois" & extracted_EventName %flike% "Glenbard West",
                                                  "Glen Ellyn, Illinois",
                                                  cleaned_location_v6 %flike% ", Illinois" & extracted_EventName %flike% "Glenbrook South HS",
                                                  "Glenview, Illinois",
                                                  cleaned_location_v6 %flike% ", New York" & extracted_EventName %flike% "Half Hollow Hills",
                                                  "Dix Hills, New York",
                                                  cleaned_location_v6 %flike% ", Illinois" & extracted_EventName %flike% "Homewood-Flossmoor",
                                                  "Flossmoor, Illinois",
                                                  cleaned_location_v6 %flike% ", Illinois" & extracted_EventName %flike% "JB Conant",
                                                  "Hoffman Estates, Illinois",
                                                  cleaned_location_v6 %flike% ", New York" & extracted_EventName %flike% "John H. Glenn High School",
                                                  "Elwood, New York",
                                                  cleaned_location_v6 %flike% ", Illinois" & extracted_EventName %flike% "John Hersey HS",
                                                  "Arlington Heights, Illinois",
                                                  cleaned_location_v6 %flike% ", Illinois" & extracted_EventName %flike% "Joliet Catholic",
                                                  "Joliet, Illinois",
                                                  cleaned_location_v6 %flike% ", Illinois" & extracted_EventName %flike% "Josephinum Academy",
                                                  "Chicago, Illinois",
                                                  cleaned_location_v6 %flike% ", New York" & extracted_EventName %flike% "Kings Park High School",
                                                  "Kings Park, New York",
                                                  cleaned_location_v6 %flike% ", Indiana" & extracted_EventName %flike% "Lafayette Jefferson HS",
                                                  "Lafayette, Indiana",
                                                  cleaned_location_v6 %flike% ", Illinois" & extracted_EventName %flike% "Lake Park HS",
                                                  "Roselle, Illinois",
                                                  cleaned_location_v6 %flike% ", Illinois" & extracted_EventName %flike% "Lincoln-Way West HS",
                                                  "New Lenox, Illinois",
                                                  cleaned_location_v6 %flike% ", Illinois" & extracted_EventName %flike% "Lincoln-Way Central HS",
                                                  "New Lenox, Illinois",
                                                  cleaned_location_v6 %flike% ", Illinois" & extracted_EventName %flike% "Lincoln-Way East HS",
                                                  "Frankfort, Illinois",
                                                  cleaned_location_v6 %flike% ", Illinois" & extracted_EventName %flike% "Lockport Township HS",
                                                  "Lockport, Illinois",
                                                  cleaned_location_v6 %flike% ", Illinois" & extracted_EventName %flike% "Loyola Academy",
                                                  "Wilmette, Illinois",
                                                  cleaned_location_v6 %flike% ", Illinois" & extracted_EventName %flike% "Maine East HS",
                                                  "Park Ridge, Illinois",
                                                  cleaned_location_v6 %flike% ", Illinois" & extracted_EventName %flike% "Maine West HS",
                                                  "Des Plaines, Illinois",
                                                  cleaned_location_v6 %flike% ", Illinois" & extracted_EventName %flike% "Maine South HS",
                                                  "Park Ridge, Illinois",
                                                  cleaned_location_v6 %flike% ", Illinois" & extracted_EventName %flike% "McHenry County College",
                                                  "Crystal Lake, Illinois",
                                                  cleaned_location_v6 %flike% ", Illinois" & extracted_EventName %flike% "Minooka HS",
                                                  "Minooka, Illinois",
                                                  cleaned_location_v6 %flike% ", Illinois" & extracted_EventName %flike% "Montini Catholic",
                                                  "Lombard, Illinois",
                                                  cleaned_location_v6 %flike% ", Illinois" & extracted_EventName %flike% "Naperville North HS",
                                                  "Naperville, Illinois",
                                                  cleaned_location_v6 %flike% ", Illinois" & extracted_EventName %flike% "Neuqua Valley HS",
                                                  "Naperville, Illinois",
                                                  cleaned_location_v6 %flike% ", Illinois" & extracted_EventName %flike% "New Trier HS",
                                                  "Winnetka, Illinois",
                                                  cleaned_location_v6 %flike% ", Alabama" & extracted_EventName %flike% "North Jackson, North Sand Mountain and Pisgah High Schools",
                                                  "Stevenson, Alabama",
                                                  cleaned_location_v6 %flike% ", New York" & extracted_EventName %flike% "Northport High School",
                                                  "Northport, New York",
                                                  cleaned_location_v6 %flike% ", Illinois" & extracted_EventName %flike% "Oak Park and River Forest HS",
                                                  "Oak Park, Illinois",
                                                  cleaned_location_v6 %flike% ", Illinois" & extracted_EventName %flike% "Oswego East HS",
                                                  "Oswego, Illinois",
                                                  cleaned_location_v6 %flike% ", Illinois" & extracted_EventName %flike% "Prospect HS",
                                                  "Mount Prospect, Illinois",
                                                  cleaned_location_v6 %flike% ", Illinois" & extracted_EventName %flike% "Proviso Math and Science",
                                                  "Forest Park, Illinois",
                                                  cleaned_location_v6 %flike% ", Illinois" & extracted_EventName %flike% "Riverside Brookfield HS",
                                                  "Riverside, Illinois",
                                                  cleaned_location_v6 %flike% ", Illinois" & extracted_EventName %flike% "Schaumburg HS",
                                                  "Schaumburg, Illinois",
                                                  cleaned_location_v6 %flike% ", Alabama" & extracted_EventName %flike% "Section, Skyline and Woodville High Schools",
                                                  "Section, Alabama",
                                                  cleaned_location_v6 %flike% ", New York" & extracted_EventName %flike% "Smithtown High School East",
                                                  "Saint James, New York",
                                                  cleaned_location_v6 %flike% ", New York" & extracted_EventName %flike% "Smithtown High School West",
                                                  "Smithtown, New York",
                                                  cleaned_location_v6 %flike% ", Illinois" & extracted_EventName %flike% "St Charles East HS",
                                                  "Saint Charles, Illinois",
                                                  cleaned_location_v6 %flike% ", Illinois" & extracted_EventName %flike% "St Charles North HS",
                                                  "Saint Charles, Illinois",
                                                  cleaned_location_v6 %flike% ", Illinois" & extracted_EventName %flike% "St Ignatius",
                                                  "Chicago, Illinois",
                                                  cleaned_location_v6 %flike% ", New York" & extracted_EventName %flike% "St. Anthony's High School",
                                                  "South Huntington, New York",
                                                  cleaned_location_v6 %flike% ", Alabama" & extracted_EventName %flike% "St. Clair County Fair",
                                                  "Odenville, Alabama",
                                                  cleaned_location_v6 %flike% ", Connecticut" & extracted_EventName %flike% "Stamford High School Gym",
                                                  "Stamford, Connecticut",
                                                  cleaned_location_v6 %flike% ", New York" & extracted_EventName %flike% "Suffolk County Community College Health, Sports & Education Center, Grant Campus",
                                                  "Brentwood, New York",
                                                  cleaned_location_v6 %flike% ", Alabama" & extracted_EventName %flike% "Talladega County College Fair",
                                                  "Talladega, Alabama",
                                                  cleaned_location_v6 %flike% ", Tennessee" & extracted_EventName %flike% "Tennessee State University",
                                                  "Nashville, Tennessee",
                                                  cleaned_location_v6 %flike% ", Illinois" & extracted_EventName %flike% "Victor J Andrew HS",
                                                  "Tinley Park, Illinois",
                                                  cleaned_location_v6 %flike% ", New York" & extracted_EventName %flike% "Walt Whitman High School",
                                                  "Huntington Station, New York",
                                                  cleaned_location_v6 %flike% ", Illinois" & extracted_EventName %flike% "Wheaton Warrenville HS",
                                                  "Wheaton, Illinois",
                                                  cleaned_location_v6 %flike% ", Connecticut" & extracted_EventName %flike% "Wilton High School Field House",
                                                  "Wilton, Connecticut",
                                                  cleaned_location_v6 %flike% "Scottsboro High School, Alabama", "Scottsboro, Alabama",
                                                  cleaned_location_v6 %flike% "Hayward High School, California", "Hayward, California",
                                                  cleaned_location_v6 %flike% "Plano East Sr. High School, Texas", "Plano, Texas",
                                                  cleaned_location_v6 %flike% "Greenwich High School, Connecticut", "Greenwich, Connecticut",
                                                  cleaned_location_v6 %flike% "Nyack Senior High School, New York", "Nyack, New York",
                                                  cleaned_location_v6 %flike% "Ridgewood High School, New Jersey", "Ridgewood, New Jersey",
                                                  cleaned_location_v6 %flike% "Charles J Colgan Sr High School Manassas, Virginia", "Manassas, Virginia",
                                                  cleaned_location_v6 %flike% "Nolensville High School Nolensville, Tennessee", "Nolensville, Tennessee",
                                                  cleaned_location_v6 %flike% "Discovery High School Lake Alfred, Florida", "Lake Alfred, Florida",
                                                  cleaned_location_v6 %flike% "Martin Luther King, Jr. Magnet High School", "Nashville, Tennessee",
                                                  cleaned_location_v6 %flike% "Apex Friendship High School Apex, North Carolina", "Apex, North Carolina",
                                                  cleaned_location_v6 %flike% "Washington, DC National College Fair", "Washington, DC",
                                                  cleaned_location_v6 %flike% "Bryant Hall, Salem College, Winston-Salem, North Carolina", "Winston-Salem, North Carolina",
                                                  cleaned_location_v6 %flike% "Carrollton, Alabama, Alabama", "Carrollton, Alabama",
                                                  cleaned_location_v6 %flike% "Plano, Frisco, Texas", "Frisco, Texas",
                                                  cleaned_location_v6 %flike% "Naaman Forest Boulevard, Garland, Texas", "Garland, Texas",
                                                  cleaned_location_v6 %flike% "Civic Center Lane, Waxahachie, Texas", "Waxahachie, Texas",
                                                  cleaned_location_v6 %flike% "Pico Rivera, California, California", "Pico Rivera, California",
                                                  cleaned_location_v6 %flike% "Lancaster, California, California", "Lancaster, California",
                                                  cleaned_location_v6 %flike% "Monroeville, Alabama , Mississippi", "Monroeville, Alabama",
                                                  cleaned_location_v6 %flike% "Switzer Rd, Gulfport,, Mississippi", "Gulfport, Mississippi",
                                                  cleaned_location_v6 %flike% "Willow Park, Fort Worth, & Benbrook, Texas", "Forth Worth, Texas",
                                                  cleaned_location_v6 %flike% "Bridgeport, Decatur, & Wichita Falls, Texas", "Bridgeport, Texas",
                                                  cleaned_location_v6 %flike% "S Bedford Rd,Bedford, New York", "Bedford, New York",
                                                  cleaned_location_v6 %flike% "Colleyville, Grapevine, and North Richland Hills, Texas", "Colleyville, Texas",
                                                  # stopped at j for problem rows
                                                  default = cleaned_location_v6)]

# break into city and state
to_save_output_final_dt[, `:=` (
  city = str_trim(sub(",.*", "", final_location)),   # everything before the first comma
  state = str_trim(sub(".*?,\\s*", "", final_location))  # everything after the first comma
)]
# remove extra commas
to_save_output_final_dt[, state := sub("^,+\\s*", "", state)]
to_save_output_final_dt[, state := fifelse(state %flike% "DC", "District of Columbia", state)]

# keep only if state is normal
ultimate_final_dt <- to_save_output_final_dt[state != "Inc. College Fair" & 
                                               state != "Science, & Technology (AAST) College Fair" &
                                               state != "Montgomery, and Willis ISD College Night - College Fair" &
                                               state != "Your Future" &
                                               state != "(College of Engineering Open House) –" &
                                               state != "– Shelton State Community College (Martin Campus) –" &
                                               state != "(College of Communication & Information Sciences Open House) –" &
                                               state != "Inc." &
                                               state != "Career and Military Signing Day" &
                                               state != "" & 
                                               state != "San Diego, Riverside, San Bernadino Counties)" & 
                                               !(extracted_EventName %flike% "Tide Chat") & 
                                               !(extracted_EventName %flike% "Tide chat")]

ultimate_final_unique_dt <- unique(ultimate_final_dt, by = c("extracted_date","extracted_EventName"))
# see how much i dropped 
nrow(unique(ultimate_final_dt, by = c("extracted_date","extracted_EventName")))
nrow(unique(call_output_dt, by = c("extracted_date","extracted_EventName")))

# final manual cleanup 
ultimate_final_unique_dt[, city := fcase(
  city %flike% "South Houston Levee Road", "Eads",
  city == "Road", "Pike Road",
  city %flike% "N Broadway Lexington", "Lexington",
  city %flike% "TUSCALOOSA", "Tuscaloosa",
  default = city
)]

visits_by_state_year <- ultimate_final_unique_dt[,.N,.(state,extracted_year)]
visits_by_city_state_year <- ultimate_final_unique_dt[,.N,.(city,state,extracted_year)]
visits_by_year <- ultimate_final_unique_dt[,.N,.(extracted_year)]

# replace state with abbreviation
state_abbrev_map <- c(
  "Alabama" = "AL", "Alaska" = "AK", "Arizona" = "AZ", "Arkansas" = "AR", "California" = "CA",
  "Colorado" = "CO", "Connecticut" = "CT", "Delaware" = "DE", "Florida" = "FL", "Georgia" = "GA",
  "Hawaii" = "HI", "Idaho" = "ID", "Illinois" = "IL", "Indiana" = "IN", "Iowa" = "IA",
  "Kansas" = "KS", "Kentucky" = "KY", "Louisiana" = "LA", "Maine" = "ME", "Maryland" = "MD",
  "Massachusetts" = "MA", "Michigan" = "MI", "Minnesota" = "MN", "Mississippi" = "MS",
  "Missouri" = "MO", "Montana" = "MT", "Nebraska" = "NE", "Nevada" = "NV", "New Hampshire" = "NH",
  "New Jersey" = "NJ", "New Mexico" = "NM", "New York" = "NY", "North Carolina" = "NC",
  "North Dakota" = "ND", "Ohio" = "OH", "Oklahoma" = "OK", "Oregon" = "OR", "Pennsylvania" = "PA",
  "Rhode Island" = "RI", "South Carolina" = "SC", "South Dakota" = "SD", "Tennessee" = "TN",
  "Texas" = "TX", "Utah" = "UT", "Vermont" = "VT", "Virginia" = "VA", "Washington" = "WA",
  "West Virginia" = "WV", "Wisconsin" = "WI", "Wyoming" = "WY", "District of Columbia" = "DC"
)

#convert full state names to abbreviations
ultimate_final_unique_dt[, state_abbrev := state_abbrev_map[state]]

## (d) write output to data folder - not done xxx

setwd("/Users/crossancooper/Dropbox/Professional/active-projects/admissions_project")
fwrite(ultimate_final_unique_dt, file.path(getwd(), "data","full_ua_recruiting_scrape.csv"))

