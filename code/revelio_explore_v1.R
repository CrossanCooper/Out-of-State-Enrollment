#=====================================================================
## Created by: Crossan Cooper
## Last Modified: 8-20-24

## explore revelio data
#=====================================================================

#=====================================================================
# 0 - clean environment and load libraries
#=====================================================================

# default list of packages and cleaning command
rm(list=ls())
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,data.table,ggplot2,skimr,
               dplyr,fixest,ggmap,stargazer,sjmisc,
               Hmisc,tseries,DescTools,here,censusapi,
               tidycensus,ggiplot,educationdata,foreach,
               doParallel,readxl,did,ggExtra)

#=====================================================================
# 1 - read and edit the education data
#=====================================================================

# Define the file path and chunk size
file_path <- here("revelio_data", "revelio_query_ed.csv")
chunk_size <- 2500000

# Initialize the starting row
start_row <- 1

# Initialize a flag to check if it's the first iteration
first_iteration <- TRUE

# Initialize a variable to store the correct column names
correct_col_names <- NULL

# Loop to read and filter data in chunks
repeat {
  # Read a chunk of data
  dt <- fread(file_path, nrows = chunk_size, skip = start_row - 1, header = TRUE)
  
  # Break the loop if no more rows are read
  if (nrow(dt) == 0) break
  
  # On the first pass, capture the column names
  if (first_iteration) {
    correct_col_names <- colnames(dt)
    first_iteration <- FALSE
  } else {
    # For subsequent iterations, manually set the column names
    setnames(dt, correct_col_names)
  }
  
  # Filter the data for rows where university_name contains "University of Alabama"
  filtered_dt <- dt[university_name %flike% "University of Alabama"]
  
  # Initialize or append to bama_dt
  if (start_row == 1) {
    bama_dt <- filtered_dt
  } else {
    bama_dt <- rbind(bama_dt, filtered_dt, fill = TRUE)
  }
  
  # Update the starting row for the next chunk
  start_row <- start_row + chunk_size
}

# 131,296 unique students at UA flagship
ua_dt <- bama_dt[university_name == "The University of Alabama" | university_name == "University of Alabama Department of Chemical and Biological Engineering"]

fwrite(bama_dt, here("revelio_data","bama_students.csv"))
fwrite(ua_dt, here("revelio_data","tuscaloosa_students.csv"))

# unique UA userid's

ua_ids <- ua_dt[,.N,.(user_id)]

ids_list <- ua_ids[,1]

fwrite(ids_list, here("revelio_data","tuscaloosa_student_id_list.csv"))

#=====================================================================
# 2 - read and edit the profile data (not super useful?)
#=====================================================================

profile_dt <- fread(here("revelio_data","revelio_query_profile.csv"), nrows = 1000)

#=====================================================================
# 3 - read and edit the job positions data
#=====================================================================

test_positions_dt <- fread(here("revelio_data","revelio_positions_test.csv"), nrows = 1000)