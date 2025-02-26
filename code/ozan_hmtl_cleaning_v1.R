#=====================================================================
## Created by: Crossan Cooper
## Last Modified: 2-26-25

## file use: extract visits from html
#=====================================================================

#=====================================================================
# 0 - clean environment and load libraries
#=====================================================================

# default list of packages and cleaning command
rm(list=ls())
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,data.table,ggplot2,skimr,
               dplyr,fixest,ggmap,stargazer,sjmisc,
               Hmisc,tseries,DescTools,here,censusapi,RSQLite,
               tidycensus,educationdata,foreach,binsreg,
               doParallel,readxl,did,ggExtra,DBI,arrow)

#=====================================================================
# 1 - load rds files
#=====================================================================

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
test <- combined_dt[24000,2]
print(test)
