#=====================================================================
## Created by: Crossan Cooper
## Last Modified: 8-27-24

## query revelio data
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
               tidycensus,educationdata,foreach,
               doParallel,readxl,did,ggExtra, remotes, RPostgres)

#=====================================================================
# 1 - query the job positions data (Commented out)
#=====================================================================

# # 131296 unique user id's
# user_ids <- readLines(here("revelio_data","tuscaloosa_student_id_list.txt"))
# 
# user_ids_string <- paste0("'", paste(user_ids, collapse = "','"), "'")
# 
# # "tuscaloosa_students.csv" is the list of UA students
# 
# wrds <- dbConnect(Postgres(),
#                   host='wrds-pgdata.wharton.upenn.edu',
#                   port=9737,
#                   dbname='wrds',
#                   sslmode='require',
#                   user='crossancooper')
# 
# query <- paste0("
# SELECT * 
# FROM revelio_individual.individual_positions 
# WHERE user_id IN (", user_ids_string, ")")
# 
# # query a database
# res <- dbSendQuery(wrds, query)
# all_ua_jobs_data <- dbFetch(res, n = -1)
# dbClearResult(res)
# 
# # set as DT
# job_spell_ua_dt <- setDT(all_ua_jobs_data)
# 
# # 590852 job spells across 112848 individuals
# 
# fwrite(job_spell_ua_dt, here("revelio_data", "job_spells_data_ua.csv"))

#=====================================================================
# 2 - find the analysis sample
#=====================================================================

# profile_dt <- fread(here("revelio_data","revelio_query_profile.csv"), nrows = 1000)

### CHECK THE JOB SPELL DATA

job_spell_ua_dt <- fread(here("revelio_data", "job_spells_data_ua.csv"))
# select only the job spells after 2006
within_sample_ua_dt <- job_spell_ua_dt[startdate >= "2006-01-01"]

### CHECK THE EDUCATION RECORDS SAMPLE 

all_ua_students <- fread(here("revelio_data","revelio_tuscaloosa_students.csv"))
# post 2006
post_2006_students <- all_ua_students[enddate >= "2006-01-01" & enddate < "2025-01-01"]
# bachelors degree
bachelors_post_2006 <- post_2006_students[degree %flike% "Bachelor" | degree == ""]

### CHECK THE USER PROFILE DATA

ua_profiles_dt <- fread(here("revelio_data", "revelio_ua_student_names.csv"))

ua_profiles_final_dt <- ua_profiles_dt[!(fullname %flike% "" &  fullname %flike% "-")]


### LINK NAMES AND ED RECORDS

combined_names_ua_dt <- merge(bachelors_post_2006, ua_profiles_final_dt, by = "user_id")

fwrite(combined_names_ua_dt, here("revelio_data","linked_revelio_data"))