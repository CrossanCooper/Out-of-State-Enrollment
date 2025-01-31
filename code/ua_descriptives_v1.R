#=====================================================================
## Created by: Crossan Cooper
## Last Modified: 1-31-25

## file use: explore cleaned UA data
#
## figure outputs:
# (i) xxx
# (ii) xxx
# (iii) xxx
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
               doParallel,readxl,did,ggExtra)

# set working directoru
setwd("/Users/crossancooper/Dropbox/Professional/active-projects/admissions_project")


#=====================================================================
# 1 - read and edit the cleaned commencement data
#=====================================================================

### i. read data 
ua_dt <- fread(file = here("data","linked_commencement_revelio_profile_data.csv"))

### ii. clean degree and honors information
ua_dt[, DegreeGroup := fcase(
  Degree %flike% "Business", "Business",
  Degree %flike% "Engineer" | Degree %flike% "Eng.", "Engineering",
  Degree %flike% "Computer Science", "Computer Science",
  Degree %flike% "Nursing", "Nursing",
  Degree %flike% "Social Work", "Social Work",
  Degree %flike% "Education", "Education",
  Degree %flike% "Music", "Music",
  Degree %flike% "Fine Arts", "Fine Arts",
  Degree %flike% "Chemistry", "Chemistry",
  Degree %flike% "Geology", "Geology",
  Degree %flike% "Biology" | Degree %flike% "biology", "Biology",
  Degree %flike% "Athletic Training", "Athletic Training",
  Degree %flike% "Communication", "Communication",
  Degree %flike% "Human", "Human Environmental Sciences",
  default = "General"
)]

ua_dt[, HonorsGroup := fcase(
  Honors %flike% "No" | Honors %flike% "None", "None",
  Honors %flike% "summa" | Honors %flike% "Summa", "Summa Cum Laude",
  Honors %flike% "magna" | Honors %flike% "Magna", "Magna Cum Laude",
  (Honors %flike% "cum laude" | Honors %flike% "Cum laude") & !(Honors %flike% "magna") 
  & !(Honors %flike% "summa") & !(Honors %flike% "Magna") & !(Honors %flike% "Summa"), "Cum Laude",
  default = "University Honors"
)]
