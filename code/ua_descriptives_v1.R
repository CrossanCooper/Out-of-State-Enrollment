#=====================================================================
## Created by: Crossan Cooper
## Last Modified: 2-4-25

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
               tidycensus,educationdata,foreach,binsreg,
               doParallel,readxl,did,ggExtra)

# set working directoru
setwd("/Users/crossancooper/Dropbox/Professional/active-projects/admissions_project")


#=====================================================================
# 1 - read and edit the cleaned commencement data
#=====================================================================

### i. read data 
ua_dt <- fread(file = here("data","all_alabama_data.csv"))

### ii. clean degree and honors information
ua_dt[, DegreeGroup := fcase(
  Degree %flike% "Business", "Business",
  Degree %flike% "Computer", "Computer Science",
  Degree %flike% "Music", "Music",
  Degree %flike% "Engineer" | Degree %flike% "Eng.", "Engineering",
  Degree %flike% "Nursing", "Nursing",
  Degree %flike% "Social Work", "Social Work",
  Degree %flike% "Education", "Education",
  Degree %flike% "Fine Arts", "Fine Arts",
  Degree %flike% "Communication", "Communication and Information Sciences",
  Degree %flike% "Human" | Degree %flike% "Athletic", "Human Environmental Sciences",
  default = "General"
)]

ua_dt[, DegreeGroupFinal := fcase(
  DegreeGroup == "General" & Degree %flike% "Arts", "BA - General", 
  DegreeGroup == "General" & Degree %flike% "Science", "BS - General", 
  DegreeGroup == "General" & Degree %flike% "Science", "BS - General", 
  DegreeGroup == "Engineering" & Degree %flike% "Construction", "Construction Engineering",
  DegreeGroup == "Engineering" & Degree %flike% "Aerospace", "Aerospace Engineering",
  DegreeGroup == "Engineering" & Degree %flike% "Chemical", "Chemical Engineering",
  DegreeGroup == "Engineering" & Degree %flike% "Civil", "Civil Engineering",
  DegreeGroup == "Engineering" & Degree %flike% "Electric", "Electrical Engineering",
  DegreeGroup == "Engineering" & Degree %flike% "Industrial", "Industrial Engineering",
  DegreeGroup == "Engineering" & Degree %flike% "Mechanical", "Mechanical Engineering",
  DegreeGroup == "Engineering" & Degree %flike% "Metallurgical", "Metallurgical Engineering",
  DegreeGroup == "Engineering" & Degree %flike% "Architectural", "Architectural Engineering",
  DegreeGroup == "Engineering" & Degree %flike% "Environmental", "Environmental Engineering",
  default = DegreeGroup
)]

ua_dt[, HonorsGroup := fcase(
  Honors %flike% "No" | Honors %flike% "None", "None",
  Honors %flike% "Yes", "Yes",
  Honors %flike% "summa" | Honors %flike% "Summa", "Summa Cum Laude",
  Honors %flike% "magna" | Honors %flike% "Magna", "Magna Cum Laude",
  (Honors %flike% "cum laude" | Honors %flike% "Cum laude") & !(Honors %flike% "magna") 
  & !(Honors %flike% "summa") & !(Honors %flike% "Magna") & !(Honors %flike% "Summa"), "Cum Laude",
  default = "University Honors"
)]

ua_dt[, HonorsFlag := fifelse(
  HonorsGroup != "None", 1, 0
)]

ua_dt[, HonorsFlag := fifelse(
  HonorsGroup != "None", 1, 0
)]



### iii. add out of state indicator

ua_dt[, OutFlag := fifelse(`Origin State` != "AL", 1, 0)]

### iv. calculate OOS shares over time 

out_of_state_by_year <- ua_dt[, .(
  total_students = .N,
  out_of_state_students = sum(OutFlag),
  out_of_state_share = sum(OutFlag) / .N
), by = Year]

### v. clean and extract state and town name from origin state / town

ua_dt[, OriginState := trimws(gsub("[.,]", "", `Origin State`))]

ua_dt[, OriginTown := trimws(gsub(",.*", "", `Origin Town`))] # Remove everything after the first comma
ua_dt[, OriginTown := gsub("([A-Z])([A-Z])", "\\1 \\2", OriginTown)] # Add space between consecutive capital letters


#=====================================================================
# 2 - descriptive work -- honors and major choice
#=====================================================================

### i. major choice

## (a) by year

major_summary <- ua_dt[, .(
  total_students = .N,
  out_of_state_students = sum(OutFlag),
  in_state_students = .N - sum(OutFlag)
), by = .(Year,DegreeGroupFinal)]

major_summary[, `:=`(
  out_of_state_share = out_of_state_students / total_students,
  in_state_share = in_state_students / total_students
)]

## (b) relative to enrollment share

# compute overall year-level shares
year_summary <- ua_dt[, .(
  total_students = .N,
  out_of_state_students = sum(OutFlag),
  in_state_students = .N - sum(OutFlag)
), by = Year]

# compute the overall in-state and out-of-state share per year
year_summary[, `:=`(
  out_of_state_share_year = out_of_state_students / total_students,
  in_state_share_year = in_state_students / total_students
)]


# merge overall year-level shares with major-level data
major_summary <- merge(major_summary, year_summary[, .(Year, out_of_state_share_year, in_state_share_year)], 
                       by = "Year", all.x = TRUE)


# compute the difference between major-level share and expected year-level share
major_summary[, `:=`(
  out_of_state_diff = out_of_state_share - out_of_state_share_year,
  in_state_diff = in_state_share - in_state_share_year
)]

# aggregate to compute the average differences across all years
major_avg_summary <- major_summary[, .(
  avg_out_of_state_diff = mean(out_of_state_diff, na.rm = TRUE),
  avg_in_state_diff = mean(in_state_diff, na.rm = TRUE)
), by = DegreeGroupFinal]

# reshape the data for plotting (long format)
melted_avg_summary <- melt(major_avg_summary, 
                           id.vars = "DegreeGroupFinal", 
                           measure.vars = c("avg_out_of_state_diff", "avg_in_state_diff"),
                           variable.name = "Student Type", 
                           value.name = "Average Difference")

# rename `Student Type` values for clarity
melted_avg_summary[, `Student Type` := fcase(
  `Student Type` == "avg_out_of_state_diff", "Out-of-State",
  `Student Type` == "avg_in_state_diff", "In-State"
)]


ggplot(melted_avg_summary[`Student Type` == 'Out-of-State'], aes(x = DegreeGroupFinal, y = `Average Difference`, fill = `Student Type`)) +
  geom_col(position = "dodge",color = 'black', alpha = 1) +  # Use bars for comparison
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(y = "", 
       fill = "Student Type") +
  theme_bw() + removeGridX() + 
  scale_fill_viridis_d() + 
 #  scale_fill_manual(values = c("Out-of-State" = "#FDE725FF", "In-State" = "#440154FF")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(
    legend.position = "bottom",
    legend.background = element_rect(color = "black", linetype = "solid", linewidth = 0.25),
    text = element_text(size = 12)) 

## (c) different visualization

# compute the share of all students in each major relative to total students for that year
major_summary[, major_total_share := total_students / sum(total_students), by = Year]

# compute the ratio of out-of-state share in a major to the overall out-of-state share in that year
major_summary[, out_of_state_ratio := out_of_state_share / out_of_state_share_year]

# aggregate to compute the average ratio across years
major_avg_summary_two <- major_summary[, .(
  avg_out_of_state_ratio = mean(out_of_state_ratio, na.rm = TRUE),
  avg_major_total_share = mean(major_total_share, na.rm = TRUE)
), by = DegreeGroupFinal]


ggplot(major_avg_summary_two, aes(x = DegreeGroupFinal, y = avg_out_of_state_ratio, fill = avg_major_total_share)) +
  geom_col(color = "black", alpha = 0.6, fill = '#440154FF') +  # Use bars with black borders
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +  # Reference line at 1
  labs(
    y = "Participation Ratio"
  ) +
  theme_bw() + removeGridX() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

### ii. honors

## university honors -- Honors College + 3.5 or higher

honors_summary <- ua_dt[Year %in% c(2008:2017, 2022:2024), .(
  total_students = .N,
  out_of_state_students = sum(OutFlag),
  in_state_students = .N - sum(OutFlag)
), by = .(Year,HonorsFlag)]

honors_summary[, `:=`(
  out_of_state_share = out_of_state_students / total_students,
  in_state_share = in_state_students / total_students
)]

#=====================================================================
# 3 - read and edit the recruiting data
#=====================================================================

### i. read data 

recruiting_dt <- fread(file = here("data","recruiting_data_ozan.csv"))

# 2017 calendar year -- students would matriculate in 2018 and graduate in 2022
ua_recruiting_dt <- recruiting_dt[univ_id == 100751]

# 67.5% public hs, 31.9% private hs
ua_hs_visit_dt <- ua_recruiting_dt[categorized_event_type %flike% "hsvisit"]

### ii. merge with ua_dt

ua_dt[, TownName := OriginTown]
ua_dt[, StateName := OriginState]

ua_2022_dt <- ua_dt[Year == 2022 & StateName != "International" & TownName != ""]

ua_counts_dt <- ua_2022_dt[,.N,.(TownName, StateName)]

setnames(ua_counts_dt, "N", "UACount")

ua_hs_visit_dt[, TownName := event_city]
ua_hs_visit_dt[, StateName := event_state]

visit_counts_dt <- ua_hs_visit_dt[TownName != "",.N,.(TownName, StateName)]

setnames(visit_counts_dt, "N", "VisitCount")

merged_ua_hs_visit_dt <- merge(ua_counts_dt, visit_counts_dt, by = c("TownName", "StateName"), all.x = T, all.y = T)

merged_ua_hs_visit_dt[, VisitCount := fifelse(is.na(VisitCount), 0, VisitCount)]
merged_ua_hs_visit_dt[, UACount := fifelse(is.na(UACount), 0, UACount)]

merged_ua_hs_visit_dt[, InAL := fifelse(StateName == "AL", 1, 0)]

feols(UACount ~ VisitCount | InAL, data = merged_ua_hs_visit_dt, vcov = 'hc1')

feols(UACount ~ VisitCount, data = merged_ua_hs_visit_dt[InAL == 0], vcov = 'hc1')


