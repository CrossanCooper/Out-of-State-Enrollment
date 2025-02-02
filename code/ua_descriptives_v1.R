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



#=====================================================================
# 2 - descriptive work
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

# Compute overall year-level shares
year_summary <- ua_dt[, .(
  total_students = .N,
  out_of_state_students = sum(OutFlag),
  in_state_students = .N - sum(OutFlag)
), by = Year]

# Compute the overall in-state and out-of-state share per year
year_summary[, `:=`(
  out_of_state_share_year = out_of_state_students / total_students,
  in_state_share_year = in_state_students / total_students
)]


# Merge overall year-level shares with major-level data
major_summary <- merge(major_summary, year_summary[, .(Year, out_of_state_share_year, in_state_share_year)], 
                       by = "Year", all.x = TRUE)


# Compute the difference between major-level share and expected year-level share
major_summary[, `:=`(
  out_of_state_diff = out_of_state_share - out_of_state_share_year,
  in_state_diff = in_state_share - in_state_share_year
)]

# Aggregate to compute the average differences across all years
major_avg_summary <- major_summary[, .(
  avg_out_of_state_diff = mean(out_of_state_diff, na.rm = TRUE),
  avg_in_state_diff = mean(in_state_diff, na.rm = TRUE)
), by = DegreeGroupFinal]

# Reshape the data for plotting (long format)
melted_avg_summary <- melt(major_avg_summary, 
                           id.vars = "DegreeGroupFinal", 
                           measure.vars = c("avg_out_of_state_diff", "avg_in_state_diff"),
                           variable.name = "Student Type", 
                           value.name = "Average Difference")

# Rename `Student Type` values for clarity
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

# Compute the share of all students in each major relative to total students for that year
major_summary[, major_total_share := total_students / sum(total_students), by = Year]

# Compute the ratio of out-of-state share in a major to the overall out-of-state share in that year
major_summary[, out_of_state_ratio := out_of_state_share / out_of_state_share_year]

# Aggregate to compute the average ratio across years
major_avg_summary_two <- major_summary[, .(
  avg_out_of_state_ratio = mean(out_of_state_ratio, na.rm = TRUE),
  avg_major_total_share = mean(major_total_share, na.rm = TRUE)
), by = DegreeGroupFinal]

# Plot: X = Overall average out-of-state share, Y = Average ratio, Color = Major
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

## University Honors -- Honors College + 3.5 or higher

honors_summary <- ua_dt[Year %in% c(2008:2017, 2022:2024), .(
  total_students = .N,
  out_of_state_students = sum(OutFlag),
  in_state_students = .N - sum(OutFlag)
), by = .(Year,HonorsFlag)]

honors_summary[, `:=`(
  out_of_state_share = out_of_state_students / total_students,
  in_state_share = in_state_students / total_students
)]
