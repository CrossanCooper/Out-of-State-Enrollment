#=====================================================================
## Created by: Crossan Cooper
## Last Modified: 2-6-25

## analyze public data from chetty, friedman, deming wp
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
               doParallel,readxl,did,ggExtra,readr)

#=====================================================================
# 1 - read and edit data
#=====================================================================

### i. chetty data

dt_0 <- fread(file = file.path(getwd(),"data","CollegeAdmissions_Data.csv"))
dt_1 <- dt_0[public == "Public"]


# 29 out of 51 are public flagships

dt_1[, big10 := fifelse(
       name %flike% "Michigan State" | name %flike% "Ohio State" |
         name %flike% "Purdue" | name %flike% "Rutgers" |
         name %flike% "Iowa" | name %flike% "Michigan" | name %flike% "Oregon", 1, 0)]

dt_1[, sec := fifelse(
  name %flike% "Auburn" | name %flike% "Texas A&M" |
    name %flike% "Alabama" | name %flike% "Arkansas" |
    name %flike% "University Of Florida" | name %flike% "University Of Georgia" | 
    name %flike% "University Of Texas" | name %flike% "Oklahoma" | 
    name %flike% "Kentucky" | name %flike% "Mississippi" , 1, 0)]

dt_1[, Conference := fcase(
  sec == 1, "SEC",
  big10 == 1, "Big 10",
  sec == 0 & big10 == 0, "Other"
)]

### iii. appropriations data 

appropriations_dt <- fread(file = file.path(getwd(),"data","appropriations_data.csv"))
appropriations_sample_dt <- appropriations_dt[FY >= 2011 & FY <= 2015]

appropriations_sample_dt[, StateApp := parse_number(`Total State Support`)]
appropriations_sample_dt[, Enrollment := parse_number(`Net FTE Enrollment`)]
appropriations_sample_dt[, AppPerStudent := StateApp/Enrollment]
appropriations_sample_dt[, CPI := `CPI (Inflation) Adjustment`]

app_dt <- appropriations_sample_dt[,.(State,FY,CPI,StateApp,Enrollment,AppPerStudent)]

app_dt_agg <- app_dt[
  , .(
    avgStateApp      = mean(StateApp, na.rm = TRUE),
    avgAppPerStudent = mean(AppPerStudent, na.rm = TRUE)
  )
  , by = .(State)
]

app_dt_agg[, LogApp := log(avgStateApp)]
app_dt_agg[, LogAppPerStudent := log(avgAppPerStudent)]


#=====================================================================
# 2 - plots of attendance by income
#=====================================================================

### i. limit data

## OUTCOME VARIABLES FOR EACH INCOME BIN:
# rel_attend & rel_apply & rel_att_cond_app
# rel_apply_instate & rel_apply_oostate
# rel_attend_instate & rel_attend_oostate
# rel_att_cond_app_instate & rel_att_cond_app_oostate

dt_2 <- dt_1[par_income_lab != "Top 1"]

dt_3 <- dt_1[par_income_lab != "Top 0.1" & par_income_lab != "99-99.9"]

dt_3b <- dt_3

dt_3b[, Conference := fifelse(Conference == "Other", "All Other",
                              Conference)]

### ii. attendance rate plots

dt_summary_oo <- dt_3b[, .(mean_oostate = mean(rel_attend_oostate, na.rm = T)), by = .(Conference, par_income_lab)]
dt_summary_in <- dt_3b[, .(mean_instate = mean(rel_attend_instate, na.rm = T)), by = .(Conference, par_income_lab)]


chetty_plot_1a <- ggplot(dt_summary_oo, aes(x = par_income_lab, y = mean_oostate, fill = Conference)) +
  geom_bar(stat = "identity", position = "dodge", color = 'black', alpha = 0.8) +
  labs(x = "Parental Income Percentile",
    y = "Relative Attendance Rate",
    fill = "Conference"
  ) +
  theme_bw() + removeGridX() + scale_fill_viridis_d() + 
  annotate("rect", xmin = 6.49, xmax = 12.52, ymin = 0, ymax = 3.9, alpha = 0, size = 1, color = "red")

print(chetty_plot_1a)

ggsave(file.path(getwd(),"figures","chetty_plot_sec_oostate.png"), plot = chetty_plot_1a,
       width = 8, height = 4.5)

chetty_plot_1b <- ggplot(dt_summary_in, aes(x = par_income_lab, y = mean_instate, fill = Conference)) +
  geom_bar(stat = "identity", position = "dodge", color = 'black', alpha = 0.8) +
  labs(x = "Parental Income Percentile",
       y = "Relative Attendance Rate",
       fill = "Conference"
  ) +
  theme_bw() + removeGridX() + scale_fill_viridis_d() + 
  annotate("rect", xmin = 6.49, xmax = 12.52, ymin = 0, ymax = 3.9, alpha = 0, size = 1, color = "red")

print(chetty_plot_1b)

ggsave(file.path(getwd(),"figures","chetty_plot_sec_instate.png"), plot = chetty_plot_1b,
       width = 8, height = 4.5)

### iii. application rate plots

dt_summary_oo_apply <- dt_3b[, .(mean_oostate = mean(rel_apply_oostate, na.rm = T)), by = .(Conference, par_income_lab)]

chetty_plot_1c <- ggplot(dt_summary_oo_apply, aes(x = par_income_lab, y = mean_oostate, fill = Conference)) +
  geom_bar(stat = "identity", position = "dodge", color = 'black', alpha = 0.8) +
  labs(x = "Parental Income Percentile",
       y = "Relative Application Rate",
       fill = "Conference"
  ) +
  theme_bw() + removeGridX() + scale_fill_viridis_d() + 
  annotate("rect", xmin = 6.49, xmax = 12.52, ymin = 0, ymax = 3.9, alpha = 0, size = 1, color = "red")

print(chetty_plot_1c)

dt_summary_in_apply <- dt_3b[, .(mean_instate = mean(rel_apply_instate, na.rm = T)), by = .(Conference, par_income_lab)]

chetty_plot_1d <- ggplot(dt_summary_in_apply, aes(x = par_income_lab, y = mean_instate, fill = Conference)) +
  geom_bar(stat = "identity", position = "dodge", color = 'black', alpha = 0.8) +
  labs(x = "Parental Income Percentile",
       y = "Relative Application Rate",
       fill = "Conference"
  ) +
  theme_bw() + removeGridX() + scale_fill_viridis_d() + 
  annotate("rect", xmin = 6.49, xmax = 12.52, ymin = 0, ymax = 3.9, alpha = 0, size = 1, color = "red")

print(chetty_plot_1d)

### iv. attendance rate plots -- by school in SEC

chetty_plot_school_out <- ggplot(dt_3b[Conference == "SEC"], aes(x = par_income_lab, y = rel_attend_oostate, fill = name)) +
  geom_bar(stat = "identity", position = "dodge", color = 'black', alpha = 0.8) +
  labs(x = "Parental Income Percentile",
       y = "Relative Attendance Rate",
       fill = "University"
  ) +
  theme_bw() + removeGridX() + scale_fill_brewer(palette = 'Paired') + 
  annotate("rect", xmin = 6.49, xmax = 12.52, ymin = 0, ymax = 6.03, alpha = 0, size = 1, color = "red")

print(chetty_plot_school_out)


chetty_plot_school_in <- ggplot(dt_3b[Conference == "SEC"], aes(x = par_income_lab, y = rel_attend_instate, fill = name)) +
  geom_bar(stat = "identity", position = "dodge", color = 'black', alpha = 0.8) +
  labs(x = "Parental Income Percentile",
       y = "Relative Attendance Rate",
       fill = "University"
  ) +
  theme_bw() + removeGridX() + scale_fill_brewer(palette = 'Paired') + 
  annotate("rect", xmin = 6.49, xmax = 12.52, ymin = 0, ymax = 6.03, alpha = 0, size = 1, color = "red")

print(chetty_plot_school_in)

### v. applicaton rate plots -- by school in SEC

chetty_plot_school_out_app <- ggplot(dt_3b[Conference == "SEC"], aes(x = par_income_lab, y = rel_apply_oostate, fill = name)) +
  geom_bar(stat = "identity", position = "dodge", color = 'black', alpha = 0.8) +
  labs(x = "Parental Income Percentile",
       y = "Relative Application Rate",
       fill = "University"
  ) +
  theme_bw() + removeGridX() + scale_fill_brewer(palette = 'Paired') + 
  annotate("rect", xmin = 6.49, xmax = 12.52, ymin = 0, ymax = 4.24, alpha = 0, size = 1, color = "red")

print(chetty_plot_school_out_app)


chetty_plot_school_in_app <- ggplot(dt_3b[Conference == "SEC"], aes(x = par_income_lab, y = rel_apply_instate, fill = name)) +
  geom_bar(stat = "identity", position = "dodge", color = 'black', alpha = 0.8) +
  labs(x = "Parental Income Percentile",
       y = "Relative Application Rate",
       fill = "University"
  ) +
  theme_bw() + removeGridX() + scale_fill_brewer(palette = 'Paired') + 
  annotate("rect", xmin = 6.49, xmax = 12.52, ymin = 0, ymax = 4.11, alpha = 0, size = 1, color = "red")

print(chetty_plot_school_in_app)


#=====================================================================
# 3 - high income enrollment and appropriations
#=====================================================================

### i. add state to chetty data 

chetty_top1_dt <- dt_3b[par_income_lab %flike% "Top 1"]

chetty_top1_dt[, State := fcase(
  name == "Auburn University", "Alabama",
  name == "Clemson University", "South Carolina",
  name == "Colorado School Of Mines", "Colorado",
  name == "Georgia Institute Of Technology", "Georgia",
  name == "North Carolina State University", "North Carolina",
  name == "Purdue University", "Indiana",
  name == "State University Of New York At Buffalo", "New York",
  name == "Texas A&M University", "Texas",
  name == "University Of Arkansas", "Arkansas",
  name == "University Of California, Davis", "California",
  name == "University Of California, Los Angeles", "California",
  name == "University Of California, San Diego", "California",
  name == "University Of California, Santa Cruz", "California",
  name == "University Of Delaware", "Delaware",
  name == "University Of Georgia", "Georgia",
  name == "University Of Iowa", "Iowa",
  name == "University Of Kentucky", "Kentucky",
  name == "University Of Mississippi", "Mississippi",
  name %flike% "University Of Nevada", "Nevada",
  name == "University Of New Mexico", "New Mexico",
  name == "University Of North Dakota", "North Dakota",
  name == "University Of Oregon", "Oregon",
  name == "University Of Rhode Island", "Rhode Island",
  name == "University Of Texas At Austin", "Texas",
  name == "University Of Virginia", "Virginia",
  name == "Virginia Polytechnic Institute & State University", "Virginia",
  name == "Binghamton University", "New York",
  name == "College Of William & Mary", "Virginia",
  name == "Florida State University", "Florida",
  name == "Michigan State University", "Michigan",
  name == "Ohio State University", "Ohio",
  name == "Rutgers, The State University Of New Jersey", "New Jersey",
  name == "State University Of New York At Stony Brook", "New York",
  name == "University Of Alabama", "Alabama",
  name == "University Of California, Berkeley", "California",
  name == "University Of California, Irvine", "California",
  name == "University Of California, Riverside", "California",
  name == "University Of California, Santa Barbara", "California",
  name == "University Of Connecticut", "Connecticut",
  name == "University Of Florida", "Florida",
  name == "University Of Idaho", "Idaho",
  name == "University Of Kansas", "Kansas",
  name == "University Of Michigan - Ann Arbor", "Michigan",
  name == "University Of Montana", "Montana",
  name == "University Of New Hampshire", "New Hampshire",
  name == "University Of North Carolina - Chapel Hill", "North Carolina",
  name == "University Of Oklahoma", "Oklahoma",
  name == "University Of Pittsburgh System", "Pennsylvania",
  name == "University Of South Florida", "Florida",
  name == "University Of Utah", "Utah",
  name == "University Of Wyoming", "Wyoming",
  default = NA_character_
)]

### ii. merge chetty and agg data 

merged_app_chetty_dt <- merge(app_dt_agg, chetty_top1_dt, by = "State")

merged_app_chetty_dt[, avgAppPerStudent := avgAppPerStudent/1000]

## (a) out of state

feols(rel_attend_oostate ~ avgAppPerStudent, data = merged_app_chetty_dt, vcov = ~State)
feols(rel_attend_oostate ~ LogAppPerStudent, data = merged_app_chetty_dt, vcov = ~State)

feols(rel_apply_oostate ~ avgAppPerStudent, data = merged_app_chetty_dt, vcov = ~State)
feols(rel_apply_oostate ~ LogAppPerStudent, data = merged_app_chetty_dt, vcov = ~State)

## (b) in state 

feols(rel_attend_instate ~ avgAppPerStudent, data = merged_app_chetty_dt, vcov = ~State)
feols(rel_attend_instate ~ LogAppPerStudent, data = merged_app_chetty_dt, vcov = ~State)


feols(rel_apply_instate ~ avgAppPerStudent, data = merged_app_chetty_dt, vcov = ~State)
feols(rel_apply_instate ~ LogAppPerStudent, data = merged_app_chetty_dt, vcov = ~State)



