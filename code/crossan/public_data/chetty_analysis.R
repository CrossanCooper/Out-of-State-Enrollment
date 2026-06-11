#=====================================================================
## Created by: Crossan Cooper
## Last Modified: 4-9-26

## analyze public data from chetty, friedman, deming wp for 
## section 3 regressions + motivating figures
#
## inputs:
## 1. CollegeAdmissions_Data.csv -- opportunity insights public data
## 2. appropriations_data.csv -- SHEEO state appropriations data
## 3. xxx - IPEDS appropriations/state support data
#
## outputs:
## -- a lot of figures (need to itemize them)
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

# set working directory
setwd("/Users/crossancooper/Dropbox/Professional/active-projects/admissions_project")

#=====================================================================
# 1 - read and edit data
#=====================================================================

### i. read in chetty et al. institution-level data

# the baseline file contains 139 colleges and universities
dt_0 <- fread(file = file.path(getwd(),"data","CollegeAdmissions_Data.csv"))
# limit to just the 51 public universities (at least 29 of them are flagships)
dt_1 <- dt_0[public == "Public"]

# big ten conference indicator (note: ucla is a recent add to this script, was missing them)
dt_1[, big10 := fifelse(
       name %flike% "Michigan State" | name %flike% "Ohio State" |
         name %flike% "Purdue" | name %flike% "Rutgers" | name %flike% "University Of California, Los Angeles" |
         name %flike% "Iowa" | name %flike% "Michigan" | name %flike% "Oregon", 1, 0)]
# sec indicator
dt_1[, sec := fifelse(
  name %flike% "Auburn" | name %flike% "Texas A&M" |
    name %flike% "Alabama" | name %flike% "Arkansas" |
    name %flike% "University Of Florida" | name %flike% "University Of Georgia" | 
    name %flike% "University Of Texas" | name %flike% "Oklahoma" | 
    name %flike% "Kentucky" | name %flike% "Mississippi" , 1, 0)]
# general conference categorical variable
dt_1[, Conference := fcase(
  sec == 1, "SEC",
  big10 == 1, "Big 10",
  sec == 0 & big10 == 0, "Other"
)]

### ii. read in appropriations data and limit to 2011-2015 fiscal years 

appropriations_dt <- fread(file = file.path(getwd(),"data","appropriations_data.csv"))
appropriations_sample_dt <- appropriations_dt[FY >= 2011 & FY <= 2015]

appropriations_sample_dt[, StateApp := parse_number(`Total State Support`)]
appropriations_sample_dt[, Enrollment := parse_number(`Net FTE Enrollment`)]
appropriations_sample_dt[, AppPerStudent := StateApp/Enrollment]
appropriations_sample_dt[, CPI := as.numeric(`CPI (Inflation) Adjustment`)]
appropriations_sample_dt[, HECA := as.numeric(`HECA (Inflation) Adjustment`)]

app_dt <- appropriations_sample_dt[,.(State, FY, CPI, HECA, StateApp, Enrollment, AppPerStudent)]

# average state support values over 2011-2015 period
app_dt_agg <- app_dt[, .(
    avgStateApp      = mean(StateApp, na.rm = TRUE),
    avgAppPerStudent = mean(AppPerStudent, na.rm = TRUE))
  , by = .(State)]

# take the log of the average state support amounts
app_dt_agg[, LogApp := log(avgStateApp)]
app_dt_agg[, LogAppPerStudent := log(avgAppPerStudent)]


#=====================================================================
# 2 - plots of attendance by income
#=====================================================================

### i. limit the data to the desired income bins

## (a) list of outcome variables for each income bin - 
# pooled: rel_attend & rel_apply & rel_att_cond_app
# apply by residence: rel_apply_instate & rel_apply_oostate
# attend by residence: rel_attend_instate & rel_attend_oostate
# attend conditional on apply by residence: rel_att_cond_app_instate & rel_att_cond_app_oostate

# remove top 1%
dt_2 <- dt_1[par_income_lab != "Top 1"]
# remove the split between top 0.1% and 99-99.9%
dt_3 <- dt_1[par_income_lab != "Top 0.1" & par_income_lab != "99-99.9"]
# rename this second file
dt_3b <- dt_3
# rename one of the conference groups
dt_3b[, Conference := fifelse(Conference == "Other", "All Other",
                              Conference)]

### ii. attendance rate plots

## (a) average the relative attendance rates for in-state and out-of-state by conference and income bin
dt_summary_oo <- dt_3b[, .(mean_oostate = mean(rel_attend_oostate, na.rm = T)), by = .(Conference, par_income_lab)]
dt_summary_in <- dt_3b[, .(mean_instate = mean(rel_attend_instate, na.rm = T)), by = .(Conference, par_income_lab)]

## (b) use these to make the first plot (out-of-state)
chetty_plot_1a <- ggplot(dt_summary_oo, aes(x = par_income_lab, y = mean_oostate, fill = Conference)) +
  geom_bar(stat = "identity", position = "dodge", color = 'black', alpha = 0.8) +
  labs(x = "Parental Income Percentile",
    y = "Relative Attendance Rate",
    fill = "Conference"
  ) +
  theme_minimal() + removeGridX() + scale_fill_viridis_d() + 
  annotate("rect", xmin = 6.49, xmax = 12.52, ymin = 0, ymax = 3.9, alpha = 0, size = 1, color = "red") +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background  = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA)
  )

print(chetty_plot_1a)

## (c) save the plot
ggsave(file.path(getwd(),"figures","chetty_plot_sec_oostate.png"), plot = chetty_plot_1a,
       width = 8, height = 4.5)

## (b) use the average to make the first plot (in-state)
chetty_plot_1b <- ggplot(dt_summary_in, aes(x = par_income_lab, y = mean_instate, fill = Conference)) +
  geom_bar(stat = "identity", position = "dodge", color = 'black', alpha = 0.8) +
  labs(x = "Parental Income Percentile",
       y = "Relative Attendance Rate",
       fill = "Conference"
  ) +
  theme_minimal() + removeGridX() + scale_fill_viridis_d() + 
  annotate("rect", xmin = 6.49, xmax = 12.52, ymin = 0, ymax = 3.9, alpha = 0, size = 1, color = "red") +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background  = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA)
  )

print(chetty_plot_1b)

ggsave(file.path(getwd(),"figures","chetty_plot_sec_instate.png"), plot = chetty_plot_1b,
       width = 8, height = 4.5)

### iii. application rate plots

## (a) out-of-state
dt_summary_oo_apply <- dt_3b[, .(mean_oostate = mean(rel_apply_oostate, na.rm = T)), by = .(Conference, par_income_lab)]

chetty_plot_1c <- ggplot(dt_summary_oo_apply, aes(x = par_income_lab, y = mean_oostate, fill = Conference)) +
  geom_bar(stat = "identity", position = "dodge", color = 'black', alpha = 0.8) +
  labs(x = "Parental Income Percentile",
       y = "Relative Application Rate",
       fill = "Conference"
  ) +
  theme_minimal() + removeGridX() + scale_fill_viridis_d() + 
  annotate("rect", xmin = 6.49, xmax = 12.52, ymin = 0, ymax = 3.9, alpha = 0, size = 1, color = "red") +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background  = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA)
  )

print(chetty_plot_1c)

ggsave(file.path(getwd(),"figures","descriptive-figs","chetty_plot_oostate_app.png"), plot = chetty_plot_1c,
       width = 8, height = 4.5)

## (b) in-state
dt_summary_in_apply <- dt_3b[, .(mean_instate = mean(rel_apply_instate, na.rm = T)), by = .(Conference, par_income_lab)]

chetty_plot_1d <- ggplot(dt_summary_in_apply, aes(x = par_income_lab, y = mean_instate, fill = Conference)) +
  geom_bar(stat = "identity", position = "dodge", color = 'black', alpha = 0.8) +
  labs(x = "Parental Income Percentile",
       y = "Relative Application Rate",
       fill = "Conference"
  ) +
  theme_minimal() + removeGridX() + scale_fill_viridis_d() + 
  annotate("rect", xmin = 6.49, xmax = 12.52, ymin = 0, ymax = 3.9, alpha = 0, size = 1, color = "red") +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background  = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA)
  )

print(chetty_plot_1d)

ggsave(file.path(getwd(),"figures","descriptive-figs","chetty_plot_instate_app.png"), plot = chetty_plot_1d,
       width = 8, height = 4.5)

### iv. attendance rate plots -- by school in SEC

## (a) out-of-state by school
chetty_plot_school_out <- ggplot(dt_3b[Conference == "SEC"], aes(x = par_income_lab, y = rel_attend_oostate, fill = name)) +
  geom_bar(stat = "identity", position = "dodge", color = 'black', alpha = 0.8) +
  labs(x = "Parental Income Percentile",
       y = "Relative Attendance Rate",
       fill = "University"
  ) +
  theme_minimal() + removeGridX() + scale_fill_brewer(palette = 'Paired') + 
  annotate("rect", xmin = 6.49, xmax = 12.52, ymin = 0, ymax = 6.03, alpha = 0, size = 1, color = "red") +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background  = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA)
  )

print(chetty_plot_school_out)

## (b) in-state by school
chetty_plot_school_in <- ggplot(dt_3b[Conference == "SEC"], aes(x = par_income_lab, y = rel_attend_instate, fill = name)) +
  geom_bar(stat = "identity", position = "dodge", color = 'black', alpha = 0.8) +
  labs(x = "Parental Income Percentile",
       y = "Relative Attendance Rate",
       fill = "University"
  ) +
  theme_minimal() + removeGridX() + scale_fill_brewer(palette = 'Paired') + 
  annotate("rect", xmin = 6.49, xmax = 12.52, ymin = 0, ymax = 6.03, alpha = 0, size = 1, color = "red") +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background  = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA)
  )

print(chetty_plot_school_in)

### v. applicaton rate plots -- by school in SEC

## (a) out-of-state 
chetty_plot_school_out_app <- ggplot(dt_3b[Conference == "SEC"], aes(x = par_income_lab, y = rel_apply_oostate, fill = name)) +
  geom_bar(stat = "identity", position = "dodge", color = 'black', alpha = 0.8) +
  labs(x = "Parental Income Percentile",
       y = "Relative Application Rate",
       fill = "University"
  ) +
  theme_minimal() + removeGridX() + scale_fill_brewer(palette = 'Paired') + 
  annotate("rect", xmin = 6.49, xmax = 12.52, ymin = 0, ymax = 4.24, alpha = 0, size = 1, color = "red") +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background  = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA)
  )

print(chetty_plot_school_out_app)

## (b) in-state
chetty_plot_school_in_app <- ggplot(dt_3b[Conference == "SEC"], aes(x = par_income_lab, y = rel_apply_instate, fill = name)) +
  geom_bar(stat = "identity", position = "dodge", color = 'black', alpha = 0.8) +
  labs(x = "Parental Income Percentile",
       y = "Relative Application Rate",
       fill = "University"
  ) +
  theme_minimal() + removeGridX() + scale_fill_brewer(palette = 'Paired') + 
  annotate("rect", xmin = 6.49, xmax = 12.52, ymin = 0, ymax = 4.11, alpha = 0, size = 1, color = "red") +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background  = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA)
  )

print(chetty_plot_school_in_app)

#=====================================================================
# 3 - high income enrollment and appropriations
#=====================================================================

### i. add state of university to chetty data 

## (a) limit to just top 1% income bin
chetty_top1_dt <- dt_3b[par_income_lab %flike% "Top 1"]

## (b) add the state for each university (not needed with ipeds data xxx)
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

### ii. merge chetty and appropriations data (redo with the ipeds data xxx)

merged_app_chetty_dt <- merge(app_dt_agg, chetty_top1_dt, by = "State")
# set the average per student to the average in 1000s
merged_app_chetty_dt[, avgAppPerStudent := avgAppPerStudent/1000]

## (a) out of state regressions

out_state_attend_avg_model <- feols(rel_attend_oostate ~ avgAppPerStudent, data = merged_app_chetty_dt, vcov = ~State)
out_state_attend_log_model <- feols(rel_attend_oostate ~ LogAppPerStudent, data = merged_app_chetty_dt, vcov = ~State)

summary(out_state_attend_avg_model)
summary(out_state_attend_log_model)

mean_out_attend <- mean(merged_app_chetty_dt[,rel_attend_oostate],na.rm = T)
mean_out_apply  <- mean(merged_app_chetty_dt[,rel_apply_oostate],na.rm = T)

out_state_apply_avg_model <- feols(rel_apply_oostate ~ avgAppPerStudent, data = merged_app_chetty_dt, vcov = ~State)
out_state_apply_log_model <- feols(rel_apply_oostate ~ LogAppPerStudent, data = merged_app_chetty_dt, vcov = ~State)

summary(out_state_apply_avg_model)
summary(out_state_apply_log_model)

## (b) in state regressions

in_state_attend_avg_model <- feols(rel_attend_instate ~ avgAppPerStudent, data = merged_app_chetty_dt, vcov = ~State)
in_state_attend_log_model <- feols(rel_attend_instate ~ LogAppPerStudent, data = merged_app_chetty_dt, vcov = ~State)

summary(in_state_attend_avg_model)
summary(in_state_attend_log_model)

mean_in_attend <- mean(merged_app_chetty_dt[,rel_attend_instate],na.rm = T)
mean_in_apply  <- mean(merged_app_chetty_dt[,rel_apply_instate],na.rm = T)

in_state_apply_avg_model <- feols(rel_apply_instate ~ avgAppPerStudent, data = merged_app_chetty_dt, vcov = ~State)
in_state_apply_log_model <- feols(rel_apply_instate ~ LogAppPerStudent, data = merged_app_chetty_dt, vcov = ~State)

summary(in_state_apply_avg_model)
summary(in_state_apply_log_model)
