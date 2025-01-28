#=====================================================================
## Created by: Crossan Cooper
## Last Modified: 12-27-24

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
               tidycensus,ggiplot,educationdata,foreach,
               doParallel,readxl,did,ggExtra)

#=====================================================================
# 1 - read and edit data
#=====================================================================

dt_0 <- fread(here("data","CollegeAdmissions_Data.csv"))
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

# result <- dt_1[, .(out = sum(big10)), by = name]
# result <- dt_1[, .(out = sum(sec)), by = name]

#=====================================================================
# 2 - quick analysis
#=====================================================================

# OUTCOME VARIABLES FOR EACH INCOME BIN:
# rel_attend & rel_apply & rel_att_cond_app
# rel_apply_instate & rel_apply_oostate
# rel_attend_instate & rel_attend_oostate
# rel_att_cond_app_instate & rel_att_cond_app_oostate

dt_2 <- dt_1[par_income_lab != "Top 1"]

dt_3 <- dt_1[par_income_lab != "Top 0.1" & par_income_lab != "99-99.9"]

dt_3b <- dt_3

dt_3b[, Conference := fifelse(Conference == "Other", "All Other",
                              Conference)]

###### HERE ARE THE FIGURES I ACTUALLY USE ######

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

ggsave(here("figures","chetty_plot_sec_oostate.png"), plot = chetty_plot_1a,
       width = 8, height = 4.5)

chetty_plot_1b <- ggplot(dt_summary_in, aes(x = par_income_lab, y = mean_instate, fill = Conference)) +
  geom_bar(stat = "identity", position = "dodge", color = 'black', alpha = 0.8) +
  labs(x = "Parental Income Percentile",
       y = "Relative Attendance Rate",
       fill = "Conference"
  ) +
  theme_bw() + removeGridX() + scale_fill_viridis_d() + 
  annotate("rect", xmin = 6.49, xmax = 12.52, ymin = 0, ymax = 3.9, alpha = 0, size = 1, color = "red")

ggsave(here("figures","chetty_plot_sec_instate.png"), plot = chetty_plot_1b,
       width = 8, height = 4.5)
