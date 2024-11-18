#=====================================================================
## Created by: Crossan Cooper
## Last Modified: 11-15-24

## file use: process alabama data (all commencements 2006-2024)
# and merge commencement data with Revelio data
#
## data outputs:
# (i) data/all_alabama_data.csv -- all cleaned commencement data
# (ii) data/cleaned_ua_revelio.csv -- all cleaned Revelio (UA) data
#
## figure outputs:
# (i) figures/trends_enrollment_area.png -- enrollment trends as area plot
# (ii) figures/trends_enrollment.png -- enrollment trends as bar plot
# (iii) figures/oos_shares_plot.png -- 2006 vs. 2023 oos student shares by state
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

#=====================================================================
# 1 - read and edit the spring commencement data
#=====================================================================

bama_2018_2021_dt <- fread(here("generated_data","spring_2018_to_2021_all.csv"))

bama_2006_dt <- setDT(read_excel(here("generated_data","spring_2006_all.xlsx")))
bama_2007_dt <- setDT(read_excel(here("generated_data","spring_2007_all.xlsx")))
bama_2008_dt <- setDT(read_excel(here("generated_data","spring_2008_bachelors.xlsx")))
bama_2009_dt <- setDT(read_excel(here("generated_data","spring_2009_bachelors.xlsx")))
bama_2010_dt <- setDT(read_excel(here("generated_data","spring_2010_bachelors.xlsx")))
bama_2011_dt <- setDT(read_excel(here("generated_data","spring_2011_bachelors.xlsx")))
bama_2012_dt <- setDT(read_excel(here("generated_data","spring_2012_bachelors.xlsx")))
bama_2013_dt <- setDT(read_excel(here("generated_data","spring_2013_bachelors.xlsx")))
bama_2014_dt <- setDT(read_excel(here("generated_data","spring_2014_bachelors.xlsx")))
bama_2015_dt <- setDT(read_excel(here("generated_data","spring_2015_bachelors.xlsx")))
bama_2016_dt <- setDT(read_excel(here("generated_data","spring_2016_bachelors.xlsx")))
bama_2017_dt <- setDT(read_excel(here("generated_data","spring_2017_bachelors.xlsx")))
bama_2022_dt <- setDT(read_excel(here("generated_data","spring_2022_bachelors.xlsx")))
bama_2023_dt <- setDT(read_excel(here("generated_data","spring_2023_bachelors.xlsx")))
bama_2024_dt <- setDT(read_excel(here("generated_data","spring_2024_bachelors.xlsx")))

# has state already: 06, 07, 18, 19, 20, 21
# has ", AL" for Alabama residents: 08, 09, 16, 17, 22, 23, 24
# does not have ", AL" for Alabama residents: 10, 11, 12, 13, 14, 15, 

bama_2008_dt[, State := str_sub(Location, -3, -1)]
bama_2009_dt[, State := str_sub(Location, -3, -1)]
bama_2016_dt[, State := str_sub(Location, -3, -1)]
bama_2017_dt[, State := str_sub(Location, -3, -1)]
bama_2022_dt[, State := str_sub(Location, -3, -1)]
bama_2023_dt[, State := str_sub(Location, -3, -1)]
bama_2024_dt[, State := str_sub(Location, -3, -1)]

bama_2010_dt[, State := fifelse(grepl(",", Location), str_sub(Location, -3, -1), "AL")]
bama_2011_dt[, State := fifelse(grepl(",", Location), str_sub(Location, -3, -1), "AL")]
bama_2012_dt[, State := fifelse(grepl(",", Location), str_sub(Location, -3, -1), "AL")]
bama_2013_dt[, State := fifelse(grepl(",", Location), str_sub(Location, -3, -1), "AL")]
bama_2014_dt[, State := fifelse(grepl(",", Location), str_sub(Location, -3, -1), "AL")]
bama_2015_dt[, State := fifelse(grepl(",", Location), str_sub(Location, -3, -1), "AL")]

bama_2008_dt[, State := fifelse(grepl("[A-Z]", State), State, "International")]
bama_2009_dt[, State := fifelse(grepl("[A-Z]", State), State, "International")]
bama_2010_dt[, State := fifelse(grepl("[A-Z]", State), State, "International")]
bama_2011_dt[, State := fifelse(grepl("[A-Z]", State), State, "International")]
bama_2012_dt[, State := fifelse(grepl("[A-Z]", State), State, "International")]
bama_2013_dt[, State := fifelse(grepl("[A-Z]", State), State, "International")]
bama_2014_dt[, State := fifelse(grepl("[A-Z]", State), State, "International")]
bama_2015_dt[, State := fifelse(grepl("[A-Z]", State), State, "International")]
bama_2016_dt[, State := fifelse(grepl("[A-Z]", State), State, "International")]
bama_2017_dt[, State := fifelse(grepl("[A-Z]", State), State, "International")]
bama_2018_2021_dt[, State := fifelse(grepl("[A-Z]", State), State, "International")]
bama_2022_dt[, State := fifelse(grepl("[A-Z]", State), State, "International")]
bama_2023_dt[, State := fifelse(grepl("[A-Z]", State), State, "International")]
bama_2024_dt[, State := fifelse(grepl("[A-Z]", State), State, "International")]

# modify 2018-2021 to just bachelors
setnames(bama_2018_2021_dt, "City","Location")
bama_2018_2021_dt <- bama_2018_2021_dt[,c(1:3,5:6)]
bama_2018_2021_dt <- bama_2018_2021_dt[Degree %flike% "Bachelor" | Degree %flike% "B S"]

# modify 06, 07 to just bachelors
bama_2006_dt <- bama_2006_dt[Degree %flike% "Bachelor"]
bama_2007_dt <- bama_2007_dt[Degree %flike% "Bachelor"]

# add year to all but 2018-2021
bama_2006_dt[, Year := 2006]
bama_2007_dt[, Year := 2007]
bama_2008_dt[, Year := 2008]
bama_2009_dt[, Year := 2009]
bama_2010_dt[, Year := 2010]
bama_2011_dt[, Year := 2011]
bama_2012_dt[, Year := 2012]
bama_2013_dt[, Year := 2013]
bama_2014_dt[, Year := 2014]
bama_2015_dt[, Year := 2015]
bama_2016_dt[, Year := 2016]
bama_2017_dt[, Year := 2017]
bama_2022_dt[, Year := 2022]
bama_2023_dt[, Year := 2023]
bama_2024_dt[, Year := 2024]

### combine into single file

spring_all_dt <- rbind(bama_2006_dt, bama_2007_dt, bama_2008_dt, 
                       bama_2009_dt, bama_2010_dt, bama_2011_dt, 
                       bama_2012_dt, bama_2013_dt, bama_2014_dt,
                       bama_2015_dt, bama_2016_dt, bama_2017_dt,
                       bama_2018_2021_dt, bama_2022_dt, bama_2023_dt, 
                       bama_2024_dt, fill = T)

#=====================================================================
# 1 - read and edit the summer and fall commencement data
#=====================================================================

summer_fall_2007_bachelors_dt <- setDT(read_excel(here("generated_data","Summer and Fall","summer_fall_2007_bachelors.xlsx")))
summer_fall_2008_bachelors_dt <- setDT(read_excel(here("generated_data","Summer and Fall","summer_fall_2008_bachelors.xlsx")))
summer_fall_2009_bachelors_dt <- setDT(read_excel(here("generated_data","Summer and Fall","summer_fall_2009_bachelors.xlsx")))
summer_fall_2010_bachelors_dt <- setDT(read_excel(here("generated_data","Summer and Fall","summer_fall_2010_bachelors.xlsx")))
summer_fall_2011_bachelors_dt <- setDT(read_excel(here("generated_data","Summer and Fall","summer_fall_2011_bachelors.xlsx")))
summer_fall_2012_bachelors_dt <- setDT(read_excel(here("generated_data","Summer and Fall","summer_fall_2012_bachelors.xlsx")))
summer_fall_2013_bachelors_dt <- setDT(read_excel(here("generated_data","Summer and Fall","summer_fall_2013_bachelors.xlsx")))
summer_fall_2014_bachelors_dt <- setDT(read_excel(here("generated_data","Summer and Fall","summer_fall_2014_bachelors.xlsx")))
summer_fall_2015_bachelors_dt <- setDT(read_excel(here("generated_data","Summer and Fall","summer_fall_2015_bachelors.xlsx")))
summer_fall_2016_bachelors_dt <- setDT(read_excel(here("generated_data","Summer and Fall","summer_fall_2016_bachelors.xlsx")))
summer_fall_2017_bachelors_dt <- setDT(read_excel(here("generated_data","Summer and Fall","summer_fall_2017_bachelors.xlsx")))
summer_fall_2021_bachelors_dt <- setDT(read_excel(here("generated_data","Summer and Fall","summer_fall_2021_bachelors.xlsx")))
summer_fall_2022_bachelors_dt <- setDT(read_excel(here("generated_data","Summer and Fall","summer_fall_2022_bachelors.xlsx")))
summer_fall_2023_bachelors_dt <- setDT(read_excel(here("generated_data","Summer and Fall","summer_fall_2023_bachelors.xlsx")))

summer_2024_bachelors_dt <- setDT(read_excel(here("generated_data","Summer and Fall","summer_2024_bachelors.xlsx")))

summer_fall_2006_all_dt <- setDT(read_excel(here("generated_data","Summer and Fall","summer_fall_2006_all.xlsx")))
summer_fall_2018_all_dt <- setDT(read_excel(here("generated_data","Summer and Fall","summer_fall_2018_all.xlsx"), col_types = c("text", "text", "text", "text", "text")))
summer_fall_2019_all_dt <- setDT(read_excel(here("generated_data","Summer and Fall","summer_fall_2019_all.xlsx"), col_types = c("text", "text", "text", "text", "text")))
summer_fall_2020_all_dt <- setDT(read_excel(here("generated_data","Summer and Fall","summer_fall_2020_all.xlsx"), col_types = c("text", "text", "text", "text", "text")))

### limit "all" degree years to just bachelors - modify 2006, 2018-2020 to just bachelors
summer_fall_2006_bachelors_dt <- summer_fall_2006_all_dt[Degree %flike% "Bachelor"]
summer_fall_2018_bachelors_dt <- summer_fall_2018_all_dt[Degree %flike% "Bachelor" | Degree %flike% "B S"]
summer_fall_2019_bachelors_dt <- summer_fall_2019_all_dt[Degree %flike% "Bachelor" | Degree %flike% "B.S."]
summer_fall_2020_bachelors_dt <- summer_fall_2020_all_dt[Degree %flike% "Bachelor" | Degree %flike% "B S"]

### add year to all files
summer_fall_2006_bachelors_dt[, Year := 2006]
summer_fall_2007_bachelors_dt[, Year := 2007]
summer_fall_2008_bachelors_dt[, Year := 2008]
summer_fall_2009_bachelors_dt[, Year := 2009]
summer_fall_2010_bachelors_dt[, Year := 2010]
summer_fall_2011_bachelors_dt[, Year := 2011]
summer_fall_2012_bachelors_dt[, Year := 2012]
summer_fall_2013_bachelors_dt[, Year := 2013]
summer_fall_2014_bachelors_dt[, Year := 2014]
summer_fall_2015_bachelors_dt[, Year := 2015]
summer_fall_2016_bachelors_dt[, Year := 2016]
summer_fall_2017_bachelors_dt[, Year := 2017]
summer_fall_2018_bachelors_dt[, Year := 2018]
summer_fall_2019_bachelors_dt[, Year := 2019]
summer_fall_2020_bachelors_dt[, Year := 2020]
summer_fall_2021_bachelors_dt[, Year := 2021]
summer_fall_2022_bachelors_dt[, Year := 2022]
summer_fall_2023_bachelors_dt[, Year := 2023]
summer_2024_bachelors_dt[, Year := 2024]

### standardize state information

# has state already: 06, 18, 19, 20 
# has ", AL" for Alabama residents: 16, 17, 21, 22, 23, 24
# does not have ", AL" for Alabama residents: 07, 08, 09, 10, 11, 12, 13, 14, 15, 

summer_fall_2016_bachelors_dt[, State := str_sub(Location, -3, -1)]
summer_fall_2017_bachelors_dt[, State := str_sub(Location, -3, -1)]
summer_fall_2021_bachelors_dt[, State := str_sub(Location, -3, -1)]
summer_fall_2022_bachelors_dt[, State := str_sub(Location, -3, -1)]
summer_fall_2023_bachelors_dt[, State := str_sub(Location, -3, -1)]
summer_2024_bachelors_dt[, State := str_sub(Location, -3, -1)]

summer_fall_2007_bachelors_dt[, State := fifelse(grepl(",", Location), str_sub(Location, -3, -1), "AL")]
summer_fall_2008_bachelors_dt[, State := fifelse(grepl(",", Location), str_sub(Location, -3, -1), "AL")]
summer_fall_2009_bachelors_dt[, State := fifelse(grepl(",", Location), str_sub(Location, -3, -1), "AL")]
summer_fall_2010_bachelors_dt[, State := fifelse(grepl(",", Location), str_sub(Location, -3, -1), "AL")]
summer_fall_2011_bachelors_dt[, State := fifelse(grepl(",", Location), str_sub(Location, -3, -1), "AL")]
summer_fall_2012_bachelors_dt[, State := fifelse(grepl(",", Location), str_sub(Location, -3, -1), "AL")]
summer_fall_2013_bachelors_dt[, State := fifelse(grepl(",", Location), str_sub(Location, -3, -1), "AL")]
summer_fall_2014_bachelors_dt[, State := fifelse(grepl(",", Location), str_sub(Location, -3, -1), "AL")]
summer_fall_2015_bachelors_dt[, State := fifelse(grepl(",", Location), str_sub(Location, -3, -1), "AL")]

summer_fall_2006_bachelors_dt[, State := fifelse(grepl("[A-Z]", State), State, "International")]
summer_fall_2007_bachelors_dt[, State := fifelse(grepl("[A-Z]", State), State, "International")]
summer_fall_2008_bachelors_dt[, State := fifelse(grepl("[A-Z]", State), State, "International")]
summer_fall_2009_bachelors_dt[, State := fifelse(grepl("[A-Z]", State), State, "International")]
summer_fall_2010_bachelors_dt[, State := fifelse(grepl("[A-Z]", State), State, "International")]
summer_fall_2011_bachelors_dt[, State := fifelse(grepl("[A-Z]", State), State, "International")]
summer_fall_2012_bachelors_dt[, State := fifelse(grepl("[A-Z]", State), State, "International")]
summer_fall_2013_bachelors_dt[, State := fifelse(grepl("[A-Z]", State), State, "International")]
summer_fall_2014_bachelors_dt[, State := fifelse(grepl("[A-Z]", State), State, "International")]
summer_fall_2015_bachelors_dt[, State := fifelse(grepl("[A-Z]", State), State, "International")]
summer_fall_2016_bachelors_dt[, State := fifelse(grepl("[A-Z]", State), State, "International")]
summer_fall_2017_bachelors_dt[, State := fifelse(grepl("[A-Z]", State), State, "International")]
summer_fall_2018_bachelors_dt[, State := fifelse(grepl("[A-Z]", State), State, "International")]
summer_fall_2019_bachelors_dt[, State := fifelse(grepl("[A-Z]", State), State, "International")]
summer_fall_2020_bachelors_dt[, State := fifelse(grepl("[A-Z]", State), State, "International")]
summer_fall_2021_bachelors_dt[, State := fifelse(grepl("[A-Z]", State), State, "International")]
summer_fall_2022_bachelors_dt[, State := fifelse(grepl("[A-Z]", State), State, "International")]
summer_fall_2023_bachelors_dt[, State := fifelse(grepl("[A-Z]", State), State, "International")]
summer_2024_bachelors_dt[, State := fifelse(grepl("[A-Z]", State), State, "International")]


### combine into single data table

su_fa_06_dt <- summer_fall_2006_bachelors_dt
su_fa_07_dt <- summer_fall_2007_bachelors_dt
su_fa_08_dt <- summer_fall_2008_bachelors_dt
su_fa_09_dt <- summer_fall_2009_bachelors_dt
su_fa_10_dt <- summer_fall_2010_bachelors_dt
su_fa_11_dt <- summer_fall_2011_bachelors_dt
su_fa_12_dt <- summer_fall_2012_bachelors_dt
su_fa_13_dt <- summer_fall_2013_bachelors_dt
su_fa_14_dt <- summer_fall_2014_bachelors_dt
su_fa_15_dt <- summer_fall_2015_bachelors_dt
su_fa_16_dt <- summer_fall_2016_bachelors_dt
su_fa_17_dt <- summer_fall_2017_bachelors_dt
su_fa_18_dt <- summer_fall_2018_bachelors_dt[,c(1:3,5:6)]
su_fa_19_dt <- summer_fall_2019_bachelors_dt[,c(1:3,5:6)]
su_fa_20_dt <- summer_fall_2020_bachelors_dt[,c(1:3,5:6)]
su_fa_21_dt <- summer_fall_2021_bachelors_dt
su_fa_22_dt <- summer_fall_2022_bachelors_dt
su_fa_23_dt <- summer_fall_2023_bachelors_dt
su_24_dt <- summer_2024_bachelors_dt

su_fa_all_dt <- rbind(su_fa_06_dt, su_fa_07_dt, su_fa_08_dt, 
                      su_fa_09_dt, su_fa_10_dt, su_fa_11_dt, 
                      su_fa_12_dt, su_fa_13_dt, su_fa_14_dt,
                      su_fa_15_dt, su_fa_16_dt, su_fa_17_dt,
                      su_fa_18_dt, su_fa_19_dt, su_fa_20_dt, 
                      su_fa_21_dt, su_fa_22_dt, su_fa_23_dt, su_24_dt, fill = T)

for_plot_su_fa_dt <- su_fa_all_dt[,c(2,5)]

#=====================================================================
# 4 - time series out-of-state plot (all commencements)
#=====================================================================

for_plot_2006_dt <- bama_2006_dt[,c(4,5)]
for_plot_2007_dt <- bama_2007_dt[,c(2,5)]
for_plot_2008_dt <- bama_2008_dt[,c(5,6)]
for_plot_2009_dt <- bama_2009_dt[,c(5,6)]
for_plot_2010_dt <- bama_2010_dt[,c(5,6)]
for_plot_2011_dt <- bama_2011_dt[,c(5,6)]
for_plot_2012_dt <- bama_2012_dt[,c(5,6)]
for_plot_2013_dt <- bama_2013_dt[,c(5,6)]
for_plot_2014_dt <- bama_2014_dt[,c(5,6)]
for_plot_2015_dt <- bama_2015_dt[,c(5,6)]
for_plot_2016_dt <- bama_2016_dt[,c(5,6)]
for_plot_2017_dt <- bama_2017_dt[,c(5,6)]
for_plot_2018_2021_dt <- bama_2018_2021_dt[,c(3,5)]
for_plot_2022_dt <- bama_2022_dt[,c(5,6)]
for_plot_2023_dt <- bama_2023_dt[,c(5,6)]
for_plot_2024_dt <- bama_2024_dt[,c(5,6)]

combined_spring_dt <- rbind(for_plot_2006_dt, for_plot_2007_dt, for_plot_2008_dt, 
                            for_plot_2009_dt, for_plot_2010_dt, for_plot_2011_dt, 
                            for_plot_2012_dt, for_plot_2013_dt, for_plot_2014_dt,
                            for_plot_2015_dt, for_plot_2016_dt, for_plot_2017_dt,
                            for_plot_2018_2021_dt, for_plot_2022_dt, for_plot_2023_dt, 
                            for_plot_2024_dt)

combined_plots_dt <- rbind(combined_spring_dt, for_plot_su_fa_dt)

combined_plots_dt[, Origin := fcase(State %flike% "AL", "In-State",
                                    State %flike% "International", "International",
                                    default = "Out-of-State")] 

for_plot_all_years_dt <- combined_plots_dt[,.N,.(Origin,Year)]
for_plot_all_years_dt[, Total := sum(N), by = Year]
for_plot_all_years_dt[, Share := 100*(N / Total)]

for_plot_all_years_dt <- rbind(for_plot_all_years_dt,
                               data.table(Year = 2006, Origin = "International", N = 0,
                                          Total = 3165, Share = 0))

for_plot_all_years_dt[, Origin := factor(Origin, levels = c("In-State", "Out-of-State", "International"))]

### i. bar plot 

bar_plot <- ggplot(for_plot_all_years_dt, aes(x = Year, y = Share, group = Origin, fill = Origin)) + 
  geom_bar(stat = "identity", position = "dodge") + theme_bw() + scale_fill_brewer(palette = "Paired") + removeGridX() + 
  ylim(0,100) + ylab("% of Undergraduate Degree Recipients")

print(bar_plot)

ggsave(here("figures","trends_enrollment.png"), plot = bar_plot,
       width = 8, height = 4.5)

### ii. area plot (preferred figure)

for_area_plot_dt <- combined_plots_dt[,.N,.(Origin,Year)]

for_area_plot_dt[, RelativeTo2006 := fcase(
  Origin %flike% "International", N,
  Origin %flike% "In-State", as.integer(N - 2674),
  Origin %flike% "Out-of-State",  as.integer(N - 491)
)]

for_area_plot_dt[, Origin := factor(Origin, levels = c( "International","Out-of-State", "In-State"))]

area_plot <- ggplot(for_area_plot_dt[Year <= 2023], aes(x = Year, y = RelativeTo2006, group = Origin, fill = Origin)) + 
  geom_area() + theme_bw() + scale_fill_manual(values = c("#B2DF8A", "#1F78B4","#A6CEE3")) + removeGridX() + 
  ylab("# of Degree Recipients Relative to 2006") + xlim(2006,2023)

print(area_plot)

ggsave(here("figures","trends_enrollment_area.png"), plot = area_plot,
       width = 8, height = 4.5)


#=====================================================================
# 5 - 2006 vs 2023 state shares plot
#=====================================================================

bachelors_06_spring_dt <- bama_2006_dt[Degree %flike% "Bachelor"]
bachelors_06_spring_dt[, InState := fifelse(State %flike% "AL", 1, 0)]

combined_2006_dt <- rbind(bachelors_06_spring_dt, summer_fall_2006_bachelors_dt, fill = T)

shares_06_dt <- combined_2006_dt[,.N,.(State)]

bama_2023_dt[, State := str_sub(Location, -3, -1)]
bama_2023_dt[, InState := fifelse(State %flike% "AL", 1, 0)]

combined_2023_dt <- rbind(bama_2023_dt, summer_fall_2023_bachelors_dt, fill = T)

illinois_2006 <- combined_2006_dt[State %flike% "IL"]
illinois_2023 <- combined_2023_dt[State %flike% "IL"]

shares_23_dt <- combined_2023_dt[,.N,.(State)]

### i. construct state-level OOS shares

shares_23_dt[, Shares23 := 100*(N/nrow(combined_2023_dt[!(State %flike% "AL")]))]
shares_23_dt[, State := trimws(State)]
shares_06_dt[, Shares06 := 100*(N/nrow(combined_2006_dt[!(State %flike% "AL")]))]

combined_shares_dt <- merge(shares_06_dt, shares_23_dt, by = "State", all.y = T)

combined_shares_dt[, Shares06 := fifelse(is.na(Shares06), 0, Shares06)]

combined_shares_for_plot_dt <- combined_shares_dt[State != "AL"]

lm_fit <- lm(Shares23 ~ Shares06, data = combined_shares_for_plot_dt)
slope <- coef(lm_fit)[2]
slope_text <- paste("Slope:", round(slope,2))

### ii. Produce Figure 3 (06 vs 23 shares)

shares_plot <- ggplot(combined_shares_for_plot_dt, aes(x = Shares06, y = Shares23)) + 
  geom_point(color = "#A6CEE3", size = 3, alpha = 0.8) + theme_bw() +
  geom_abline(slope = 1, intercept = 0, color = "#1F78B4", linetype = "dashed", linewidth = 1) + 
  # geom_smooth(method = "lm", se = T, color = "blue", linetype = "dashed") + 
  annotate("text", x = 2, y = 9, label = "IL", hjust = 1.1, vjust = 2, size = 4,
           color = "#33A02C") + 
  annotate("text", x = 24.6, y = 12.4, label = "GA", hjust = 1.1, vjust = 2, size = 4,
           color = "#33A02C") + 
  annotate("text", x = 17.3, y = 8.2, label = "TN", hjust = 1.1, vjust = 2, size = 4,
           color = "#33A02C") + 
  annotate("text", x = 3.2, y = 6, label = "CA", hjust = 1.1, vjust = 2, size = 4,
           color = "#33A02C") + 
  labs(y = "2023 Share of Out-of-State Students (%)", x = "2006 Share of Out-of-State Students (%)") + ylim(0,15) + xlim(0,25)

print(shares_plot)

ggsave(here("figures","oos_shares_plot.png"), plot = shares_plot,
       width = 8, height = 4.5)

#=====================================================================
# 6 - combine all files and clean names
#=====================================================================

spring_all_dt[, Commencement := "Spring"]
# setnames(spring_all_dt, c("Degree"), c("Program"))
su_fa_all_dt[, Commencement := "Summer or Fall"]

all_bachelors_dt <- rbind(spring_all_dt, su_fa_all_dt, fill = T)

### i. fix weird name error (replace non unicode character)
all_bachelors_dt[36359, Name := "Brelahn Josephine Wyatt"]

### ii. fill in null honors with "No" (standardize)
all_bachelors_dt[,Honors := fifelse(is.na(Honors) | Honors %flike% "posthumous", "No", Honors)]

# iii. Construct function to split names and extract first, middle, last names, handling suffixes
extract_names <- function(name) {
  # split the name into parts
  parts <- unlist(strsplit(name, " "))
  
  # define common suffixes
  suffixes <- c("Jr.", "Sr.", "II", "III", "IV", "V", 'PhD', "(posthumous)")
  
  # check if the last part is a suffix
  has_suffix <- parts[length(parts)] %in% suffixes
  
  # determine first, middle, last names with or without suffix
  first_name <- tolower(parts[1])
  
  if (has_suffix) {
    last_name <- tolower(parts[length(parts) - 1])  
    suffix <- tolower(parts[length(parts)])  
    if (length(parts) == 4) {
      middle_name <- tolower(parts[2])  
    } else if (length(parts) > 4) {
      middle_name <- tolower(paste(parts[2:(length(parts) - 2)], collapse = " ")) 
    } else {
      middle_name <- ""
    }
  } else {
    last_name <- tolower(tail(parts, 1))  
    suffix <- ""  
    if (length(parts) == 3) {
      middle_name <- tolower(parts[2])  
    } else if (length(parts) > 3) {
      middle_name <- tolower(paste(parts[2:(length(parts) - 1)], collapse = " ")) 
    } else {
      middle_name <- ""
    }
  }
  
  # return a list with first, middle, last names, and suffix
  list(first_name = first_name, middle_name = middle_name, last_name = last_name, suffix = suffix)
}

### iv. Apply the function to each row and add new columns to the data table
all_bachelors_dt[, c("first_name", "middle_name", "last_name", "suffix") := 
                   transpose(lapply(Name, extract_names))]

all_bachelors_dt[, `:=`(
  first_name = unlist(lapply(first_name, as.character)),
  middle_name = unlist(lapply(middle_name, as.character)),
  last_name = unlist(lapply(last_name, as.character)),
  suffix = unlist(lapply(suffix, as.character))
)]

# 106362 unique values (currently at 49.5% match rate)
nrow(unique(all_bachelors_dt, by = c("first_name", "middle_name", "last_name", "suffix", "Year")))

setnames(all_bachelors_dt, c("Location","State"), c("Origin Town", "Origin State"))

# remove commas from strings
all_bachelors_dt[, `:=` (
  first_name = gsub(",","",first_name),
  middle_name = gsub(",","",middle_name),
  last_name = gsub(",","",last_name)
                    )]

fwrite(all_bachelors_dt, here("data","all_alabama_data.csv"))

#=====================================================================
# 7 - set up the merge with revelio data
#=====================================================================

### i. read in revelio labs data (linked ed data / profile data )

revelio_dt <- fread(here("revelio_data","linked_revelio_data.csv"))

# process name strings -- REMOVES SUFFIXES POST COMMAS
revelio_dt[, fullname := sub(",.*","", fullname)]

revelio_dt[, count := .N, by = .(fullname, startdate, enddate)]

revelio_dt[, lastUpdate := as.Date(updated_dt, format = "%m/%d/%y")]

### ii. only keep the last update for each individual (some individuals 
###     have duplicate entries)
result_dt <- revelio_dt[, .SD[which.max(lastUpdate)], by = .(fullname, startdate, enddate)]

# modest duplication? first returns 105746, second returns 104327
nrow(unique(result_dt, by = c("fullname", "startdate","enddate")))
nrow(unique(result_dt, by = c("user_id")))

result_dt[, count := .N, by = .(user_id)]

### iii. only keep earliest start date for each user id
almost_final_revelio_dt <- result_dt[, .SD[which.min(startdate)], by = .(user_id)]

nrow(unique(almost_final_revelio_dt, by = c("fullname","enddate")))

almost_final_revelio_dt[, count := .N, by = .(fullname, enddate)]

### iv. only keep earliest start date for each name and end date
final_revelio_dt <- almost_final_revelio_dt[, .SD[which.min(startdate)], by = .(fullname, enddate)]

# match user_id 1-to-1 to full name and end date: 104059 in each
nrow(unique(final_revelio_dt, by = c("fullname","enddate")))
nrow(unique(final_revelio_dt, by = c("user_id")))

### v. Write function to remove all non-English language characters and keep only basic characters
clean_fullname <- function(name) {
  # remove anything after a comma, including the comma
  name <- gsub(",.*$", "", name)
  # use gsub to replace non-English characters
  cleaned_name <- gsub("[^a-zA-Z' ]", "", name) 
  # trim white space
  return(trimws(cleaned_name)) 
}

### vi. Apply the function to the fullname column
final_revelio_dt[, fullname := sapply(fullname, clean_fullname)]
final_revelio_dt <- final_revelio_dt[fullname != ""]
final_revelio_dt[, c("first_name", "middle_name", "last_name", "suffix") := 
                   transpose(lapply(fullname, extract_names))]

final_revelio_dt[, `:=`(
  first_name = unlist(lapply(first_name, as.character)),
  middle_name = unlist(lapply(middle_name, as.character)),
  last_name = unlist(lapply(last_name, as.character)),
  suffix = unlist(lapply(suffix, as.character))
)]

final_revelio_dt[, Year := year(enddate)]

# note: we now lose some people because we drop columns where fullname is NULL
nrow(unique(final_revelio_dt, by = c("fullname","enddate"))) # 103663
nrow(unique(final_revelio_dt, by = c("user_id"))) # 103869

formatch_revelio_dt <- final_revelio_dt[, .SD[which.max(lastUpdate)], by = .(first_name, last_name, Year)]

# 98,055 for both calls (98k)
nrow(unique(formatch_revelio_dt, by = c("first_name","last_name","Year")))
nrow(unique(formatch_revelio_dt, by = c("user_id")))

### vii. write the processed revelio data to output
fwrite(formatch_revelio_dt, here("data","cleaned_ua_revelio.csv"))

#=====================================================================
# 8 - run the merge with revelio data
#=====================================================================

### i. Perform initial merge -- first name, last name, graduation year
formatch_bachelors_dt <- unique(all_bachelors_dt, by = c("Name", "Origin Town", "Year"))

# quick name check
name_check_bachelors_dt <- unique(formatch_bachelors_dt[,.(first_name)], by = "first_name")

formatch_revelio_dt[, merged_flag_revelio := 1]
formatch_bachelors_dt[, merged_flag_bachelors := 1]


merged_dt <- merge(
  formatch_revelio_dt, 
  formatch_bachelors_dt, 
  by = c("first_name","last_name", "Year"), 
  all = F)

# 48.7k unique by user_id and name / year
nrow(unique(merged_dt, by = c("user_id")))
nrow(unique(merged_dt, by = c("first_name","last_name","Year")))

### ii. Perform the 2nd merge -- first name, last name, and year
#       after processing names for abbreviations (abbreviate commencement names)

# collect those that didn't merge in the first step
unmerged_bachelors_dt <- formatch_bachelors_dt[!merged_dt, on = c("first_name", "last_name", "Year")]
unmerged_revelio_dt <- formatch_revelio_dt[!merged_dt, on = c("first_name", "last_name", "Year")]

# apply abbreviations, returning only the abbreviation itself
unmerged_bachelors_dt[, abbreviated_first_name := fcase(
  grepl("^alexander", first_name), "alex",
  grepl("^ali", first_name), "ali",
  grepl("^aly", first_name), "aly",
  grepl("^ally", first_name), "ally",
  grepl("^alli", first_name), "alli",
  grepl("^amber", first_name), "amber",
  grepl("^ana", first_name), "ana",
  grepl("^anna", first_name), "anna",
  grepl("^angel", first_name), "angel",
  grepl("^ann", first_name), "ann",
  grepl("^anne", first_name), "anne",
  grepl("^ari", first_name), "ari",
  grepl("^art", first_name), "art",
  grepl("^aubri", first_name), "aubri",
  grepl("^audri", first_name), "audri",
  grepl("^ava", first_name), "ava",
  grepl("^bea", first_name), "bea",
  grepl("^ben", first_name), "ben",
  grepl("^brad", first_name), "brad",
  grepl("^bre", first_name), "bre",
  grepl("^bree", first_name), "bree",
  grepl("^breigh", first_name), "breigh",
  grepl("^bri", first_name), "bri",
  grepl("^carol", first_name), "carol",
  grepl("^chad", first_name), "chad",
  grepl("^chris", first_name), "chris",
  grepl("^christi", first_name), "christi",
  grepl("^cole", first_name), "cole",
  grepl("^cori", first_name), "cori",
  grepl("^corri", first_name), "corri",
  grepl("^curt", first_name), "curt",
  grepl("^dani", first_name), "dani",
  grepl("^doug", first_name), "doug",
  grepl("^eboni", first_name), "eboni",
  grepl("^elen", first_name), "elen",
  grepl("^elizabeth", first_name), "liz",
  grepl("^ellen", first_name), "ellen",
  grepl("^emma", first_name), "emma",
  grepl("^fran", first_name), "fran",
  grepl("^frank", first_name), "frank",
  grepl("^frankie", first_name), "frankie",
  grepl("^fred", first_name), "fred",
  grepl("^gray", first_name), "gray",
  grepl("^greg", first_name), "greg",
  grepl("^grey", first_name), "grey",
  grepl("^gwen", first_name), "gwen",
  grepl("^gwyn", first_name), "gwyn",
  grepl("^hanah", first_name), "hanah",
  grepl("^hannah", first_name), "hannah",
  grepl("^hugh", first_name), "hugh",
  grepl("^jack", first_name), "jack",
  grepl("^jacob", first_name), "jacob",
  grepl("^jamar", first_name), "jamar",
  grepl("^jeff", first_name), "jeff",
  grepl("^jess", first_name), "jess",
  grepl("^johnothan", first_name), "john",
  grepl("^johnathan", first_name), "john",
  grepl("^john", first_name), "jack",
  grepl("^jon", first_name), "jon",
  grepl("^josh", first_name), "josh",
  grepl("^joyce", first_name), "joyce",
  grepl("^julia", first_name), "julia",
  grepl("^juli", first_name), "juli",
  grepl("^kala", first_name), "kala",
  grepl("^kali", first_name), "kali",
  grepl("^kate", first_name), "kate",
  grepl("^ken", first_name), "ken",
  grepl("^kim", first_name), "kim",
  grepl("^krista", first_name), "krista",
  grepl("^kristi", first_name), "kristi",
  grepl("^kris", first_name), "kris",
  grepl("^krys", first_name), "krys",
  grepl("^kurt", first_name), "kurt",
  # other abbreviations not listed above / later in alphabet
  grepl("^abigail", first_name), "abby",
  grepl("^andrew", first_name), "drew",
  grepl("^daniel", first_name), "dan",
  grepl("^jacob", first_name), "jake",
  grepl("^james", first_name), "jim",
  grepl("^jessica", first_name), "jess",
  grepl("^joseph", first_name), "joe",
  grepl("^katherine", first_name), "katie",
  grepl("^michael", first_name), "mike",
  grepl("^matthew", first_name), "matt",
  grepl("^nicholas", first_name), "nick",
  grepl("^robert", first_name), "bobby",
  grepl("^samuel", first_name), "sam",
  grepl("^samantha", first_name), "sam",
  grepl("^thomas", first_name), "tom",
  grepl("^timothy", first_name), "tim",
  grepl("^trenton", first_name), "trent",
  grepl("^tyler", first_name), "ty",
  grepl("^william", first_name), "will",
  grepl("^zachary", first_name), "zach",
  
  # Default case: NA if no match
  default = NA_character_
)]

unmerged_revelio_dt[, abbreviated_first_name := first_name]


merged_abbreviation_dt <- merge(
  unmerged_revelio_dt, 
  unmerged_bachelors_dt, 
  by = c("abbreviated_first_name","last_name", "Year"), 
  all = F)

# additional 2.4k unique by user_id and name / year (> 2%)
nrow(unique(merged_abbreviation_dt, by = c("user_id")))
nrow(unique(merged_abbreviation_dt, by = c("abbreviated_first_name","last_name","Year")))

### iii. Perform the 3rd merge -- middle name of commencement to first name of revelio

# start from those who didn't merge in step 2 
unmerged_bachelors_rd2_dt <- unmerged_bachelors_dt[!merged_abbreviation_dt, on = c("abbreviated_first_name", "last_name", "Year")]
unmerged_revelio_rd2_dt <- unmerged_revelio_dt[!merged_abbreviation_dt, on = c("abbreviated_first_name", "last_name", "Year")]

test_b <- unmerged_bachelors_rd2_dt[,.N,.(first_name, middle_name, last_name, Year)]
test_r <- unmerged_revelio_rd2_dt[,.N,.(first_name, middle_name, last_name, Year)]

unmerged_bachelors_rd2_dt[, middle_as_first_name := middle_name]
unmerged_revelio_rd2_dt[, middle_as_first_name := first_name]

merged_middle_dt <- merge(
  unmerged_revelio_rd2_dt, 
  unmerged_bachelors_rd2_dt, 
  by = c("middle_as_first_name","last_name", "Year"), 
  all = F)

# additional 2.1k unique by user_id and name / year (> 2%)
nrow(unique(merged_middle_dt, by = c("user_id")))
nrow(unique(merged_middle_dt, by = c("middle_as_first_name","last_name","Year")))


### iv. validation check -- look at field in revelio vs degree in commencement data