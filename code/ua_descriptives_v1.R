#=====================================================================
## Created by: Crossan Cooper
## Last Modified: 2-12-25

## file use: explore cleaned UA data
#
## figure outputs:
# (i) figures/recruiting_iv.png -- how recruiting visits in 2017  
#      affect enrollment in a cross-section
# (ii) figures/recruiting_iv_changes.png -- how recruiting visits
#       in 2017 affect enrollment changes from 2006 - 2022
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
               doParallel,readxl,did,ggExtra,DBI)

# set working directory
setwd("/Users/crossancooper/Dropbox/Professional/active-projects/admissions_project")

# replace here with wd: file.path(getwd(),

#=====================================================================
# 1 - read and edit the cleaned commencement data
#=====================================================================

### i. read data 
ua_dt <- fread(file = file.path(getwd(), "data", "all_alabama_data.csv"))

### ii. edit degree and honors information

# (a) degree 

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


ua_dt[, DegreeGroupFinish := fcase(
  DegreeGroupFinal %flike% "General", "General", 
  DegreeGroupFinal %flike% "Engineering", "Engineering", 
  DegreeGroupFinal %flike% "Communication", "Communication", 
  DegreeGroupFinal %flike% "Human", "Human Env. Sciences", 
  default = DegreeGroupFinal
)]

ua_dt[, STEM := fifelse(DegreeGroup == "Engineering" | DegreeGroupFinal %flike% "BS" | 
                          DegreeGroup %in% c("Human Environmental Sciences", "Nursing", "Computer Science"), 1, 0 )]

# (b) honors 

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

### iv. simple summary stats

summary_stats_dt <- ua_dt[, .(
  total_count = .N,  # Count of observations per year
  avg_STEM = round(100* mean(STEM, na.rm = TRUE),2),  # Average STEM indicator
  avg_1_minus_OutFlag = round(100* mean(1 - OutFlag, na.rm = TRUE),2)  # Average of 1 - OutFlag
), by = Year]



### v. calculate OOS shares over time 

out_of_state_by_year <- ua_dt[, .(
  total_students = .N,
  out_of_state_students = sum(OutFlag),
  out_of_state_share = sum(OutFlag) / .N
), by = Year]

### vi. clean and extract state and town name from origin state / town

ua_dt[, OriginState := trimws(gsub("[.,]", "", `Origin State`))]

ua_dt[, OriginTown := trimws(gsub(",.*", "", `Origin Town`))] # Remove everything after the first comma
ua_dt[, OriginTown := gsub("([A-Z])([A-Z])", "\\1 \\2", OriginTown)] # Add space between consecutive capital letters


#=====================================================================
# 2 - descriptive work -- honors and major choice
#=====================================================================

### i. major choice

## (a) by year

# exluding years have missing data 
major_summary <- ua_dt[Year != "2006" & Year != "2024" & Year != "2007" &
                         Year != "2020" & Year != "2021", .(
  total_students = .N,
  out_of_state_students = sum(OutFlag),
  in_state_students = .N - sum(OutFlag)
), by = .(Year,DegreeGroupFinish)]

major_summary_pooled <- ua_dt[Year != "2006" & Year != "2024" & Year != "2007" &
                         Year != "2020" & Year != "2021" & 
                         Year != "2022" & Year != "2023", .(
                           total_students = .N,
                           out_of_state_students = sum(OutFlag),
                           in_state_students = .N - sum(OutFlag)
                         ), by = .(DegreeGroupFinish)]

out_sum <- sum(major_summary_pooled$out_of_state_students)
in_sum <- sum(major_summary_pooled$in_state_students)

major_summary_pooled[, InShare := 100*(in_state_students/in_sum)]
major_summary_pooled[, OutShare := 100*(out_of_state_students/out_sum)]

major_summary_pooled[, Multiple := InShare/OutShare]
major_summary_pooled[, ReverseMultiple := OutShare/InShare]

major_summary_long <- melt(
  data = major_summary,
  id.vars        = c("Year", "DegreeGroupFinish"),
  measure.vars   = c("in_state_students", "out_of_state_students"),
  variable.name  = "Residency",
  value.name     = "Count"
)

major_summary_long[, Residency := fifelse(
  Residency == "in_state_students", "In-State", "Out-of-State"
)]


residency_totals <- major_summary_long[, .(TotalResidencyYear = sum(Count)), 
                                       by = .(Year, Residency)]

major_summary_long <- merge(
  x = major_summary_long,
  y = residency_totals,
  by = c("Year", "Residency"),
  all.x = TRUE
)

major_summary_long[, Share := Count / TotalResidencyYear]

major_plot <- ggplot(
  major_summary_long[
    DegreeGroupFinish != "Fine Arts" & DegreeGroupFinish != "Music" 
  ],
  aes(x = Year, y = Share, fill = Residency)
) +
  geom_bar(stat = "identity", 
           position = position_dodge(width = 0.8), 
           color = 'black', 
           alpha = 0.8) +
  facet_wrap(~ DegreeGroupFinish
  ) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Graduation Year",
    y = "Share of Students in Major",
    fill = "Residency Status"
  ) +
  scale_fill_viridis_d() + 
  theme_bw() + removeGridX() + 
  theme(
    legend.position = "bottom",
    legend.background = element_rect(color = "black", linetype = "solid", linewidth = 0.25),
    text = element_text(size = 12),
    axis.text.x = element_text(angle = 0, hjust = 1),
    strip.text = element_text(face = "bold")
  ) +
  scale_x_continuous(
    breaks = c(2010, 2014, 2018),
    labels = c(2010, 2014, 2018)
  )

print(major_plot)

ggsave(
  filename = file.path(getwd(),"figures","descriptive-figs","major_facet_wrap_share.png"),
  plot = major_plot,
  width = 8,
  height = 8
)


### ii. honors average participation (maybe miscoded?)

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
# 3 - read and explore the 2017 recruiting data
#=====================================================================

### i. read data 

recruiting_dt <- fread(file = file.path(getwd(), "data","recruiting_data_ozan.csv"))

### ii. get just hs visits for UA and UGA and all others

hs_recruiting_dt <- recruiting_dt[categorized_event_type %flike% "hsvisit"]

hs_recruiting_dt[, InState := fifelse(event_state == univ_state, 1, 0)]

hs_recruiting_dt[, InState_Mean := mean(InState, na.rm = TRUE), by = univ_id]

recruiting_scale_dt <- hs_recruiting_dt[,.N,.(InState_Mean, univ_id)]

uga_hs_dt <- hs_recruiting_dt[univ_id == 139959]

ua_recruiting_dt <- recruiting_dt[univ_id == 100751]

## (a) check descriptives

# 67.5% public hs, 31.9% private hs
ua_hs_visit_dt <- ua_recruiting_dt[categorized_event_type %flike% "hsvisit"]

### iii. merge visits with ua_dt for just 2022

ua_dt[, TownName := OriginTown]
ua_dt[, StateName := OriginState]

## (a) merge performed here 

ua_2022_dt <- ua_dt[Year == 2022 & StateName != "International" & TownName != ""]

ua_counts_dt <- ua_2022_dt[,.N,.(TownName, StateName)]

setnames(ua_counts_dt, "N", "UACount")

## (b) clean names

ua_hs_visit_dt[, TownName := event_city]
ua_hs_visit_dt[, StateName := event_state]

visit_counts_dt <- ua_hs_visit_dt[TownName != "",.N,.(TownName, StateName)]

setnames(visit_counts_dt, "N", "VisitCount")

### iv. merge visits with enrollment (2022) counts

merged_ua_hs_visit_dt <- merge(ua_counts_dt, visit_counts_dt, by = c("TownName", "StateName"), all.x = T, all.y = T)

merged_ua_hs_visit_dt[, VisitCount := fifelse(is.na(VisitCount), 0, VisitCount)]
merged_ua_hs_visit_dt[, UACount := fifelse(is.na(UACount), 0, UACount)]

merged_ua_hs_visit_dt[, LogPlusOneUA := log(1+UACount)]
merged_ua_hs_visit_dt[, LogPlusOneVisit := log(1+VisitCount)]

merged_ua_hs_visit_dt[, InAL := fifelse(StateName == "AL", 1, 0)]

## (a) regression -- 2022 enrollment on visits

feols(UACount ~ VisitCount | InAL, data = merged_ua_hs_visit_dt, vcov = 'hc1')
feols(UACount ~ VisitCount, data = merged_ua_hs_visit_dt[InAL == 0], vcov = 'hc1')

## (b) visualization

merged_ua_hs_visit_dt[InAL == 0, VisitBucket := ifelse(VisitCount >= 10, ">= 10", as.character(VisitCount))]

agg_data <- merged_ua_hs_visit_dt[InAL == 0, .(
  Mean_UACount = mean(UACount, na.rm = TRUE),
  Place_Count = .N  
), by = VisitBucket]

agg_data[, VisitBucket := factor(VisitBucket, levels = sort(as.numeric(unique(VisitBucket[VisitBucket != ">= 10"]))) %>% as.character() %>% c(">= 10"))]

iv_plot <- ggplot(agg_data, aes(x = VisitBucket)) +
  geom_bar(aes(y = Place_Count / max(Place_Count) * max(Mean_UACount)), stat = "identity", fill = "#440154FF", alpha = 0.5) +  # Places distribution (scaled)
  geom_point(aes(y = Mean_UACount), color = "#21908CFF", size = 3) +  # Mean UACount
  geom_line(aes(y = Mean_UACount, group = 1), color = "#21908CFF", size = 1.5, linetype = 'dashed') +  # Line connecting mean values
  scale_y_continuous(
    name = "Average # of UA Graduates (2022)",
    sec.axis = sec_axis(~ . * max(agg_data$Place_Count) / max(agg_data$Mean_UACount), name = "# of Towns")  # Secondary y-axis
  ) +
  theme_bw() + removeGridX() +
  labs(x = "Visit Count (Grouped)") +
  theme(plot.title = element_text(hjust = 0.5))

print(iv_plot)

ggsave(file.path(getwd(),"figures","recruiting_iv.png"), plot = iv_plot,
       width = 8, height = 4.5)


### v. extend cross-section IV to look at changes from 2006 

## (a) merges and descriptive regressions

ua_dt[, TownName := OriginTown]
ua_dt[, StateName := OriginState]

ua_2021_dt <- ua_dt[Year == 2021 & StateName != "International" & TownName != ""]
ua_2006_dt <- ua_dt[Year == 2006 & StateName != "International" & TownName != ""]

ua_counts_2021_dt <- ua_2021_dt[,.N,.(TownName, StateName)]
ua_counts_2006_dt <- ua_2006_dt[,.N,.(TownName, StateName)]

setnames(ua_counts_2021_dt, "N", "UACount_2021")
setnames(ua_counts_2006_dt, "N", "UACount_2006")

merge_counts_dt <- merge(ua_counts_2006_dt, ua_counts_2021_dt, by = c("TownName", "StateName"), all.x = T, all.y = T)

merge_counts_dt[, UACount_2006 := fifelse(is.na(UACount_2006), 0, UACount_2006)]
merge_counts_dt[, UACount_2021 := fifelse(is.na(UACount_2021), 0, UACount_2021)]

merge_counts_dt[, ChangeUACount := UACount_2021 - UACount_2006]

ua_hs_visit_dt[, TownName := event_city]
ua_hs_visit_dt[, StateName := event_state]

visit_counts_dt <- ua_hs_visit_dt[TownName != "",.N,.(TownName, StateName)]

setnames(visit_counts_dt, "N", "VisitCount")

merged_ua_hs_visit_changes_dt <- merge(merge_counts_dt, visit_counts_dt, by = c("TownName", "StateName"),  all.x = T, all.y = T)

merged_ua_hs_visit_changes_dt[, VisitCount := fifelse(is.na(VisitCount), 0, VisitCount)]
merged_ua_hs_visit_changes_dt[, ChangeUACount := fifelse(is.na(ChangeUACount), 0, ChangeUACount)]

merged_ua_hs_visit_changes_dt[, InAL := fifelse(StateName == "AL", 1, 0)]

feols(ChangeUACount ~ VisitCount | InAL, data = merged_ua_hs_visit_changes_dt, vcov = 'hc1')
feols(ChangeUACount ~ VisitCount, data = merged_ua_hs_visit_changes_dt[InAL == 0], vcov = 'hc1')
feols(ChangeUACount ~ VisitCount, data = merged_ua_hs_visit_changes_dt[InAL == 1], vcov = 'hc1')

merged_ua_hs_visit_changes_dt[InAL == 0, VisitBucket := ifelse(VisitCount >= 10, ">= 10", as.character(VisitCount))]

## (b) same visualization as before with changes

agg_data_changes <- merged_ua_hs_visit_changes_dt[InAL == 0, .(
  Mean_UACount = mean(ChangeUACount, na.rm = TRUE),
  Place_Count = .N  
), by = VisitBucket]

agg_data_changes[, VisitBucket := factor(VisitBucket, levels = sort(as.numeric(unique(VisitBucket[VisitBucket != ">= 10"]))) %>% as.character() %>% c(">= 10"))]

iv_plot_changes <- ggplot(agg_data_changes, aes(x = VisitBucket)) +
  geom_bar(aes(y = Place_Count / max(Place_Count) * max(Mean_UACount)), stat = "identity", fill = "#440154FF", alpha = 0.5) +  # Places distribution (scaled)
  geom_point(aes(y = Mean_UACount), color = "#21908CFF", size = 3) +  # Mean UACount
  geom_line(aes(y = Mean_UACount, group = 1), color = "#21908CFF", size = 1.5, linetype = 'dashed') +  # Line connecting mean values
  scale_y_continuous(
    name = "Avg Change in Graduates (2006 - 2021)",
    sec.axis = sec_axis(~ . * max(agg_data$Place_Count) / max(agg_data$Mean_UACount), name = "# of Towns")  # Secondary y-axis
  ) +
  theme_bw() + removeGridX() +
  labs(x = "2017 Recruiting Visits Count") +
  theme(plot.title = element_text(hjust = 0.5))

print(iv_plot_changes)

ggsave(file.path(getwd(),"figures","recruiting_iv_changes.png"), plot = iv_plot_changes,
       width = 8, height = 4.5)

### vi. look at correlation of recruiting IV with pre-visit (2021) shares

## (a) prep data

ua_2021_dt <- ua_dt[Year == 2021 & OriginState != "International" & OriginTown != ""]

total_out_ua_2021 <- ua_2021_dt[OriginState != "AL",.N]

ua_counts_2021_dt <- ua_2021_dt[,.N, by = .(OriginTown, OriginState)]
setnames(ua_counts_2021_dt, "N", "UACount_2021")

out_state_2021_dt <- ua_counts_2021_dt[OriginState != "AL"]

out_state_2021_dt[, Share_UACount_2021 := 100 * (UACount_2021 / total_out_ua_2021)]

ua_hs_visit_dt[, TownName := event_city]
ua_hs_visit_dt[, StateName := event_state]

visit_counts_dt <- ua_hs_visit_dt[TownName != "", .N, by = .(TownName, StateName)]
setnames(visit_counts_dt, "N", "VisitCount")

merged_2021_dt <- merge(out_state_2021_dt, visit_counts_dt, by.x = c("OriginTown", "OriginState"), 
                   by.y = c("TownName", "StateName"), all.x = T, all.y = T)

merged_2021_dt[, Share_UACount_2021 := fifelse(is.na(Share_UACount_2021), 0, Share_UACount_2021)]
merged_2021_dt[, UACount_2021 := fifelse(is.na(UACount_2021), 0, UACount_2021)]

merged_2021_dt[, VisitCount := fifelse(is.na(VisitCount), 0, VisitCount)]

merged_2021_dt[, LogCount := fifelse(VisitCount >= 1, log(VisitCount), 0)]

merged_2021_dt[, VisitFlag := fifelse(VisitCount > 0, 1, 0)]

## (b) descriptive regressions

feols(VisitCount ~ Share_UACount_2021, data = merged_2021_dt, vcov = 'hc1')
feols(VisitCount ~ UACount_2021, data = merged_2021_dt, vcov = 'hc1')
feols(VisitFlag ~ UACount_2021, data = merged_2021_dt, vcov = 'hc1')

merged_2021_dt[VisitCount >= 10, VisitBucket := ">= 10"]
merged_2021_dt[is.na(VisitBucket), VisitBucket := as.character(VisitCount)]

agg_data_2021 <- merged_2021_dt[, .(
  Mean_UAShare_2021 = mean(Share_UACount_2021, na.rm = TRUE),
  Mean_UACount_2021 = mean(UACount_2021, na.rm = TRUE),
  Place_Count = .N
), by = VisitBucket]

agg_data_2021[, VisitBucket := factor(VisitBucket, levels = 
                                        sort(as.numeric(unique(VisitBucket[VisitBucket != ">= 10"])), na.last = TRUE) %>% as.character() %>% c(">= 10"))]

## (c) correlation plot 

correlation_plot <- ggplot(agg_data_2021, aes(x = VisitBucket)) +
  geom_bar(aes(y = Place_Count / max(Place_Count) * max(Mean_UACount_2021)), 
           stat = "identity", fill = "#440154FF", color = 'black', alpha = 0.6) +  # Scaled background bars for places
  geom_point(aes(y = Mean_UACount_2021), color = "#FDE725FF", size = 3) +  # Scatter points
  geom_line(aes(y = Mean_UACount_2021, group = 1), color = "#FDE725FF", size = 1.5, linetype = 'dashed') +  # Line connecting points
  scale_y_continuous(
    name = "Avg Graduate Count (2021)",
    sec.axis = sec_axis(~ . * max(agg_data_2021$Place_Count) / max(agg_data_2021$Mean_UACount_2021), 
                        name = "# of Towns")  # Secondary y-axis
  ) +
  theme_bw() + removeGridX() +  
  labs(x = "Recruiting Visits (2017)") +
  theme(plot.title = element_text(hjust = 0.5))

## display the plot
print(correlation_plot)

ggsave(file.path(getwd(),"figures","descriptive-figs","recruiting_iv_2021_counts.png"), plot = correlation_plot,
       width = 8, height = 4.5)


### vii. recruiting regressions for appendix table

dt_bin <- copy(merged_ua_hs_visit_changes_dt)

dt_bin[, VisitFlag := fifelse(VisitCount > 0, 1, 0)]

dt_bin[, Log2006 := fifelse(UACount_2006 > 0, log(UACount_2006), 0)]
dt_bin[, Log2021 := fifelse(UACount_2021 > 0, log(UACount_2021), 0)]

dt_bin[, ChangeUAPercent := 100*(ChangeUACount/UACount_2006)]

feols(VisitFlag ~ ChangeUACount + UACount_2006, 
      data = dt_bin[!is.na(UACount_2006) & !is.na(UACount_2021) & StateName == "AL"], vcov = 'hc1')
feols(VisitFlag ~ UACount_2006, 
      data = dt_bin[!is.na(UACount_2006) & !is.na(UACount_2021) & StateName == "AL"], vcov = 'hc1')

feols(VisitFlag ~ ChangeUACount + UACount_2006, 
      data = dt_bin[!is.na(UACount_2006) & !is.na(UACount_2021) & StateName != "AL"], vcov = ~StateName)
feols(VisitFlag ~ UACount_2006, 
      data = dt_bin[!is.na(UACount_2006) & !is.na(UACount_2021) & StateName != "AL"], vcov = ~StateName)


#=====================================================================
# 4 - recruiting visit regression with predicted shares 
#     and past enrollment
#=====================================================================

### i. past enrollment

# extract 2017 & 2022 data (excluding international)
ua_2017_dt <- ua_dt[Year == 2017 & OriginState != "International" & OriginTown != "" & OriginState != "AL"]
ua_2022_dt <- ua_dt[Year == 2022 & OriginState != "International" & OriginTown != "" & OriginState != "AL"]

# compute total UA students per year
total_ua_2017 <- ua_2017_dt[, .N]
total_ua_2022 <- ua_2022_dt[, .N]

# compute town-level counts
ua_counts_2017_dt <- ua_2017_dt[,.N, by = .(OriginTown, OriginState)]
ua_counts_2022_dt <- ua_2022_dt[,.N, by = .(OriginTown, OriginState)]

setnames(ua_counts_2017_dt, "N", "UACount_2017")
setnames(ua_counts_2022_dt, "N", "UACount_2022")

# compute town-level OOS share for 2017 & 2022
ua_counts_2017_dt[, Share_OOS_2017 := 100 * (UACount_2017 / total_ua_2017)]
ua_counts_2022_dt[, Share_OOS_2022 := 100 * (UACount_2022 / total_ua_2022)]

# merge 2017 and 2022 data
merged_oos_dt <- merge(ua_counts_2017_dt, ua_counts_2022_dt, 
                       by = c("OriginTown", "OriginState"), all = TRUE)

# fill NA values with 0 (for towns that appear in only one year)
merged_oos_dt[, Share_OOS_2017 := fifelse(is.na(Share_OOS_2017), 0, Share_OOS_2017)]
merged_oos_dt[, Share_OOS_2022 := fifelse(is.na(Share_OOS_2022), 0, Share_OOS_2022)]
merged_oos_dt[, UACount_2017 := fifelse(is.na(UACount_2017), 0, UACount_2017)]
merged_oos_dt[, UACount_2022 := fifelse(is.na(UACount_2022), 0, UACount_2022)]

# merge with recruiting visits
visit_counts_dt <- ua_hs_visit_dt[event_city != "", .N, by = .(event_city, event_state)]
setnames(visit_counts_dt, c("event_city", "event_state"), c("OriginTown", "OriginState"))
setnames(visit_counts_dt, "N", "VisitCount")

# merge visits with OOS shares
merged_oos_dt <- merge(merged_oos_dt, visit_counts_dt, 
                       by = c("OriginTown", "OriginState"), all.x = TRUE)

# replace NA counts with 0 (no recorded visits)
merged_oos_dt[, VisitCount := fifelse(is.na(VisitCount), 0, VisitCount)]

# run regressions
oos_model_shares <- feols(Share_OOS_2022 ~ VisitCount + Share_OOS_2017, 
                          data = merged_oos_dt, vcov = ~OriginState)
summary(oos_model_shares)

oos_model_levels <- feols(UACount_2022 ~ VisitCount + UACount_2017, 
                   data = merged_oos_dt, vcov = ~OriginState)
summary(oos_model_levels)

# Step 1: Compute town-level enrollment counts and shares for every year from 2006-2022
ua_yearly_counts <- list()
total_ua_per_year <- ua_dt[OriginState != "International" & OriginTown != "" & OriginState != "AL", .N, by = Year]

for (yr in 2006:2022) {
  # Extract town-level counts
  ua_yearly_counts[[as.character(yr)]] <- ua_dt[Year == yr & OriginState != "International" & OriginTown != "" & OriginState != "AL",
                                                .N, by = .(OriginTown, OriginState)]
  setnames(ua_yearly_counts[[as.character(yr)]], "N", paste0("UACount_", yr))
  
  # Merge with total UA students per year to compute shares
  total_students <- total_ua_per_year[Year == yr, N]
  if (!is.na(total_students)) {
    ua_yearly_counts[[as.character(yr)]][, paste0("Share_OOS_", yr) := 100 * get(paste0("UACount_", yr)) / total_students]
  }
}

# Step 2: Merge all years together into a single dataset
merged_oos_dt <- Reduce(function(dt1, dt2) merge(dt1, dt2, by = c("OriginTown", "OriginState"), all = TRUE), ua_yearly_counts)

# Fill missing values with 0 for towns that appear in only some years
for (yr in 2006:2022) {
  merged_oos_dt[, paste0("UACount_", yr) := fifelse(is.na(get(paste0("UACount_", yr))), 0, get(paste0("UACount_", yr)))]
  merged_oos_dt[, paste0("Share_OOS_", yr) := fifelse(is.na(get(paste0("Share_OOS_", yr))), 0, get(paste0("Share_OOS_", yr)))]
}

# Step 3: Merge with recruiting visit data
visit_counts_dt <- ua_hs_visit_dt[event_city != "", .N, by = .(event_city, event_state)]
setnames(visit_counts_dt, c("event_city", "event_state"), c("OriginTown", "OriginState"))
setnames(visit_counts_dt, "N", "VisitCount")

merged_oos_dt <- merge(merged_oos_dt, visit_counts_dt, by = c("OriginTown", "OriginState"), all.x = TRUE)
merged_oos_dt[, VisitCount := fifelse(is.na(VisitCount), 0, VisitCount)]

merged_oos_dt[, VisitFlag := fifelse(VisitCount > 0, 1, 0)]

## (a) visit count

# initialize a list to store results
results <- list()

# loop over years and run regressions
for (yr in 2018:2021) {
  # construct formula dynamically
  formula_levels <- as.formula(paste0("UACount_2022 ~ VisitCount + UACount_", yr))
  
  model_levels <- feols(formula_levels, data = merged_oos_dt, vcov = ~OriginState)
  
  coef_visit <- coef(model_levels)["VisitCount"]
  pval_visit <- pvalue(model_levels)["VisitCount"]
  tstat_visit <- tstat(model_levels)["VisitCount"]
  se_visit <- se(model_levels)["VisitCount"]  
  
  coef_uacount <- coef(model_levels)[paste0("UACount_", yr)]
  pval_uacount <- pvalue(model_levels)[paste0("UACount_", yr)]
  tstat_uacount <- tstat(model_levels)[paste0("UACount_", yr)]
  se_uacount <- se(model_levels)[paste0("UACount_", yr)]  
  
  sample_size <- nobs(model_levels)  
  r2_value <- r2(model_levels)[1]  
  
  # store results in a single-row data.table
  results[[as.character(yr)]] <- data.table(
    Year = as.numeric(yr), 
    Beta_Visit = coef_visit, 
    SE_Visit = se_visit,  
    P_Value_Visit = pval_visit, 
    T_Stat_Visit = tstat_visit,
    
    Beta_UACount = coef_uacount,
    SE_UACount = se_uacount,  
    P_Value_UACount = pval_uacount,
    T_Stat_UACount = tstat_uacount,
    
    N = sample_size,
    R2 = r2_value
  )
}

# combine all results into a single data.table
beta_results_dt <- rbindlist(results)

# print results
print(beta_results_dt)

## (b) visit flag

# initialize a list to store results
results <- list()

# loop over years and run regressions
for (yr in 2018:2021) {
  # construct formula dynamically
  formula_levels <- as.formula(paste0("UACount_2022 ~ VisitFlag + UACount_", yr))
  
  model_levels <- feols(formula_levels, data = merged_oos_dt, vcov = ~OriginState)
  
  coef_visit <- coef(model_levels)["VisitFlag"]
  pval_visit <- pvalue(model_levels)["VisitFlag"]
  tstat_visit <- tstat(model_levels)["VisitFlag"]
  se_visit <- se(model_levels)["VisitFlag"]  
  
  coef_uacount <- coef(model_levels)[paste0("UACount_", yr)]
  pval_uacount <- pvalue(model_levels)[paste0("UACount_", yr)]
  tstat_uacount <- tstat(model_levels)[paste0("UACount_", yr)]
  se_uacount <- se(model_levels)[paste0("UACount_", yr)]  
  
  sample_size <- nobs(model_levels)  
  r2_value <- r2(model_levels)[1]  
  
  # store results in a single-row data.table
  results[[as.character(yr)]] <- data.table(
    Year = as.numeric(yr), 
    Beta_Visit = coef_visit, 
    SE_Visit = se_visit,  
    P_Value_Visit = pval_visit, 
    T_Stat_Visit = tstat_visit,
    
    Beta_UACount = coef_uacount,
    SE_UACount = se_uacount,  
    P_Value_UACount = pval_uacount,
    T_Stat_UACount = tstat_uacount,
    
    N = sample_size,
    R2 = r2_value
  )
}

# combine all results into a single data.table
beta_results_dt <- rbindlist(results)

# print results
print(beta_results_dt)

### ii. predicted enrollment regressions

# compute the growth rate gm
merged_oos_dt[, enrollment_growth := (UACount_2021 / UACount_2020) - 1]

# compute predicted 2022 enrollment
merged_oos_dt[, Predicted_UACount_2022 := (1 + enrollment_growth) * UACount_2021]

results <- list()

# Construct formula using Predicted_UACount_2022 instead of UACount_yr
formula_levels <- as.formula("UACount_2022 ~ VisitFlag + Predicted_UACount_2022")

# Run fixed effects regression with clustered SE by OriginState
model_levels <- feols(formula_levels, data = merged_oos_dt, vcov = ~OriginState)

# Extract statistics for VisitCount
coef_visit <- coef(model_levels)["VisitFlag"]
pval_visit <- pvalue(model_levels)["VisitFlag"]
tstat_visit <- tstat(model_levels)["VisitFlag"]
se_visit <- se(model_levels)["VisitFlag"]

# Extract statistics for Predicted_UACount_2022
coef_predicted <- coef(model_levels)["Predicted_UACount_2022"]
pval_predicted <- pvalue(model_levels)["Predicted_UACount_2022"]
tstat_predicted <- tstat(model_levels)["Predicted_UACount_2022"]
se_predicted <- se(model_levels)["Predicted_UACount_2022"]

# Extract additional model statistics
sample_size <- nobs(model_levels)  # Sample size (N)
r2_value <- r2(model_levels)[1]  # R-squared value

# Store results in a single-row data.table
results[[as.character(yr)]] <- data.table(
  Beta_Visit = coef_visit, 
  SE_Visit = se_visit, 
  P_Value_Visit = pval_visit, 
  T_Stat_Visit = tstat_visit,
  
  Beta_Predicted_UACount = coef_predicted,
  SE_Predicted_UACount = se_predicted,
  P_Value_Predicted_UACount = pval_predicted,
  T_Stat_Predicted_UACount = tstat_predicted,
  
  N = sample_size,
  R2 = r2_value
)

# Combine all results into a single data.table
beta_results_dt <- rbindlist(results)

# Print results
print(beta_results_dt)

## (a) use 2019 - 2020 to get growth rates instead and redo regression output

# Compute the growth rate gm using 2019 to 2020 enrollment
merged_oos_dt[, enrollment_growth := (UACount_2020 / UACount_2019) - 1]

# Compute predicted 2022 enrollment
merged_oos_dt[, Predicted_UACount_2022 := (1 + enrollment_growth) * UACount_2021]

results <- list()

# Construct formula using Predicted_UACount_2022 instead of UACount_yr
formula_levels <- as.formula("UACount_2022 ~ VisitCount + Predicted_UACount_2022")

# Run fixed effects regression with clustered SE by OriginState
model_levels <- feols(formula_levels, data = merged_oos_dt, vcov = ~OriginState)

# Extract statistics for VisitCount
coef_visit <- coef(model_levels)["VisitCount"]
pval_visit <- pvalue(model_levels)["VisitCount"]
tstat_visit <- tstat(model_levels)["VisitCount"]
se_visit <- se(model_levels)["VisitCount"]

# Extract statistics for Predicted_UACount_2022
coef_predicted <- coef(model_levels)["Predicted_UACount_2022"]
pval_predicted <- pvalue(model_levels)["Predicted_UACount_2022"]
tstat_predicted <- tstat(model_levels)["Predicted_UACount_2022"]
se_predicted <- se(model_levels)["Predicted_UACount_2022"]

# Extract additional model statistics
sample_size <- nobs(model_levels)  # Sample size (N)
r2_value <- r2(model_levels)[1]  # R-squared value

# Store results in a single-row data.table
results[[1]] <- data.table(
  Beta_Visit = coef_visit, 
  SE_Visit = se_visit, 
  P_Value_Visit = pval_visit, 
  T_Stat_Visit = tstat_visit,
  
  Beta_Predicted_UACount = coef_predicted,
  SE_Predicted_UACount = se_predicted,
  P_Value_Predicted_UACount = pval_predicted,
  T_Stat_Predicted_UACount = tstat_predicted,
  
  N = sample_size,
  R2 = r2_value
)

# Combine all results into a single data.table
beta_results_dt <- rbindlist(results)

# Print results
print(beta_results_dt)

#=====================================================================
# 5 - read and edit the linked commencement data
#=====================================================================

### i. read data 
linked_dt <- fread(file = file.path(getwd(), "data", "linked_commencement_revelio_profile_data.csv"))

### ii. clean degree and honors information
linked_dt[, DegreeGroup := fcase(
  uaDegree %flike% "Business", "Business",
  uaDegree %flike% "Computer", "Computer Science",
  uaDegree %flike% "Music", "Music",
  uaDegree %flike% "Engineer" | uaDegree %flike% "Eng.", "Engineering",
  uaDegree %flike% "Nursing", "Nursing",
  uaDegree %flike% "Social Work", "Social Work",
  uaDegree %flike% "Education", "Education",
  uaDegree %flike% "Fine Arts", "Fine Arts",
  uaDegree %flike% "Communication", "Communication and Information Sciences",
  uaDegree %flike% "Human" | uaDegree %flike% "Athletic", "Human Environmental Sciences",
  default = "General"
)]

linked_dt[, DegreeGroupFinal := fcase(
  DegreeGroup == "General" & uaDegree %flike% "Arts", "BA - General", 
  DegreeGroup == "General" & uaDegree %flike% "Science", "BS - General", 
  DegreeGroup == "General" & uaDegree %flike% "Science", "BS - General", 
  DegreeGroup == "Engineering" & uaDegree %flike% "Construction", "Construction Engineering",
  DegreeGroup == "Engineering" & uaDegree %flike% "Aerospace", "Aerospace Engineering",
  DegreeGroup == "Engineering" & uaDegree %flike% "Chemical", "Chemical Engineering",
  DegreeGroup == "Engineering" & uaDegree %flike% "Civil", "Civil Engineering",
  DegreeGroup == "Engineering" & uaDegree %flike% "Electric", "Electrical Engineering",
  DegreeGroup == "Engineering" & uaDegree %flike% "Industrial", "Industrial Engineering",
  DegreeGroup == "Engineering" & uaDegree %flike% "Mechanical", "Mechanical Engineering",
  DegreeGroup == "Engineering" & uaDegree %flike% "Metallurgical", "Metallurgical Engineering",
  DegreeGroup == "Engineering" & uaDegree %flike% "Architectural", "Architectural Engineering",
  DegreeGroup == "Engineering" & uaDegree %flike% "Environmental", "Environmental Engineering",
  default = DegreeGroup
)]

linked_dt[, STEM := fifelse(DegreeGroup == "Engineering" | DegreeGroupFinal %flike% "BS" | 
                          DegreeGroup %in% c("Human Environmental Sciences", "Nursing", "Computer Science"), 1, 0 )]



### iii. add out of state indicator

linked_dt[, OutFlag := fifelse(originState != "AL", 1, 0)]

## (a) simple summary stats

summary_stats_linked_dt <- linked_dt[, .(
  total_count = .N,  # Count of observations per year
  avg_STEM = round(100* mean(STEM, na.rm = TRUE),2),  # Average STEM indicator
  avg_1_minus_OutFlag = round(100* mean(1 - OutFlag, na.rm = TRUE),2)  # Average of 1 - OutFlag
), by = Year]


#=====================================================================
# 6 - read and edit the linked commencement data + job histories
#=====================================================================

### i. read data 

## (a) linked data 
revelio_ua_link_dt <- fread(file = file.path(getwd(), "data", "linked_commencement_revelio_profile_data.csv"))

## (b) first spell
first_spells_join_dt <- readRDS(file.path(getwd(), "revelio_data", "first_spell_join.rds"))
setDT(first_spells_join_dt)


states_dt <- data.table(originState = state.abb,
                        state = state.name) %>%
  bind_rows(data.frame(originState = 'DC', state = 'Washington, D.C.'))


### ii. clean up revelio - UA link origin states

# clean up states of origin
revelio_ua_link_dt <- revelio_ua_link_dt %>%
  # remove students with missing graduation years
  filter(!is.na(Year)) %>%
  mutate(originState = gsub(' ', '', originState)) %>%
  # state will be NA for origins outside the US
  left_join(states_dt) %>%
  # filter to US origins for now
  filter(!is.na(state)) %>%
  rename(grad_y = Year)

### iii. clean up first job 

first_spells_join_dt <- first_spells_join_dt %>%
  filter(country == 'United States')

first_spells_limited_dt <- first_spells_join_dt[,c("user_id","role_k1500","state","salary","total_compensation")]

setnames(first_spells_limited_dt, "state", "postgrad_state")

### iv. link two datasets and do some quick share calculations

join_dt <- merge(revelio_ua_link_dt, first_spells_limited_dt, by = 'user_id')

join_dt <- setDT(join_dt)



### v. clean degree and honors information
join_dt[, DegreeGroup := fcase(
  uaDegree %flike% "Business", "Business",
  uaDegree %flike% "Computer", "Computer Science",
  uaDegree %flike% "Music", "Music",
  uaDegree %flike% "Engineer" | uaDegree %flike% "Eng.", "Engineering",
  uaDegree %flike% "Nursing", "Nursing",
  uaDegree %flike% "Social Work", "Social Work",
  uaDegree %flike% "Education", "Education",
  uaDegree %flike% "Fine Arts", "Fine Arts",
  uaDegree %flike% "Communication", "Communication and Information Sciences",
  uaDegree %flike% "Human" | uaDegree %flike% "Athletic", "Human Environmental Sciences",
  default = "General"
)]

join_dt[, DegreeGroupFinal := fcase(
  DegreeGroup == "General" & uaDegree %flike% "Arts", "BA - General", 
  DegreeGroup == "General" & uaDegree %flike% "Science", "BS - General", 
  DegreeGroup == "General" & uaDegree %flike% "Science", "BS - General", 
  DegreeGroup == "Engineering" & uaDegree %flike% "Construction", "Construction Engineering",
  DegreeGroup == "Engineering" & uaDegree %flike% "Aerospace", "Aerospace Engineering",
  DegreeGroup == "Engineering" & uaDegree %flike% "Chemical", "Chemical Engineering",
  DegreeGroup == "Engineering" & uaDegree %flike% "Civil", "Civil Engineering",
  DegreeGroup == "Engineering" & uaDegree %flike% "Electric", "Electrical Engineering",
  DegreeGroup == "Engineering" & uaDegree %flike% "Industrial", "Industrial Engineering",
  DegreeGroup == "Engineering" & uaDegree %flike% "Mechanical", "Mechanical Engineering",
  DegreeGroup == "Engineering" & uaDegree %flike% "Metallurgical", "Metallurgical Engineering",
  DegreeGroup == "Engineering" & uaDegree %flike% "Architectural", "Architectural Engineering",
  DegreeGroup == "Engineering" & uaDegree %flike% "Environmental", "Environmental Engineering",
  default = DegreeGroup
)]

join_dt[, STEM := fifelse(DegreeGroup == "Engineering" | DegreeGroupFinal %flike% "BS" | 
                              DegreeGroup %in% c("Human Environmental Sciences", "Nursing", "Computer Science"), 1, 0 )]



### vi. add out of state indicator

join_dt[, OutFlag := fifelse(originState != "AL", 1, 0)]

## (a) simple summary stats

summary_stats_linked_history_dt <- join_dt[, .(
  total_count = .N,  # count of observations per year
  avg_STEM = round(100* mean(STEM, na.rm = TRUE),2),  # average STEM indicator
  avg_1_minus_OutFlag = round(100* mean(1 - OutFlag, na.rm = TRUE),2)  # in-state share
), by = grad_y]

pooled_linked_dt <- join_dt[, .(
  total_count = .N,  
  avg_1_minus_OutFlag = round(100* mean(1 - OutFlag, na.rm = TRUE),2)  
)]

join_dt[! (grad_y %in% c(2006, 2007, 2020, 2021)), .(
  avg_STEM = round(100* mean(STEM, na.rm = TRUE),2) 
)]


pooled_rl_dt <- linked_dt[, .(
  total_count = .N,  
  avg_1_minus_OutFlag = round(100* mean(1 - OutFlag, na.rm = TRUE),2) 
)]

linked_dt[! (Year %in% c(2006, 2007, 2020, 2021)), .(
  avg_STEM = round(100* mean(STEM, na.rm = TRUE),2) 
)]

pooled_ua_dt <- ua_dt[, .(
  total_count = .N,  
  avg_1_minus_OutFlag = round(100* mean(1 - OutFlag, na.rm = TRUE),2)  
)]

ua_dt[! (Year %in% c(2006, 2007, 2020, 2021)), .(
  avg_STEM = round(100* mean(STEM, na.rm = TRUE),2) 
)]
