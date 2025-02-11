#=====================================================================
## Created by: Crossan Cooper
## Last Modified: 2-9-25

## file use: produce worker location descriptives
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
# 1 - read in job-spell data
#=====================================================================

### i. read in first and all spell data and UA data 

## (a) all spells
all_spells_join_dt <- readRDS(file.path(getwd(), "revelio_data", "all_spells_join.rds"))
setDT(all_spells_join_dt)

## (b) first spell
first_spells_join_dt <- readRDS(file.path(getwd(), "revelio_data", "first_spell_join.rds"))
setDT(first_spells_join_dt)

## (c) UA data

revelio_ua_link_dt <- fread(file.path(getwd(), "data", "linked_commencement_revelio_profile_data.csv"))

#=====================================================================
# 2 - produce descriptive analysis
#=====================================================================

### i. make state abbreviation data table

states_dt <- data.table(originState = state.abb,
                     state = state.name) %>%
  bind_rows(data.frame(originState = 'DC', state = 'Washington, D.C.'))


### ii. clean up revelio - UA link origin states

# Clean up states of origin
revelio_ua_link_dt <- revelio_ua_link_dt %>%
  # Remove students with missing graduation years
  filter(!is.na(Year)) %>%
  mutate(originState = gsub(' ', '', originState)) %>%
  # State will be NA for origins outside the US
  left_join(states_dt) %>%
  # Filter to US origins for now
  filter(!is.na(state)) %>%
  rename(grad_y = Year)

### iii. clean up first job 

first_spells_join_dt <- first_spells_join_dt %>%
  filter(country == 'United States')

first_spells_limited_dt <- first_spells_join_dt[,c("user_id","role_k1500","state","salary","total_compensation")]

setnames(first_spells_limited_dt, "state", "postgrad_state")

### iv. link two datasets and do some quick share calculations

join_dt <- merge(revelio_ua_link_dt[grad_y < 2024], first_spells_limited_dt, by = 'user_id')

join_dt <- setDT(join_dt)

join_dt[, OutPost := fifelse(postgrad_state %flike% "Alabama", 0, 1)]
join_dt[, OutPre := fifelse(originState %flike% "AL", 0, 1)]
join_dt[, ReturnHome := fifelse(state == postgrad_state, 1, 0)]

join_dt[,.N,.(OutPost, OutPre, grad_y)]
join_dt[,.N,.(OutPost, OutPre)]
join_dt[,.N,.(ReturnHome,OutPre)]

dest_states_by_year <- join_dt[,.N,.(postgrad_state, grad_y)]
dest_states_by_year[, share := N / sum(N), by = grad_y]

orig_states_by_year <- join_dt[,.N,.(originState, grad_y)]
orig_states_by_year[, share := N / sum(N), by = grad_y]

### iv. location over time 

## (a) pooled

outPost_share_by_year <- join_dt[, .(
  total_count = .N,
  outPost_count = sum(OutPost == 1, na.rm = TRUE),
  outPost_share = 100 * sum(OutPost == 1, na.rm = TRUE) / .N
), by = grad_y]

ggplot(outPost_share_by_year, aes(x = grad_y, y = outPost_share)) +
  geom_line(size = 2, color = "#440154FF", alpha = 0.8) +
  geom_point(size = 4, color = "#440154FF", alpha = 0.6) + # Add points for clarity
  labs(x = "Graduation Year",
    y = "Share Out-Migrating"
  ) +
  theme_bw() + removeGridX() + scale_y_continuous(limits = c(28,66),
                                                  breaks = c(30,40,50,60),
                                                  labels = c("30%", "40%", "50%", "60%"))

## (b) separate for in-state and out-of-state

outPost_share_by_year_pre <- join_dt[, .(
  total_count = .N,
  outPost_count = sum(OutPost == 1, na.rm = TRUE),
  outPost_share = 100 * sum(OutPost == 1, na.rm = TRUE) / .N
), by = .(grad_y, OutPre)] 

out_plots_type <- ggplot(outPost_share_by_year_pre, aes(x = grad_y, y = outPost_share, color = as.factor(OutPre))) +
  geom_line(size = 1, alpha = 1, linetype = 'dashed') +
  geom_point(size = 3, alpha = 0.8) + 
  scale_color_viridis_d(labels = c("In-State", "Out-of-State")) +
  labs(
    x = "Graduation Year",
    y = "Share Out-Migrating",
    color = "Student Type"
  ) +
  theme_bw() + removeGridX() + ylim(29,89)+ 
  theme(
    legend.position = "bottom",
    legend.background = element_rect(color = "black", linetype = "solid", linewidth = 0.25),
    text = element_text(size = 12)) 

print(out_plots_type)

ggsave(file.path(getwd(),"figures","descriptive-figs","out-migration-trends.png"), plot = out_plots_type,
       width = 8, height = 4.5)


## (c) by state groups


join_dt[, OriginGroup := fifelse(originState == "AL", "Alabama",
                                 fifelse(originState %in% c("IL"), "Illinois", "Other States"))]

join_dt[, PostgradGroup := fifelse(postgrad_state == "Alabama", "Alabama",
                                   fifelse(postgrad_state %in% c("Illinois"), " Illinois", "Other States"))]

total_by_year <- join_dt[, .N, by = grad_y]  # Total students per grad year

pre_mig_share_by_year <- join_dt[, .N, by = .(grad_y, OriginGroup)]  # Count per group
pre_mig_share_by_year <- merge(pre_mig_share_by_year, total_by_year, by = "grad_y")  # Merge total count
pre_mig_share_by_year[, share := 100 * N.x / N.y]  # Compute proper share

post_mig_share_by_year <- join_dt[, .N, by = .(grad_y, PostgradGroup)]
post_mig_share_by_year <- merge(post_mig_share_by_year, total_by_year, by = "grad_y")
post_mig_share_by_year[, share := 100 * N.x / N.y]

ggplot(pre_mig_share_by_year, aes(x = grad_y, y = share, color = OriginGroup)) +
  geom_line(size = 1, alpha = 1, linetype = 'dashed') +
  geom_point(size = 3, alpha = 0.8) +
  scale_color_viridis_d(labels = c("Alabama", "Illinois", "Other States")) +
  labs(
    x = "Graduation Year",
    y = "Share of Students by Origin",
    color = "Origin Group"
  ) +
  theme_bw() + ylim(0, 100)

ggplot(post_mig_share_by_year, aes(x = grad_y, y = share, color = PostgradGroup)) +
  geom_line(size = 1, alpha = 1, linetype = 'dashed') +
  geom_point(size = 3, alpha = 0.8) +
  scale_color_viridis_d(labels = c("Alabama", "Illinois", "Other States")) +
  labs(
    x = "Graduation Year",
    y = "Share of Students by Postgrad Destination",
    color = "Postgrad Group"
  ) + theme_bw() + ylim(0, 100)
  
  
  
