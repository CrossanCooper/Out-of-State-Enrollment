#=====================================================================
## Created by: Crossan Cooper
## Last Modified: 1-31-25

## analyze out-of-state enrollment data
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
               doParallel,readxl,did,ggExtra,stringi)

# set working directory
setwd("/Users/crossancooper/Dropbox/Professional/active-projects/admissions_project")

#=====================================================================
# 1 - read and edit data
#=====================================================================

# Function to read and merge individual files and add a 'year' column
merge_files <- function(year) {
  # Generate file names based on the year
  ins_file_name <- paste0("data/ipeds_ins/hd", year, ".csv")
  fe_file_name <- paste0("data/ipeds_fe/ef", year, "c.csv")
  
  # Read the files into data tables
  ins_file <- fread(ins_file_name, na.strings = "", showProgress = FALSE)
  fe_file <- fread(fe_file_name, na.strings = "", showProgress = FALSE)
  
  # Check if 'UNITID' exists in both files
  if ("UNITID" %in% names(ins_file) && "UNITID" %in% names(fe_file)) {
    # Merge the data tables based on 'UNITID'
    merged_data <- merge(ins_file, fe_file, by = "UNITID", all = TRUE)
    
    # Add a 'year' column to the merged data
    merged_data[, year := year]
    
    return(merged_data)
  } else {
    stop(paste("Missing 'UNITID' in files for year", year))
  }
}

# Initialize an empty list to hold the merged data tables
merged_list <- list()

# Generate a list of years to process
years_to_process <- seq(2000, 2021)

# Register parallel backend
cl <- makeCluster(detectCores())
registerDoParallel(cl)

# Use foreach to read and merge files in parallel
merged_list <- foreach(year = years_to_process, .combine = 'list', .multicombine = TRUE, .packages = 'data.table') %dopar% {
  merge_files(year)
}

# Stop the cluster
stopCluster(cl)

# At this point, 'merged_list' is a list of data.tables. You can manually combine them later as needed.

# Function to standardize column names and keep only the desired columns
filterColumns <- function(dt) {
  # Convert all column names to lower case
  setnames(dt, tolower(names(dt)))
  
  # Define the columns to keep
  cols_to_keep <- c("instnm", "stabbr", "unitid", "fips", "line", "efres01", "year")
  
  # Subset the data table to only keep the desired columns
  dt <- dt[, ..cols_to_keep]
  
  return(dt)
}

# Apply the filter_columns function to each data table in merged_list
filtered_list <- lapply(merged_list, filterColumns)

# Now, filtered_list contains data.tables with only the columns you're interested in, for each year.

# Drop merged list from memory (garbage collector)
merged_list <- NULL
gc()

# List of U.S. public flagship universities
flagships <- c( "University of Pittsburgh", "Michigan State University", "Kansas State University",
                "Mississippi State University", "Montana State University", "Stony Brook University",
                "Oklahoma State University", "Oregon State University", "University of Memphis",
                "University of Houston", "University of North Texas", "Texas Tech University", 
                "Washington State University", "University of Alabama", "Auburn University", 
                "University of Alaska Fairbanks", "University of Arizona", "Arizona State University",
  "University of Arkansas Main Campus", "University of California-Berkeley", "University of Colorado at Boulder",
  "Colorado State University", "University of Connecticut", "University of Delaware", "University of Florida", 
  "University of Georgia", "University of Hawaii at Manoa", "University of Idaho", 
  "University of Illinois at Urbana-Champaign", "Indiana University-Bloomington", 
  "University of Iowa", "University of Kansas Main Campus", "University of Kentucky", 
  "Louisiana State Univ & Ag & Mech & Hebert Laws Ctr", 
  "University of Maine", "University of Maryland-College Park",
  "University of Massachusetts-Amherst", "University of Michigan-Ann Arbor", 
  "University of Minnesota-Twin Cities", "University of Mississippi Main Campus", "University of Missouri-Columbia",
  "The University of Montana-Missoula", "University of Nebraska at Lincoln", 
  "University of Nevada-Reno", "University of New Hampshire-Main Campus", "Rutgers University-New Brunswick",
  "University of New Mexico-Main Campus", "New Mexico State University-Main Campus",
  "SUNY at Buffalo", "University of North Carolina at Chapel Hill",
  "University of North Dakota-Main Campus", "Ohio State University-Main Campus", 
  "University of Oklahoma Norman Campus", 
  "University of Oregon", "Pennsylvania State University-Main Campus", 
  "Purdue University-Main Campus", "University of Rhode Island", 
  "University of South Carolina at Columbia", "University of South Dakota", "The University of Tennessee", 
  "The University of Texas at Austin", "Texas A & M University", "University of Utah", 
  "University of Vermont and State Agricultural Coll", 
  "University of Virginia-Main Campus", "University of Washington-Seattle Campus", "West Virginia University", 
  "University of Wisconsin-Madison", "University of Wyoming"
)

flagship_ids <- c(
  215293, 171100, 155399, 176080, 180461, 196097, 207388, 209542, 220862, 225432, 227216, 229115, 236939,
  100751, 100858, 102614, 104179, 104151, 106397, 110635, 126614, 126818, 129020, 130943, 134130, 
  139959, 141574, 142285, 145637, 151351, 153658, 155317,
  157085, 159391, 161253, 163286, 166629, 170976, 174066, 176017, 178396, 180489, 181464, 182290, 183044, 186380, 187985, 188030,
  196088, 199120, 200280, 204796, 207500, 209551, 214777, 217484, 218663, 219471, 221759, 228723, 228778, 230764, 231174, 234076,
  236948, 238032, 240444, 240727, 243780
)
flagships <- toupper(flagships)

# filterFlagships <- function(dt) {
#   # Remove or replace invalid characters
#   dt[, instnm := stri_replace_all_regex(instnm, "[^\x01-\x7F]", "")]
#   
#   # Convert the 'instnm' column to uppercase for case-insensitive matching
#   dt[, instnm := toupper(instnm)]
#   
#   # Filter rows where 'instnm' matches any name in the 'flagships' list
#   dt <- dt[instnm %chin% flagships]
#   
#   return(dt)
# }


### USING IDs RATHER THAN NAMES TO MATCH NOW

filterFlagships <- function(dt) {
  dt <- dt[unitid %chin% flagship_ids]
  return(dt)
}

# Apply the function to each data table in 'filtered_list'
flagship_list <- lapply(filtered_list, filterFlagships)

# Rename 'line' to 'state' across all data tables in filtered_list
flagship_list <- lapply(flagship_list, function(dt) {
  setnames(dt, old = "line", new = "state")
  return(dt)
})


# OKAY THE LIST IS A LIST OF DATA TABLES BY YEAR WITH INFORMATION FOR EACH FLAGSHIP
# line == 99 means total
# line == 90 means foreign
# line == 98 means not listed
# line == fips means in-state
# so out-of-state is 99 - 98 - 90 - (line == fips)

# Function to transform a single data table
aggregateTable <- function(dt) {
  # Reshape to wide format first
  dt_wide <- dcast(dt, instnm + unitid + fips + year ~ state, 
                   fun.aggregate = sum,
                   value.var = "efres01")
  return(dt_wide)
}
# Apply the aggregate_table function to each data table in flasgship_list
aggregated_list <- lapply(flagship_list, aggregateTable)

# Rename 'line' to 'state' across all data tables in filtered_list
aggregated_list <- lapply(aggregated_list, function(dt) {
  setnames(dt, old = c("90","98","99"), new = c("foreign","not_listed","total"))
  return(dt)
})
# Drop '999' column if it exists
aggregated_list <- lapply(aggregated_list, function(dt) {
  if ("999" %in% names(dt)) {
    dt[, `999` := NULL]
  }
  return(dt)
})

# dynamically selects cell to put into in_state column (needs fips to equal column name)
inState <- function(dt_wide) {
  # Initialize 'in_state' column with NA values
  dt_wide[, in_state := NA_integer_]
  # Loop through each row to fill 'in_state'
  for(i in seq_len(nrow(dt_wide))) {
    fip_value <- as.character(dt_wide$fips[i])
    if (fip_value %in% names(dt_wide)) {
      temp <- dt_wide[i, get(fip_value)]
      dt_wide[i, in_state := temp]
    }
  }
 return(dt_wide) 
}

# Apply the aggregate_table function to each data table in aggregated_list
final_list <- lapply(aggregated_list, inState)

cleanList <- function(dt) {
  cols_to_select <- c(1:4, (ncol(dt) - 3):ncol(dt))
  dt <- dt[, ..cols_to_select]
}

clean_list <- lapply(final_list, cleanList)

analysis_dt <- rbindlist(clean_list)

analysis_dt <- analysis_dt[, out_state := total - foreign - not_listed - in_state]

analysis_dt <- analysis_dt[, out_share := out_state/total]

analysis_dt <- analysis_dt[, in_share := in_state/total]

analysis_dt <- analysis_dt[, other_share := (foreign + not_listed)/total]

final_analysis_dt <- analysis_dt[other_share != 1]

fwrite(final_analysis_dt, file.path(getwd(),"data","flagship_panel.csv"))

# temporary check

temp_dt <- fread(file.path(getwd(),"data","flagship_panel.csv"))

temp_2000_dt <- temp_dt[year == 2000]

#=====================================================================
# 2 - use flagships panel
#=====================================================================

# PLOT -- shares weighted by enrollment

# Function to compute weighted mean
weighted_mean_fun <- function(x, w) {
  sum(x * w) / sum(w)
  # mean(x)
}

# Calculate weighted averages
agg_dt <- final_analysis_dt[, .(
  avg_in = weighted_mean_fun(in_share, total), 
  avg_out = weighted_mean_fun(out_share, total), 
  avg_other = weighted_mean_fun(other_share, total)
), by = .(year)]

# melt the table 
melted_dt <- melt(agg_dt, id.vars = c("year"), 
                  measure.vars = c("avg_in", "avg_out","avg_other"),
                  variable.name = "enrollment_type",
                  value.name = "average")

melted_dt[, `Student Type` := fcase(
  enrollment_type == "avg_in", "In-State",
  enrollment_type == "avg_out", "Out-of-State",
  enrollment_type == "avg_other", "Other"
)]

# Create the plot
shares_timeseries <- ggplot(data = melted_dt, aes(x = year, y = average, group = `Student Type`, color = `Student Type`)) +
  geom_line(linewidth = 1.5, alpha = 0.5, linetype = "solid") +
  geom_point(size = 3) + ylim(0,0.8) + 
  labs(
    x = "Year",
    y = "Enrollment Shares",
    color = "Student Type"
  ) + theme_bw() + removeGridX() + scale_color_viridis_d() + 
  theme(
    legend.position = "bottom",
    legend.background = element_rect(color = "black", linetype = "solid", linewidth = 0.25),
    text = element_text(size = 12)) 

ggsave(file.path(getwd(),"figures","enrollment_shares_time.jpg"),
       plot = shares_timeseries,width = 8, height = 4.5)

## changes from 2000

# Compute difference from 2000
base_values <- melted_dt[year == 2000, .(year, enrollment_type, base_value = average)]
setnames(base_values, "year", "base_year")
for_plot_diff_dt <- merge(melted_dt, base_values, by = c("enrollment_type"), all.x = TRUE)

for_plot_diff_dt[, diff_from_2000 := 100 * (average - base_value)/(base_value)]

# Create the plot
shares_diff_plot <- ggplot(data = for_plot_diff_dt[`Student Type` != 'Other'], aes(x = year, y = diff_from_2000, group = `Student Type`, color = `Student Type`)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 0.5) +
  geom_line(linewidth = 2, alpha = 0.6, linetype = "solid") +
  geom_point(size = 3) +
  labs(
    x = "Year",
    y = "Change from Shares in 2000",
    color = "Enrollment Share"
  ) + 
  theme_bw() + 
  removeGridX() + 
  scale_color_viridis_d() + 
  theme(
    legend.position = "bottom",
    legend.background = element_rect(color = "black", linetype = "solid", linewidth = 0.25),
    text = element_text(size = 12)
  ) +
  scale_y_continuous(limits = c(-16,43),
                     breaks = c(-10,0,10,20,30,40),
                     labels = c("-15%","0%","10%","20%","30%","40%"))

print(shares_diff_plot)

ggsave(file.path(getwd(),"figures","enrollment_share_changes_over_time.png"),
       plot = shares_diff_plot,width = 8, height = 4.5)


# just for a few schools
# oregon (209551), maine (161253), alabama (100751), 
# utah (230764), mississippi (176017), wyoming (243780) steep
# 2012 - utah gets a new college president

plot_dt <- final_analysis_dt[,
                             Institution := 
                               fcase(
                                 unitid == 100751, "Alabama",
                                 unitid == 209551, "Oregon",
                                 unitid == 161253, "Maine",
                                 unitid == 230764, "Utah",
                                 unitid == 176017, "Ole Miss"
                               )
]

final_analysis_dt[, University := fifelse(unitid == 100751, "Alabama", "All Others")]

trend_ua <- final_analysis_dt[University == "Alabama", average_out_share := mean(out_share), by = year]

# Calculate the trends for "All Other" with weighted average
trend_all_other <- final_analysis_dt[University == "All Others", 
                                     average_out_share := sum(out_share * total) / sum(total), 
                                     by = year]

# Combine the trends into one data table for plotting or further analysis
combined_trends <- rbind(trend_ua, trend_all_other)

ua_versus_all <- ggplot(data = combined_trends, aes(x = year, y = average_out_share, color = University)) +
  geom_line(linewidth = 1.2, alpha = 0.5) +
  geom_point(size = 2.4) + ylim(0,0.8) + 
  labs(
    x = "Year",
    y = "Out-of-State % of First Year Enrollment",
  ) + theme_bw() + removeGridX() + scale_color_viridis_d() + 
  theme(
    legend.position = "bottom",
    legend.background = element_rect(color = "black", linetype = "solid", linewidth = 0.25),
    text = element_text(size = 12)) 

ggsave(file.path(getwd(),"figures","ua_and_all_others.jpg"),
       plot = ua_versus_all,width = 8, height = 4.5)

case_study <- ggplot(data = plot_dt[unitid == 100751| 
                                  unitid == 209551 | unitid == 176017 |
                                  unitid == 161253 | unitid == 230764 
                                 ], aes(x = year, y = out_share, color = Institution)) +
  geom_line(linewidth = 1.2, alpha = 0.5) +
  geom_point(size = 2.4) + ylim(0,0.7) + 
  labs(
    x = "Year",
    y = "Enrollment Shares") + theme_bw() + removeGridX() + scale_color_viridis_d() + 
  theme(
    legend.position = "bottom",
    legend.background = element_rect(color = "black", linetype = "solid", linewidth = 0.25),
    text = element_text(size = 12)) 

print(case_study)

ggsave(file.path(getwd(),"figures","case_study_out.jpg"),
       plot = case_study,width = 8, height = 4.5)


# just alabama (2003 a new president arrives)

ggplot(data = final_analysis_dt[unitid == 100751], aes(x = year, y = out_share)) +
  geom_line(linewidth = 1.5, color = "#21908CFF", alpha = 0.5) +
  geom_point(size = 3, color = "#440154FF") + ylim(0,0.8) + 
  labs(
    x = "Year",
    y = "Enrollment Shares"
  ) + theme_bw() + removeGridX() 




bama_dt <- melt(final_analysis_dt[unitid == 100751], id.vars = c("year"), 
                  measure.vars = c("in_state", "out_state","total"),
                  variable.name = "Type",
                  value.name = "Student Count")


ggplot(data = bama_dt, aes(x = year, y = `Student Count`, color = Type)) +
  geom_line(linewidth = 1.5, linetype = "dashed") +
  geom_point(size = 2.5) + 
  labs(
    x = "Year",
    y = "Enrollment",
    title = "University of Alabama First Year Enrollment 2000-2021"
  ) + theme_bw() + removeGridX() + scale_color_brewer(palette = "Set2")

out_2000 <- mean(final_analysis_dt[year == 2000, out_share])
out_2021 <- mean(final_analysis_dt[year == 2021, out_share])

# across all school 
density <- ggplot(final_analysis_dt[year == 2000 | year == 2021], aes(x=out_share, fill = factor(year))) + 
  geom_density(alpha=0.6,outline.type = "lower") +
  labs(title="", x="Out-of-State Student Share", y="Density", fill = "Enrollment Year") + 
  scale_fill_brewer(palette = "Paired") + 
  theme_bw() + removeGridX() + geom_vline(xintercept = 0.351,
                                            linetype="dashed", 
                                            color = "red", size=1,
                                          alpha = 0.5) + 
  geom_label(
    label="2021 Average: 35.1%", 
    x=0.495,
    y=2.75,
    label.padding = unit(0.55, "lines"), # Rectangle size around label
    label.size = 0.35,
    color = "white",
    fill="#1F78B4"
  ) + geom_vline(xintercept = 0.253,
                   linetype="dashed", 
                   color = "red", size=1, alpha = 0.5) + 
  geom_label(
    label="2000 Average: 25.3%", 
    x=0.125,
    y=3.25,
    label.padding = unit(0.55, "lines"), # Rectangle size around label
    label.size = 0.35,
    color = "white",
    fill="#A6CEE3"
  ) + ylim(0,4)


print(density)
  
ggsave(file.path(getwd(),"figures","density_out.jpg"),
       plot = density,width = 8, height = 4.5)

## converted continuous density to histogram

histogram_plot <- ggplot(final_analysis_dt[year == 2000 | year == 2021], aes(x = out_share, fill = factor(year))) + 
  geom_histogram(aes(y = ..density..), bins = 50, alpha = 0.6, position = "identity", color = 'black') + 
  labs(title = "", x = "Out-of-State Student Share", y = "Density", fill = "Enrollment Year") + 
  scale_fill_viridis_d() + 
  theme_bw() + 
  removeGridX() + 
  geom_vline(xintercept = 0.351, linetype = "dashed", color = "#21908CFF", size = 1, alpha = 0.5) + 
  geom_label(
    label = "2021 Average: 35.1%", 
    x = 0.495, 
    y = 2.75, 
    label.padding = unit(0.55, "lines"), 
    label.size = 0.35, 
    color = "black", 
    fill = "#FDE725FF"
  ) + 
  geom_vline(xintercept = 0.253, linetype = "dashed", color = "#21908CFF", size = 1, alpha = 0.5) + 
  geom_label(
    label = "2000 Average: 25.3%", 
    x = 0.125, 
    y = 3.25, 
    label.padding = unit(0.55, "lines"), 
    label.size = 0.35, 
    color = "black", 
    fill = "#440154FF"
  ) + 
  ylim(0, 6)

print(histogram_plot)

