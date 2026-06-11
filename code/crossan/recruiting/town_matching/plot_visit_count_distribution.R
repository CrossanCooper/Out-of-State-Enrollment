#=====================================================================
## Author: Crossan Cooper
## Last Modified: 06-08-2026
#
## file use: Summarize and plot the distribution of UA admissions-office
## visits across towns and recruiting school years.
#
## inputs:
## 1. town_matching/output/ua_visit_t_graduates_t_plus_5_panel.csv -- recruiting town-year panel
## 2. data/all_alabama_data.csv -- full UA commencement town universe
#
## outputs:
## 1. town_matching/output/visit_distribution/*.csv -- visit distribution summaries
## 2. output/docs/figures/recruiting_visit_count_distribution_all_commencement_towns.pdf/png -- full-denominator visit-bin figure
## 3. output/docs/figures/recruiting_any_visit_share_by_year.pdf/png -- any-visit share figure
## 4. output/docs/figures/recruiting_visit_extensive_margin_switchers.pdf/png -- switcher figure
#=====================================================================

#=====================================================================
# 1 - Packages, paths, and town normalization helpers
#=====================================================================

# default list of packages and cleaning command
rm(list=ls())
if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table,ggplot2,ggExtra)

default_data_root <- "/Users/crossancooper/Dropbox/Professional/active-projects/admissions_project"
repo_root <- normalizePath(
  Sys.getenv("ADMISSIONS_PROJECT_DATA_ROOT", default_data_root),
  mustWork = FALSE
)

panel_path <- file.path(repo_root, "town_matching", "output", "ua_visit_t_graduates_t_plus_5_panel.csv")
student_path <- file.path(repo_root, "data", "all_alabama_data.csv")
output_dir <- file.path(repo_root, "town_matching", "output", "visit_distribution")
figure_dir <- file.path(repo_root, "output", "docs", "figures")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

ascii_text <- function(value) {
  value[is.na(value)] <- ""
  value <- gsub("[\u2018\u2019\u02bc]", "", value)
  accent_map <- c(
    "\u00e1" = "a", "\u00c1" = "A", "\u00e0" = "a", "\u00c0" = "A",
    "\u00e2" = "a", "\u00c2" = "A", "\u00e4" = "a", "\u00c4" = "A",
    "\u00e3" = "a", "\u00c3" = "A", "\u00e5" = "a", "\u00c5" = "A",
    "\u00e9" = "e", "\u00c9" = "E", "\u00e8" = "e", "\u00c8" = "E",
    "\u00ea" = "e", "\u00ca" = "E", "\u00eb" = "e", "\u00cb" = "E",
    "\u00ed" = "i", "\u00cd" = "I", "\u00ec" = "i", "\u00cc" = "I",
    "\u00ee" = "i", "\u00ce" = "I", "\u00ef" = "i", "\u00cf" = "I",
    "\u00f1" = "n", "\u00d1" = "N",
    "\u00f3" = "o", "\u00d3" = "O", "\u00f2" = "o", "\u00d2" = "O",
    "\u00f4" = "o", "\u00d4" = "O", "\u00f6" = "o", "\u00d6" = "O",
    "\u00f5" = "o", "\u00d5" = "O",
    "\u00fa" = "u", "\u00da" = "U", "\u00f9" = "u", "\u00d9" = "U",
    "\u00fb" = "u", "\u00db" = "U", "\u00fc" = "u", "\u00dc" = "U",
    "\u00e7" = "c", "\u00c7" = "C"
  )
  for (accented_char in names(accent_map)) {
    value <- gsub(accented_char, accent_map[[accented_char]], value, fixed = TRUE)
  }
  iconv(value, from = "", to = "ASCII//TRANSLIT", sub = "")
}

squash_spaces <- function(value) {
  trimws(gsub("\\s+", " ", value))
}

collapse_spelled_letters <- function(value) {
  vapply(
    strsplit(value, " ", fixed = TRUE),
    function(tokens) {
      if (length(tokens) >= 2 && all(nchar(tokens) == 1) && all(grepl("^[A-Za-z]$", tokens))) {
        paste0(tokens, collapse = "")
      } else {
        paste0(tokens, collapse = " ")
      }
    },
    character(1)
  )
}

clean_student_town <- function(raw_town) {
  town <- squash_spaces(raw_town)
  town <- gsub(",.*", "", town)
  town <- gsub("(?<=[a-z])(?=[A-Z])", " ", town, perl = TRUE)

  manual_map <- c(
    "A LB ER TV IL LE" = "Albertville",
    "A LL EN" = "Allen",
    "A RL IN GT ON" = "Arlington",
    "ABBBEVILLE" = "Abbeville",
    "AGOURA" = "Agoura Hills",
    "ALEX CITY" = "Alexander City",
    "ALEXANDER CITY" = "Alexander City",
    "VESTAVIA" = "Vestavia Hills",
    "VESTAVIA HILL" = "Vestavia Hills",
    "VESTAVIA HILLS" = "Vestavia Hills"
  )

  key <- toupper(squash_spaces(town))
  matched <- key %chin% names(manual_map)
  town[matched] <- unname(manual_map[key[matched]])
  trimws(town)
}

clean_student_state <- function(raw_state) {
  gsub("[.,]", "", squash_spaces(raw_state))
}

normalize_town <- function(town_value) {
  town <- toupper(collapse_spelled_letters(squash_spaces(ascii_text(town_value))))
  town <- gsub("&", " AND ", town, fixed = TRUE)
  town <- gsub("\\bFT\\.?\\b", "FORT", town)
  town <- gsub("\\bMT\\.?\\b", "MOUNT", town)
  town <- gsub("\\bST\\.?\\b", "SAINT", town)
  town <- gsub("\\bTWP\\.?\\b", "TOWNSHIP", town)
  town <- gsub("[^[:alnum:]_[:space:]]", " ", town)
  town <- gsub("_", " ", town, fixed = TRUE)
  squash_spaces(town)
}

normalize_state <- function(state_value) {
  state_lookup <- c(
    stats::setNames(state.abb, toupper(state.name)),
    "DISTRICT OF COLUMBIA" = "DC"
  )
  state <- toupper(gsub("[.,]", "", collapse_spelled_letters(squash_spaces(ascii_text(state_value)))))
  matched <- state %chin% names(state_lookup)
  state[matched] <- unname(state_lookup[state[matched]])
  state
}

#=====================================================================
# 2 - Build full commencement and panel town universes
#=====================================================================

panel <- fread(panel_path)
numeric_cols <- c(
  "recruiting_school_year",
  "visits_t",
  "any_visit_t",
  "in_student_origin_town_universe",
  "visits_t_with_same_state_fuzzy",
  "all_raw_visit_rows_t",
  "review_needed_visits_t",
  "fixed_hs_denominator_available"
)
numeric_cols <- intersect(numeric_cols, names(panel))
panel[, (numeric_cols) := lapply(.SD, as.numeric), .SDcols = numeric_cols]

students <- fread(student_path)
students[, Year := as.integer(Year)]
max_commencement_year <- max(students$Year, na.rm = TRUE)
min_commencement_year <- min(students$Year, na.rm = TRUE)
commencement_start_year <- 2015L

student_town_years <- students[
  !is.na(Year),
  .(
    processed_town = normalize_town(clean_student_town(`Origin Town`)),
    processed_state = normalize_state(clean_student_state(`Origin State`)),
    Year
  )
]
student_town_years <- student_town_years[nzchar(processed_town) & nzchar(processed_state)]

all_commencement_towns <- unique(student_town_years[, .(processed_town, processed_state)])
all_commencement_towns[, all_commencement_record_town := TRUE]
all_commencement_towns[
  ,
  state_group := fifelse(processed_state == "AL", "Alabama", "Outside Alabama")
]

commencement_towns <- unique(
  student_town_years[
    Year >= commencement_start_year & Year <= max_commencement_year,
    .(processed_town, processed_state)
  ]
)
commencement_towns <- unique(
  commencement_towns[nzchar(processed_town) & nzchar(processed_state)]
)
commencement_towns[, commencement_2015plus_town := TRUE]

# The underlying visit data are from August 2016 through June 2020. In the
# constructed panel, that corresponds to school years 2016-17 through 2019-20.
analysis_panel <- panel[recruiting_school_year %in% 2016:2019]

visited_towns <- unique(
  analysis_panel[visits_t > 0, .(processed_town, processed_state)]
)
visited_towns[, preferred_visit_town := TRUE]

analysis_panel_all_commencement <- merge(
  analysis_panel,
  all_commencement_towns,
  by = c("processed_town", "processed_state"),
  all = FALSE
)

any_visit_all_commencement_by_year <- analysis_panel_all_commencement[
  ,
  .(
    towns = .N,
    visited_towns = sum(any_visit_t),
    share_any_visit = mean(any_visit_t)
  ),
  by = .(recruiting_school_year, recruiting_school_year_label)
][order(recruiting_school_year)]
any_visit_all_commencement_overall <- analysis_panel_all_commencement[
  ,
  .(
    recruiting_school_year = NA_integer_,
    recruiting_school_year_label = "2016-17 to 2019-20",
    towns = uniqueN(paste(processed_town, processed_state, sep = "||")),
    visited_towns = uniqueN(
      paste(processed_town[visits_t > 0], processed_state[visits_t > 0], sep = "||")
    )
  )
]
any_visit_all_commencement_overall[
  ,
  share_any_visit := visited_towns / towns
]
any_visit_all_commencement_out <- rbind(
  any_visit_all_commencement_by_year,
  any_visit_all_commencement_overall,
  fill = TRUE
)
fwrite(
  any_visit_all_commencement_out,
  file.path(output_dir, "any_visit_share_by_school_year_all_commencement_towns.csv")
)

any_visit_all_commencement_by_year_state <- analysis_panel_all_commencement[
  ,
  .(
    towns = .N,
    visited_towns = sum(any_visit_t),
    share_any_visit = mean(any_visit_t)
  ),
  by = .(recruiting_school_year, recruiting_school_year_label, state_group)
][order(state_group, recruiting_school_year)]
any_visit_all_commencement_overall_state <- analysis_panel_all_commencement[
  ,
  .(
    recruiting_school_year = NA_integer_,
    recruiting_school_year_label = "2016-17 to 2019-20",
    towns = uniqueN(paste(processed_town, processed_state, sep = "||")),
    visited_towns = uniqueN(
      paste(processed_town[visits_t > 0], processed_state[visits_t > 0], sep = "||")
    )
  ),
  by = state_group
]
any_visit_all_commencement_overall_state[
  ,
  share_any_visit := visited_towns / towns
]
any_visit_all_commencement_state_out <- rbind(
  any_visit_all_commencement_by_year_state,
  any_visit_all_commencement_overall_state,
  fill = TRUE
)
fwrite(
  any_visit_all_commencement_state_out,
  file.path(output_dir, "any_visit_share_by_school_year_all_commencement_towns_by_state_group.csv")
)

plot_universe <- merge(
  commencement_towns,
  visited_towns,
  by = c("processed_town", "processed_state"),
  all = TRUE
)
plot_universe[is.na(commencement_2015plus_town), commencement_2015plus_town := FALSE]
plot_universe[is.na(preferred_visit_town), preferred_visit_town := FALSE]

missing_from_panel <- fsetdiff(
  plot_universe[, .(processed_town, processed_state)],
  unique(panel[, .(processed_town, processed_state)])
)
fwrite(missing_from_panel, file.path(output_dir, "visit_count_universe_missing_from_panel.csv"))

#=====================================================================
# 3 - Visit-count distributions and any-visit shares
#=====================================================================

analysis_panel <- merge(
  analysis_panel,
  plot_universe,
  by = c("processed_town", "processed_state"),
  all = FALSE
)

summary_by_year <- analysis_panel[
  ,
  .(
    towns = .N,
    total_visits = sum(visits_t),
    mean_visits = mean(visits_t),
    share_any_visit = mean(any_visit_t),
    visited_town_years = sum(any_visit_t),
    mean_visits_if_visited = mean(visits_t[visits_t > 0]),
    median_visits_if_visited = median(visits_t[visits_t > 0]),
    max_visits = max(visits_t)
  ),
  by = .(recruiting_school_year, recruiting_school_year_label)
][order(recruiting_school_year)]

summary_overall <- analysis_panel[
  ,
  .(
    recruiting_school_year = NA_integer_,
    recruiting_school_year_label = "2016-17 to 2019-20",
    towns = .N,
    total_visits = sum(visits_t),
    mean_visits = mean(visits_t),
    share_any_visit = mean(any_visit_t),
    visited_town_years = sum(any_visit_t),
    mean_visits_if_visited = mean(visits_t[visits_t > 0]),
    median_visits_if_visited = median(visits_t[visits_t > 0]),
    max_visits = max(visits_t)
  )
]

summary_out <- rbind(summary_by_year, summary_overall, fill = TRUE)
fwrite(summary_out, file.path(output_dir, "visit_count_summary_by_school_year.csv"))

any_visit_commencement_by_year <- analysis_panel[
  commencement_2015plus_town == TRUE,
  .(
    towns = .N,
    visited_towns = sum(any_visit_t),
    share_any_visit = mean(any_visit_t)
  ),
  by = .(recruiting_school_year, recruiting_school_year_label)
][order(recruiting_school_year)]
any_visit_commencement_overall <- analysis_panel[
  commencement_2015plus_town == TRUE,
  .(
    recruiting_school_year = NA_integer_,
    recruiting_school_year_label = "2016-17 to 2019-20",
    towns = uniqueN(paste(processed_town, processed_state, sep = "||")),
    visited_towns = uniqueN(
      paste(processed_town[visits_t > 0], processed_state[visits_t > 0], sep = "||")
    )
  )
]
any_visit_commencement_overall[
  ,
  share_any_visit := visited_towns / towns
]
any_visit_commencement_out <- rbind(
  any_visit_commencement_by_year,
  any_visit_commencement_overall,
  fill = TRUE
)
fwrite(
  any_visit_commencement_out,
  file.path(output_dir, "any_visit_share_by_school_year_commencement_towns.csv")
)

town_sample <- analysis_panel[
  ,
  .(
    commencement_2015plus_town = as.integer(any(commencement_2015plus_town)),
    panel_student_origin_town_all_years = as.integer(any(in_student_origin_town_universe == 1, na.rm = TRUE)),
    preferred_visit_town = as.integer(any(visits_t > 0, na.rm = TRUE)),
    broad_visit_town = as.integer(any(visits_t_with_same_state_fuzzy > 0, na.rm = TRUE)),
    raw_recruiting_row_town = as.integer(any(all_raw_visit_rows_t > 0, na.rm = TRUE)),
    review_needed_town = as.integer(any(review_needed_visits_t > 0, na.rm = TRUE)),
    fixed_2017_denominator_town = as.integer(any(fixed_hs_denominator_available == 1, na.rm = TRUE))
  ),
  by = .(processed_town, processed_state)
]

town_sample_summary <- town_sample[
  ,
  .(
    commencement_start_year = commencement_start_year,
    commencement_end_year = max_commencement_year,
    all_commencement_start_year = min_commencement_year,
    all_commencement_end_year = max_commencement_year,
    towns = .N,
    alabama_towns = sum(processed_state == "AL"),
    outside_alabama_towns = sum(processed_state != "AL"),
    all_commencement_record_towns = nrow(all_commencement_towns),
    commencement_2015plus_towns = sum(commencement_2015plus_town),
    panel_student_origin_towns_all_years = sum(panel_student_origin_town_all_years),
    raw_recruiting_row_towns = sum(raw_recruiting_row_town),
    preferred_visit_towns = sum(preferred_visit_town),
    broad_visit_towns = sum(broad_visit_town),
    review_needed_towns = sum(review_needed_town),
    fixed_2017_denominator_towns = sum(fixed_2017_denominator_town),
    commencement_only_towns = sum(commencement_2015plus_town == 1 & preferred_visit_town == 0),
    commencement_and_preferred_visit_towns = sum(commencement_2015plus_town == 1 & preferred_visit_town == 1),
    visited_no_2015plus_graduate_towns = sum(commencement_2015plus_town == 0 & preferred_visit_town == 1),
    panel_student_only_towns_all_years = sum(panel_student_origin_town_all_years == 1 & preferred_visit_town == 0),
    panel_student_and_preferred_visit_towns_all_years = sum(
      panel_student_origin_town_all_years == 1 & preferred_visit_town == 1
    ),
    raw_recruiting_only_review_towns_all_years = sum(
      panel_student_origin_town_all_years == 0 & raw_recruiting_row_town == 1
    )
  )
]
fwrite(town_sample_summary, file.path(output_dir, "visit_count_town_sample_summary.csv"))

distribution <- analysis_panel[
  ,
  .N,
  by = .(visits_t)
][order(visits_t)]
distribution[, share := N / sum(N)]
fwrite(distribution, file.path(output_dir, "visit_count_distribution.csv"))

plot_data_all_commencement <- copy(analysis_panel_all_commencement)
plot_data_all_commencement[
  ,
  visit_bucket_full_commencement := fifelse(
    visits_t == 0,
    "0",
    fifelse(
      visits_t == 1,
      "1",
      fifelse(
        visits_t == 2,
        "2",
        fifelse(visits_t <= 5, "3-5", fifelse(visits_t <= 10, "6-10", "11+"))
      )
    )
  )
]
plot_data_all_commencement[
  ,
  visit_bucket_full_commencement := factor(
    visit_bucket_full_commencement,
    levels = c("0", "1", "2", "3-5", "6-10", "11+")
  )
]

bucket_share_all_commencement <- plot_data_all_commencement[
  ,
  .N,
  by = .(recruiting_school_year_label, visit_bucket_full_commencement)
]
bucket_share_all_commencement[
  ,
  share := N / sum(N),
  by = recruiting_school_year_label
]
fwrite(
  bucket_share_all_commencement[order(recruiting_school_year_label, visit_bucket_full_commencement)],
  file.path(output_dir, "visit_count_bucket_distribution_all_commencement_towns.csv")
)

#=====================================================================
# 4 - Additional visit-bin summaries
#=====================================================================

plot_data <- copy(analysis_panel)
plot_data[
  ,
  visit_bucket := fifelse(
    visits_t == 0,
    "0",
    fifelse(
      visits_t == 1,
      "1",
      fifelse(
        visits_t == 2,
        "2",
        fifelse(
          visits_t == 3,
          "3",
          fifelse(visits_t <= 5, "4-5", fifelse(visits_t <= 10, "6-10", "11+"))
        )
      )
    )
  )
]
plot_data[
  ,
  visit_bucket := factor(visit_bucket, levels = c("0", "1", "2", "3", "4-5", "6-10", "11+"))
]

bucket_share <- plot_data[, .N, by = .(recruiting_school_year_label, visit_bucket)]
bucket_share[
  ,
  share := N / sum(N),
  by = recruiting_school_year_label
]

positive_density <- plot_data[visits_t > 0]
positive_density[
  ,
  visit_bucket_positive := fifelse(
    visits_t == 1,
    "1",
    fifelse(
      visits_t == 2,
      "2",
      fifelse(
        visits_t == 3,
        "3",
        fifelse(visits_t <= 5, "4-5", fifelse(visits_t <= 10, "6-10", "11+"))
      )
    )
  )
]
positive_density[
  ,
  visit_bucket_positive := factor(
    visit_bucket_positive,
    levels = c("1", "2", "3", "4-5", "6-10", "11+")
  )
]

positive_share <- positive_density[, .N, by = .(recruiting_school_year_label, visit_bucket_positive)]
positive_share[
  ,
  share := N / sum(N),
  by = recruiting_school_year_label
]

#=====================================================================
# 5 - Extensive-margin switcher summaries
#=====================================================================

transition_panel <- analysis_panel[
  order(processed_state, processed_town, recruiting_school_year),
  .(processed_town, processed_state, recruiting_school_year, recruiting_school_year_label, any_visit_t)
]
transition_panel[
  ,
  `:=`(
    previous_any_visit = shift(any_visit_t),
    previous_school_year = shift(recruiting_school_year),
    previous_school_year_label = shift(recruiting_school_year_label)
  ),
  by = .(processed_town, processed_state)
]
transition_panel <- transition_panel[
  !is.na(previous_any_visit) & recruiting_school_year == previous_school_year + 1
]
transition_panel[
  ,
  transition_period := paste(previous_school_year_label, recruiting_school_year_label, sep = " to ")
]
transition_panel[
  ,
  transition_type := fcase(
    previous_any_visit == 0 & any_visit_t == 0, "0 to 0",
    previous_any_visit == 0 & any_visit_t == 1, "0 to 1",
    previous_any_visit == 1 & any_visit_t == 0, "1 to 0",
    previous_any_visit == 1 & any_visit_t == 1, "1 to 1"
  )
]

transition_denominators <- transition_panel[
  ,
  .(
    towns = .N,
    prior_zero_towns = sum(previous_any_visit == 0),
    prior_one_towns = sum(previous_any_visit == 1)
  ),
  by = .(recruiting_school_year, transition_period)
]

transition_summary <- transition_panel[
  ,
  .(transition_towns = .N),
  by = .(recruiting_school_year, transition_period, transition_type)
]
transition_summary <- merge(
  transition_summary,
  transition_denominators,
  by = c("recruiting_school_year", "transition_period")
)
transition_summary[
  ,
  `:=`(
    share_of_towns = transition_towns / towns,
    conditional_transition_share = fifelse(
      transition_type %chin% c("0 to 0", "0 to 1"),
      transition_towns / prior_zero_towns,
      transition_towns / prior_one_towns
    )
  )
]
transition_summary <- transition_summary[
  order(recruiting_school_year, transition_type)
]
fwrite(transition_summary, file.path(output_dir, "visit_extensive_margin_transition_summary.csv"))

transition_panel[
  ,
  state_group := fifelse(processed_state == "AL", "Alabama", "Outside Alabama")
]

transition_state_denominators <- transition_panel[
  ,
  .(
    state_group_towns = .N,
    state_group_prior_zero_towns = sum(previous_any_visit == 0),
    state_group_prior_one_towns = sum(previous_any_visit == 1)
  ),
  by = .(recruiting_school_year, transition_period, state_group)
]

transition_state_summary <- transition_panel[
  ,
  .(transition_towns = .N),
  by = .(recruiting_school_year, transition_period, state_group, transition_type)
]
transition_state_summary <- merge(
  transition_state_summary,
  transition_state_denominators,
  by = c("recruiting_school_year", "transition_period", "state_group")
)
transition_state_summary[
  ,
  `:=`(
    share_of_state_group_towns = transition_towns / state_group_towns,
    conditional_transition_share = fifelse(
      transition_type %chin% c("0 to 0", "0 to 1"),
      transition_towns / state_group_prior_zero_towns,
      transition_towns / state_group_prior_one_towns
    )
  )
]
transition_state_summary <- transition_state_summary[
  order(recruiting_school_year, transition_type, state_group)
]
fwrite(
  transition_state_summary,
  file.path(output_dir, "visit_extensive_margin_transition_summary_by_state_group.csv")
)

switcher_state_composition <- transition_panel[
  transition_type %chin% c("0 to 1", "1 to 0"),
  .(switcher_towns = .N),
  by = .(recruiting_school_year, transition_period, transition_type, state_group)
]
switcher_state_composition[
  ,
  total_switchers := sum(switcher_towns),
  by = .(recruiting_school_year, transition_period, transition_type)
]
switcher_state_composition[
  ,
  share_of_switchers := switcher_towns / total_switchers
]

switcher_state_overall <- transition_panel[
  transition_type %chin% c("0 to 1", "1 to 0"),
  .(switcher_towns = .N),
  by = .(transition_type, state_group)
]
switcher_state_overall[
  ,
  `:=`(
    recruiting_school_year = NA_integer_,
    transition_period = "All transitions",
    total_switchers = sum(switcher_towns)
  ),
  by = transition_type
]
switcher_state_overall[
  ,
  share_of_switchers := switcher_towns / total_switchers
]
switcher_state_composition <- rbind(
  switcher_state_composition,
  switcher_state_overall,
  fill = TRUE
)
switcher_state_composition <- switcher_state_composition[
  order(recruiting_school_year, transition_type, state_group)
]
fwrite(
  switcher_state_composition,
  file.path(output_dir, "visit_extensive_margin_switcher_state_composition.csv")
)

unique_switcher_towns <- transition_panel[
  ,
  .(
    state_group = first(state_group),
    any_extensive_margin_switcher = any(transition_type %chin% c("0 to 1", "1 to 0")),
    ever_newly_visited = any(transition_type == "0 to 1"),
    ever_no_longer_visited = any(transition_type == "1 to 0"),
    transition_switch_count = sum(transition_type %chin% c("0 to 1", "1 to 0")),
    newly_visited_switch_count = sum(transition_type == "0 to 1"),
    no_longer_visited_switch_count = sum(transition_type == "1 to 0")
  ),
  by = .(processed_town, processed_state)
]
fwrite(
  unique_switcher_towns,
  file.path(output_dir, "visit_extensive_margin_unique_switcher_towns.csv")
)

unique_switcher_summary <- unique_switcher_towns[
  ,
  .(
    towns = .N,
    any_extensive_margin_switcher_towns = sum(any_extensive_margin_switcher),
    ever_newly_visited_towns = sum(ever_newly_visited),
    ever_no_longer_visited_towns = sum(ever_no_longer_visited),
    share_any_extensive_margin_switcher = mean(any_extensive_margin_switcher),
    share_ever_newly_visited = mean(ever_newly_visited),
    share_ever_no_longer_visited = mean(ever_no_longer_visited)
  ),
  by = state_group
]
unique_switcher_summary <- rbind(
  unique_switcher_summary,
  unique_switcher_towns[
    ,
    .(
      state_group = "All towns",
      towns = .N,
      any_extensive_margin_switcher_towns = sum(any_extensive_margin_switcher),
      ever_newly_visited_towns = sum(ever_newly_visited),
      ever_no_longer_visited_towns = sum(ever_no_longer_visited),
      share_any_extensive_margin_switcher = mean(any_extensive_margin_switcher),
      share_ever_newly_visited = mean(ever_newly_visited),
      share_ever_no_longer_visited = mean(ever_no_longer_visited)
    )
  ],
  fill = TRUE
)
fwrite(
  unique_switcher_summary,
  file.path(output_dir, "visit_extensive_margin_unique_switcher_summary_by_state_group.csv")
)

switch_share <- transition_summary[transition_type %chin% c("0 to 1", "1 to 0")]
switch_share[
  ,
  transition_type := factor(
    transition_type,
    levels = c("0 to 1", "1 to 0"),
    labels = c("Newly visited", "No longer visited")
  )
]

#=====================================================================
# 6 - Paper figures
#=====================================================================

theme_recruiting <- theme_minimal(base_size = 10) +
  removeGridX() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold")
  )

p_all <- ggplot(bucket_share, aes(x = visit_bucket, y = share, fill = recruiting_school_year_label)) +
  geom_col(position = position_dodge(width = 0.78), width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_viridis_d(option = "D") +
  labs(
    x = "Admissions officer visits",
    y = "Share of towns"
  ) +
  theme_recruiting

p_positive <- ggplot(
  positive_share,
  aes(x = visit_bucket_positive, y = share, fill = recruiting_school_year_label)
) +
  geom_col(position = position_dodge(width = 0.78), width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_viridis_d(option = "D") +
  labs(
    x = "Admissions officer visits",
    y = "Share of towns"
  ) +
  theme_recruiting

p_switch <- ggplot(switch_share, aes(x = transition_period, y = share_of_towns, fill = transition_type)) +
  geom_col(position = position_dodge(width = 0.72), width = 0.62) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_viridis_d(option = "D") +
  labs(
    x = "School-year transition",
    y = "Share of towns"
  ) +
  theme_recruiting

p_any_visit <- ggplot(
  any_visit_all_commencement_by_year,
  aes(x = recruiting_school_year_label, y = share_any_visit, fill = recruiting_school_year_label)
) +
  geom_col(width = 0.62, show.legend = FALSE) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_viridis_d(option = "D") +
  labs(
    x = "Recruiting school year",
    y = "Share of towns"
  ) +
  theme_recruiting

p_any_visit_2015plus <- ggplot(
  any_visit_commencement_by_year,
  aes(x = recruiting_school_year_label, y = share_any_visit, fill = recruiting_school_year_label)
) +
  geom_col(width = 0.62, show.legend = FALSE) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_viridis_d(option = "D") +
  labs(
    x = "Recruiting school year",
    y = "Share of towns"
  ) +
  theme_recruiting

p_any_visit_state <- ggplot(
  any_visit_all_commencement_by_year_state,
  aes(x = recruiting_school_year_label, y = share_any_visit, fill = state_group)
) +
  geom_col(position = position_dodge(width = 0.72), width = 0.62) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_viridis_d(option = "D") +
  labs(
    x = "Recruiting school year",
    y = "Share of towns"
  ) +
  theme_recruiting

p_all_commencement_bins <- ggplot(
  bucket_share_all_commencement,
  aes(x = visit_bucket_full_commencement, y = share, fill = recruiting_school_year_label)
) +
  geom_col(position = position_dodge(width = 0.78), width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_viridis_d(option = "D") +
  labs(
    x = "Admissions officer visits",
    y = "Share of towns"
  ) +
  theme_recruiting

ggsave(
  file.path(figure_dir, "recruiting_visit_count_distribution_all.pdf"),
  p_all,
  width = 8.5,
  height = 5.2,
  units = "in",
  device = "pdf"
)
ggsave(
  file.path(figure_dir, "recruiting_visit_count_distribution_all.png"),
  p_all,
  width = 8.5,
  height = 5.2,
  units = "in",
  dpi = 220
)
ggsave(
  file.path(figure_dir, "recruiting_visit_count_distribution_positive.pdf"),
  p_positive,
  width = 8.5,
  height = 5.2,
  units = "in",
  device = "pdf"
)
ggsave(
  file.path(figure_dir, "recruiting_visit_count_distribution_positive.png"),
  p_positive,
  width = 8.5,
  height = 5.2,
  units = "in",
  dpi = 220
)
ggsave(
  file.path(figure_dir, "recruiting_visit_extensive_margin_switchers.pdf"),
  p_switch,
  width = 8.5,
  height = 5.2,
  units = "in",
  device = "pdf"
)
ggsave(
  file.path(figure_dir, "recruiting_visit_extensive_margin_switchers.png"),
  p_switch,
  width = 8.5,
  height = 5.2,
  units = "in",
  dpi = 220
)
ggsave(
  file.path(figure_dir, "recruiting_any_visit_share_by_year.pdf"),
  p_any_visit,
  width = 6.5,
  height = 4.0,
  units = "in",
  device = "pdf"
)
ggsave(
  file.path(figure_dir, "recruiting_any_visit_share_by_year.png"),
  p_any_visit,
  width = 6.5,
  height = 4.0,
  units = "in",
  dpi = 220
)
ggsave(
  file.path(figure_dir, "recruiting_any_visit_share_by_year_2015plus_commencement.pdf"),
  p_any_visit_2015plus,
  width = 6.5,
  height = 4.0,
  units = "in",
  device = "pdf"
)
ggsave(
  file.path(figure_dir, "recruiting_any_visit_share_by_year_2015plus_commencement.png"),
  p_any_visit_2015plus,
  width = 6.5,
  height = 4.0,
  units = "in",
  dpi = 220
)
ggsave(
  file.path(figure_dir, "recruiting_any_visit_share_by_year_state_split.pdf"),
  p_any_visit_state,
  width = 6.5,
  height = 4.0,
  units = "in",
  device = "pdf"
)
ggsave(
  file.path(figure_dir, "recruiting_any_visit_share_by_year_state_split.png"),
  p_any_visit_state,
  width = 6.5,
  height = 4.0,
  units = "in",
  dpi = 220
)
ggsave(
  file.path(figure_dir, "recruiting_visit_count_distribution_all_commencement_towns.pdf"),
  p_all_commencement_bins,
  width = 8.5,
  height = 5.2,
  units = "in",
  device = "pdf"
)
ggsave(
  file.path(figure_dir, "recruiting_visit_count_distribution_all_commencement_towns.png"),
  p_all_commencement_bins,
  width = 8.5,
  height = 5.2,
  units = "in",
  dpi = 220
)

cat("Wrote visit summaries to ", output_dir, "\n", sep = "")
cat("Wrote visit distribution figures to ", figure_dir, "\n", sep = "")
