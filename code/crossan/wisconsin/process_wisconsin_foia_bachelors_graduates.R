#!/usr/bin/env Rscript

#=====================================================================
## Author: Crossan Cooper
## Last Modified: 06-11-2026

## file use: Process Wisconsin FOIA graduate records into one cleaned
## individual-year file and one U.S. home ZIP-by-year bachelor graduate
## count file. The script keeps all cleaned individual-year records but
## restricts the ZIP-year count output to U.S. home ZIPs with valid five-
## digit ZIP codes.
#
## inputs:
## 1. wisconsin_graduate_data.xlsx -- raw Wisconsin FOIA graduate award
##    records. Each row is an award/major record, so a student with several
##    majors or degrees can appear multiple times in a confer year.
#
## outputs:
## 1. wisconsin_bachelors_grads_individual_year_names.csv -- cleaned
##    individual-by-confer-year records with collapsed names, locations,
##    terms, degree descriptions, degree groups, majors, and honors flags.
## 2. wisconsin_bachelors_grads_zip_year_counts.csv -- U.S. home ZIP-by-
##    confer-year counts of unique Wisconsin bachelor graduate person-years.
## 3. wisconsin_bachelors_grads_processing_summary.csv -- compact QA
##    summary of raw rows, bachelor rows, collapsed individual-years, and
##    ZIP coverage.
#=====================================================================


#=====================================================================
# 1 - Load packages and define file paths
#=====================================================================

## (a) Load packages with pacman, following the broader Crossan code pattern.
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  readxl,
  dplyr,
  stringr,
  readr,
  tidyr
)

script_args <- commandArgs(trailingOnly = TRUE)

## (b) The active code lives in Out-of-State-Enrollment, but the canonical data
## folder remains the original admissions_project directory.
default_input_path <- file.path(
  "/Users/crossancooper/Dropbox/Professional/active-projects/admissions_project",
  "data",
  "wisconsin_foia",
  "wisconsin_graduate_data.xlsx"
)

default_output_dir <- file.path(
  "/Users/crossancooper/Dropbox/Professional/active-projects/admissions_project",
  "data",
  "wisconsin_foia",
  "processed"
)

## (c) Optional command-line arguments make testing possible without touching
## the production data folder: arg 1 is input path, arg 2 is output directory.
input_path <- if (length(script_args) >= 1 && nzchar(script_args[[1]])) {
  script_args[[1]]
} else {
  default_input_path
}

output_dir <- if (length(script_args) >= 2 && nzchar(script_args[[2]])) {
  script_args[[2]]
} else {
  default_output_dir
}

individual_year_output_path <- file.path(
  output_dir,
  "wisconsin_bachelors_grads_individual_year_names.csv"
)

zip_year_output_path <- file.path(
  output_dir,
  "wisconsin_bachelors_grads_zip_year_counts.csv"
)

summary_output_path <- file.path(
  output_dir,
  "wisconsin_bachelors_grads_processing_summary.csv"
)

## (d) Keep the expected schema explicit so a changed FOIA extract fails before
## producing misleading processed outputs.
expected_columns <- c(
  "NAME",
  "HOME_COUNTRY",
  "HOME_STATE",
  "HOME_COUNTY",
  "HOME_CITY",
  "HOME_ZIP_CODE",
  "AWARD_CONFER_YEAR",
  "AWARD_COMPLETION_TERM",
  "AWARD_MAJOR",
  "AWARD_FORMAL_DESCR",
  "DEGREE_HONORS_SUFFIX",
  "DEGREE_HONORS"
)


#=====================================================================
# 2 - Define cleaning helpers
#=====================================================================

### i. General string utilities

normalize_text <- function(x) {
  ## (a) Treat blank strings as missing values before downstream grouping.
  out <- str_squish(as.character(x))
  out[out == ""] <- NA_character_
  out
}

normalize_upper <- function(x) {
  out <- normalize_text(x)
  str_to_upper(out)
}

normalize_city <- function(x) {
  ## (b) City capitalization is only for readability; matching should use
  ## separate standardized geographic crosswalks if needed later.
  out <- normalize_text(x)
  str_to_title(str_to_lower(out))
}

clean_name <- function(x) {
  out <- normalize_text(x)

  ## (c) Standardize comma spacing while preserving the original name order.
  out <- str_replace_all(out, "\\s*,\\s*", ", ")
  out
}

collapse_unique <- function(x) {
  ## (d) Collapse repeated award-row values into deterministic pipe-delimited
  ## lists, so output diffs are stable across runs.
  values <- normalize_text(x)
  values <- sort(unique(values[!is.na(values)]))

  if (length(values) == 0) {
    return(NA_character_)
  }

  paste(values, collapse = " | ")
}

modal_text <- function(x) {
  ## (e) Return the most frequent non-missing string, using alphabetical order
  ## to break ties deterministically.
  values <- normalize_text(x)
  values <- values[!is.na(values)]

  if (length(values) == 0) {
    return(NA_character_)
  }

  value_counts <- sort(table(values), decreasing = TRUE)
  tied_values <- names(value_counts)[value_counts == max(value_counts)]

  sort(tied_values)[[1]]
}

min_integer_or_na <- function(x) {
  ## (f) dplyr::summarise() should return an integer NA for all-missing groups.
  values <- x[!is.na(x)]

  if (length(values) == 0) {
    return(NA_integer_)
  }

  min(values)
}

max_integer_or_na <- function(x) {
  ## (g) Mirror min_integer_or_na() for the maximum academic-year-start field.
  values <- x[!is.na(x)]

  if (length(values) == 0) {
    return(NA_integer_)
  }

  max(values)
}

### ii. Degree, term, and geography utilities

extract_confer_year <- function(x) {
  ## (a) readxl imports AWARD_CONFER_YEAR as a date in the current workbook, but
  ## this fallback also handles future extracts where the field is text.
  if (inherits(x, "Date") || inherits(x, "POSIXt")) {
    return(as.integer(format(as.Date(x), "%Y")))
  }

  as.integer(str_extract(as.character(x), "[0-9]{4}"))
}

extract_completion_season <- function(x) {
  str_to_title(str_extract(normalize_text(x), "^[A-Za-z]+"))
}

extract_academic_year_start <- function(x) {
  term <- normalize_text(x)

  ## (b) Spring/Fall terms are labeled by academic year, while Summer labels use
  ## the calendar year directly in the current extract.
  case_when(
    str_detect(term, "^Summer") ~ as.integer(str_extract(term, "[0-9]{4}$")),
    str_detect(term, "^(Fall|Spring)") ~ as.integer(str_extract(term, "[0-9]{4}")),
    TRUE ~ NA_integer_
  )
}

clean_zip5 <- function(x) {
  ## (c) Preserve foreign postal codes elsewhere; this helper only creates the
  ## five-digit U.S. ZIP used for the ZIP-year count file.
  out <- normalize_text(x)
  str_extract(out, "[0-9]{5}")
}

is_bachelor_degree <- function(x) {
  ## (d) Wisconsin uses both full "Bachelor of ..." labels and abbreviated
  ## "BS-..." labels for bachelor-level awards.
  degree <- str_to_lower(normalize_text(x))
  str_detect(degree, "\\bbachelor\\b|^bs-")
}

clean_degree_short <- function(x) {
  degree <- normalize_text(x)

  ## (e) Create a compact degree family while retaining the full formal
  ## description in award_formal_descr_list.
  case_when(
    str_detect(degree, "^Bachelor of Business Administration") ~ "BBA",
    str_detect(degree, "^Bachelor of Arts") ~ "BA",
    str_detect(degree, "^Bachelor of Science") ~ "BS",
    str_detect(degree, "^BS-") ~ "BS",
    str_detect(degree, "^Bachelor of Fine Arts") ~ "BFA",
    str_detect(degree, "^Bachelor of Music") ~ "BM",
    str_detect(degree, "^Bachelor of Social Work") ~ "BSW",
    str_detect(degree, "^Bachelor of Landscape Architecture") ~ "BLA",
    str_detect(degree, "^Bachelor of Naval Science") ~ "BNS",
    str_detect(degree, "^Bachelor") ~ "Bachelor",
    TRUE ~ NA_character_
  )
}

clean_degree_area <- function(x) {
  degree <- normalize_text(x)

  ## (f) For specialized degree labels, store the post-hyphen field as a broad
  ## area descriptor; generic BA/BS rows have no area suffix.
  area <- case_when(
    str_detect(degree, "^BS-") ~ str_remove(degree, "^BS-"),
    str_detect(degree, "^Bachelor of [^-]+-") ~
      str_remove(degree, "^Bachelor of [^-]+-"),
    TRUE ~ NA_character_
  )

  normalize_text(area)
}


#=====================================================================
# 3 - Read and standardize raw award rows
#=====================================================================

if (!file.exists(input_path)) {
  stop("Input workbook does not exist: ", input_path)
}

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

raw_awards <- read_xlsx(input_path, sheet = "results")

## (a) Validate after reading, before any column-specific cleaning runs.
missing_columns <- setdiff(expected_columns, names(raw_awards))

if (length(missing_columns) > 0) {
  stop(
    "The Wisconsin FOIA workbook is missing expected columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

awards_clean <- raw_awards %>%
  mutate(
    ## (b) Preserve row order from the raw workbook for audit and tie-breaking.
    raw_award_row_id = row_number(),
    name_clean = clean_name(NAME),
    ## (c) Names appear as "Last,First Middle"; split for review while keeping
    ## the full cleaned name as the primary key input.
    name_last = if_else(
      str_detect(name_clean, ","),
      str_squish(str_extract(name_clean, "^[^,]+")),
      NA_character_
    ),
    name_first_middle = if_else(
      str_detect(name_clean, ","),
      str_squish(str_remove(name_clean, "^[^,]+,")),
      name_clean
    ),
    confer_year = extract_confer_year(AWARD_CONFER_YEAR),
    completion_term_clean = normalize_text(AWARD_COMPLETION_TERM),
    completion_season = extract_completion_season(AWARD_COMPLETION_TERM),
    academic_year_start = extract_academic_year_start(AWARD_COMPLETION_TERM),
    award_major_clean = normalize_text(AWARD_MAJOR),
    award_formal_descr_clean = normalize_text(AWARD_FORMAL_DESCR),
    degree_short = clean_degree_short(AWARD_FORMAL_DESCR),
    degree_area = clean_degree_area(AWARD_FORMAL_DESCR),
    degree_honors_suffix_clean = normalize_upper(DEGREE_HONORS_SUFFIX),
    degree_honors_clean = normalize_upper(DEGREE_HONORS),
    home_country_clean = normalize_upper(HOME_COUNTRY),
    home_state_clean = normalize_upper(HOME_STATE),
    home_county_clean = normalize_upper(HOME_COUNTY),
    home_city_clean = normalize_city(HOME_CITY),
    home_postal_code_clean = normalize_text(HOME_ZIP_CODE),
    home_zip5 = clean_zip5(HOME_ZIP_CODE),
    ## (d) Coalesce missing geography flags to FALSE so output booleans are
    ## usable without additional missing-value handling.
    is_us_home = coalesce(home_country_clean == "UNITED STATES", FALSE),
    is_wisconsin_home = is_us_home & coalesce(home_state_clean == "WISCONSIN", FALSE),
    is_bachelor_award = is_bachelor_degree(AWARD_FORMAL_DESCR)
  )


#=====================================================================
# 4 - Collapse bachelor awards to cleaned individual-year records
#=====================================================================

bachelor_awards <- awards_clean %>%
  filter(is_bachelor_award)

## (a) A zero-row bachelor extract would indicate a schema/content change, not
## a valid no-output case for this project.
if (nrow(bachelor_awards) == 0) {
  stop("No bachelor-level awards were identified in the Wisconsin FOIA data.")
}

graduates_individual_year <- bachelor_awards %>%
  ## (b) The FOIA file does not include a stable student identifier, so use
  ## cleaned name plus home location within confer year as the person-year key.
  group_by(
    name_clean,
    confer_year,
    home_country_clean,
    home_state_clean,
    home_county_clean,
    home_city_clean,
    home_postal_code_clean,
    home_zip5,
    is_us_home,
    is_wisconsin_home
  ) %>%
  summarise(
    ## (c) Retain raw names and first raw row id so suspicious collapses can be
    ## traced back to the source workbook.
    name_raw_values = collapse_unique(NAME),
    name_last = collapse_unique(name_last),
    name_first_middle = collapse_unique(name_first_middle),
    first_raw_award_row_id = min(raw_award_row_id),
    award_rows_collapsed = n(),
    ## (d) Count and list degree/major/term fields separately; counts make QA
    ## easy, while lists preserve the substantive information.
    degree_count = n_distinct(award_formal_descr_clean, na.rm = TRUE),
    degree_short_count = n_distinct(degree_short, na.rm = TRUE),
    major_count = n_distinct(award_major_clean, na.rm = TRUE),
    completion_term_count = n_distinct(completion_term_clean, na.rm = TRUE),
    academic_year_start_count = n_distinct(academic_year_start, na.rm = TRUE),
    academic_year_start_min = min_integer_or_na(academic_year_start),
    academic_year_start_max = max_integer_or_na(academic_year_start),
    award_formal_descr_list = collapse_unique(award_formal_descr_clean),
    degree_short_list = collapse_unique(degree_short),
    degree_area_list = collapse_unique(degree_area),
    award_major_list = collapse_unique(award_major_clean),
    completion_term_list = collapse_unique(completion_term_clean),
    completion_season_list = collapse_unique(completion_season),
    degree_honors_suffix_list = collapse_unique(degree_honors_suffix_clean),
    degree_honors_list = collapse_unique(degree_honors_clean),
    has_degree_honors_suffix = any(!is.na(degree_honors_suffix_clean)),
    has_degree_honors = any(!is.na(degree_honors_clean)),
    .groups = "drop"
  ) %>%
  ## (e) Sort before assigning ids so individual_year_id is reproducible.
  arrange(confer_year, name_clean, home_country_clean, home_state_clean, home_city_clean) %>%
  mutate(individual_year_id = row_number()) %>%
  select(
    individual_year_id,
    name_clean,
    name_raw_values,
    name_last,
    name_first_middle,
    confer_year,
    is_us_home,
    is_wisconsin_home,
    home_country_clean,
    home_state_clean,
    home_county_clean,
    home_city_clean,
    home_postal_code_clean,
    home_zip5,
    award_rows_collapsed,
    degree_count,
    degree_short_count,
    major_count,
    completion_term_count,
    academic_year_start_count,
    academic_year_start_min,
    academic_year_start_max,
    award_formal_descr_list,
    degree_short_list,
    degree_area_list,
    award_major_list,
    completion_term_list,
    completion_season_list,
    degree_honors_suffix_list,
    degree_honors_list,
    has_degree_honors_suffix,
    has_degree_honors,
    first_raw_award_row_id
  )


#=====================================================================
# 5 - Build U.S. ZIP-by-year bachelor graduate counts
#=====================================================================

zip_year_counts <- graduates_individual_year %>%
  ## (a) Retain foreign postal codes in the individual-year file, but restrict
  ## the ZIP-year count table to U.S. homes with a valid five-digit ZIP.
  filter(is_us_home, !is.na(home_zip5), !is.na(confer_year)) %>%
  group_by(confer_year, home_zip5) %>%
  summarise(
    ## (b) Count rows after the person-year collapse, not raw award rows.
    wisconsin_bachelors_grads = n(),
    ## (c) ZIPs can contain multiple reported cities, and some raw city values
    ## include spelling variants. Keep a primary modal value plus full lists.
    primary_home_city = modal_text(home_city_clean),
    primary_home_state = modal_text(home_state_clean),
    home_city_list = collapse_unique(home_city_clean),
    home_state_list = collapse_unique(home_state_clean),
    in_state_grads = sum(is_wisconsin_home, na.rm = TRUE),
    ## (d) Keep missing-state U.S. records separate from out-of-state records.
    out_of_state_grads = sum(
      is_us_home & !is.na(home_state_clean) & home_state_clean != "WISCONSIN",
      na.rm = TRUE
    ),
    missing_home_state_grads = sum(is.na(home_state_clean)),
    distinct_home_cities = n_distinct(home_city_clean, na.rm = TRUE),
    distinct_home_states = n_distinct(home_state_clean, na.rm = TRUE),
    distinct_cleaned_names = n_distinct(name_clean, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(confer_year, home_zip5)


#=====================================================================
# 6 - Write processed files and QA summary
#=====================================================================

processing_summary <- tibble(
  ## (a) These metrics are intended to make silent row loss visible in logs and
  ## version-control diffs after a new FOIA workbook arrives.
  metric = c(
    "raw_award_rows",
    "bachelor_award_rows",
    "non_bachelor_award_rows",
    "individual_year_rows",
    "individual_year_rows_with_us_home_zip",
    "zip_year_rows",
    "minimum_confer_year",
    "maximum_confer_year"
  ),
  value = c(
    nrow(raw_awards),
    nrow(bachelor_awards),
    nrow(awards_clean) - nrow(bachelor_awards),
    nrow(graduates_individual_year),
    sum(graduates_individual_year$is_us_home & !is.na(graduates_individual_year$home_zip5)),
    nrow(zip_year_counts),
    min(graduates_individual_year$confer_year, na.rm = TRUE),
    max(graduates_individual_year$confer_year, na.rm = TRUE)
  )
)

## (b) Use empty strings for missing CSV values to keep files compact and easy
## to inspect in spreadsheet software.
write_csv(graduates_individual_year, individual_year_output_path, na = "")
write_csv(zip_year_counts, zip_year_output_path, na = "")
write_csv(processing_summary, summary_output_path, na = "")

message("Wrote cleaned Wisconsin bachelor graduate individual-year file to: ",
        individual_year_output_path)
message("Wrote Wisconsin bachelor graduate ZIP-year counts to: ",
        zip_year_output_path)
message("Wrote processing summary to: ", summary_output_path)
