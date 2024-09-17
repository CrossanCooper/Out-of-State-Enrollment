#=====================================================================
## Created by: Crossan Cooper
## Last Modified: 7-29-24

## convert alabama commencement pdf to text file
#=====================================================================

#=====================================================================
# 0 - clean environment and load default libraries
#=====================================================================

# default list of packages and cleaning command
rm(list=ls())
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,data.table,ggplot2,skimr,
               dplyr,fixest,ggmap,stargazer,sjmisc,
               Hmisc,tseries,DescTools,here,censusapi,
               tidycensus,ggiplot,educationdata,foreach,
               doParallel,readxl,did,ggExtra,pdftools)

#=====================================================================
# 1 - read in pdf file
#=====================================================================

pdf_08 <- pdf_text(here("data","ua_spring08.pdf"))
pdf_09 <- pdf_text(here("data","ua_spring09.pdf"))
pdf_10 <- pdf_text(here("data","ua_spring10.pdf"))
pdf_11 <- pdf_text(here("data","ua_spring11.pdf"))
pdf_12 <- pdf_text(here("data","ua_spring12.pdf"))
pdf_13 <- pdf_text(here("data","ua_spring13.pdf"))
pdf_14 <- pdf_text(here("data","ua_spring14.pdf"))
pdf_15 <- pdf_text(here("data","ua_spring15.pdf"))
pdf_16 <- pdf_text(here("data","ua_spring16.pdf"))
pdf_17 <- pdf_text(here("data","ua_spring17.pdf"))
pdf_22 <- pdf_text(here("data","ua_spring22.pdf"))
pdf_23 <- pdf_text(here("data","ua_spring23.pdf"))
pdf_24 <- pdf_text(here("data","ua_spring24.pdf"))

pdf_08_clean <- pdf_08[c(11:43)]

# Extract text from PDF
pdf_dt <- pdf_data(here("data","ua_spring23.pdf"))
# example with page 35
page_35 <- pdf_dt[[35]]
page_35_text_elements <- page_35$text
page_35_text <- paste(page_35_text_elements, collapse = " ")
# first remove "University Honors" and "University Honors with Thesis" and
# "Arts and Sciences" and "Bachelors of Arts" 

# Define patterns to remove
patterns_to_remove <- c("University Honors", "Arts and Sciences",
                        "University Honors with Thesis", "with Thesis",
                        "Bachelors of Arts","35","35","35","36",
                        "37","38","39","40","41")

# Remove the patterns
for (pattern in patterns_to_remove) {
  page_35_text <- gsub(pattern, "", page_35_text)
}

# Trim extra white space that might have been introduced
page_35_text <- str_trim(page_35_text)

entries <- strsplit(page_35_text, "(?<=\\b[A-Z]{2}\\b)", perl = TRUE)[[1]]


# Initialize vectors to store extracted data
names <- c()
locations <- c()
honors <- c()

# Loop through each entry
for (entry in entries) {
  # Extract name and location
  parts <- strsplit(entry, " -+ ")[[1]]
  name <- parts[1]
  location <- parts[2]
  
  # Append to vectors
  names <- c(names, name)
  locations <- c(locations, location)
}

# Create a data frame
df_35 <- data.frame(Name = names, Location = locations, stringsAsFactors = FALSE)


# @crossan: ERROR HERE
# Assuming each line is a separate row and fields are separated by commas
# all_text <- strsplit(pdf_text, "\n")[[1]]
# all_rows <- lapply(all_text, function(x) strsplit(x, ",")[[1]])
