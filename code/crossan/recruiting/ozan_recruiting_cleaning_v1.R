#=====================================================================
## Created by: Crossan Cooper
## Last Modified: 2-26-25

## file use: explore zipped recruiting files
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
               doParallel,readxl,did,ggExtra,DBI,arrow)

#=====================================================================
# 1 - check tar file data
#=====================================================================

### i. original version -- 500 file chunks

## (a) define folder path
folder_path_a <- "/Volumes/T7/unzipped_higher_ed/ucla-scraping-data/scraping_data/scraping_data.scraped_html/"
## (b) list all .gz.parquet files in the folder
parquet_files_a <- list.files(folder_path_a, pattern = "\\.gz\\.parquet$", full.names = TRUE)
## (c) check if files are found
if (length(parquet_files_a) == 0) {
  stop("No .gz.parquet files found in the directory!")
}
## (d) define output directory on external drive and create if it doesn't exist
output_dir <- file.path("/Volumes/T7/", "ua_chunks")
if (!dir.exists(output_dir)) { dir.create(output_dir) }
## (e) set the chunk size for processing files (500 per chunk)
chunk_size <- 500
## (f) loop over the files in 500 file chunks, processing and flushing each chunk
chunk_counter <- 1
for (chunk_start in seq(1, length(parquet_files_a), by = chunk_size)) {
  chunk_end <- min(chunk_start + chunk_size - 1, length(parquet_files_a))
  message(sprintf("Processing chunk %d: files %d to %d", chunk_counter, chunk_start, chunk_end))
  current_files <- parquet_files_a[chunk_start:chunk_end]
  
  combined_df_a <- NULL
  for (i in seq_along(current_files)) {
    file <- current_files[i]
    df <- tryCatch({
      read_parquet(file)
    }, error = function(e) {
      message(sprintf("Error reading file %s: %s", file, e))
      return(NULL)
    })
    if (!is.null(df)) {
      temp_df <- tryCatch({
        df[df$univ_id == "100751", ]
      }, error = function(e) {
        message(sprintf("Error extracting univ_id '100751' in file %s: %s", file, e))
        return(NULL)
      })
      if (!is.null(temp_df) && nrow(temp_df) > 0) {
        if (is.null(combined_df_a)) {
          combined_df_a <- temp_df
        } else {
          combined_df_a <- tryCatch({
            rbind(combined_df_a, temp_df)
          }, error = function(e) {
            message(sprintf("Error combining file %s: %s", file, e))
            combined_df_a
          })
        }
      }
    }
  }
  ## (g) flush combined data for the current chunk to disk as an RDS file
  if (!is.null(combined_df_a) && nrow(combined_df_a) > 0) {
    output_file <- file.path(output_dir, sprintf("ua_chunk_%03d.rds", chunk_counter))
    tryCatch({
      saveRDS(combined_df_a, output_file)
    }, error = function(e) {
      message(sprintf("Error writing chunk %d: %s", chunk_counter, e))
    })
    message(sprintf("Chunk %d saved with %d rows.", chunk_counter, nrow(combined_df_a)))
  } else {
    message(sprintf("No valid data found in chunk %d.", chunk_counter))
  }
  chunk_counter <- chunk_counter + 1
  combined_df_a <- NULL
  gc()
}
