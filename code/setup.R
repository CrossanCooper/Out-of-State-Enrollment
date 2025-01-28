#////////////////////////////////////////////////////////////////////////////////
# Filename: setup.R
# Author: Ryan Haygood
# Date: 11/11/24
# Description: Loads packages and declares file paths.
#////////////////////////////////////////////////////////////////////////////////

rm(list = ls())

# Packages ----------------------------------------------------------------------

library(dplyr)
library(stringr)
library(tidyr)
library(lfe)
library(ggplot2)
library(readxl)
library(usmap)
library(maps)
library(stargazer)
library(haven)
library(readr)
library(ggpubr)
library(estimatr)
library(tidycensus)

# Paths -------------------------------------------------------------------------

pathHome <- 'C:/Users/ryanh/Dropbox/admissions_project/'

pathFigures <- paste0(pathHome, 'figures/')

pathCode <- paste0(pathHome, 'code/')

# Functions ---------------------------------------------------------------------

# Standard error of a weighted mean

# Type 1: weights proportional to importance
# Type 2: weights proportional to variances
se_wtd_mean <- function(x, w, type = 1) {
  
  if (type == 1) {
    
    N <- length(x)
    
  } else if (type == 2) {
    
    N <- sum(w)^2 / sum(w^2)
    
  }
  
  sqrt(weighted.mean((x - weighted.mean(x, w))^2, w) / N)
  
}

# Cosine distance
cosine <- function(x, y) {
  
  sum(x * y) / sqrt(sum(x^2) * sum(y^2))
  
}

# Mode; if multiple, return first appearing mode
Mode <- function(x) {
  
  ux <- unique(x)
  if (NA %in% ux) {
    ux <- ux[-which(is.na(ux))]
  }
  ux[which.max(tabulate(match(x, ux)))]
  
}
