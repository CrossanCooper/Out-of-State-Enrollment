#////////////////////////////////////////////////////////////////////////////////
# Filename: build_trial_data.R
# Author: Ryan Haygood
# Date: 3/4/25
# Description: Get a synthetic dataset for use in grumps_trial.jl to figure out
# how to use the Grumps package for ML estimation with exclusion restrictions.
#////////////////////////////////////////////////////////////////////////////////

# Setup
source('C:/Users/ryanh/OneDrive/Documents/Grad School/Research/Out-of-State-Enrollment/code/setup.R')

# Simulate choice data ----------------------------------------------------------

set.seed(1)

# Key features:
# One product-level characteristic (~ o_share)
# One observable consumer-level characteristic (~ in-state)
# One characteristic varying at the product/consumer level (~ home_state)
# One instrument for the product-level characteristic (~ selectivity/tuition)
# A few markets (variation in outside-good utility across years)

# 5 markets
years <- 2020:2024
# 10 products
product_ids <- paste0('prod', seq(1, 10))
# 1000 consumers
consumer_ids <- 1:1000

# Get product/year-level dataset
products <- data.frame(year = rep(years, length(product_ids)),
                       # Product ID
                       product = rep(product_ids, each = length(years)),
                       # Time-varying characteristic
                       x = runif(length(years) * length(product_ids))) %>%
  # Get instrument
  mutate(z = 4*x + rnorm(length(years) * length(product_ids)))

# Consumer-level dataset
consumers <- data.frame(year = rep(years, length(consumer_ids) / length(years)),
                        w = runif(length(consumer_ids)) > 0.5,
                        y = sample(product_ids[-1], length(consumer_ids), replace = T)) %>%
  # If w = 1 (in-state), y = prod1 (outside good)
  # If w = 0 (out-of-state), y is sampled randomly from the other products
  mutate(y = if_else(w, 'prod1', y))
# Product/consumer-level characteristic will be equal an indicator for y == product
# (like a home state)

# Get market-level dataset
markets <- consumers %>%
  group_by(year) %>%
  summarize(N = n())

# Preference parameters
beta_x_w0 <- 1
beta_x_w1 <- 2
beta_y_w0 <- 4
beta_y_w1 <- 3
# Time-varying outside-good preference
gamma_w0 <- 1:length(years) / 5
gamma_w1 <- gamma_w0 * 1.5
# Mean utilities
delta <- c(0, rnorm(length(product_ids) - 1))

# Choice function
choice <- function(w, y, year) {
  
  # Vector of utilities
  u <- delta + if_else(w, gamma_w1[year == years], gamma_w0[year == years]) * (product_ids == 'prod1') + if_else(w, beta_x_w1, beta_x_w0) * products$x[products$year == year] + if_else(w, beta_y_w1, beta_y_w0) * (y == product_ids)
  
  # Choice probability vector
  probs <- exp(u) / sum(exp(u))
  
  # Assign a choice according to these probabilities
  sample(product_ids, size = 1, prob = probs)
  
}

# Get choices
consumers$choice <- mapply(choice, consumers$w, consumers$y, consumers$year)

# Get product shares
products <- consumers %>%
  group_by(year) %>%
  mutate(N = n()) %>%
  group_by(year, choice) %>%
  summarize(share = n() / mean(N)) %>%
  rename(product = choice) %>%
  right_join(products) %>%
  mutate(share = if_else(is.na(share), 0, share))

# Convert everything to numeric
consumers <- consumers %>%
  mutate(w = if_else(w, 1, 0))

# Also convert market variable into string
consumers <- consumers %>%
  mutate(year = paste0('year', year))
products <- products %>%
  mutate(year = paste0('year', year))
markets <- markets %>%
  mutate(year = paste0('year', year))
draws <- draws %>%
  mutate(year = paste0('year', year))

# Get some consumer characteristics to draw from (not sure if necessary)
draws <- consumers %>%
  select(-choice)

# Try omitting outside good from product dataframe
products <- products %>%
  filter(product != 'prod1')

# Save as .csv files
write.csv(consumers, paste0(pathHome, 'data/grumps_trial_data/consumers.csv'), row.names = F)
write.csv(products, paste0(pathHome, 'data/grumps_trial_data/products.csv'), row.names = F)
write.csv(markets, paste0(pathHome, 'data/grumps_trial_data/markets.csv'), row.names = F)
write.csv(draws, paste0(pathHome, 'data/grumps_trial_data/draws.csv'), row.names = F)