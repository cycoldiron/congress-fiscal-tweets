# Load packages
library(pacman)
p_load(tidyverse, janitor, readr, stringr, ISLR, gt, AER, fixest, modelsummary, estimatr, ggtext, tidylog, lubridate, readxl)

# === Load and clean external economic data ===
us_deficit <- read.csv("data/raw/deficit_timeseries_treasury.csv", stringsAsFactors = FALSE) %>% 
  clean_names() %>% 
  mutate(date = as.Date(record_date)) %>%
  select(date, debt_held_by_the_public, intragovernmental_holdings, total_public_debt_outstanding)

congress_approval <- read.csv("data/raw/approval_rating_congress.csv") %>%
  clean_names() %>%
  mutate(date = mdy(date)) %>%
  filter(date >= ymd("2017-01-01") & date <= ymd("2023-01-31")) %>%
  
  # Remove Jan 2023 (if it exists)
  filter(date != ymd("2023-01-01")) %>%
  
  # Add Dec 2022 manually with 21% if it's still missing
  bind_rows(
    tibble(date = ymd("2022-12-01"), approval_perc = 21)
  ) %>%
  
  # Remove potential duplicates of Dec 2022 just in case
  distinct(date, .keep_all = TRUE)


# 10 yr bonds

interest_rates <- read_excel("data/raw/interest_rates_10yr.xlsx") %>% 
  clean_names() %>% rename(int_rate = gs10) %>% 
  rename(date = observation_date) %>% 
  mutate(date = as_date(date))
  
save(us_deficit, congress_approval, interest_rates,
     file = "/Users/cycoldiron/Desktop/congress-fiscal-tweets/data/processed/04_econ_indicators_historical.Rdata")