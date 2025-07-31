# === Load Required Libraries ===
library(tidyverse)
library(lubridate)
library(janitor)

# === Load Data Files ===
load("data/processed/02_matched_congress_tweet.Rdata")      # loads: all_tweets_final
load("data/processed/03_power_policy_historical.Rdata")     # loads: power_status
load("data/processed/04_econ_indicators_historical.Rdata")  # loads: congress_approval, us_deficit, interest_rates

# === Prepare and Clean Tweet Dates ===
all_tweets_final_index <- all_tweets_final %>%
  mutate(
    tweet_date = ym(tweet_date),                     # FIX: parse "YYYY-MM" safely
    tweet_month = floor_date(tweet_date, "month")    # safe for joins
  )


# === Process power_status and define gov_type ===
power_status_date <- power_status %>%
  mutate(month = as.Date(month)) %>%
  mutate(
    gov_type = case_when(
      president_party == house_control & house_control == senate_control ~ paste0(president_party, " Unified"),
      TRUE ~ paste0(president_party, " Divided")
    )
  )

# === Process Congress Approval Ratings ===
congress_approval_monthly <- congress_approval %>%
  mutate(date = as.Date(date)) %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarise(congress_approval_perc = mean(approval_perc, na.rm = TRUE), .groups = "drop")

# === Process US Deficit Data ===
us_deficit_monthly <- us_deficit %>%
  mutate(date = as.Date(date)) %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarise(
    debt_held_by_the_public = mean(debt_held_by_the_public, na.rm = TRUE),
    intragovernmental_holdings = mean(intragovernmental_holdings, na.rm = TRUE),
    total_public_debt_outstanding = mean(total_public_debt_outstanding, na.rm = TRUE),
    .groups = "drop"
  )

# === Clean interest rate dates (if needed) ===
interest_rates <- interest_rates %>%
  mutate(date = as.Date(date)) %>%
  mutate(month = floor_date(date, "month"))

# === Join Contextual Data ===
all_tweets_final_index <- all_tweets_final_index %>%
  left_join(power_status_date, by = c("tweet_month" = "month")) %>%
  left_join(congress_approval_monthly, by = c("tweet_month" = "month")) %>%
  left_join(us_deficit_monthly, by = c("tweet_month" = "month")) %>%
  left_join(interest_rates, by = c("tweet_month" = "month"))

# === Calculate Power Status Variables ===
all_tweets_final_index <- all_tweets_final_index %>%
  mutate(
    in_power_govtype = case_when(
      str_detect(gov_type, "Republican") & party == "Republican" ~ TRUE,
      str_detect(gov_type, "Democratic") & party == "Democratic" ~ TRUE,
      str_detect(gov_type, "Republican") & party != "Republican" ~ FALSE,
      str_detect(gov_type, "Democratic") & party != "Democratic" ~ FALSE,
      TRUE ~ NA
    ),
    in_power_2of3 = (
      (party == president_party) +
        (party == house_control) +
        (party == senate_control)
    ) >= 2
  )

# === Save Final Dataset ===
save(all_tweets_final_index, tweets_monthly,
     file = "data/processed/05_full_tweets_with_indices.RData")
