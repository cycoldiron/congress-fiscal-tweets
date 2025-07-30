# === Load Matched Tweet Data ===
load("data/processed/02_matched_congress_tweet.Rdata")

# === Load Required Libraries ===
library(tidyverse)
library(lubridate)

# === Create Monthly Tweet Summary Dataset ===
tweets_monthly_by_rep <- all_tweets_final %>%
  mutate(month = as.Date(paste0(tweet_date, "-01"))) %>%
  group_by(month, full_name, party) %>%
  summarise(
    n_tweets = n(),
    n_debt_tweets = sum(is_debt),
    pct_debt_tweets = mean(is_debt),
    .groups = "drop"
  )

tweets_monthly_by_party <- all_tweets_final %>%
  mutate(tweet_month = ym(tweet_date)) %>%
  filter(party %in% c("Democratic", "Republican")) %>%
  group_by(party, tweet_month) %>%
  summarise(
    total_tweets = n(),
    deficit_tweets = sum(is_debt),
    percent_deficit = 100 * deficit_tweets / total_tweets,
    .groups = "drop"
  )

# === Save Existing and New Object Together ===
save(list = c(ls(), "tweets_monthly_by_rep", "tweets_monthly_by_party"), 
     file = "data/processed/02_matched_congress_tweet.Rdata")
