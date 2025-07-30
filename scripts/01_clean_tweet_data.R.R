# 01_clean_tweet_data.R
# Purpose: Clean and filter tweet data, keeping specific leadership accounts

# Load packages
library(pacman)
p_load(tidyverse, janitor, readr, stringr, ISLR, gt, AER, fixest, modelsummary, estimatr, ggtext, tidylog)

# Load raw tweet data
tweet_raw_df <- read.csv("/Users/cycoldiron/Desktop/congress-fiscal-tweets/data/external/tweet_summary_raw_python.csv", stringsAsFactors = FALSE)

# Create tweet summary: total and debt tweets per screen_name
tweet_summary <- tweet_raw_df %>%
  group_by(screen_name) %>%
  summarise(
    total_tweets = n(),
    debt_tweets = sum(is_debt),
    pct_debt = mean(is_debt),
    .groups = "drop"
  ) %>%
  arrange(desc(debt_tweets))

# === Custom account whitelist ===
whitelist_accounts <- c("McConnellPress", "LeaderMcConnell", "SpeakerMcCarthy", "kevinomccarthy", "PRyan")

# Apply filtering, but preserve whitelisted accounts
tweet_raw_df_2 <- tweet_raw_df %>%
  filter(
    screen_name %in% whitelist_accounts |
      (
        !str_detect(tolower(screen_name), "aocenespanol|johnsonleads|for|4|vote|elect|haleylive|jctgov|transport|vachair|senfinance|rules|repcloakroom|team|gop|dem|cmte|committee|caucus|study|progress|press|office|house|senate|congressional|nrcc|nrsc|srcc|dscc|dccc|capac|dsenfloor|cbcpac|energycommerce|jecrepublicans|officialcbc|natresources|hascrepublicans|covidoversight|seec|climatecrisis|jenniffer2012|jenniffer|sascmajority|covidselect|emmerpac|jgo_2020|halrogersky5|kendrashorn|kellyforms01|senjudiciary")
      )
  )

# Summarize after filtering
tweet_summary_2 <- tweet_raw_df_2 %>%
  group_by(screen_name) %>%
  summarise(
    total_tweets = n(),
    debt_tweets = sum(is_debt),
    pct_debt = mean(is_debt),
    .groups = "drop"
  ) %>%
  arrange(desc(debt_tweets))

# Remove accounts with <50 tweets unless whitelisted
valid_users <- tweet_summary_2 %>%
  filter(total_tweets >= 50 | screen_name %in% whitelist_accounts) %>%
  pull(screen_name)

tweet_raw_df_2 <- tweet_raw_df_2 %>%
  filter(screen_name %in% valid_users)

# Recalculate final tweet summary
tweet_summary_2 <- tweet_raw_df_2 %>%
  group_by(screen_name) %>%
  summarise(
    total_tweets = n(),
    debt_tweets = sum(is_debt),
    pct_debt = mean(is_debt),
    .groups = "drop"
  ) %>%
  arrange(desc(debt_tweets))

# Preview final result
print(head(tweet_summary_2, 50))
# Save all 4 data frames to RData file
save(tweet_raw_df, tweet_raw_df_2,
     file = "/Users/cycoldiron/Desktop/congress-fiscal-tweets/data/processed/01_cleandata.Rdata")

