library(pacman)
p_load(tidyverse, janitor, readr, stringr, ISLR, gt, AER, fixest, modelsummary, estimatr, ggtext, tidylog)

# Step 1: Convert tweet_date to Date class
all_tweets_final <- all_tweets_final %>%
  mutate(tweet_month = ym(tweet_date))  # Adds a new column

# Step 2: Join with power_status on month
tweets_with_power <- all_tweets_final %>%
  left_join(power_status, by = c("tweet_month" = "month")) %>% 
  mutate(rep_party = party) %>%
  select(-party) %>% 
  select(-tweet_date)






