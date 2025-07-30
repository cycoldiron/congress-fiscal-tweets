library(pacman)
p_load(tidyverse, writexl, readxl, janitor, fuzzyjoin, stringr)
load("data/processed/05_full_tweets_with_indices.Rdata")  

# === Create Member-Level Summary ===
member_summary <- all_tweets_final_index %>%
  group_by(full_name, party) %>%
  summarise(
    total_tweets = n(),
    n_debt_tweets = sum(is_debt, na.rm = TRUE),
    pct_debt_tweets = mean(is_debt, na.rm = TRUE),
    .groups = "drop"
  )

# === Summary by Power Status (in_power_govtype) ===
member_power_summary <- all_tweets_final_index %>%
  group_by(full_name, party, in_power_govtype) %>%
  summarise(
    n_tweets = n(),
    n_debt_tweets = sum(is_debt, na.rm = TRUE),
    pct_debt_tweets = mean(is_debt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = in_power_govtype,
    values_from = c(n_tweets, n_debt_tweets, pct_debt_tweets),
    # This will produce columns like n_tweets_TRUE / n_tweets_FALSE
    values_fill = 0
  ) %>%
  # Rename logical suffixes for clarity
  rename_with(~ str_replace(., "_TRUE", "_in_power"), ends_with("_TRUE")) %>%
  rename_with(~ str_replace(., "_FALSE", "_out_power"), ends_with("_FALSE")) %>%
  mutate(
    pct_debt_diff_inout = pct_debt_tweets_out_power - pct_debt_tweets_in_power
  )

# === Merge Full Summary with Power Summary ===
member_deficit_behavior_summary <- member_summary %>%
  left_join(member_power_summary, by = c("full_name", "party"))

save(member_deficit_behavior_summary,
     file = "data/processed/06_member_deficit_behavior_summary.RData")
