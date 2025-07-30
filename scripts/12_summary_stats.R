library(tidyverse)
library(lubridate)
library(gt)

# === STEP 1: Summarize data ===
summary_df_block <- all_tweets_final_index %>%
  mutate(
    tweet_date = ymd(tweet_date),
    period = case_when(
      tweet_date >= ymd("2017-01-01") & tweet_date < ymd("2019-01-01") ~ "2017–2019",
      tweet_date >= ymd("2019-01-01") & tweet_date < ymd("2021-01-01") ~ "2019–2021",
      tweet_date >= ymd("2021-01-01") & tweet_date <= ymd("2023-01-01") ~ "2021–2023",
      TRUE ~ NA_character_
    ),
    gov_type = case_when(
      period == "2017–2019" ~ "Unified Republicans",
      period == "2019–2021" ~ "Divided Republicans",
      period == "2021–2023" ~ "Unified Democrats",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(period)) %>%
  group_by(period, gov_type, party) %>%
  summarise(
    avg_deficit_tweets = mean(is_debt, na.rm = TRUE),
    num_tweets = n(),
    .groups = "drop"
  ) %>%
  arrange(period, party) %>%
  mutate(
    period = ifelse(duplicated(period), "", period),
    gov_type = ifelse(duplicated(gov_type), "", gov_type)
  )

# === STEP 2: Build gt table ===
summary_table_block <- summary_df_block %>%
  mutate(avg_deficit_tweets = round(avg_deficit_tweets, 3)) %>%
  gt() %>%
  fmt_percent(columns = vars(avg_deficit_tweets), decimals = 1) %>%
  fmt_number(columns = vars(num_tweets), use_seps = TRUE, decimals = 0) %>%
  cols_label(
    period = "Date",
    gov_type = "Gov Type",
    party = "Party",
    avg_deficit_tweets = "Avg Def Tweets",
    num_tweets = "Num of Tweets"
  ) %>%
  tab_header(
    title = "Deficit Tweeting by Party and Government Period"
  ) %>%
  tab_options(
    table.font.size = "small",
    heading.title.font.size = 14,
    column_labels.font.weight = "bold",
    data_row.padding = px(6)
  )

# === STEP 3: Save as PNG ===
gtsave(
  summary_table_block,
  filename = "/Users/cycoldiron/Desktop/congress-fiscal-tweets/results/p3_deficit_summary_grouped_block_fixed.png"
)
