# === Load Libraries ===
library(tidyverse)
library(lubridate)
library(data.table)

# === Prepare Tweets Data ===
tweets_dt <- as.data.table(all_tweets_final_index)
tweets_dt[, tweet_date := ymd(tweet_date)]
tweets_dt[, tweet_start := tweet_date]
tweets_dt[, tweet_end := tweet_date]  # treat each tweet as a 1-day interval

# === Prepare Legislation Timeline Data (Remove Bipartisan Budget Act of 2018) ===
policy_dt <- as.data.table(legislation_timeline) %>%
  filter(bill_name != "Bipartisan Budget Act of 2018")
policy_dt[, window_start := ymd(window_start)]
policy_dt[, window_end := ymd(window_end)]
setkey(policy_dt, window_start, window_end)
setkey(tweets_dt, tweet_start, tweet_end)

# === Perform Efficient Interval Join ===
tweets_policy_joined <- foverlaps(
  x = tweets_dt,
  y = policy_dt,
  by.x = c("tweet_start", "tweet_end"),
  by.y = c("window_start", "window_end"),
  type = "within",
  nomatch = NA
)

# === Fill in unmatched tweets as 'No_Event' ===
tweets_policy_joined <- tweets_policy_joined %>%
  mutate(
    bill_name = ifelse(is.na(bill_name), "No_Event", bill_name),
    is_policy_window = !is.na(congress)
  )

# === Deduplicate: Assign each tweet to one bill (if multiple matches) ===
tweets_policy_deduped <- tweets_policy_joined %>%
  group_by(screen_name, tweet_date) %>%
  slice(1) %>%
  ungroup()

# === Add period_type flag ===
tweets_policy_deduped <- tweets_policy_deduped %>%
  mutate(
    period_type = ifelse(bill_name == "No_Event", "Baseline", "Policy_Window")
  )

# === Add Party Sponsorship Info (Remove Bipartisan Budget Act of 2018) ===
bill_sponsors <- tibble(
  bill_name = c(
    "Tax Cuts and Jobs Act (TCJA)",
    "Consolidated Appropriations Act, 2018",
    "Coronavirus Aid, Relief, and Economic Security (CARES) Act",
    "Paycheck Protection Program and Health Care Enhancement Act",
    "Consolidated Appropriations Act, 2021",
    "American Rescue Plan Act (ARPA)",
    "Infrastructure Investment and Jobs Act (IIJA)",
    "CHIPS and Science Act",
    "Inflation Reduction Act (IRA)"
  ),
  sponsor_party = c(
    "Republican", "Republican", "Bipartisan", "Bipartisan", "Republican",
    "Democratic", "Democratic", "Democratic", "Democratic"
  )
)

# === Join Sponsorship Info and Classify Party Position ===
tweets_policy_deduped <- tweets_policy_deduped %>%
  left_join(bill_sponsors, by = "bill_name") %>%
  mutate(
    party_position = case_when(
      bill_name == "No_Event" ~ "No_Event",
      sponsor_party == "Bipartisan" ~ "Bipartisan",
      sponsor_party == party ~ "In-Party",
      sponsor_party != party ~ "Out-Party"
     )
  )

# === Summarize Tweet Activity ===
tweet_summary <- tweets_policy_deduped %>%
  group_by(period_type, party, party_position) %>%
  summarise(
    n_total_tweets = n(),
    n_deficit_tweets = sum(is_debt, na.rm = TRUE),
    pct_deficit_tweets = mean(is_debt, na.rm = TRUE),
    .groups = "drop"
  )

# === Display Summary ===
print(tweet_summary)
