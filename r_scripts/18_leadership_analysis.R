library(pacman)
p_load(tidyverse, gt, gtsave, lubridate)

# === Load Data ===
load("data/processed/05_full_tweets_with_indices.Rdata")

# === Summarize Leadership Tweeting Behavior ===
leader_summary <- all_tweets_final_index %>%
  filter(leadership_role != "Rank-and-File") %>%
  group_by(full_name, leadership_role, party) %>%
  summarise(
    total_tweets = n(),
    debt_tweets = sum(is_debt),
    pct_debt = round(100 * debt_tweets / total_tweets, 2),
    .groups = "drop"
  )

# === Create GT Table ===
leader_gt <- leader_summary %>%
  arrange(full_name, leadership_role) %>%
  gt(groupname_col = "full_name") %>%
  tab_header(
    title = md("**Deficit Tweeting Rates by Congressional Leaders (2017–2023)**"),
    subtitle = md("*Includes only leaders who served during this period*")
  ) %>%
  cols_label(
    leadership_role = md("**Leadership Role**"),
    party = md("**Party**"),
    total_tweets = md("**Total Tweets**"),
    debt_tweets = md("**Debt Tweets**"),
    pct_debt = md("**% Deficit-Related**")
  ) %>%
  fmt_number(
    columns = pct_debt,
    decimals = 2
  ) %>%
  data_color(
    columns = everything(),
    colors = scales::col_factor(
      palette = c("Democratic" = "#A3C7F4", "Republican" = "#F4A3A3"),
      domain = c("Democratic", "Republican")
    )
  )

# === Save Table to File ===
gtsave(
  data = leader_gt,
  filename = "/Users/cycoldiron/Desktop/congress-fiscal-tweets/figures/leadership/leader_deficit_tweets.png"
)
