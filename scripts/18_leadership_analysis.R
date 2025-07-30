library(pacman)
p_load(tidyverse, writexl,  readxl, janitor, fuzzyjoin, stringr, gt)

load("data/processed/05_full_tweets_with_indices.Rdata")  


# Step 1: Compute summary stats by leader
leader_summary <- all_tweets_final_index %>%
  filter(leadership_role != "Rank-and-File") %>%
  group_by(full_name, leadership_role, party) %>%
  summarise(
    total_tweets = n(),
    debt_tweets = sum(is_debt),
    pct_debt = round(100 * debt_tweets / total_tweets, 2),
    .groups = "drop"
  )

# Step 2: Build table with custom row color styling
leader_summary %>%
  arrange(desc(pct_debt)) %>%
  gt() %>%
  tab_header(
    title = md("**Deficit Tweeting Rates by Congressional Leaders (2017–2023)**"),
    subtitle = md("*Includes only leaders who served during this period*")
  ) %>%
  cols_label(
    full_name = md("**Member**"),
    leadership_role = md("**Leadership Role**"),
    party = md("**Party**"),
    total_tweets = md("**Total Tweets**"),
    debt_tweets = md("**Debt Tweets**"),
    pct_debt = md("**% Deficit-Related**")
  ) %>%
  data_color(
    columns = everything(),
    rows = party == "Democratic",
    colors = scales::col_factor(
      palette = c("white", "#A3C7F4"),
      domain = c("Republican", "Democratic")
    )
  ) %>%
  data_color(
    columns = everything(),
    rows = party == "Republican",
    colors = scales::col_factor(
      palette = c("#F4A3A3", "white"),
      domain = c("Republican", "Democratic")
    )
  )
