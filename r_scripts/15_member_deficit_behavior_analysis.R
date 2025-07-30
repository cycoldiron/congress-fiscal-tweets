# === Load Data ===
load("data/processed/05_full_tweets_with_indices.Rdata")  
load("data/processed/06_member_deficit_behavior_summary.RData")

# === Required Packages ===
library(tidyverse)
library(gt)
library(scales)

# === Shared Helper: Party Row Coloring ===
add_party_row_color <- function(gt_table, party_col = "party") {
  gt_table %>%
    tab_style(
      style = list(cell_fill(color = "#cfe2f3")),
      locations = cells_body(rows = !!sym(party_col) == "Democratic")
    ) %>%
    tab_style(
      style = list(cell_fill(color = "#f4cccc")),
      locations = cells_body(rows = !!sym(party_col) == "Republican")
    )
}

# === Shared Note Text ===
note_text <- "*‘In power’ refers to when a member’s party held the presidency.*"

# === A. Top 15 by Total Deficit-Related Tweets ===
top_total_deficit_tweeters <- member_deficit_behavior_summary %>%
  filter(total_tweets >= 300) %>%
  arrange(desc(n_debt_tweets)) %>%
  slice_head(n = 15) %>%
  select(full_name, party, total_tweets, n_debt_tweets, pct_debt_tweets,
         pct_debt_tweets_in_power, pct_debt_tweets_out_power, pct_debt_diff_inout) %>%
  gt() %>%
  tab_header(
    title = md("**Top 15 Members by Total Deficit-Related Tweets (2017–2023)**"),
    subtitle = md(glue::glue("<span style='font-size:14px'>{note_text}</span>"))
  ) %>%
  cols_label(
    full_name = md("**Member**"),
    total_tweets = md("**Total Tweets**"),
    n_debt_tweets = md("**Deficit Tweets**"),
    pct_debt_tweets = md("**% Deficit Tweets**"),
    pct_debt_tweets_in_power = md("**% In Power**"),
    pct_debt_tweets_out_power = md("**% Out of Power**"),
    pct_debt_diff_inout = md("**In–Out Difference**")
  ) %>%
  fmt_percent(columns = c(pct_debt_tweets, pct_debt_tweets_in_power,
                          pct_debt_tweets_out_power, pct_debt_diff_inout), decimals = 2) %>%
  data_color(
    columns = c(total_tweets, n_debt_tweets, pct_debt_tweets, pct_debt_tweets_in_power,
                pct_debt_tweets_out_power, pct_debt_diff_inout),
    fn = col_numeric(c("white", "gold"), domain = NULL)
  ) %>%
  cols_align("left", columns = full_name) %>%
  cols_align("center", columns = everything()) %>%
  tab_style(style = cell_text(weight = "bold"), locations = cells_body(columns = full_name)) %>%
  add_party_row_color() %>%
  cols_hide(columns = party)

# === B. Top 15 by % of Tweets About the Deficit (≥ 500 Tweets) ===
top_pct_deficit_tweeters <- member_deficit_behavior_summary %>%
  filter(total_tweets >= 500) %>%
  arrange(desc(pct_debt_tweets)) %>%
  slice_head(n = 15) %>%
  select(full_name, party, n_debt_tweets, pct_debt_tweets, pct_debt_tweets_out_power) %>%
  gt() %>%
  tab_header(
    title = md("**Top 15 Members by Share of Tweets About the Deficit (Min. 500 Tweets)**"),
    subtitle = md(glue::glue("<span style='font-size:14px'>{note_text}</span>"))
  ) %>%
  cols_label(
    full_name = md("**Member**"),
    n_debt_tweets = md("**Deficit Tweets**"),
    pct_debt_tweets = md("**% Deficit Tweets**"),
    pct_debt_tweets_out_power = md("**% Out of Power**")
  ) %>%
  fmt_percent(columns = c(pct_debt_tweets, pct_debt_tweets_out_power), decimals = 2) %>%
  data_color(
    columns = c(n_debt_tweets, pct_debt_tweets, pct_debt_tweets_out_power),
    fn = col_numeric(c("white", "gold"), domain = NULL)
  ) %>%
  cols_align("left", columns = full_name) %>%
  cols_align("center", columns = everything()) %>%
  tab_style(style = cell_text(weight = "bold"), locations = cells_body(columns = full_name)) %>%
  add_party_row_color() %>%
  cols_hide(columns = party)

# === C. Top 15 with Largest Shift In vs. Out of Power ===
# === C. Top 15 with Largest Shift In vs. Out of Power ===

top_inout_shifters <- member_deficit_behavior_summary %>%
  filter(n_tweets_in_power >= 300, n_tweets_out_power >= 300, !is.na(pct_debt_diff_inout)) %>%
  arrange(desc(abs(pct_debt_diff_inout))) %>%
  slice_head(n = 15) %>%
  mutate(
    less_when_in_power = pct_debt_diff_inout < 0
  )

top_inout_shifters %>%
  select(
    full_name,
    party,
    pct_debt_tweets_in_power,
    pct_debt_tweets_out_power,
    pct_debt_diff_inout,
    less_when_in_power
  ) %>%
  gt() %>%
  tab_header(
    title = md("**Top 15 Members with Largest Shift in Deficit Tweeting (In vs. Out of Power)**"),
    subtitle = md("*‘In power’ refers to when a member’s party held the presidency.*")
  ) %>%
  cols_label(
    full_name = md("**Member**"),
    pct_debt_tweets_in_power = md("**% In Power**"),
    pct_debt_tweets_out_power = md("**% Out of Power**"),
    pct_debt_diff_inout = md("**In–Out Difference**")
  ) %>%
  fmt_percent(
    columns = c(pct_debt_tweets_in_power, pct_debt_tweets_out_power, pct_debt_diff_inout),
    decimals = 2
  ) %>%
  data_color(
    columns = c(pct_debt_tweets_in_power, pct_debt_tweets_out_power, pct_debt_diff_inout),
    fn = scales::col_numeric(c("white", "gold"), domain = NULL)
  ) %>%
  cols_align("left", columns = full_name) %>%
  cols_align("center", columns = everything()) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = full_name,
      rows = less_when_in_power == TRUE
    )
  ) %>%
  add_party_row_color() %>%
  cols_hide(columns = c(party, less_when_in_power)) %>%
  tab_source_note(
    source_note = md("<span style='display:block; text-align:center'><strong>Bolded names indicate members who tweeted <em>less</em> about the deficit while in power.</strong></span>")
  )


# === Save Output ===
gtsave(top_total_deficit_tweeters, filename = "figures/individuals/top_total_deficit_tweeters.png")
gtsave(top_pct_deficit_tweeters, filename = "figures/individuals/top_pct_deficit_tweeters.png")
gtsave(top_inout_shifters, filename = "figures/individuals/top_inout_shifters.png")
