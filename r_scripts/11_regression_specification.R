library(tidyverse)
library(modelsummary)
library(gt)

load("data/processed/05_full_tweets_with_indices.Rdata")

tweets_monthly <- all_tweets_final_index %>%
  mutate(month = floor_date(tweet_date, "month")) %>%
  group_by(month, full_name, party) %>%
  summarise(
    n_tweets = n(),
    n_debt_tweets = sum(is_debt),
    pct_deficit = mean(is_debt),
    .groups = "drop"
  ) %>%
  mutate(
    party_clean = if_else(party == "Democratic", "Democratic", "Republican")
  ) %>%
  left_join(
    all_tweets_final_index %>%
      select(month = tweet_month, president_party, house_control, senate_control) %>%
      distinct(),
    by = "month"
  ) %>%
  mutate(
    power_level = case_when(
      party_clean == president_party & party_clean == house_control & party_clean == senate_control ~ "full",
      party_clean != president_party & party_clean != house_control & party_clean != senate_control ~ "none",
      TRUE ~ "partial"
    )
  )


# === Rebuild models ===
mod_rep <- tweets_monthly %>%
  filter(party_clean == "Republican") %>%
  mutate(
    has_full_control = power_level == "full",
    is_minority_era = month >= ymd("2021-01-01")
  ) %>%
  lm(pct_deficit ~ has_full_control + is_minority_era, data = .)

mod_dem <- tweets_monthly %>%
  filter(party_clean == "Democratic") %>%
  mutate(
    has_full_control = power_level == "full",
    is_minority_era = month >= ymd("2017-01-01") & month < ymd("2019-01-01")
  ) %>%
  lm(pct_deficit ~ has_full_control + is_minority_era, data = .)

# === Shorter wrapped coefficient labels ===
coef_labels <- c(
  "(Intercept)" = "Baseline:\nPartial Power\n(2019–2020)",
  "has_full_controlTRUE" = "In Full Power\n(Unified Gov)",
  "is_minority_eraTRUE" = "In Minority\n(Opposition Era)"
)

# === Create side-by-side table with wrapped labels ===
wrapped_table <- modelsummary(
  list("Republicans" = mod_rep, "Democrats" = mod_dem),
  coef_map = coef_labels,
  stars = TRUE,
  output = "gt",
  title = "Effect of Power Status on Deficit Tweeting by Party",
  gof_map = c("nobs" = "Num. Obs.", "r.squared" = "R²", "adj.r.squared" = "Adj. R²", "fstat" = "F")
) %>%
  fmt_markdown(columns = 1) %>%  # allows line breaks in coefficient names
  cols_width(
    1 ~ px(260),                 # slightly wider coefficient label column
    everything() ~ px(130)       # modestly wider party columns
  ) %>%
  tab_options(
    table.font.size = "small",
    heading.title.font.size = 14,
    column_labels.font.weight = "bold",
    data_row.padding = px(6),    # slightly taller rows
    column_labels.padding = px(6)
  ) %>%
  tab_source_note(
    source_note = "Each observation represents a party's average monthly tweet share about the deficit.\nThe baseline period is 2019–2020, when each party held partial congressional control."
  )

# === Save as PNG ===
gtsave(
  wrapped_table,
  filename = "/Users/cycoldiron/Desktop/congress-fiscal-tweets/results/deficit_power_comparison_condensed_final.png"
)

