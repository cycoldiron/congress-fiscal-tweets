library(tidyverse)
library(lubridate)

# === Step 1: Summarize tweets by month and party ===
monthly_deficit <- all_tweets_final_index %>%
  group_by(tweet_month, party) %>%
  summarise(
    n_tweets = n(),
    n_deficit = sum(is_debt, na.rm = TRUE),
    pct_deficit = n_deficit / n_tweets,
    president_party = first(president_party),
    house_control = first(house_control),
    senate_control = first(senate_control),
    .groups = "drop"
  ) %>%
  
  # === Step 2: Assign power level ===
  mutate(
    power_level = case_when(
      party == president_party & party == house_control & party == senate_control ~ "full",
      party != president_party & party != house_control & party != senate_control ~ "none",
      TRUE ~ "partial"
    ),
    party_clean = if_else(party == "Democratic", "Democratic", "Republican"),
    party_power = paste(party_clean, power_level, sep = "_"),
    power_label = case_when(
      party_power == "Democratic_none" ~ "Democrats: No Control",
      party_power == "Democratic_partial" ~ "Democrats: Partial Control",
      party_power == "Democratic_full" ~ "Democrats: Unified Control",
      party_power == "Republican_none" ~ "Republicans: No Control",
      party_power == "Republican_partial" ~ "Republicans: Partial Control",
      party_power == "Republican_full" ~ "Republicans: Unified Control"
    )
  ) %>%
  rename(month = tweet_month)

# === Step 3: Define color palette ===
color_map_named <- c(
  "Democrats: No Control" = "#c6dbef",
  "Democrats: Partial Control" = "#4292c6",
  "Democrats: Unified Control" = "#08306B",
  "Republicans: No Control" = "#fcbba1",
  "Republicans: Partial Control" = "#fb6a4a",
  "Republicans: Unified Control" = "#99000d"
)

# === Step 4: Generate plot ===
ggplot(monthly_deficit,
       aes(x = month, y = pct_deficit, color = power_label, group = 1)) +
  geom_line(size = 1.2) +
  facet_wrap(~ party_clean, ncol = 2) +
  scale_color_manual(values = color_map_named) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "% of Deficit-Related Tweets by Party and Institutional Power",
    subtitle = "Darker lines indicate periods of unified party control (Presidency + Congress)",
    x = "Month", y = "% Deficit Tweets", color = "Power Level"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 10)
  )

# === Step 5: Save the figure ===
ggsave(
  filename = "/Users/cycoldiron/Desktop/congress-fiscal-tweets/figures/summary/deficit_tweet_trends_by_party_and_power.png",
  width = 12,
  height = 6,
  dpi = 300
)
