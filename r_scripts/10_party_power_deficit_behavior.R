library(tidyverse)
library(lubridate)

# === Load data ===
load("data/processed/05_full_tweets_with_indices.Rdata")  

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
  mutate(
    power_level = case_when(
      party == president_party & party == house_control & party == senate_control ~ "Unified Control",
      party != president_party & party != house_control & party != senate_control ~ "No Control",
      TRUE ~ "Partial Control"
    ),
    party_clean = if_else(party == "Democratic", "Democratic", "Republican"),
    color_group = paste(party_clean, power_level, sep = "_")  # for coloring only
  ) %>%
  rename(month = tweet_month)

# === Step 2: Define color map (distinct by party + power) ===
color_map_named <- c(
  "Democratic_No Control"     = "#c6dbef",
  "Democratic_Partial Control" = "#4292c6",
  "Democratic_Unified Control" = "#08306B",
  "Republican_No Control"     = "#fcbba1",
  "Republican_Partial Control" = "#fb6a4a",
  "Republican_Unified Control" = "#99000d"
)

# === Step 3: Plot with fixed legend and continuous lines ===
plot_deficit <- ggplot(monthly_deficit,
                       aes(x = month, y = pct_deficit,
                           group = party_clean,  # ensures continuous lines
                           color = color_group)) +
  geom_line(size = 1.2) +
  facet_wrap(~ party_clean, ncol = 2) +
  scale_color_manual(
    values = color_map_named,
    breaks = c(
      "Democratic_No Control", "Democratic_Partial Control", "Democratic_Unified Control",
      "Republican_No Control", "Republican_Partial Control", "Republican_Unified Control"
    ),
    labels = rep(c("No Control", "Partial Control", "Unified Control"), 2)
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Deficit-Related Tweets by Party and Institutional Power",
    subtitle = "Darker lines indicate periods of unified party control (Presidency + Congress)",
    x = "Month", y = "% Deficit Tweets", color = "Power Level"
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 10),
    axis.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold", size = 13)
  )

# === Step 4: Save the figure ===
ggsave(
  filename = "/Users/cycoldiron/Desktop/congress-fiscal-tweets/figures/summary/deficit_tweet_trends_by_party_and_power.png",
  plot = plot_deficit,
  width = 12,
  height = 6,
  dpi = 300,
  bg = "white"
)
