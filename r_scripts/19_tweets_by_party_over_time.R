load("data/processed/05_full_tweets_with_indices.Rdata")
library(tidyverse)
library(glue)
library(scales)

# Cumulative totals
totals <- tweets_monthly %>%
  group_by(party_clean) %>%
  summarise(total = sum(n_tweets, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = party_clean, values_from = total)

subtitle_txt <- glue(
  "Total Tweets — Democratic: {comma(totals$Democratic)}, Republican: {comma(totals$Republican)}"
)

# Monthly totals by party
by_month <- tweets_monthly %>%
  group_by(month, party_clean) %>%
  summarise(total_tweets = sum(n_tweets, na.rm = TRUE), .groups = "drop")

plot_tweet_volume <- ggplot(by_month, aes(x = as.Date(month), y = total_tweets, color = party_clean)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c("Democratic" = "blue", "Republican" = "red")) +
  scale_y_continuous(limits = c(0, 55000), labels = comma) +
  labs(
    title = "Monthly Tweet Volume by Party (2017–2023)",
    subtitle = subtitle_txt,
    x = "Month",
    y = "Number of Tweets",
    color = "Party"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, face = "italic"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

# Save plot
ggsave(
  filename = "/Users/cycoldiron/Desktop/congress-fiscal-tweets/figures/summary/tweet_volume_by_party.png",
  plot = plot_tweet_volume,
  width = 10,
  height = 6,
  dpi = 300
)
