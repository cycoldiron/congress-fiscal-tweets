library(pacman)
p_load(tidyverse, scales, lubridate, ggtext)

# Assign shading color by president
legislation_timeline <- legislation_timeline %>%
  mutate(
    president = if_else(bill_date < as.Date("2021-01-20"), "Trump", "Biden"),
    fill_color = if_else(president == "Trump", "#F4A3A3", "#A3C7F4")  # light red, light blue
  )

# Tweet totals for caption
tweet_totals <- all_tweets_final %>%
  filter(party %in% c("Democratic", "Republican")) %>%
  group_by(party) %>%
  summarise(total_tweets = n(), .groups = "drop")

# Construct caption
caption_text <- paste0(
  "**Tweet totals (2017–2023):** ",
  tweet_totals$party[1], " = ", format(tweet_totals$total_tweets[1], big.mark = ","),
  " | ",
  tweet_totals$party[2], " = ", format(tweet_totals$total_tweets[2], big.mark = ",")
)

# === FIGURE 1: Party Deficit Tweet Share (No Shading) ===
deficit_tweet_share_by_party <- ggplot() +
  geom_line(
    data = tweets_monthly_by_party %>% filter(party %in% c("Democratic", "Republican")),
    aes(x = tweet_month, y = percent_deficit, color = party),
    linewidth = 1.1
  ) +
  scale_color_manual(values = c("Democratic" = "#0056A0", "Republican" = "#B2182B")) +
  scale_y_continuous(labels = percent_format(scale = 1), limits = c(0, 3.5)) +
  labs(
    title = "% of Congressional Tweets About the Deficit",
    subtitle = "Monthly share of deficit-related tweets by party",
    x = "Month",
    y = "% Deficit Tweets",
    caption = caption_text
  ) +
  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 13),
    plot.caption = ggtext::element_markdown(size = 10, face = "bold", hjust = 0.5, margin = margin(t = 10)),
    legend.position = "bottom",
    legend.title = element_blank(),
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 10, 30, 10)
  )

ggsave(
  filename = "figures/summary/deficit_tweet_share_by_party.png",
  plot = deficit_tweet_share_by_party,
  width = 11,
  height = 6.5,
  dpi = 300
)

# === FIGURE 2: Party Deficit Tweet Share with Legislation Shading ===
deficit_tweet_share_with_legislation <- ggplot() +
  geom_rect(data = legislation_timeline,
            aes(xmin = as.Date(window_start),
                xmax = as.Date(window_end),
                ymin = -0.5, ymax = Inf),
            fill = legislation_timeline$fill_color,
            alpha = 0.2,
            color = NA) +
  geom_line(
    data = tweets_monthly_by_party %>% filter(party %in% c("Democratic", "Republican")),
    aes(x = tweet_month, y = percent_deficit, color = party),
    linewidth = 1.1
  ) +
  scale_color_manual(values = c("Democratic" = "#0056A0", "Republican" = "#B2182B")) +
  scale_y_continuous(labels = percent_format(scale = 1), limits = c(-0.5, 3.5)) +
  labs(
    title = "% of Congressional Tweets About the Deficit",
    subtitle = "Shaded Areas = One Month Before and After Major Legislation",
    x = "Month",
    y = "% Deficit Tweets",
    caption = caption_text
  ) +
  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 13),
    plot.caption = ggtext::element_markdown(size = 10, face = "bold", hjust = 0.5, margin = margin(t = 10)),
    legend.position = "bottom",
    legend.title = element_blank(),
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 10, 30, 10)
  )

ggsave(
  filename = "figures/summary/deficit_tweet_share_with_legislation.png",
  plot = deficit_tweet_share_with_legislation,
  width = 11,
  height = 6.5,
  dpi = 300
)
