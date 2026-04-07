library(pacman)
p_load(tidyverse, lubridate, scales, ggtext)

# ---------- Load updated data ----------
df0 <- readRDS("/Users/cycoldiron/Desktop/congress-fiscal-tweets/data/processed/tweets_monthly_with_leg.rds")

# Make sure month is Date and parties are consistent
df0 <- df0 %>%
  mutate(
    month = as.Date(month),
    party = if_else(party %in% c("Democratic","Republican"), party, NA_character_)
  ) %>%
  drop_na(party)

# ---------- Build party-month series ----------
# (recompute party share of tweets about the deficit each month)
tweets_monthly_by_party <- df0 %>%
  group_by(month, party) %>%
  summarise(
    total_tweets  = sum(n_tweets, na.rm = TRUE),
    deficit_tweets = sum(n_debt_tweets, na.rm = TRUE),
    .groups = "drop_last"
  ) %>%
  mutate(percent_deficit = 100 * deficit_tweets / if_else(total_tweets > 0, total_tweets, NA_real_)) %>%
  ungroup()

# ---------- Spline interpolation (geometric smoothing — passes through all data points) ----------
smooth_series <- function(df) {
  df <- df %>% arrange(month) %>% filter(!is.na(percent_deficit))
  x  <- as.numeric(df$month)
  sp <- splinefun(x, df$percent_deficit, method = "monoH.FC")
  x_dense <- seq(min(x), max(x), length.out = 500)
  tibble(
    month                  = as.Date(x_dense, origin = "1970-01-01"),
    percent_deficit_smooth = sp(x_dense),
    party                  = df$party[1]
  )
}

tweets_smooth <- tweets_monthly_by_party %>%
  filter(party %in% c("Democratic", "Republican")) %>%
  group_by(party) %>%
  group_split() %>%
  map_dfr(smooth_series)

# ---------- Build shaded-legislative windows from leg_period ----------
# Collapse consecutive months where leg_period == 1 into start/end windows.
# The presidency split is at 2021-01-20.
cutoff <- as.Date("2021-01-20")

# One row per month with a single leg flag (any member==1 => month is legislative window)
leg_months <- df0 %>%
  group_by(month) %>%
  summarise(leg_period = as.integer(any(leg_period == 1, na.rm = TRUE)), .groups = "drop") %>%
  arrange(month)

# Identify runs of consecutive 1s
leg_months <- leg_months %>%
  mutate(run_id = cumsum(coalesce(leg_period != lag(leg_period, default = 0), TRUE)))

leg_df <- leg_months %>%
  filter(leg_period == 1) %>%
  group_by(run_id) %>%
  summarise(
    start = min(month),
    end   = max(month) + months(1),   # draw to the first day AFTER the last month
    .groups = "drop"
  ) %>%
  mutate(
    president = if_else(start < cutoff, "Trump", "Biden"),
    fill_color = if_else(president == "Trump", "#F4A3A3", "#A3C7F4") # soft red/blue
  )

# ---------- Plot ----------
ts_deficit_by_party <- ggplot() +
  # shaded windows
  geom_rect(
    data = leg_df,
    aes(xmin = start, xmax = end, ymin = -0.5, ymax = Inf),
    inherit.aes = FALSE,
    fill = leg_df$fill_color, alpha = 0.18, color = NA
  ) +
  # party lines (spline-interpolated — geometric smoothing only, DGP unchanged)
  geom_line(
    data = tweets_smooth,
    aes(x = month, y = percent_deficit_smooth, color = party),
    linewidth = 1.1
  ) +
  scale_color_manual(values = c("Democratic" = "#4E7EAD", "Republican" = "#B85C5C")) +
  scale_y_continuous(labels = percent_format(scale = 1), limits = c(-0.5, 3.5)) +
  labs(
    title = "Congressional Tweets About the Deficit, 2017–2023",
    subtitle = "Shaded bands mark legislative windows (±1 month)",
    x = "Month",
    y = "Share of Tweets on the Deficit"
  ) +
  theme_bw(base_size = 14) +
  theme(
    plot.title    = element_text(hjust = 0.5, size = 18, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12.5),
    legend.position = "bottom",
    legend.title = element_blank(),
    panel.grid.major = element_line(color = "grey83"),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 10, 22, 10)
  )

# ---------- Save ----------
ggsave(
  filename = "/Users/cycoldiron/Desktop/congress-fiscal-tweets/figures/summary/deficit_tweet_share_with_legislation.png",
  plot = ts_deficit_by_party,
  width = 11, height = 6.5, dpi = 300
)
