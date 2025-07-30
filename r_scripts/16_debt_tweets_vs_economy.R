load("data/processed/05_full_tweets_with_indices.Rdata")  

library(tidyverse)
library(scales)

# === Step 1: Aggregate monthly deficit tweet share ===
debt_tweet_summary_total <- all_tweets_final_index %>%
  group_by(tweet_month) %>%
  summarise(
    n_tweets = n(),
    n_debt = sum(is_debt),
    pct_debt = 100 * mean(is_debt),
    .groups = "drop"
  )

# === Step 2: Economic indicators ===
econ_vars <- all_tweets_final_index %>%
  select(tweet_month, congress_approval_perc, int_rate) %>%
  distinct()

# === Step 3: Fix missing value for June 2020 ===
econ_vars_monthly <- econ_vars %>%
  mutate(
    congress_approval_perc = case_when(
      tweet_month == as.Date("2020-06-01") ~ 23,
      TRUE ~ congress_approval_perc
    )
  )

# === Step 4: Join tweet and economic data ===
debt_vs_econ_total <- debt_tweet_summary_total %>%
  left_join(econ_vars_monthly, by = "tweet_month")

# === Step 5: Rescale & reshape for panel plotting ===
panel_df <- debt_vs_econ_total %>%
  mutate(
    approval_scaled = rescale(congress_approval_perc, to = range(pct_debt, na.rm = TRUE)),
    int_rate_scaled = rescale(int_rate, to = range(pct_debt, na.rm = TRUE))
  ) %>%
  select(tweet_month, pct_debt, approval_scaled, int_rate_scaled) %>%
  pivot_longer(
    cols = c(approval_scaled, int_rate_scaled),
    names_to = "econ_var",
    values_to = "econ_value"
  ) %>%
  mutate(
    econ_var = recode(econ_var,
                      approval_scaled = "Congress Approval (rescaled)",
                      int_rate_scaled = "Interest Rate (rescaled)")
  )

# === Step 6: Compute correlation coefficients ===
cor_labels <- panel_df %>%
  group_by(econ_var) %>%
  summarise(cor_val = cor(pct_debt, econ_value, use = "complete.obs")) %>%
  mutate(label = paste0("r = ", round(cor_val, 2)))

# === Step 7: Faceted plot with enhancements ===
panel_plot <- ggplot(panel_df, aes(x = tweet_month)) +
  # Main tweet line
  geom_line(aes(y = pct_debt), color = "#0072B2", size = 1.2) +
  # Smoothing (optional, comment out if undesired)
  geom_smooth(aes(y = pct_debt), method = "loess", se = FALSE, color = "#0072B2", linetype = "solid") +
  # Dashed econ indicator line
  geom_line(aes(y = econ_value), color = "black", linetype = "dashed") +
  # Add correlation label
  # Add bold, centered correlation label
  geom_text(
    data = cor_labels,
    aes(x = as.Date("2020-12-01"), y = 2.6, label = label),
    inherit.aes = FALSE,
    size = 4,
    hjust = 0.45,
    fontface = "bold"
  )+
  facet_wrap(~econ_var, ncol = 1, scales = "free_y") +
  scale_x_date(
    breaks = seq(as.Date("2017-01-01"), as.Date("2023-12-01"), by = "1 year"),
    date_labels = "%Y"
  ) +
  labs(
    title = "Deficit Tweet Share and Key Economic Indicators",
    subtitle = "Comparison of deficit-related tweeting with congressional approval and 10-year interest rates",
    x = "Month",
    y = "% of Tweets Mentioning Deficit"
  )+
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    legend.position = "none",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_line(color = "gray90")
  )


# === Step 8: Save the figure ===
ggsave(
  filename = "/Users/cycoldiron/Desktop/congress-fiscal-tweets/figures/economic_indicators/deficit_vs_approval_int_panel.png",
  plot = panel_plot,
  width = 10,
  height = 8,
  dpi = 300
)

save(
  congress_approval,
  interest_rates,
  us_deficit,
  econ_vars_monthly,
  debt_tweet_summary_total,
  file = "/Users/cycoldiron/Desktop/congress-fiscal-tweets/data/processed/04_econ_indicators_historical.RData"
)
