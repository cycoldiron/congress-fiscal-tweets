# === Load Data and Libraries ===
load("data/processed/05_full_tweets_with_indices.Rdata")  
load("data/processed/04_econ_indicators_historical.RData") 

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

# === Step 5: Calculate smoothed tweet share using LOESS ===
debt_vs_econ_total <- debt_vs_econ_total %>%
  mutate(
    smoothed_pct_debt = predict(loess(pct_debt ~ as.numeric(tweet_month), span = 0.3))
  )

# === Step 6: Rescale economic variables ===
debt_vs_econ_total <- debt_vs_econ_total %>%
  mutate(
    approval_scaled = rescale(congress_approval_perc, to = range(smoothed_pct_debt, na.rm = TRUE)),
    int_rate_scaled = rescale(int_rate, to = range(smoothed_pct_debt, na.rm = TRUE))
  )

# === Step 7: Prepare data for faceted plot ===
panel_df <- debt_vs_econ_total %>%
  select(tweet_month, pct_debt, smoothed_pct_debt, congress_approval_perc, int_rate, approval_scaled, int_rate_scaled) %>%
  pivot_longer(
    cols = c(approval_scaled, int_rate_scaled),
    names_to = "econ_var",
    values_to = "econ_value"
  ) %>%
  mutate(
    econ_var = recode(econ_var,
                      approval_scaled = "Congress Approval (rescaled)",
                      int_rate_scaled = "Interest Rate (rescaled)"),
    econ_value_orig = if_else(econ_var == "Congress Approval (rescaled)", congress_approval_perc, int_rate)
  )

# === Step 8: Compute correlation (based on smoothed deficit values) ===
cor_labels <- panel_df %>%
  group_by(econ_var) %>%
  summarise(
    cor_val = cor(smoothed_pct_debt, econ_value, use = "complete.obs")
  ) %>%
  mutate(label = paste0("r = ", round(cor_val, 2)))

# === Step 9: Find min/max for annotation ===
# === Step 9: Find min/max for annotation (with improved label placement) ===
min_max_labels <- panel_df %>%
  group_by(econ_var) %>%
  summarise(
    min_val = min(econ_value_orig, na.rm = TRUE),
    max_val = max(econ_value_orig, na.rm = TRUE),
    min_month = tweet_month[which.min(econ_value_orig)],
    max_month = tweet_month[which.max(econ_value_orig)],
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(min_val, max_val), names_to = "which_label", values_to = "value") %>%
  mutate(
    label = case_when(
      econ_var == "Congress Approval (rescaled)" ~ paste0(if_else(which_label == "min_val", "Min: ", "Max: "), value, "% Approval"),
      econ_var == "Interest Rate (rescaled)" ~ paste0(if_else(which_label == "min_val", "Min: ", "Max: "), value, "% Rate")
    ),
    month = if_else(which_label == "min_val", min_month, max_month),
    
    # --- fine-tuned nudges in DAYS ---
    x_nudge = case_when(
      econ_var == "Congress Approval (rescaled)" & which_label == "min_val" ~ 90,   # already working
      econ_var == "Interest Rate (rescaled)" & which_label == "max_val" ~ -120,      # move further left
      TRUE ~ 0
    ),
    vjust_adj = if_else(which_label == "min_val", -0.35, 0.35),
    hjust_adj = if_else(which_label == "min_val", 1.1, -0.1),
    
    tweet_month_adj = month + x_nudge
  ) %>%
  left_join(
    panel_df %>% select(tweet_month, econ_var, econ_value),
    by = c("month" = "tweet_month", "econ_var")
  ) %>%
  rename(tweet_month = month)



# === Step 10: Build the plot ===
panel_plot <- ggplot(panel_df, aes(x = tweet_month)) +
  geom_line(aes(y = pct_debt), color = "#0072B2", alpha = 0.4, size = 0.5) +
  geom_line(aes(y = smoothed_pct_debt), color = "#0072B2", size = 1.2) +
  geom_line(aes(y = econ_value), color = "black", linetype = "dashed") +
  geom_text(
    data = min_max_labels,
    aes(
      x = tweet_month_adj,
      y = econ_value + vjust_adj,
      label = label,
      hjust = hjust_adj
    ),
    size = 3.3,
    fontface = "bold.italic",
    vjust = 0.5,
    inherit.aes = FALSE,
    color = "black"
  )+
  geom_text(
    data = cor_labels,
    aes(x = as.Date("2020-12-01"), y = 2.6, label = label),
    inherit.aes = FALSE,
    size = 4,
    hjust = 0.45,
    fontface = "bold"
  ) +
  facet_wrap(~econ_var, ncol = 1, scales = "free_y") +
  scale_x_date(
    breaks = seq(as.Date("2017-01-01"), as.Date("2023-12-01"), by = "1 year"),
    date_labels = "%Y"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.25))) +  # more top/bottom room
  labs(
    title = "Deficit Tweet Share and Key Economic Indicators",
    subtitle = "Rescaled indicators (dashed) vs. smoothed deficit tweeting (bold). Correlation uses smoothed data.",
    x = "Month",
    y = "% of Tweets Mentioning Deficit",
    caption = "Min/Max labels reflect actual values (not rescaled)."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    plot.subtitle = element_text(face = "italic", hjust = 0.5, size = 12),
    plot.caption = element_text(face = "italic", hjust = 0.5, size = 10),
    axis.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    legend.position = "none",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_line(color = "gray90")
  )


# === Step 11: Save the figure ===
ggsave(
  filename = "/Users/cycoldiron/Desktop/congress-fiscal-tweets/figures/economic_indicators/g1_deficit_vs_approval_int_panel.png",
  plot = panel_plot,
  width = 10,
  height = 8,
  dpi = 300
)

# === Step 12: Save intermediate data ===
save(
  congress_approval,
  interest_rates,
  us_deficit,
  econ_vars_monthly,
  debt_tweet_summary_total,
  debt_vs_econ_total,
  file = "/Users/cycoldiron/Desktop/congress-fiscal-tweets/data/processed/04_econ_indicators_historical.RData"
)
