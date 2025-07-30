library(tidyverse)
library(gt)
library(scales)

# Step 1: Prepare lag and difference variables
cor_df <- cor_df %>%
  arrange(tweet_month) %>%
  mutate(
    approval_lag1 = lag(congress_approval_perc, 1),
    int_rate_lag1 = lag(int_rate, 1),
    approval_diff = congress_approval_perc - lag(congress_approval_perc),
    int_rate_diff = int_rate - lag(int_rate),
    pct_debt_diff = pct_debt - lag(pct_debt)
  )

# Step 2: Compute correlations
cor_results <- tribble(
  ~group, ~`Variable Comparison`, ~r,
  "Congress Approval", "Deficit Tweets vs Approval", cor(cor_df$pct_debt, cor_df$congress_approval_perc, use = "complete.obs"),
  "Congress Approval", "Deficit Tweets vs Lag(Approval)", cor(cor_df$pct_debt, cor_df$approval_lag1, use = "complete.obs"),
  "Congress Approval", "Deficit Tweets vs ΔApproval", cor(cor_df$pct_debt, cor_df$approval_diff, use = "complete.obs"),
  "Congress Approval", "ΔDeficit Tweets vs ΔApproval", cor(cor_df$pct_debt_diff, cor_df$approval_diff, use = "complete.obs"),
  
  "Interest Rate", "Deficit Tweets vs Interest Rate", cor(cor_df$pct_debt, cor_df$int_rate, use = "complete.obs"),
  "Interest Rate", "Deficit Tweets vs Lag(Interest Rate)", cor(cor_df$pct_debt, cor_df$int_rate_lag1, use = "complete.obs"),
  "Interest Rate", "Deficit Tweets vs ΔInterest Rate", cor(cor_df$pct_debt, cor_df$int_rate_diff, use = "complete.obs"),
  "Interest Rate", "ΔDeficit Tweets vs ΔInterest Rate", cor(cor_df$pct_debt_diff, cor_df$int_rate_diff, use = "complete.obs")
) %>%
  mutate(r = round(r, 3))

# Step 3: Make a gt table
cor_gt <- cor_results %>%
  gt(groupname_col = "group") %>%
  tab_header(
    title = md("**Correlation Between Deficit-Related Tweets and Economic Indicators**"),
    subtitle = md("*Pearson r coefficients between tweet share and monthly congressional approval or 10-year interest rates.*")
  ) %>%
  fmt_number(columns = r, decimals = 3) %>%
  cols_label(
    `Variable Comparison` = "Variable Comparison",
    r = "r"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_style(
    style = cell_borders(sides = "top", color = "grey40", weight = px(1.5)),
    locations = cells_row_groups()
  ) %>%
  tab_options(
    table.font.size = "medium",
    heading.title.font.size = 16,
    heading.subtitle.font.size = 12,
    row_group.font.weight = "bold",
    row_group.border.bottom.style = "solid",
    row_group.border.bottom.color = "grey50"
  ) %>%
  tab_source_note(md("_Lag specifications refer to one-month lags. Δ variables represent month-over-month changes._"))

# Step 4: Save the table
gtsave(
  data = cor_gt,
  filename = "/Users/cycoldiron/Desktop/congress-fiscal-tweets/figures/economic_indicators/deficit_tweet_correlation_table.png"
)
