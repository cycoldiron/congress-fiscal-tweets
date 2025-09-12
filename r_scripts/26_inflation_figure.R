library(dplyr)
library(ggplot2)

# paths
m_path  <- "/Users/cycoldiron/Desktop/congress-fiscal-tweets/data/processed/tweets_monthly_with_leg.rds"
fig_dir <- "/Users/cycoldiron/Desktop/congress-fiscal-tweets/figures/economic_indicators"
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)
out_png <- file.path(fig_dir, "deficit_share_vs_inflation_smoothed.png")

# data: aggregate to one row per month
mm <- readRDS(m_path)
stopifnot(all(c("month","n_tweets","n_debt_tweets","inflation_yoy_sa") %in% names(mm)))

overall <- mm %>%
  group_by(month) %>%
  summarise(
    n_tweets      = sum(n_tweets, na.rm = TRUE),
    n_deficit     = sum(n_debt_tweets, na.rm = TRUE),
    pct_deficit   = 100 * n_deficit / n_tweets,
    inflation_yoy = first(inflation_yoy_sa),
    .groups = "drop"
  ) %>%
  arrange(month)

# dual-axis mapping (right axis = inflation in true units)
y1_min <- min(overall$pct_deficit, na.rm = TRUE)
y1_max <- max(overall$pct_deficit, na.rm = TRUE)
y2_min <- min(overall$inflation_yoy, na.rm = TRUE)
y2_max <- max(overall$inflation_yoy, na.rm = TRUE)
a <- (y1_max - y1_min) / (y2_max - y2_min)
b <- y1_min - a * y2_min
overall <- overall %>% mutate(inflation_scaled = a * inflation_yoy + b)

# --- compute SMOOTHED series so r uses smoothed data (same span as plot) ---
span_val <- 0.25
xnum <- as.numeric(overall$month)
overall <- overall %>%
  mutate(
    sm_deficit     = as.numeric(predict(loess(pct_deficit ~ xnum, span = span_val))),
    sm_infl_scaled = as.numeric(predict(loess(inflation_scaled ~ xnum, span = span_val)))
  )

# correlation using smoothed series
r_val <- cor(overall$sm_deficit, overall$sm_infl_scaled, use = "complete.obs")
r_lab <- paste0("r = ", round(r_val, 2))
x_mid <- as.Date(mean(range(overall$month, na.rm = TRUE)))
yr <- range(overall$sm_deficit, na.rm = TRUE)
y_mid <- yr[1] + 0.6 * diff(yr)

# theme (white background, centered caption, bold axes)
base <- 13
theme_pub <- theme_minimal(base_size = base) +
  theme(
    plot.title         = element_text(face = "bold", hjust = 0.5, size = base + 2),
    axis.title.x       = element_text(face = "bold", margin = margin(t = 8)),
    axis.title.y       = element_text(face = "bold", margin = margin(r = 8)),
    axis.title.y.right = element_text(face = "bold", margin = margin(l = 8)),
    panel.grid.minor   = element_blank(),
    legend.position    = "top",
    legend.title       = element_blank(),
    panel.background   = element_rect(fill = "white", color = NA),
    plot.background    = element_rect(fill = "white", color = NA),
    plot.caption       = element_text(hjust = 0.5, size = base - 1)
  )

# smoothed-only plot (inflation dashed)
p <- ggplot(overall, aes(x = month)) +
  geom_smooth(aes(y = pct_deficit, color = "Deficit tweet share (%)"),
              method = "loess", se = FALSE, span = span_val, size = 1.2) +
  geom_smooth(aes(y = inflation_scaled, color = "U.S. CPI inflation (YoY)"),
              method = "loess", se = FALSE, span = span_val, size = 1.2,
              linetype = "dashed") +
  annotate("text", x = x_mid, y = y_mid, label = r_lab, fontface = "bold", size = 4) +
  scale_y_continuous(
    name = "% of Tweets Mentioning Deficit",
    sec.axis = sec_axis(~ (.-b)/a, name = "U.S. CPI Inflation (YoY)")
  ) +
  scale_color_manual(values = c(
    "Deficit tweet share (%)"  = "#2C7BE5",
    "U.S. CPI inflation (YoY)" = "#6C757D"
  )) +
  labs(
    title    = "Deficit Tweet Share vs U.S. CPI Inflation",
    x        = "Month",
    caption  = "Inflation series: U.S. CPI (CPIAUCSL), YoY %, seasonally adjusted. Source: FRED."
  ) +
  theme_pub

ggsave(out_png, p, width = 9, height = 6, dpi = 320)
message("✅ Saved: ", normalizePath(out_png))


# ===== Correlation table for lags =====
suppressPackageStartupMessages({
  library(dplyr); library(tidyr)
  if (!requireNamespace("gt", quietly = TRUE)) install.packages("gt")
  if (!requireNamespace("webshot2", quietly = TRUE)) install.packages("webshot2")
})
library(gt)

# helper to shift vectors by k months (k>0 => lag/shift right)
shift_k <- function(v, k) {
  if (k > 0) dplyr::lag(v, k)
  else if (k < 0) dplyr::lead(v, -k)
  else v
}

lags <- -6:6
corr_df <- tibble(lag = lags) %>%
  rowwise() %>%
  mutate(
    r_raw       = cor(overall$pct_deficit,      shift_k(overall$inflation_scaled, lag), use = "complete.obs"),
    r_smoothed  = cor(overall$sm_deficit,       shift_k(overall$sm_infl_scaled,   lag), use = "complete.obs")
  ) %>%
  ungroup() %>%
  mutate(
    Direction = case_when(
      lag > 0  ~ "Inflation leads tweets",
      lag < 0  ~ "Tweets lead inflation",
      TRUE     ~ "Contemporaneous"
    )
  ) %>%
  select(lag, Direction, r_raw, r_smoothed)

# nice GT table
corr_tab <- corr_df %>%
  mutate(
    `Lag (months)` = lag,
    `r (raw)`      = round(r_raw, 3),
    `r (smoothed)` = round(r_smoothed, 3)
  ) %>%
  select(`Lag (months)`, Direction, `r (raw)`, `r (smoothed)`) %>%
  gt() %>%
  tab_header(
    title = md("**Correlation of Deficit Tweet Share and U.S. CPI Inflation by Lag**"),
    subtitle = md("Positive lag: inflation leads tweets (months). Correlations use overlapping periods.")
  ) %>%
  fmt_number(columns = c(`r (raw)`, `r (smoothed)`), decimals = 2) %>%
  data_color(
    columns = c(`r (raw)`, `r (smoothed)`),
    colors = scales::col_bin(
      palette = c("#f0f4f8", "#d0e3f0", "#a8cbe6", "#7fb0d9", "#5593c8", "#2e77b7"),
      domain = c(-1, 1), bins = 6
    )
  ) %>%
  tab_options(
    table.border.top.color = "white",
    table.border.bottom.color = "white",
    data_row.padding = px(6)
  )

# save table next to the figure
tab_png <- file.path(fig_dir, "table_corr_inflation_lags.png")
tab_html <- file.path(fig_dir, "table_corr_inflation_lags.html")

# Try PNG (needs webshot2 or a headless browser); fall back to HTML if needed
ok <- FALSE
try({
  gt::gtsave(corr_tab, tab_png)
  ok <- TRUE
}, silent = TRUE)
if (!ok) {
  gt::gtsave(corr_tab, tab_html)
}

message("📊 Correlation table saved: ", if (ok) normalizePath(tab_png) else normalizePath(tab_html))
