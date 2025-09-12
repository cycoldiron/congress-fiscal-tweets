library(dplyr)
library(ggplot2)
library(scales)

# ===== Paths (project-relative) =====
m_path  <- file.path("data", "processed", "tweets_monthly_with_leg.rds")
fig_dir <- file.path("figures", "economic_indicators")
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)
out_png <- file.path(fig_dir, "deficit_share_vs_debt_growth_FIXED.png")

# ===== Load & prep =====
mm <- readRDS(m_path)
stopifnot(all(c("month","n_tweets","n_debt_tweets","total_public_debt_outstanding") %in% names(mm)))

overall <- mm %>%
  mutate(
    month      = as.Date(month),
    debt_total = as.double(total_public_debt_outstanding)
  ) %>%
  arrange(month) %>%
  transmute(
    month,
    pct_deficit   = 100 * n_debt_tweets / n_tweets,                    # % of tweets (left axis)
    debt_mom_pct  = 100 * (debt_total / dplyr::lag(debt_total) - 1)    # MoM % change (right axis)
  )

# ===== Axis windows you can tweak =====
# Left axis (deficit tweets %): 0 to a rounded top (nearest 0.5)
left_top <- ceiling((max(overall$pct_deficit, na.rm = TRUE) * 1.1) / 0.5) * 0.5
y1_lim   <- c(0, max(0.5, left_top))                 # never less than 0.5 top to avoid cramped axis
y1_brks  <- seq(y1_lim[1], y1_lim[2], by = 0.5)

# Right axis (MoM %): zoom where variation is; adjust if you want a different window
y2_lim   <- c(-0.5, 1.5)                             # in percentage points
y2_brks  <- seq(y2_lim[1], y2_lim[2], by = 0.5)

# Map right-axis values onto left scale
a <- (y1_lim[2] - y1_lim[1]) / (y2_lim[2] - y2_lim[1])
b <- y1_lim[1] - a * y2_lim[1]
overall <- overall %>% mutate(debt_mom_scaled = a * debt_mom_pct + b)

# ===== Theme (white bg) =====
base <- 13
theme_pub <- theme_minimal(base_size = base) +
  theme(
    plot.title         = element_text(face = "bold", hjust = 0.5, size = base + 2),
    plot.subtitle      = element_text(hjust = 0.5),
    axis.title.x       = element_text(margin = margin(t = 8)),
    axis.title.y       = element_text(margin = margin(r = 8)),
    axis.title.y.right = element_text(margin = margin(l = 8)),
    panel.grid.minor   = element_blank(),
    legend.position    = "top",
    legend.title       = element_blank(),
    panel.background   = element_rect(fill = "white", color = NA),
    plot.background    = element_rect(fill = "white", color = NA),
    plot.caption       = element_text(hjust = 0.5, size = base - 1)
  )

# ===== Plot (smoothed, solid lines) =====
p <- ggplot(overall, aes(x = month)) +
  geom_smooth(aes(y = pct_deficit, color = "Deficit tweet share (%)"),
              method = "loess", se = FALSE, span = 0.25, size = 1.2, na.rm = TRUE) +
  geom_smooth(aes(y = debt_mom_scaled, color = "Debt growth (MoM, %)"),
              method = "loess", se = FALSE, span = 0.25, size = 1.2, na.rm = TRUE) +
  scale_y_continuous(
    name   = "% of Tweets Mentioning Deficit (monthly)",
    limits = y1_lim, breaks = y1_brks,
    sec.axis = sec_axis(
      ~ (.-b)/a,
      name   = "U.S. Public Debt Growth (MoM, %)",
      breaks = y2_brks,
      labels = label_percent(accuracy = 0.1, scale = 1)
    )
  ) +
  scale_color_manual(values = c(
    "Deficit tweet share (%)" = "#2C7BE5",
    "Debt growth (MoM, %)"    = "#6C757D"
  )) +
  labs(
    title    = "Deficit Tweet Share vs U.S. Public Debt Growth",
    subtitle = "Monthly totals, Jun 2017 – Jan 2023",
    x        = "Month",
    caption  = "Debt series: U.S. Total Public Debt Outstanding (GFDEBTN). MoM % change computed from monthly values."
  ) +
  theme_pub

ggsave(out_png, p, width = 9, height = 6, dpi = 320)
message("✅ Saved: ", normalizePath(out_png))
