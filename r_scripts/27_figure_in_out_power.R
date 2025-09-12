# ===== Libraries =====
library(tidyverse)
library(scales)
load("/Users/cycoldiron/Desktop/congress-fiscal-tweets/data/processed/06_member_deficit_behavior_summary.RData")

# ===== Parameters =====
min_total <- 500  # keep members with at least this many total tweets
out_dir   <- "/Users/cycoldiron/Desktop/congress-fiscal-tweets/figures/individuals"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Party colors (D = blue, R = red)
dem_blue <- "#2E77BB"
rep_red  <- "#D54E4E"

# ===== Prep Data =====
# Expects member_deficit_behavior_summary in memory with columns shown in your str()
df_base <- member_deficit_behavior_summary %>%
  filter(total_tweets >= min_total) %>%
  mutate(party = factor(party, levels = c("Democratic", "Republican")))

# Long format for in vs out figures
df_long <- df_base %>%
  select(full_name, party, pct_debt_tweets_in_power, pct_debt_tweets_out_power) %>%
  pivot_longer(
    c(pct_debt_tweets_in_power, pct_debt_tweets_out_power),
    names_to  = "power_status",
    values_to = "pct_deficit"
  ) %>%
  mutate(power_status = recode(power_status,
                               pct_debt_tweets_in_power  = "In Power",
                               pct_debt_tweets_out_power = "Out of Power"))

# Clip only the display (not the stats) so the “body” isn’t crushed by extreme outliers
clip_top <- quantile(df_long$pct_deficit, 0.995, na.rm = TRUE)  # y cap for boxplot
x_clip   <- quantile(df_long$pct_deficit, 0.995, na.rm = TRUE)  # x cap for density

# Party medians for vertical reference lines on density plot
meds <- df_long |>
  group_by(power_status, party) |>
  summarise(med = median(pct_deficit, na.rm = TRUE), .groups = "drop")

# Helper theme: bold axis titles
theme_axis_bold <- theme(
  axis.title.x = element_text(face = "bold"),
  axis.title.y = element_text(face = "bold")
)

# ===== (1) Boxplots by Party, split by Power =====
p_box_split <- ggplot(df_long, aes(x = party, y = pct_deficit, fill = party)) +
  geom_boxplot(outlier.alpha = 0.15, width = 0.65, color = "grey20") +
  geom_jitter(aes(color = party), width = 0.12, height = 0, alpha = 0.25, size = 0.9, show.legend = FALSE) +
  stat_summary(fun = median, geom = "point", shape = 23, size = 2.3, fill = "white", color = "grey20") +
  facet_wrap(~ power_status, ncol = 2) +
  coord_cartesian(ylim = c(0, clip_top)) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  scale_fill_manual(values = c(Democratic = dem_blue, Republican = rep_red)) +
  scale_color_manual(values = c(Democratic = dem_blue, Republican = rep_red)) +
  labs(
    title = "Members' Deficit Tweet Share by Party, Split by Power (2017–2023)",
    x = NULL,
    y = "% of Deficit Tweets",
    fill = NULL
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "none",
    panel.grid.minor = element_blank()
  ) +
  theme_axis_bold

ggsave(file.path(out_dir, "box_party_split_by_power.png"),
       p_box_split, width = 10, height = 6.2, dpi = 300)

# ===== (2) Smoothed distribution (density) by Party, split by Power =====
p_dens_power <- ggplot(df_long, aes(x = pct_deficit, fill = party, color = party)) +
  geom_density(alpha = 0.28, adjust = 1.1, linewidth = 0.6) +
  geom_vline(data = meds, aes(xintercept = med, color = party),
             linetype = 2, linewidth = 0.6, show.legend = FALSE) +
  facet_wrap(~ power_status, ncol = 2) +
  coord_cartesian(xlim = c(0, x_clip)) +
  scale_x_continuous(labels = percent_format(accuracy = 0.1)) +
  scale_fill_manual(values = c(Democratic = dem_blue, Republican = rep_red)) +
  scale_color_manual(values = c(Democratic = dem_blue, Republican = rep_red)) +
  labs(
    title    = "Distribution of Members' Deficit Tweet Share by Party, Split by Power (2017–2023)",
    subtitle = "Dashed lines show party medians.",
    x = "% of Deficit Tweets",
    y = "Density",
    fill = "Party",
    color = "Party"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid.minor = element_blank()
  ) +
  theme_axis_bold

ggsave(file.path(out_dir, "density_pct_deficit_by_party_split_power.png"),
       p_dens_power, width = 10, height = 6.2, dpi = 300)

# ===== Diff data (In – Out) for figures 3–4 =====
df_diff <- df_base %>%
  transmute(full_name, party,
            diff_in_minus_out = pct_debt_tweets_in_power - pct_debt_tweets_out_power)

# ===== (3) Histogram of In – Out difference, faceted by party =====
p_diff_faceted <- ggplot(df_diff, aes(x = diff_in_minus_out, fill = party)) +
  geom_histogram(bins = 40, alpha = 0.35, color = "white") +
  facet_wrap(~ party, ncol = 2) +
  geom_vline(xintercept = 0, linetype = 2, color = "grey35") +
  scale_fill_manual(values = c(Democratic = dem_blue, Republican = rep_red)) +
  scale_x_continuous(labels = percent_format(accuracy = 0.1)) +
  labs(
    title = "Change in Deficit Tweet Share: In Power – Out of Power (2017–2023)",
    x = "Percentage point difference",
    y = "Count of members",
    fill = "Party"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  ) +
  theme_axis_bold

ggsave(file.path(out_dir, "hist_in_minus_out_faceted.png"),
       p_diff_faceted, width = 10, height = 6.2, dpi = 300)

# ===== (4) Histogram of In – Out difference, overlaid =====
p_diff_overlay <- ggplot(df_diff, aes(x = diff_in_minus_out, fill = party, color = party)) +
  geom_histogram(position = "identity", bins = 40, alpha = 0.35) +
  geom_vline(xintercept = 0, linetype = 2, color = "grey35") +
  scale_fill_manual(values = c(Democratic = dem_blue, Republican = rep_red)) +
  scale_color_manual(values = c(Democratic = dem_blue, Republican = rep_red)) +
  scale_x_continuous(labels = percent_format(accuracy = 0.1)) +
  labs(
    title = "Change in Deficit Tweet Share: In Power – Out of Power (2017–2023)",
    x = "Percentage point difference",
    y = "Count of members",
    fill = "Party",
    color = "Party"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank()
  ) +
  theme_axis_bold

ggsave(file.path(out_dir, "hist_in_minus_out_overlay.png"),
       p_diff_overlay, width = 10, height = 6.2, dpi = 300)

# ===== (5) Histogram: Distribution of % deficit tweets by member, colored by party =====
p_hist_party <- ggplot(df_base, aes(x = pct_debt_tweets, fill = party, color = party)) +
  geom_histogram(position = "identity", bins = 40, alpha = 0.35) +
  scale_x_continuous(labels = percent_format(accuracy = 0.1)) +
  scale_fill_manual(values = c(Democratic = dem_blue, Republican = rep_red)) +
  scale_color_manual(values = c(Democratic = dem_blue, Republican = rep_red)) +
  labs(
    title = "Distribution of Members' Deficit Tweet Share by Party (2017–2023)",
    x = "% of Deficit Tweets",
    y = "Count of members",
    fill = "Party",
    color = "Party"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank()
  ) +
  theme_axis_bold

ggsave(file.path(out_dir, "hist_pct_deficit_by_party.png"),
       p_hist_party, width = 10, height = 6.2, dpi = 300)

# ===== Table: Quartiles, Median, and Range by Party × Power =====
library(gt)

# helper for percent formatting
p_fmt <- function(x, acc = 0.1) scales::percent(x, accuracy = acc)

tbl_stats <-
  df_long %>%
  group_by(power_status, party) %>%
  summarise(
    N      = dplyr::n(),
    min    = min(pct_deficit, na.rm = TRUE),
    q1     = quantile(pct_deficit, 0.25, na.rm = TRUE),
    median = median(pct_deficit, na.rm = TRUE),
    q3     = quantile(pct_deficit, 0.75, na.rm = TRUE),
    max    = max(pct_deficit, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    range_str = paste0(p_fmt(min), " – ", p_fmt(max))
  ) %>%
  select(
    `Power Status` = power_status,
    Party = party,
    N,
    `Min` = min,
    `Q1` = q1,
    `Median` = median,
    `Q3` = q3,
    `Max` = max,
    `Range` = range_str
  )

# party color map for row striping / text accents
party_colors <- c("Democratic" = dem_blue, "Republican" = rep_red)

gt_tbl <-
  tbl_stats |>
  arrange(`Power Status`, factor(Party, levels = c("Democratic","Republican"))) |>
  gt(rowname_col = NULL, groupname_col = "Power Status") |>
  tab_header(
    title    = md("**Quartiles, Median, and Range of Members' Deficit Tweet Share**"),
    subtitle = md("By party and power context (members with ≥ **500** total tweets)")
  ) |>
  fmt_number(columns = "N", decimals = 0, use_seps = TRUE) |>
  fmt_percent(columns = c(Min, Q1, Median, Q3, Max), decimals = 1) |>
  cols_align(
    align = "center",
    columns = everything()
  ) |>
  tab_style( # bold column labels
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) |>
  tab_style( # subtle group headers
    style = list(cell_text(weight = "bold")),
    locations = cells_row_groups()
  ) |>
  data_color( # light party-tinted background on Party column
    columns = Party,
    colors = scales::col_factor(
      palette = c(party_colors[["Democratic"]], party_colors[["Republican"]]),
      domain  = c("Democratic", "Republican"),
      alpha   = 0.08
    )
  ) |>
  tab_options(
    table.font.names = c("Inter","Helvetica","Arial", "sans-serif"),
    table.width = pct(100),
    data_row.padding = px(6),
    row_group.as_column = TRUE
  ) |>
  tab_source_note(md("Values shown as percent of member's tweets that mention the deficit. Range shown as *min – max*."))

# Save PNG
gt_path <- file.path(out_dir, "t_quartiles_by_party_power.png")
gtsave(gt_tbl, filename = gt_path, vwidth = 1200, vheight = 700)

# (Optional) also print to viewer
gt_tbl
