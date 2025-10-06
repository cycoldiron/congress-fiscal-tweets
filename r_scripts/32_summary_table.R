library(dplyr)
library(gt)
library(scales)

# Colors (same as your figures)
dem_blue <- "#2E77BB"
rep_red  <- "#D54E4E"

# ===== Paths =====
out_dir <- "/Users/cycoldiron/Desktop/congress-fiscal-tweets/results"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ===== Build totals table (D/R + Overall) =====
by_party <- all_tweets_final_index %>%
  filter(party %in% c("Democratic", "Republican")) %>%
  group_by(party) %>%
  summarise(
    total_tweets   = n(),
    deficit_tweets = sum(is_debt == 1L, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(share_deficit = deficit_tweets / total_tweets)

overall_row <- by_party %>%
  summarise(
    party          = "Overall",
    total_tweets   = sum(total_tweets, na.rm = TRUE),
    deficit_tweets = sum(deficit_tweets, na.rm = TRUE),
    share_deficit  = deficit_tweets / total_tweets
  )

tbl_raw <- bind_rows(by_party, overall_row) %>%
  mutate(party = factor(party, levels = c("Democratic","Republican","Overall"))) %>%
  arrange(party)

# ===== Save CSV (unchanged) =====
write.csv(tbl_raw, file.path(out_dir, "t_party_totals.csv"), row.names = FALSE)

# ===== Styled PNG table (party-colored rows + year subtitle) =====
gt_tbl <- tbl_raw %>%
  rename(
    Party          = party,
    `Total Tweets` = total_tweets,
    `Deficit Tweets` = deficit_tweets,
    `Share Deficit`  = share_deficit
  ) %>%
  gt() %>%
  tab_header(
    title    = md("**Totals by Party (Deficit vs. All Tweets)**"),
    subtitle = md("*(2017–2023)*")
  ) %>%
  fmt_number(columns = c(`Total Tweets`, `Deficit Tweets`), decimals = 0, use_seps = TRUE) %>%
  fmt_percent(columns = `Share Deficit`, decimals = 2) %>%
  cols_align(align = "center", columns = everything()) %>%
  # Party row tinting + party-colored text
  tab_style(
    style = list(
      cell_fill(color = scales::alpha(dem_blue, 0.10)),
      cell_text(color = dem_blue)
    ),
    locations = cells_body(rows = Party == "Democratic")
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = scales::alpha(rep_red, 0.10)),
      cell_text(color = rep_red)
    ),
    locations = cells_body(rows = Party == "Republican")
  ) %>%
  # Keep "Overall" un-tinted, but you can add a light gray if you prefer:
  # tab_style(style = cell_fill(color = "#F5F5F5"), locations = cells_body(rows = Party == "Overall")) %>%
  tab_options(
    table.font.names = c("Inter","Helvetica","Arial","sans-serif"),
    data_row.padding = px(6),
    table.width = pct(100)
  )

gtsave(gt_tbl,
       filename = file.path(out_dir, "t_party_totals.png"),
       vwidth = 900, vheight = 360)
