# ---- Setup ----
library(dplyr)
library(tidyr)
library(gt)

# Path to tweet-level file with legislative flags
flags_path <- "/Users/cycoldiron/Desktop/congress-fiscal-tweets/data/processed/tweets_with_leg_flags.rds"

# Load data
tweets_with_leg_flags <- readRDS(flags_path)

# Build working df with window flags (now includes TCJA)
tw <- tweets_with_leg_flags %>%
  mutate(
    Party    = party,
    LegisAny = as.integer(is_leg_period == 1),          # keep as-is
    IRA      = as.integer(any_active_IRA   == 1),
    CHIPS    = as.integer(any_active_CHIPS == 1),
    IRA_CHIPS = as.integer( (any_active_IRA == 1) | (any_active_CHIPS == 1) ),  # <-- NEW COMBINED FLAG
    CARES    = as.integer(any_active_CARES == 1),
    PPP_HCE  = as.integer(any_active_PPP_HCE == 1),
    COVID    = as.integer(any_active_CARES == 1 | any_active_PPP_HCE == 1),
    TCJA     = as.integer(any_tcja == 1)                # <-- NEW
  )

# quick guard (include IRA_CHIPS)
stopifnot(all(c("Party","LegisAny","IRA","CHIPS","IRA_CHIPS","CARES","PPP_HCE","COVID","TCJA","is_debt") %in% names(tw)))

outdir <- "/Users/cycoldiron/Desktop/congress-fiscal-tweets/results"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

# ---- Helper: conditional deficit rate in a window ----
cond_deficit <- function(data, win_col) {
  data %>%
    summarize(
      `Tweets (N)`  = sum(.data[[win_col]] == 1, na.rm = TRUE),
      `Deficit (N)` = sum(is_debt == 1 & .data[[win_col]] == 1, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(`Deficit Rate (%)` = ifelse(`Tweets (N)` > 0, `Deficit (N)` / `Tweets (N)`, NA_real_))
}

# ---- OVERALL (tweet level) ----
overall_list <- list(
  Baseline  = tibble(
    Window = "All Tweets (Baseline)",
    `Tweets (N)`  = nrow(tw),
    `Deficit (N)` = sum(tw$is_debt == 1, na.rm = TRUE),
    `Deficit Rate (%)` = mean(tw$is_debt == 1, na.rm = TRUE)
  ),
  LegisAny  = cond_deficit(tw, "LegisAny")   %>% mutate(Window = "Any Legislative Window"),
  IRA_CHIPS = cond_deficit(tw, "IRA_CHIPS")  %>% mutate(Window = "IRA / CHIPS Window"),   # <-- REPLACES SEPARATE ROWS
  CARES     = cond_deficit(tw, "CARES")      %>% mutate(Window = "CARES Window"),
  PPP_HCE   = cond_deficit(tw, "PPP_HCE")    %>% mutate(Window = "PPP–HCE Window"),
  COVID     = cond_deficit(tw, "COVID")      %>% mutate(Window = "COVID Window (CARES or PPP–HCE)"),
  TCJA      = cond_deficit(tw, "TCJA")       %>% mutate(Window = "TCJA Window")
)

overall <- bind_rows(overall_list) %>% select(Window, everything())

gt_overall <- overall %>%
  gt() %>%
  tab_header(
    title    = md("**Deficit Share Within Each Window (Tweet Level)**"),
    subtitle = "Share of tweets that mention the deficit, conditional on being inside the window (2017–2023)."
  ) %>%
  cols_label(`Tweets (N)`="Tweets (N)", `Deficit (N)`="Deficit (N)", `Deficit Rate (%)`="Deficit Rate (%)") %>%
  fmt_number(columns = c(`Tweets (N)`, `Deficit (N)`), decimals = 0, use_seps = TRUE) %>%
  fmt_percent(columns = `Deficit Rate (%)`, decimals = 1) %>%
  cols_align("center", columns = c(`Tweets (N)`, `Deficit (N)`, `Deficit Rate (%)`)) %>%
  tab_source_note(md("_Each percentage is **deficit tweets ÷ all tweets in that window**. The baseline row is the unconditional rate for all tweets._"))

gtsave(gt_overall, file.path(outdir, "tweetlevel_deficit_share_overall.png"))

# ---- BY PARTY (tweet level) ----
by_party <- bind_rows(
  tw %>% group_by(Party) %>%
    summarize(
      `Tweets (N)`  = n(),
      `Deficit (N)` = sum(is_debt == 1, na.rm = TRUE),
      `Deficit Rate (%)` = mean(is_debt == 1, na.rm = TRUE),
      .groups = "drop"
    ) %>% mutate(Window = "All Tweets (Baseline)"),
  
  tw %>% group_by(Party) %>% cond_deficit("LegisAny")   %>% mutate(Window = "Any Legislative Window"),
  tw %>% group_by(Party) %>% cond_deficit("IRA_CHIPS")  %>% mutate(Window = "IRA / CHIPS Window"),  # <-- COMBINED
  tw %>% group_by(Party) %>% cond_deficit("CARES")      %>% mutate(Window = "CARES Window"),
  tw %>% group_by(Party) %>% cond_deficit("PPP_HCE")    %>% mutate(Window = "PPP–HCE Window"),
  tw %>% group_by(Party) %>% cond_deficit("COVID")      %>% mutate(Window = "COVID Window (CARES or PPP–HCE)"),
  tw %>% group_by(Party) %>% cond_deficit("TCJA")       %>% mutate(Window = "TCJA Window")
) %>%
  relocate(Window, Party)

gt_party <- by_party %>%
  gt(groupname_col = "Window", rowname_col = "Party") %>%
  tab_header(
    title    = md("**Deficit Share Within Each Window by Party (Tweet Level)**"),
    subtitle = "Conditional share of deficit tweets given the tweet is inside the window."
  ) %>%
  cols_label(`Tweets (N)`="Tweets (N)", `Deficit (N)`="Deficit (N)", `Deficit Rate (%)`="Deficit Rate (%)") %>%
  fmt_number(columns = c(`Tweets (N)`, `Deficit (N)`), decimals = 0, use_seps = TRUE) %>%
  fmt_percent(columns = `Deficit Rate (%)`, decimals = 1) %>%
  cols_align("center", columns = c(`Tweets (N)`, `Deficit (N)`, `Deficit Rate (%)`)) %>%
  tab_options(row_group.as_column = TRUE) %>%
  tab_source_note(md("_Percentages are conditional on being inside each window; counts are tweet totals **within** the window._"))

gtsave(gt_party, file.path(outdir, "tweetlevel_deficit_share_by_party.png"))
