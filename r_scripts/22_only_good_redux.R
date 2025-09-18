# r_scripts/22_only_good_redux.R

source("r_scripts/03a_setup.R")
source("r_scripts/utils_gt.R")

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(forcats)
  library(fixest); library(gt)
})

# ---------- Data ----------
df_mm <- readRDS("data/processed/tweets_monthly_with_leg.rds") %>%
  mutate(
    prop_def = n_debt_tweets / pmax(n_tweets, 1),
    any_def  = as.integer(n_debt_tweets > 0),
    mp_z     = ifelse(is.na(mean_partisanship_score), 0, as.numeric(scale(mean_partisanship_score))),
    def_z    = ifelse(is.na(sum_deficit_bil),        0, as.numeric(scale(sum_deficit_bil))),
    deficit_reducing = as.integer(!is.na(sum_deficit_bil) & sum_deficit_bil < 0),
    any_IRA_CHIPS = as.integer((!is.na(any_IRA) & any_IRA == 1) | (!is.na(any_CHIPS) & any_CHIPS == 1))
  ) %>%
  filter(n_tweets > 0)

norm_party <- function(x){
  x <- as.character(x)
  dplyr::case_when(
    x %in% c("R","Republican","GOP") ~ "R",
    x %in% c("D","Democrat","Democratic") ~ "D",
    TRUE ~ NA_character_
  )
}

df_mm <- df_mm %>%
  mutate(
    party_std   = norm_party(party),
    house_ctrl  = norm_party(house_control),
    senate_ctrl = norm_party(senate_control),
    pres_ctrl   = norm_party(president_party),
    hold_house  = as.integer(!is.na(party_std) & party_std == house_ctrl),
    hold_sen    = as.integer(!is.na(party_std) & party_std == senate_ctrl),
    hold_pres   = as.integer(!is.na(party_std) & party_std == pres_ctrl),
    minority_pres = 1L - hold_pres,
    control_combo = dplyr::case_when(
      hold_house == 1 & hold_sen == 1 & hold_pres == 1 ~ "H+S+P",
      hold_house == 1 & hold_sen == 1 & hold_pres == 0 ~ "H+S",
      hold_house == 1 & hold_sen == 0 & hold_pres == 1 ~ "H+P",
      hold_house == 0 & hold_sen == 1 & hold_pres == 1 ~ "S+P",
      hold_house == 1 & hold_sen == 0 & hold_pres == 0 ~ "H only",
      hold_house == 0 & hold_sen == 1 & hold_pres == 0 ~ "S only",
      hold_house == 0 & hold_sen == 0 & hold_pres == 1 ~ "P only",
      TRUE                                             ~ "none"
    ),
    control_combo = factor(control_combo,
                           levels = c("none","H only","S only","P only","H+S","H+P","S+P","H+S+P")),
    control_combo = forcats::fct_drop(control_combo),
    gov_trifecta = dplyr::case_when(
      house_ctrl == "R" & senate_ctrl == "R" & pres_ctrl == "R" ~ "R",
      house_ctrl == "D" & senate_ctrl == "D" & pres_ctrl == "D" ~ "D",
      TRUE ~ NA_character_
    ),
    gop_trifecta = ifelse(!is.na(gov_trifecta) & gov_trifecta == "R", 1L,
                          ifelse(!is.na(gov_trifecta) & gov_trifecta == "D", 0L, NA_integer_))
  )

# ---------- MODELS ----------
m_r3 <- feglm(
  prop_def ~ rep:leg_period + rep:covid_window + rep:any_IRA_CHIPS + rep:mp_z + rep:def_z + rep:deficit_reducing
  | full_name + month,
  family  = binomial("logit"),
  data    = df_mm,
  weights = ~ n_tweets,
  vcov    = ~ full_name + month
)

m_r5 <- feglm(
  prop_def ~ rep:leg_period +
    rep:minority_pres +                                      # <- NEW
    rep:leg_period:i(minority_pres, ref = 0) +               # triple interaction
    rep:covid_window + rep:any_IRA_CHIPS + rep:mp_z + rep:def_z + rep:deficit_reducing
  | full_name + month,
  family  = binomial("logit"),
  data    = df_mm %>% filter(!is.na(minority_pres)),
  weights = ~ n_tweets,
  vcov    = ~ full_name + month
)


m_r6 <- feglm(
  prop_def ~ rep:leg_period + rep:leg_period:i(control_combo, ref = "none") +
    rep:covid_window + rep:any_IRA_CHIPS + rep:mp_z + rep:def_z + rep:deficit_reducing
  | full_name + month,
  family  = binomial("logit"),
  data    = df_mm %>% filter(!is.na(control_combo)),
  weights = ~ n_tweets,
  vcov    = ~ full_name + month
)

m_r9 <- feglm(
  prop_def ~ rep:leg_period + rep:leg_period:i(gop_trifecta, ref = 0) +
    rep:covid_window + rep:any_IRA_CHIPS + rep:mp_z + rep:def_z + rep:deficit_reducing
  | full_name + month,
  family  = binomial("logit"),
  data    = df_mm %>% filter(!is.na(gop_trifecta)),
  weights = ~ n_tweets,
  vcov    = ~ full_name + month
)

# ---------- OUTPUT ----------
out_dir <- "/Users/cycoldiron/Desktop/congress-fiscal-tweets/results/good_tables"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

title_r3 <- "Legislative Windows and Major Bills"
title_r5 <- "Presidential Control and Legislative Windows"
title_r6 <- "Chamber–Presidency Control and Legislative Windows"
title_r9 <- "Trifecta Control and Legislative Windows"

# one knob to control the OR–CI gap across all tables
SPACER_PX <- 22

labels_r3 <- c(
  "rep:leg_period"       = "GOP × Legislative window",
  "rep:covid_window"     = "GOP × COVID window",
  "rep:any_IRA_CHIPS"    = "GOP × IRA/CHIPS window",
  "rep:mp_z"             = "GOP × Bill partisanship (z)",
  "rep:def_z"            = "GOP × Fiscal magnitude (z)",
  "rep:deficit_reducing" = "GOP × Deficit-reducing month"
)
tbl_r3 <- make_gt_or_table(
  m_r3,
  title = title_r3,
  desc  = "Republican–Democrat differences in the odds of tweeting about the deficit across legislative windows and major-bill periods, with member and month fixed effects.",
  notes = c(
    "- Odds ratios with 95% CIs; stars: *** p<0.001, ** p<0.01, * p<0.05.",
    "- Member and calendar-month fixed effects; SEs two-way clustered by member and month.",
    "- Bill partisanship (z): 1 SD increase in mean bill partisanship score.",
    "- Fiscal magnitude (z): 1 SD increase in total 10-year deficit impact.",
    "- COVID window and IRA/CHIPS window are month-level indicators."
  ),
  term_labels = labels_r3,
  term_header = "",
  spacer_px = SPACER_PX
)
gtsave(tbl_r3, file.path(out_dir, "r3_legislative_windows_main.png"))

# NOTE: use key WITHOUT '::1'; utils' tolerant matching will include the row
labels_r5 <- c(
  "rep:leg_period"               = "GOP × Legislative window",
  "rep:minority_pres"            = "GOP × Minority presidency",                 # <- NEW
  "rep:leg_period:minority_pres" = "GOP × Legislative window × Minority presidency",
  "rep:covid_window"             = "GOP × COVID window",
  "rep:any_IRA_CHIPS"            = "GOP × IRA/CHIPS window",
  "rep:mp_z"                     = "GOP × Bill partisanship (z)",
  "rep:def_z"                    = "GOP × Fiscal magnitude (z)"
)

tbl_r5 <- make_gt_or_table(
  m_r5,
  title = title_r5,
  desc = "Coefficients are GOP–Dem gaps: outside-leg gap under minority presidency; the legislative-window gap under same-party presidency; and how that gap changes under minority presidency.",
  notes = c(
    "- Odds ratios with 95% CIs; stars: *** p<0.001, ** p<0.01, * p<0.05.",
    "- Member and calendar-month fixed effects; SEs two-way clustered by member and month.",
    "- Bill partisanship (z) and fiscal magnitude (z) are standardized."
  ),
  term_labels = labels_r5,
  term_header = "",
  spacer_px = SPACER_PX
)
gtsave(tbl_r5, file.path(out_dir, "r5_majority_context_main.png"))

labels_r6 <- c(
  "rep:leg_period"                              = "GOP × Legislative window (no control)",
  "rep:leg_period:control_combo::H only"        = "GOP × Legislative window × House-only",
  "rep:leg_period:control_combo::S only"        = "GOP × Legislative window × Senate-only",
  "rep:leg_period:control_combo::P only"        = "GOP × Legislative window × Presidency-only",
  "rep:leg_period:control_combo::H+S"           = "GOP × Legislative window × House+Senate",
  "rep:leg_period:control_combo::H+P"           = "GOP × Legislative window × House+Presidency",
  "rep:leg_period:control_combo::S+P"           = "GOP × Legislative window × Senate+Presidency",
  "rep:leg_period:control_combo::H+S+P"         = "GOP × Legislative window × Trifecta",
  "rep:covid_window"                            = "GOP × COVID window",
  "rep:any_IRA_CHIPS"                           = "GOP × IRA/CHIPS window",
  "rep:mp_z"                                    = "GOP × Bill partisanship (z)",
  "rep:def_z"                                   = "GOP × Fiscal magnitude (z)"
)
tbl_r6 <- make_gt_or_table(
  m_r6,
  title = title_r6,
  desc  = paste0(
    "Baseline: Democrats, outside legislative windows, when their party controls none. ",
    "Rows report GOP–Dem gaps and how the legislative-window gap varies by control state; ",
    "pure Democrat shifts and a baseline GOP main effect are omitted by design."
  ),
  notes = c(
    "- Odds ratios with 95% CIs; stars: *** p<0.001, ** p<0.01, * p<0.05.",
    "- Member and month fixed effects; SEs two-way clustered by member and month.",
    "- Bill partisanship (z): 1 SD increase in mean bill partisanship score.",
    "- Fiscal magnitude (z): 1 SD increase in total 10-year deficit impact."
  ),
  term_labels = labels_r6,
  term_header = "",
  spacer_px = SPACER_PX
)
gtsave(tbl_r6, file.path(out_dir, "r6_majority_context_combos.png"))

labels_r9 <- c(
  "rep:leg_period"                 = "GOP × Legislative window (Dem trifecta)",
  "rep:leg_period:gop_trifecta::1" = "GOP × Legislative window × GOP trifecta",
  "rep:covid_window"               = "GOP × COVID window",
  "rep:any_IRA_CHIPS"              = "GOP × IRA/CHIPS window",
  "rep:mp_z"                       = "GOP × Bill partisanship (z)",
  "rep:def_z"                      = "GOP × Fiscal magnitude (z)"
)
tbl_r9 <- make_gt_or_table(
  m_r9,
  title = title_r9,
  desc  = "Baseline: Democratic trifecta. Rows are GOP–Dem odds ratios; the GOP-trifecta term is relative to the Democratic-trifecta baseline.",
  notes = c(
    "- Odds ratios with 95% CIs; stars: *** p<0.001, ** p<0.01, * p<0.05.",
    "- Member & month fixed effects; clustered SEs.",
    "- Bill partisanship (z) and fiscal magnitude (z) are standardized."
  ),
  term_labels = labels_r9,
  term_header = "",
  spacer_px = SPACER_PX
)
gtsave(tbl_r9, file.path(out_dir, "r9_trifecta_sym.png"))

message("Saved revised tables to: ", out_dir)
