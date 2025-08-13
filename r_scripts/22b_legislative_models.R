# r_scripts/22b_legislative_models.R
source("r_scripts/03a_setup.R")

suppressPackageStartupMessages({
  library(dplyr)
  library(fixest)
  library(forcats)
})

# ---------------------------
# Build member–month dataframe
# ---------------------------
df_mm <- readRDS("data/processed/tweets_monthly_with_leg.rds") %>%
  mutate(
    prop_def = n_debt_tweets / pmax(n_tweets, 1),
    any_def  = as.integer(n_debt_tweets > 0),             # <-- extensive margin
    mp_z  = ifelse(is.na(mean_partisanship_score), 0, as.numeric(scale(mean_partisanship_score))),
    def_z = ifelse(is.na(sum_deficit_bil),        0, as.numeric(scale(sum_deficit_bil))),
    deficit_reducing = as.integer(!is.na(sum_deficit_bil) & sum_deficit_bil < 0)
  ) %>%
  filter(n_tweets > 0)

# Guard (warn-only) for expected columns
needed <- c(
  "prop_def","any_def","rep","power1","power3","leg_period","any_partisan_period",
  "covid_window","any_IRA","any_CHIPS","sum_deficit_bil",
  "full_name","month","deficit_reducing",
  "party","house_control","senate_control","president_party","power_level"
)
missing <- setdiff(needed, intersect(needed, names(df_mm)))
if (length(missing)) warning("Missing (some may be unused if not modeled): ", paste(missing, collapse = ", "))

# Ensure numeric
if ("sum_deficit_bil" %in% names(df_mm)) {
  df_mm <- df_mm %>% mutate(sum_deficit_bil = as.numeric(sum_deficit_bil))
}

# Month-level vars (absorbed by month FE)
month_level <- c("leg_period","any_partisan_period","covid_window",
                 "any_IRA","any_CHIPS","deficit_reducing","mp_z","def_z")
stopifnot(all(month_level %in% names(df_mm)))

# ---------------------------
# Majority / power variables
# ---------------------------
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
    party_std    = if ("party" %in% names(.)) norm_party(party) else NA_character_,
    house_ctrl   = if ("house_control"  %in% names(.)) norm_party(house_control)  else NA_character_,
    senate_ctrl  = if ("senate_control" %in% names(.)) norm_party(senate_control) else NA_character_,
    pres_ctrl    = if ("president_party"%in% names(.)) norm_party(president_party) else NA_character_,
    hold_house   = as.integer(!is.na(party_std) & party_std == house_ctrl),
    hold_sen     = as.integer(!is.na(party_std) & party_std == senate_ctrl),
    hold_pres    = as.integer(!is.na(party_std) & party_std == pres_ctrl),
    minority_pres = 1L - hold_pres
  )

# Optional: majority in the member's own chamber
if ("chamber" %in% names(df_mm)) {
  df_mm <- df_mm %>%
    mutate(
      in_majority_chamber = if_else(
        chamber == "House", as.integer(hold_house == 1),
        if_else(chamber == "Senate", as.integer(hold_sen == 1), NA_integer_)
      )
    )
} else {
  df_mm$in_majority_chamber <- NA_integer_
}

# Control combinations relative to member's party
control_levels <- c("none","H only","S only","P only","H+S","H+P","S+P","H+S+P")
df_mm <- df_mm %>%
  mutate(
    control_combo = case_when(
      hold_house == 1 & hold_sen == 1 & hold_pres == 1 ~ "H+S+P",
      hold_house == 1 & hold_sen == 1 & hold_pres == 0 ~ "H+S",
      hold_house == 1 & hold_sen == 0 & hold_pres == 1 ~ "H+P",
      hold_house == 0 & hold_sen == 1 & hold_pres == 1 ~ "S+P",
      hold_house == 1 & hold_sen == 0 & hold_pres == 0 ~ "H only",
      hold_house == 0 & hold_sen == 1 & hold_pres == 0 ~ "S only",
      hold_house == 0 & hold_sen == 0 & hold_pres == 1 ~ "P only",
      TRUE                                             ~ "none"
    ),
    control_combo = factor(control_combo, levels = control_levels),
    control_combo = forcats::fct_drop(control_combo)
  )

# Government trifecta this month?
df_mm <- df_mm %>%
  mutate(
    gov_trifecta = case_when(
      house_ctrl == "R" & senate_ctrl == "R" & pres_ctrl == "R" ~ "R",
      house_ctrl == "D" & senate_ctrl == "D" & pres_ctrl == "D" ~ "D",
      TRUE ~ NA_character_
    ),
    gop_trifecta = ifelse(!is.na(gov_trifecta) & gov_trifecta == "R", 1L,
                          ifelse(!is.na(gov_trifecta) & gov_trifecta == "D", 0L, NA_integer_))
  )

# Diagnostics
if ("rep" %in% names(df_mm)) {
  message("Counts by party (rep) x minority_pres:\n",
          paste(capture.output(print(with(df_mm, table(rep, minority_pres, useNA="ifany")))), collapse="\n"))
  message("Counts by party (rep) x control_combo:\n",
          paste(capture.output(print(with(df_mm, table(rep, control_combo, useNA="ifany")))), collapse="\n"))
}

# ---------------------------
# Existing models on proportion outcome
# ---------------------------
m_mm_main <- feglm(
  prop_def ~
    rep:leg_period + rep:any_partisan_period +
    rep:covid_window + rep:any_IRA + rep:any_CHIPS + rep:deficit_reducing +
    rep:mp_z + rep:def_z
  | full_name + month,
  family  = binomial("logit"),
  data    = df_mm,
  weights = ~ n_tweets,
  vcov    = ~ full_name + month
)

m_mm_phase <- feglm(
  prop_def ~
    rep:pre_phase_only_mm + rep:post_phase_only_mm + rep:passage_month_mm +
    rep:any_partisan_period + rep:mp_z + rep:def_z +
    rep:covid_window + rep:any_IRA + rep:any_CHIPS + rep:deficit_reducing
  | full_name + month,
  family  = binomial("logit"),
  data    = df_mm,
  weights = ~ n_tweets,
  vcov    = ~ full_name + month
)

m_mm_base <- feglm(
  prop_def ~ rep:leg_period + rep:any_partisan_period
  | full_name + month,
  family  = binomial("logit"),
  data    = df_mm,
  weights = ~ n_tweets,
  vcov    = ~ full_name + month
)

# Presidency-centric triple
m_mm_minpres <- feglm(
  prop_def ~
    rep:leg_period * i(minority_pres, ref = 0) +
    rep:any_partisan_period + rep:covid_window + rep:any_IRA + rep:any_CHIPS +
    rep:mp_z + rep:def_z + rep:deficit_reducing
  | full_name + month,
  family  = binomial("logit"),
  data    = df_mm %>% filter(!is.na(minority_pres)),
  weights = ~ n_tweets,
  vcov    = ~ full_name + month
)

# Chamber+Presidency combos
m_mm_combo <- feglm(
  prop_def ~
    rep * leg_period * i(control_combo, ref = "none") +
    rep:any_partisan_period + rep:covid_window + rep:any_IRA + rep:any_CHIPS +
    rep:mp_z + rep:def_z + rep:deficit_reducing
  | full_name + month,
  family  = binomial("logit"),
  data    = df_mm %>% filter(!is.na(control_combo)),
  weights = ~ n_tweets,
  vcov    = ~ full_name + month
)

# Symmetric trifecta comparison
if (any(!is.na(df_mm$gop_trifecta)) && length(unique(na.omit(df_mm$gop_trifecta))) > 1) {
  m_mm_trifecta_sym <- feglm(
    prop_def ~
      rep * leg_period * i(gop_trifecta, ref = 0) +
      rep:any_partisan_period + rep:covid_window + rep:any_IRA + rep:any_CHIPS +
      rep:mp_z + rep:def_z + rep:deficit_reducing
    | full_name + month,
    family  = binomial("logit"),
    data    = df_mm %>% filter(!is.na(gop_trifecta)),
    weights = ~ n_tweets,
    vcov    = ~ full_name + month
  )
  message("m_mm_trifecta_sym fitted.")
} else {
  message("Skipped m_mm_trifecta_sym (insufficient variation in gov_trifecta).")
}

# ---------------------------
# Extensive margin (binary: any_def)
#   NOTE: do NOT weight by n_tweets for extensive margin.
# ---------------------------
m_bin_combo <- feglm(
  any_def ~
    rep * leg_period * i(control_combo, ref = "none") +
    rep:any_partisan_period + rep:covid_window + rep:any_IRA + rep:any_CHIPS +
    rep:mp_z + rep:def_z + rep:deficit_reducing
  | full_name + month,
  family  = binomial("logit"),
  data    = df_mm %>% filter(!is.na(control_combo)),
  vcov    = ~ full_name + month
)

if (exists("m_mm_trifecta_sym")) {
  m_bin_trifecta_sym <- feglm(
    any_def ~
      rep * leg_period * i(gop_trifecta, ref = 0) +
      rep:any_partisan_period + rep:covid_window + rep:any_IRA + rep:any_CHIPS +
      rep:mp_z + rep:def_z + rep:deficit_reducing
    | full_name + month,
    family  = binomial("logit"),
    data    = df_mm %>% filter(!is.na(gop_trifecta)),
    vcov    = ~ full_name + month
  )
  message("m_bin_trifecta_sym fitted.")
}

# ---------------------------
# Save models
# ---------------------------
dir.create("models", showWarnings = FALSE, recursive = TRUE)
saveRDS(m_mm_main,        "models/m_mm_main.rds")
saveRDS(m_mm_phase,       "models/m_mm_phase.rds")
saveRDS(m_mm_base,        "models/m_mm_base.rds")
saveRDS(m_mm_minpres,     "models/m_mm_minpres.rds")
saveRDS(m_mm_combo,       "models/m_mm_combo.rds")
if (exists("m_mm_trifecta_sym")) saveRDS(m_mm_trifecta_sym, "models/m_mm_trifecta_sym.rds")
saveRDS(m_bin_combo,      "models/m_bin_combo.rds")
if (exists("m_bin_trifecta_sym")) saveRDS(m_bin_trifecta_sym, "models/m_bin_trifecta_sym.rds")

# Optional: list dropped/collinear variables
opt_print <- function(m) if (!is.null(m$collin.var)) print(m$collin.var)
lapply(Filter(exists, c("m_mm_main","m_mm_phase","m_mm_base","m_mm_minpres",
                        "m_mm_combo","m_mm_trifecta_sym","m_bin_combo","m_bin_trifecta_sym")),
       function(nm) opt_print(get(nm)))
