# r_scripts/22c_legislation_tables.R
source("r_scripts/03a_setup.R")
source("r_scripts/utils_gt.R")
source("r_scripts/14b_member_month_join.R")


suppressPackageStartupMessages({
  library(gt)
  library(dplyr)
  library(tidyr)
})

# ---------------------------
# Load models saved by 22b
# ---------------------------
m_mm_main        <- readRDS("models/m_mm_main.rds")
m_mm_phase       <- readRDS("models/m_mm_phase.rds")
m_mm_minpres     <- readRDS("models/m_mm_minpres.rds")    # presidency-centric triple
m_mm_combo       <- readRDS("models/m_mm_combo.rds")      # chamber+presidency combo
has_trifecta     <- file.exists("models/m_mm_trifecta_sym.rds")
m_mm_trifecta_sym <- if (has_trifecta) readRDS("models/m_mm_trifecta_sym.rds") else NULL

has_bin_trifecta <- file.exists("models/m_bin_trifecta_sym.rds")
has_bin_combo    <- file.exists("models/m_bin_combo.rds")
m_bin_trifecta_sym <- if (has_bin_trifecta) readRDS("models/m_bin_trifecta_sym.rds") else NULL
m_bin_combo        <- if (has_bin_combo)    readRDS("models/m_bin_combo.rds")        else NULL

# ---------------------------
# Small helpers
# ---------------------------
add_section_borders <- function(tbl, after_labels) {
  for (lab in after_labels) {
    tbl <- tbl |>
      tab_style(
        style = cell_borders(sides = "bottom", weight = px(2)),
        locations = cells_body(rows = term == lab)
      )
  }
  tbl
}

cell_or_ci <- function(est, lo, hi, p) {
  paste0(sprintf("%.2f", est), stars_from_p(p),
         "\n[", sprintf("%.2f", lo), ", ", sprintf("%.2f", hi), "]")
}

# ===== Delta-method contrasts (use fixest vcov) ===================
or_contrast_trifecta <- function(m, leg = 0L, trif = 0L) {
  b  <- stats::coef(m); V <- stats::vcov(m); nm <- names(b)
  r <- setNames(rep(0, length(b)), nm)
  add <- function(term, w) if (term %in% nm) r[term] <<- w
  add("rep", 1)
  add("rep:leg_period", leg)
  add("rep:gop_trifecta::1", trif)
  add("rep:leg_period:gop_trifecta::1", leg * trif)
  
  delta <- sum(r * b)
  var   <- as.numeric(t(r) %*% V %*% r)
  se    <- sqrt(pmax(var, 0))
  z     <- ifelse(se > 0, delta / se, NA_real_)
  p     <- ifelse(is.na(z), NA_real_, 2 * stats::pnorm(-abs(z)))
  list(or = exp(delta), lo = exp(delta - 1.96 * se), hi = exp(delta + 1.96 * se), p = p)
}

or_contrast_combo <- function(m, leg = 0L, state = "none") {
  b  <- stats::coef(m); V <- stats::vcov(m); nm <- names(b)
  r <- setNames(rep(0, length(b)), nm)
  add <- function(term, w) if (term %in% nm) r[term] <<- w
  add("rep", 1)
  add("rep:leg_period", leg)
  if (!identical(state, "none")) {
    add(paste0("rep:control_combo::", state), 1)
    add(paste0("rep:leg_period:control_combo::", state), leg)
  }
  delta <- sum(r * b)
  var   <- as.numeric(t(r) %*% V %*% r)
  se    <- sqrt(pmax(var, 0))
  z     <- ifelse(se > 0, delta / se, NA_real_)
  p     <- ifelse(is.na(z), NA_real_, 2 * stats::pnorm(-abs(z)))
  list(or = exp(delta), lo = exp(delta - 1.96 * se), hi = exp(delta + 1.96 * se), p = p)
}
# =================================================================

# ==========================================================
# Existing tables (r3–r6)
# ==========================================================
labels_main <- c(
  "rep:leg_period"          = "GOP × Legislative window",
  "rep:any_partisan_period" = "GOP × Any partisan-bill month",
  "rep:covid_window"        = "GOP × COVID window",
  "rep:any_IRA"             = "GOP × IRA window",
  "rep:mp_z"                = "GOP × Bill partisanship (z)",
  "rep:def_z"               = "GOP × Fiscal magnitude (z)"
)
desc_main <- "Republican–Democrat differences in the odds of tweeting about the deficit during legislative windows and major-bill periods, with member and month fixed effects."

tbl_main <- make_gt_or_table(
  m_mm_main,
  title = "Deficit Tweeting: Legislative Windows, Partisanship & Power",
  desc  = desc_main,
  notes = c(
    "- Odds ratios with 95% CIs; stars: *** p<0.001, ** p<0.01, * p<0.05.",
    "- Member and calendar-month fixed effects; SEs two-way clustered by member and month.",
    "- Bill partisanship (z): 1 SD increase in mean bill partisanship score.",
    "- Fiscal magnitude (z): 1 SD increase in total 10-year deficit impact.",
    "- covid_window combines CARES and PPP–HCE; IRA and CHIPS are separate."
  ),
  term_labels = labels_main
)

labels_phase <- c(
  "rep:pre_phase_only_mm"   = "GOP × Pre-passage",
  "rep:post_phase_only_mm"  = "GOP × Post-passage",
  "rep:passage_month_mm"    = "GOP × Passage month",
  "rep:any_partisan_period" = "GOP × Any partisan-bill month",
  "rep:mp_z"                = "GOP × Bill partisanship (z)",
  "rep:def_z"               = "GOP × Fiscal magnitude (z)",
  "rep:covid_window"        = "GOP × COVID window",
  "rep:any_IRA"             = "GOP × IRA window",
  "rep:deficit_reducing"    = "GOP × Deficit-reducing month"
)
desc_phase <- "Republican–Democrat differences across pre-, passage-, and post-passage months with the same controls and fixed effects as the main model."

tbl_phase <- make_gt_or_table(
  m_mm_phase,
  title = "Pre/Post/Passage Effects (Member–Month Logit)",
  desc  = desc_phase,
  notes = c(
    "- Odds ratios with 95% CIs; stars: *** p<0.001, ** p<0.01, * p<0.05.",
    "- Member and calendar-month fixed effects; SEs two-way clustered by member and month.",
    "- mp_z and def_z are zero outside legislative months."
  ),
  term_labels = labels_phase
)

tbl_minpres <- gt_or_minority_pres(
  m_mm_minpres,
  title = "Majority Context: Presidency (Legislative Windows × GOP)",
  desc  = "GOP × Legislative window × Minority presidency (baseline is presidency majority). Effects are odds ratios; *** p<0.001, ** p<0.01, * p<0.05."
)

labels_combo_pretty <- c(
  "rep"                                   = "GOP baseline (non-legis., no control)",
  "control_combo::H only"                 = "Power shift: House-only (Dem baseline)",
  "control_combo::H+S+P"                  = "Power shift: Trifecta (Dem baseline)",
  "rep:leg_period"                        = "Δ GOP–Dem in legislative window (no control)",
  "leg_period:control_combo::H only"      = "Δ Legislative window under House-only (Dem baseline)",
  "leg_period:control_combo::H+S+P"       = "Δ Legislative window under Trifecta (Dem baseline)",
  "rep:any_partisan_period"               = "GOP–Dem: partisan-bill month",
  "rep:covid_window"                      = "GOP–Dem: COVID window",
  "rep:any_IRA"                           = "GOP–Dem: IRA window",
  "rep:mp_z"                              = "Per +1 SD bill partisanship (GOP–Dem change)",
  "rep:def_z"                             = "Per +1 SD fiscal magnitude (GOP–Dem change)"
)

tbl_combo <- make_gt_or_table(
  m_mm_combo,
  title = "Majority Context: Chamber + Presidency (Legislative Windows × GOP)",
  desc  = paste0(
    "Baseline: Democrats, outside legislative windows, when their party controls none. ",
    "Coefficients show GOP–Dem gaps or power/legislative adjustments relative to that baseline."
  ),
  notes = c(
    "- A) Baseline & power shifts (outside legislative windows).",
    "- B) Legislative-window adjustments to A.",
    "- C) Major-bill windows (residual GOP–Dem gaps after A–B).",
    "- D) Bill characteristics (per +1 SD).",
    "- Odds ratios with 95% CIs; stars: *** p<0.001, ** p<0.01, * p<0.05.",
    "- Member and month fixed effects; SEs two-way clustered by member and month."
  ),
  term_labels = labels_combo_pretty
) |>
  add_section_borders(after_labels = c(
    "Power shift: Trifecta (Dem baseline)",
    "Δ Legislative window under Trifecta (Dem baseline)",
    "GOP–Dem: IRA window"
  ))

# ==========================================================
# NEW: Scenario OR matrices computed via delta method (no marginaleffects)
# ==========================================================
results_dir <- "/Users/cycoldiron/Desktop/congress-fiscal-tweets/results"
dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)

# -- r11: Trifecta 2x2 (GOP/Dem OR by legislative window)
if (!is.null(m_mm_trifecta_sym)) {
  build_row <- function(trif_lab, trif_val, leg_val) {
    est <- or_contrast_trifecta(m_mm_trifecta_sym, leg = leg_val, trif = trif_val)
    tibble(
      trifecta = trif_lab,
      window   = ifelse(leg_val == 0, "Non-legislative", "Legislative window"),
      cell     = cell_or_ci(est$or, est$lo, est$hi, est$p)
    )
  }
  df <- bind_rows(
    build_row("Dem trifecta", 0, 0),
    build_row("Dem trifecta", 0, 1),
    build_row("GOP trifecta", 1, 0),
    build_row("GOP trifecta", 1, 1)
  ) |>
    tidyr::pivot_wider(names_from = window, values_from = cell)
  
  tbl_r11 <- gt(df) |>
    cols_label(trifecta = "", `Non-legislative` = "Non-legislative", `Legislative window` = "Legislative window") |>
    tab_header(title = md("**Scenario ORs: GOP vs Dem by Trifecta × Legislative Window**")) |>
    tab_source_note(md("_Cells show GOP/Dem odds ratios with 95% CIs (cluster-robust); stars: *** p<0.001, ** p<0.01, * p<0.05. Model: r9._"))
  gtsave(tbl_r11, file.path(results_dir, "r11_scenarios_trifecta.png"))
}

# -- r12: Control state × legislative window (subset to observed states)
if (!is.null(m_mm_combo)) {
  states <- c("none","H only","S+P","H+S+P")
  present <- intersect(states, gsub("^rep:control_combo::","", grep("^rep:control_combo::", names(coef(m_mm_combo)), value = TRUE)))
  states_use <- union("none", present)  # ensure baseline 'none'
  build_row <- function(state_lab, leg_val) {
    est <- or_contrast_combo(m_mm_combo, leg = leg_val, state = state_lab)
    tibble(
      control = dplyr::recode(state_lab,
                              "none" = "No control",
                              "H only" = "House-only",
                              "S+P" = "Senate+Presidency",
                              "H+S+P" = "Trifecta"),
      window  = ifelse(leg_val == 0, "Non-legislative", "Legislative window"),
      cell    = cell_or_ci(est$or, est$lo, est$hi, est$p)
    )
  }
  df <- bind_rows(lapply(states_use, function(s) bind_rows(build_row(s, 0), build_row(s, 1)))) |>
    tidyr::pivot_wider(names_from = window, values_from = cell)
  
  tbl_r12 <- gt(df) |>
    cols_label(control = "", `Non-legislative` = "Non-legislative", `Legislative window` = "Legislative window") |>
    tab_header(title = md("**Scenario ORs: GOP vs Dem by Control State × Legislative Window**")) |>
    tab_source_note(md("_Cells show GOP/Dem odds ratios with 95% CIs (cluster-robust); stars: *** p<0.001, ** p<0.01, * p<0.05. Model: r6._"))
  gtsave(tbl_r12, file.path(results_dir, "r12_scenarios_controlcombo.png"))
}

# -- r13: Extensive margin (binary), trifecta 2x2
if (!is.null(m_bin_trifecta_sym)) {
  build_row <- function(trif_lab, trif_val, leg_val) {
    est <- or_contrast_trifecta(m_bin_trifecta_sym, leg = leg_val, trif = trif_val)
    tibble(
      trifecta = trif_lab,
      window   = ifelse(leg_val == 0, "Non-legislative", "Legislative window"),
      cell     = cell_or_ci(est$or, est$lo, est$hi, est$p)
    )
  }
  df <- bind_rows(
    build_row("Dem trifecta", 0, 0),
    build_row("Dem trifecta", 0, 1),
    build_row("GOP trifecta", 1, 0),
    build_row("GOP trifecta", 1, 1)
  ) |>
    tidyr::pivot_wider(names_from = window, values_from = cell)
  
  tbl_r13 <- gt(df) |>
    cols_label(trifecta = "", `Non-legislative` = "Non-legislative", `Legislative window` = "Legislative window") |>
    tab_header(title = md("**Extensive Margin (Binary): Trifecta × Legislative Window**")) |>
    tab_source_note(md("_Outcome: any deficit tweet (0/1). Odds ratios with 95% CIs (cluster-robust)._"))
  gtsave(tbl_r13, file.path(results_dir, "r13_binary_scenarios_trifecta.png"))
}

# -- r14: Extensive margin (binary), control-combo matrix
if (!is.null(m_bin_combo)) {
  states <- c("none","H only","S+P","H+S+P")
  present <- intersect(states, gsub("^rep:control_combo::","", grep("^rep:control_combo::", names(coef(m_bin_combo)), value = TRUE)))
  states_use <- union("none", present)
  build_row <- function(state_lab, leg_val) {
    est <- or_contrast_combo(m_bin_combo, leg = leg_val, state = state_lab)
    tibble(
      control = dplyr::recode(state_lab,
                              "none" = "No control",
                              "H only" = "House-only",
                              "S+P" = "Senate+Presidency",
                              "H+S+P" = "Trifecta"),
      window  = ifelse(leg_val == 0, "Non-legislative", "Legislative window"),
      cell    = cell_or_ci(est$or, est$lo, est$hi, est$p)
    )
  }
  df <- bind_rows(lapply(states_use, function(s) bind_rows(build_row(s, 0), build_row(s, 1)))) |>
    tidyr::pivot_wider(names_from = window, values_from = cell)
  
  tbl_r14 <- gt(df) |>
    cols_label(control = "", `Non-legislative` = "Non-legislative", `Legislative window` = "Legislative window") |>
    tab_header(title = md("**Extensive Margin (Binary): Control State × Legislative Window**")) |>
    tab_source_note(md("_Outcome: any deficit tweet (0/1). Odds ratios with 95% CIs (cluster-robust)._"))
  gtsave(tbl_r14, file.path(results_dir, "r14_binary_scenarios_controlcombo.png"))
}

# ==========================================================
# Save the existing r3–r6 tables as before
# ==========================================================
gtsave(tbl_main,   file.path(results_dir, "r3_legislative_windows_main.png"))
gtsave(tbl_phase,  file.path(results_dir, "r4_legislative_windows_phase.png"))
gtsave(tbl_minpres,file.path(results_dir, "r5_majority_context_main.png"))
gtsave(tbl_combo,  file.path(results_dir, "r6_majority_context_combos.png"))
