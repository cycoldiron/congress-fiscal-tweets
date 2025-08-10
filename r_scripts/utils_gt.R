# --- utils_gt.R ---
suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(broom)
  library(gt)
  library(fixest)
})

# Labels you want to see in the table
label_map <- c(
  "rep"                         = "Republican (vs. Democrat)",
  "power1"                      = "Partial control (1 of 3)",
  "power3"                      = "Unified control (3 of 3)",
  "leg_period"                  = "Legislative month",
  "any_partisan_period"         = "Partisan bill window",
  "mp_z:leg_period"             = "Bill partisanship (z) × Legislative month",
  "def_z:leg_period"            = "Fiscal magnitude (z) × Legislative month",
  "deficit_reducing"            = "Deficit-reducing bill",
  "covid_window"                = "COVID window (CARES + PPP-HCE)",
  "any_IRA"                     = "IRA window",
  "any_CHIPS"                   = "CHIPS window",
  "rep:leg_period"              = "Republican × Legislative month",
  "rep:any_partisan_period"     = "Republican × Partisan window",
  "rep:mp_z:leg_period"         = "Republican × Partisanship (z) × Leg. month",
  "rep:def_z:leg_period"        = "Republican × Fiscal magnitude (z) × Leg. month",
  "rep:deficit_reducing"        = "Republican × Deficit-reducing",
  "rep:covid_window"            = "Republican × COVID window",
  "rep:any_IRA"                 = "Republican × IRA window",
  "rep:any_CHIPS"               = "Republican × CHIPS window",
  # phase
  "pre_phase_only_mm"           = "Pre-passage month",
  "post_phase_only_mm"          = "Post-passage month",
  "passage_month_mm"            = "Passage month",
  "rep:pre_phase_only_mm"       = "Republican × Pre-passage",
  "rep:post_phase_only_mm"      = "Republican × Post-passage",
  "rep:passage_month_mm"        = "Republican × Passage month"
)

pretty_term <- function(x){
  out <- label_map[x]
  out[is.na(out)] <- x[is.na(out)] |>
    str_replace_all(":", " × ") |>
    str_replace_all("_", " ") |>
    str_replace_all("\\brep\\b", "Republican") |>
    str_squish() |>
    str_to_sentence()
  out
}

pstars <- function(p){
  cut(p,
      breaks = c(-Inf, 0.001, 0.01, 0.05, 0.10, Inf),
      labels = c("***", "**", "*", ".", ""),
      right = FALSE)
}

make_gt_or_table <- function(model,
                             title    = "Member–month logit",
                             subtitle = "Outcome: share of deficit tweets (logit). FE: member & month. SEs clustered by member/month.",
                             notes    = c("- Odds ratios with 95% CIs.",
                                          "- Continuous bill features (mp_z, def_z) are z-scored; 0 outside legislative months.",
                                          "- Weighted by total tweets per member–month."),
                             sort_by_abs = TRUE,
                             decimals = 2) {
  
  tt <- broom::tidy(model, conf.int = TRUE) %>%
    mutate(
      OR     = exp(estimate),
      CI_lo  = exp(conf.low),
      CI_hi  = exp(conf.high),
      stars  = pstars(p.value),
      term_l = pretty_term(term)
    ) %>%
    select(term_l, OR, CI_lo, CI_hi, p.value, stars)
  
  if (sort_by_abs) {
    tt <- tt %>% arrange(desc(abs(log(OR))))
  }
  
  n_obs <- fixest::fitstat(model, "n")
  ll    <- suppressWarnings(as.numeric(logLik(model)))
  aic   <- suppressWarnings(AIC(model))
  bic   <- suppressWarnings(BIC(model))
  
  tt %>%
    mutate(
      `Odds ratio` = sprintf(paste0("%.", decimals, "f%s"), OR, stars),
      `95% CI`     = sprintf(paste0("[%.", decimals, "f, %.", decimals, "f]"), CI_lo, CI_hi),
      `p-value`    = sprintf("%.3f", p.value)
    ) %>%
    select(`Term` = term_l, `Odds ratio`, `95% CI`, `p-value`) %>%
    gt() %>%
    tab_header(title = md(paste0("**", title, "**")),
               subtitle = md(subtitle)) %>%
    fmt_number(columns = `p-value`, decimals = 3) %>%
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_column_labels(everything())) %>%
    cols_width(
      `Term` ~ px(360),
      `Odds ratio` ~ px(130),
      `95% CI` ~ px(185),
      `p-value` ~ px(90)
    ) %>%
    tab_options(
      table.font.names = c("Inter","Helvetica","Arial","sans-serif"),
      data_row.padding = px(6),
      table.border.top.style = "none",
      table.border.bottom.style = "none"
    ) %>%
    tab_style(style = cell_borders(sides = "bottom", color = "#e6e6e6"),
              locations = cells_body(columns = everything())) %>%
    tab_source_note(
      md(paste0("**Model details**  \n",
                "- N = ", format(n_obs, big.mark = ","), "  \n",
                "- LogLik = ", sprintf("%.1f", ll),
                "; AIC = ", sprintf("%.1f", aic),
                "; BIC = ", sprintf("%.1f", bic)))
    ) %>%
    tab_source_note(md(paste(notes, collapse = "\n")))
}
