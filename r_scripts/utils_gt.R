suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(broom)
  library(gt)
  library(fixest)
  library(rlang)  # for quasiquotation (!!)
})

# ---- Pretty labels used in tables ----
label_map <- c(
  # add this entry to label_map (anywhere in the block)
  "rep:minority_pres"                    = "GOP × Minority presidency",
  "rep:leg_period"                        = "GOP × Legislative window",
  "rep:covid_window"                      = "GOP × COVID window",
  "rep:any_IRA_CHIPS"                     = "GOP × IRA/CHIPS window",
  "rep:mp_z"                              = "GOP × Bill partisanship (z)",
  "rep:def_z"                             = "GOP × Fiscal magnitude (z)",
  "rep:deficit_reducing"                  = "GOP × Deficit-reducing month",
  "rep:leg_period:minority_pres"          = "GOP × Legislative window × Minority presidency",
  "rep:leg_period:control_combo::H only"  = "GOP × Legislative window × House-only",
  "rep:leg_period:control_combo::S only"  = "GOP × Legislative window × Senate-only",
  "rep:leg_period:control_combo::P only"  = "GOP × Legislative window × Presidency-only",
  "rep:leg_period:control_combo::H+S"     = "GOP × Legislative window × House+Senate",
  "rep:leg_period:control_combo::H+P"     = "GOP × Legislative window × House+Presidency",
  "rep:leg_period:control_combo::S+P"     = "GOP × Legislative window × Senate+Presidency",
  "rep:leg_period:control_combo::H+S+P"   = "GOP × Legislative window × Trifecta",
  "rep:leg_period:gop_trifecta::1"        = "GOP × Legislative window × GOP trifecta"
)

pretty_term <- function(x){
  out <- label_map[x]
  out[is.na(out)] <- x[is.na(out)] |>
    str_replace_all(":", " × ") |>
    str_replace_all("_", " ") |>
    str_replace_all("\\brep\\b", "GOP") |>
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

# ---- helper: match terms allowing fixest::i() expansions like '::1' ----
.match_term_idx <- function(term, keys){
  # 1) exact match
  idx <- match(term, keys, nomatch = 0L)
  if (idx > 0L) return(idx)
  # 2) prefix match for i() terms, e.g., "key::1"
  hits <- which(startsWith(term, paste0(keys, "::")))
  if (length(hits)) return(hits[1]) else return(0L)
}

# ---- Build a gt table (adds a spacer between OR and CI; no p-values column) ----
make_gt_or_table <- function(model,
                             title    = "Member–month logit",
                             desc     = NULL,
                             notes    = character(0),
                             term_labels = NULL,
                             decimals = 2,
                             term_header = "",
                             spacer_px = 56) {
  
  tt <- broom::tidy(model, conf.int = TRUE) %>%
    dplyr::mutate(
      OR     = exp(estimate),
      CI_lo  = exp(conf.low),
      CI_hi  = exp(conf.high),
      stars  = pstars(p.value),
      term_l = pretty_term(term)
    ) %>%
    dplyr::select(term, term_l, OR, CI_lo, CI_hi, p.value, stars)
  
  # Optional: keep/order specific terms (tolerant to i() "::level" suffixes)
  if (!is.null(term_labels)) {
    keep <- names(term_labels)
    idx  <- vapply(tt$term, .match_term_idx, integer(1), keys = keep)
    tt   <- tt[idx > 0L, , drop = FALSE] %>%
      dplyr::mutate(
        term_l = unname(term_labels[ keep[idx] ]),
        .ord   = idx
      ) %>%
      dplyr::arrange(.ord) %>%
      dplyr::select(-.ord)
  }
  
  # Safe internal names (no spaces)
  tt_render <- tt %>%
    dplyr::mutate(
      OddsRatio = sprintf(paste0("%.", decimals, "f%s"), OR, stars),
      CI95      = sprintf(paste0("[%.", decimals, "f, %.", decimals, "f]"), CI_lo, CI_hi),
      Spacer    = ""  # visual gap between OR and CI
    ) %>%
    dplyr::select(Term = term_l, OddsRatio, Spacer, CI95)
  
  # Build width formulas with *inlined* px objects so no env lookup happens
  width_specs <- list(
    expr(Term      ~ !!gt::px(520)),
    expr(OddsRatio ~ !!gt::px(170)),
    expr(Spacer    ~ !!gt::px(spacer_px)),
    expr(CI95      ~ !!gt::px(260))
  )
  
  gt::gt(tt_render) %>%
    gt::tab_header(title = gt::md(paste0("**", title, "**"))) %>%
    gt::cols_label(
      Term      = term_header,
      OddsRatio = "Odds Ratio",
      CI95      = "95% CI",
      Spacer    = gt::md(" ")
    ) %>%
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_column_labels(gt::everything())
    ) %>%
    gt::cols_width(!!!width_specs) %>%
    gt::cols_align(align = "right", columns = c(OddsRatio, CI95)) %>%
    gt::tab_options(
      table.font.names = c("Inter","Helvetica","Arial","sans-serif"),
      data_row.padding = gt::px(6),
      table.border.top.style = "none",
      table.border.bottom.style = "none"
    ) %>%
    gt::tab_style(
      style = gt::cell_borders(sides = "bottom", color = "#e6e6e6"),
      locations = gt::cells_body(columns = gt::everything())
    ) %>%
    { if (!is.null(desc)) gt::tab_source_note(., gt::md(desc)) else . } %>%
    { if (length(notes)) gt::tab_source_note(., gt::md(paste(notes, collapse = "\n"))) else . } %>%
    # >>> Italicize all descriptions/notes <<<
    gt::tab_style(
      style = gt::cell_text(style = "italic"),
      locations = gt::cells_source_notes()
    )
}
