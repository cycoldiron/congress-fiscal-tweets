#| label: setup-spec-figs
#| message: false
#| warning: false
library(tidyverse)   # includes purrr
library(gt)
library(glue)

fmt_bullets <- function(lines) paste0(paste0("• ", lines), collapse = "<br>")

specs <- tribble(
  ~file_stub, ~title, ~baseline, ~outcome, ~fe, ~ses, ~covariates, ~interactions,
  "legislative-windows", "Legislative Windows, Partisanship & Power",
  "Democrats; outside legislative windows",
  "Member–month logit of deficit-tweet share (Y_it)",
  "Member FE (α_i); Month FE (γ_t)",
  "Two-way clustered by member and month",
  c("Legislative window indicator", "Partisan legislative window",
    "COVID period", "IRA period", "Tweet volume (z)", "Deficit news magnitude (z)"),
  c("GOP × Legislative window", "GOP × Partisan window",
    "GOP × COVID", "GOP × IRA", "GOP × Tweet volume", "GOP × Deficit news magnitude"),
  
  "majority-context-presidency", "Majority Context: Presidency",
  "Democrats; presidency majority; outside legislative windows",
  "Member–month logit of deficit-tweet share (Y_it)",
  "Member FE (α_i); Month FE (γ_t)",
  "Two-way clustered by member and month",
  c("Legislative window", "Minority vs. majority presidency",
    "Partisan legislative window", "COVID", "IRA", "Tweet volume (z)", "Deficit news magnitude (z)"),
  c("GOP × Legislative window",
    "GOP × Legislative window × Minority presidency",
    "GOP × Partisan window", "GOP × COVID", "GOP × IRA",
    "GOP × Tweet volume", "GOP × Deficit news magnitude"),
  
  "majority-context-chamber-presidency", "Majority Context: Chamber + Presidency",
  "Democrats; no control (no chambers & no presidency); outside legislative windows",
  "Member–month logit of deficit-tweet share (Y_it)",
  "Member FE (α_i); Month FE (γ_t)",
  "Two-way clustered by member and month",
  c("Chamber + presidency control combo", "Legislative window",
    "Partisan legislative window", "COVID", "IRA", "Tweet volume (z)", "Deficit news magnitude (z)"),
  c("GOP × Legislative window", "Legislative window × Control combo",
    "GOP × Partisan window", "GOP × COVID", "GOP × IRA",
    "GOP × Tweet volume", "GOP × Deficit news magnitude")
)

# Now accept individual args (so pmap can pass them)
make_spec_card <- function(file_stub, title, baseline, outcome, fe, ses, covariates, interactions) {
  tibble(
    Field = c("Baseline", "Outcome", "Fixed effects", "Standard errors", "Covariates", "Key interactions"),
    Details = c(
      baseline,
      outcome,
      fe,
      ses,
      fmt_bullets(covariates),
      fmt_bullets(interactions)
    )
  ) |>
    gt() |>
    tab_header(
      title = md(glue("**{title}**")),
      subtitle = "Specification card"
    ) |>
    fmt_markdown(columns = "Details") |>
    cols_width(
      Field ~ px(180),
      Details ~ px(640)
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(rows = Field %in% c("Baseline","Outcome"))
    ) |>
    tab_options(table.font.names = c("Source Sans Pro","Helvetica","Arial"))
}

# Render inline
cards <- purrr::pmap(specs, make_spec_card)
cards

#Optional: save PNGs (requires webshot2/Chromium)
dir.create("fig/specs", recursive = TRUE, showWarnings = FALSE)
purrr::pwalk(
  specs,
  \(file_stub, title, baseline, outcome, fe, ses, covariates, interactions){
    card <- make_spec_card(file_stub, title, baseline, outcome, fe, ses, covariates, interactions)
    gtsave(card, filename = glue::glue("fig/specs/{file_stub}.png"))
  }
)

# Verify
list.files("fig/specs")

