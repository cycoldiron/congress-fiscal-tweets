# r_scripts/03a_setup.R
options(stringsAsFactors = FALSE)
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(lubridate)
  library(stringr); library(purrr)
  library(fixest); library(gt); library(broom)
})

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
dir.create("models",          recursive = TRUE, showWarnings = FALSE)
dir.create("figures/reg_tables", recursive = TRUE, showWarnings = FALSE)

# parameters used across scripts
overlap_targets     <- c("CARES","PPP_HCE","IRA","CHIPS")
pre_buffer_months  <- 0
post_buffer_months <- 0
