source("r_scripts/03a_setup.R")
source("r_scripts/utils_gt.R")
source("r_scripts/14b_member_month_join.R")


m_mm_main  <- readRDS("models/m_mm_main.rds")
m_mm_phase <- readRDS("models/m_mm_phase.rds")

tbl_main <- make_gt_or_table(
  m_mm_main,
  title = "Deficit Tweeting: Legislative Windows, Partisanship & Power",
  notes = c(
    "- Odds ratios with 95% CIs; stars: *** p<0.001, ** p<0.01, * p<0.05, . p<0.10.",
    "- Member and calendar-month fixed effects; SEs two-way clustered by member and month.",
    "- `mp_z` and `def_z` are z-scored; 0 outside legislative months.",
    "- `covid_window` combines CARES and PPP-HCE; IRA and CHIPS are separate."
  )
)

tbl_phase <- make_gt_or_table(
  m_mm_phase,
  title = "Pre/Post/Passage Effects (Member–Month Logit)",
  notes = c(
    "- `Pre-passage`, `Post-passage`, and `Passage` are month-level flags aggregated from tweets.",
    "- Includes controls for bill partisanship, fiscal magnitude, and IRA/CHIPS windows.",
    "- Same FE and clustering as the main model."
  )
)

dir.create("figures/reg_tables", TRUE, TRUE)
# gtsave(tbl_main,  "figures/reg_tables/m_mm_main_or.html")
# gtsave(tbl_phase, "figures/reg_tables/m_mm_phase_or.html")
