library(tidyverse)
library(lubridate)

# === Power Status Timeline ===
power_status <- tibble(
  month = seq(ymd("2017-01-01"), ymd("2022-12-01"), by = "month")
) %>%
  mutate(
    congress_number = case_when(
      month < ymd("2019-01-03") ~ 115,
      month < ymd("2021-01-03") ~ 116,
      TRUE ~ 117
    ),
    president = case_when(
      month < ymd("2021-01-20") ~ "Donald Trump",
      TRUE ~ "Joe Biden"
    ),
    president_party = if_else(president == "Donald Trump", "Republican", "Democratic"),
    
    # === Chamber Control ===
    senate_control = case_when(
      congress_number == 115 ~ "Republican",
      congress_number == 116 ~ "Republican",
      # For 117, Dems took control on Jan 20, 2021 via VP Harris
      month < ymd("2021-01-20") ~ "Republican",
      TRUE ~ "Democratic"
    ),
    house_control = case_when(
      congress_number == 115 ~ "Republican",
      congress_number == 116 ~ "Democratic",
      congress_number == 117 ~ "Democratic"
    ),
    
    # === Chamber Leadership ===
    senate_majority = case_when(
      month < ymd("2021-01-20") ~ "Mitch McConnell",
      TRUE ~ "Chuck Schumer"
    ),
    senate_minority = case_when(
      month < ymd("2021-01-20") ~ "Chuck Schumer",
      TRUE ~ "Mitch McConnell"
    ),
    house_majority = case_when(
      month < ymd("2019-01-03") ~ "Paul Ryan",
      TRUE ~ "Nancy Pelosi"
    ),
    house_minority = case_when(
      month < ymd("2019-01-03") ~ "Nancy Pelosi",
      month < ymd("2023-01-03") ~ "Kevin McCarthy",
      TRUE ~ NA_character_
    ),
    
    # === Government Type (Unified vs Divided) ===
    gov_type = case_when(
      president_party == house_control & president_party == senate_control ~ paste(president_party, "Unified"),
      TRUE ~ paste(president_party, "Divided")
    )
  )

# === Legislative Events ===
legislation_timeline <- tribble(
  ~bill_name, ~bill_date, ~congress, ~partisanship_score,
  
  # 115th
  "Tax Cuts and Jobs Act (TCJA)", ymd("2017-12-22"), 115, 0.965,
  "Bipartisan Budget Act of 2018", ymd("2018-02-09"), 115, 0.0,
  
  # 116th
  "Coronavirus Aid, Relief, and Economic Security (CARES) Act", ymd("2020-03-27"), 116, 0.0,
  "Paycheck Protection Program and Health Care Enhancement Act", ymd("2020-04-24"), 116, 0.0,
  "Consolidated Appropriations Act, 2021", ymd("2020-12-27"), 116, 0.0,
  
  # 117th
  "American Rescue Plan Act (ARPA)", ymd("2021-03-11"), 117, 0.998,
  "Infrastructure Investment and Jobs Act (IIJA)", ymd("2021-11-15"), 117, 0.62,
  "CHIPS and Science Act", ymd("2022-08-09"), 117, 0.62,
  "Inflation Reduction Act (IRA)", ymd("2022-08-16"), 117, 0.97
) %>%
  mutate(
    window_start = bill_date %m-% months(1),
    window_end = bill_date %m+% months(1),
    is_partisan = as.integer(partisanship_score >= 0.8),
    est_10yr_deficit_impact_billion = c(
      -1900,   # TCJA
      -420,    # Bipartisan Budget Act
      -2100,   # CARES
      -480,    # PPP + Healthcare
      -900,    # 2021 Appropriations
      -1900,   # ARPA
      -250,    # IIJA
      -280,    # CHIPS
      238      # IRA (deficit reduction)
    )
  )

# === Save Overwriting Existing Historical Policy File ===
save(power_status, legislation_timeline,
     file = "data/processed/03_power_policy_historical.Rdata")
