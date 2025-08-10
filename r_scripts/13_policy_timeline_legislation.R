load("data/processed/03_power_policy_historical.Rdata")

# --- packages ---
library(dplyr); library(tidyr); library(lubridate)
library(stringr); library(purrr)

# ---- parameters ----
pre_buffer_months  <- 0
post_buffer_months <- 0
overlap_targets <- c("CARES","PPP_HCE","IRA","CHIPS")  # bill dummies we keep separate

# ---- helpers ----
canon_bill <- function(x) case_when(
  str_detect(x, regex("Tax Cuts and Jobs Act", TRUE)) ~ "TCJA",
  str_detect(x, regex("^Bipartisan Budget Act of 2018", TRUE)) ~ "BBA2018",
  str_detect(x, regex("Coronavirus Aid.*CARES", TRUE)) ~ "CARES",
  str_detect(x, regex("^Paycheck Protection Program.*Enhancement", TRUE)) ~ "PPP_HCE",
  str_detect(x, regex("^Consolidated Appropriations Act, 2021", TRUE)) ~ "CAA2021",
  str_detect(x, regex("^American Rescue Plan Act", TRUE)) ~ "ARPA",
  str_detect(x, regex("^Infrastructure Investment and Jobs Act", TRUE)) ~ "IIJA",
  str_detect(x, regex("^CHIPS and Science Act", TRUE)) ~ "CHIPS",
  str_detect(x, regex("^Inflation Reduction Act", TRUE)) ~ "IRA",
  TRUE ~ "OTHER"
)

make_bill_maps <- function(legislation_timeline, pre_buf = 0, post_buf = 0) {
  bill_month_map <- legislation_timeline %>%
    mutate(
      bill_short        = canon_bill(bill_name),
      window_start_adj  = floor_date(window_start %m-% months(pre_buf),  "month"),
      window_end_adj    = floor_date(window_end   %m+% months(post_buf), "month"),
      bill_month_pass   = floor_date(bill_date, "month"),
      bill_month_seq    = map2(window_start_adj, window_end_adj, ~ seq(.x, .y, by = "1 month"))
    ) %>%
    select(bill_short, bill_name, bill_date, bill_month_pass, congress,
           partisanship_score, is_partisan, est_10yr_deficit_impact_billion,
           bill_month_seq) %>%
    unnest(bill_month_seq) %>%
    rename(month = bill_month_seq) %>%
    mutate(
      period_phase = case_when(
        month < bill_month_pass ~ "pre",
        month > bill_month_pass ~ "post",
        TRUE ~ "passage_month"
      )
    )
  
  bills_per_month <- bill_month_map %>% count(month, name = "n_active_bills_month")
  list(bill_month_map = bill_month_map, bills_per_month = bills_per_month)
}

# ---- 1) build bill-month mapping (with buffers) ----
maps <- make_bill_maps(legislation_timeline, pre_buffer_months, post_buffer_months)
bill_month_map  <- maps$bill_month_map
bills_per_month <- maps$bills_per_month

# ---- 2) month-keyed tweets ----
tweets_monthkey <- all_tweets_final_index %>%
  mutate(
    month   = floor_date(coalesce(tweet_month, tweet_date), "month"),
    .row_id = row_number(),
    member_month_id = paste0(screen_name, "_", month)
  )

# ---- 3) LONG join (tweet × bill) ----
tweets_x_bill_long <- tweets_monthkey %>%
  inner_join(bill_month_map, by = "month", relationship = "many-to-many") %>%
  left_join(bills_per_month, by = "month") %>%
  mutate(
    is_leg_period = 1L,
    long_weight   = 1 / n_active_bills_month
  )

# ---- 4) collapse summaries (per tweet) ----
base_collapse <- tweets_x_bill_long %>%
  group_by(.row_id) %>%
  summarise(
    is_leg_period                = 1L,
    n_active_bills               = n(),
    any_partisan_period          = as.integer(any(is_partisan == 1)),
    mean_partisanship_score      = mean(partisanship_score, na.rm = TRUE),
    max_partisanship_score       = max(partisanship_score, na.rm = TRUE),
    sum_est_10yr_deficit_billion = sum(est_10yr_deficit_impact_billion, na.rm = TRUE),
    max_abs_deficit_billion      = est_10yr_deficit_impact_billion[which.max(abs(est_10yr_deficit_impact_billion))] %||% NA_real_,
    has_pre_phase                = as.integer(any(period_phase == "pre")),
    has_post_phase               = as.integer(any(period_phase == "post")),
    has_passage_month            = as.integer(any(period_phase == "passage_month")),
    active_bill_names            = str_c(unique(bill_name), collapse = " | "),
    .groups = "drop"
  )

# ---- 5) bill-specific wide indicators for overlap bills ----
wide_by_bill <- tweets_x_bill_long %>%
  filter(bill_short %in% overlap_targets) %>%
  group_by(.row_id, bill_short) %>%
  summarise(
    any_active = 1L,
    bill_deficit_billion = sum(est_10yr_deficit_impact_billion, na.rm = TRUE),
    bill_partisanship    = mean(partisanship_score, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    id_cols   = .row_id,
    names_from  = bill_short,
    values_from = c(any_active, bill_deficit_billion, bill_partisanship),
    names_sep   = "_",
    values_fill = 0
  )

# ---- 6) final collapsed tweet-level dataset ----
tweets_with_leg_flags <- tweets_monthkey %>%
  left_join(base_collapse, by = ".row_id") %>%
  left_join(wide_by_bill,   by = ".row_id") %>%
  mutate(
    is_leg_period           = coalesce(is_leg_period, 0L),
    any_partisan_period     = coalesce(any_partisan_period, 0L),
    n_active_bills          = coalesce(n_active_bills, 0L),
    has_pre_phase           = coalesce(has_pre_phase, 0L),
    has_post_phase          = coalesce(has_post_phase, 0L),
    has_passage_month       = coalesce(has_passage_month, 0L),
    mean_partisanship_score = if_else(is_leg_period == 0L, NA_real_, mean_partisanship_score),
    max_partisanship_score  = if_else(is_leg_period == 0L, NA_real_, max_partisanship_score),
    sum_est_10yr_deficit_billion = if_else(is_leg_period == 0L, 0, sum_est_10yr_deficit_billion),
    max_abs_deficit_billion      = if_else(is_leg_period == 0L, 0, max_abs_deficit_billion),
    multi_bill_month = as.integer(n_active_bills >= 2),
    across(starts_with("any_active_"), ~ coalesce(., 0L)),
    across(starts_with("bill_deficit_billion_"), ~ coalesce(., 0)),
    across(starts_with("bill_partisanship_"), ~ if_else(is_leg_period == 0L, NA_real_, .))
  ) %>%
  mutate(
    leg_period       = is_leg_period == 1L,
    partisan_period  = any_partisan_period == 1L,
    passage_month    = has_passage_month == 1L,
    pre_phase_only   = has_pre_phase == 1L & has_post_phase == 0L & !passage_month,
    post_phase_only  = has_post_phase == 1L & has_pre_phase == 0L & !passage_month,
    rep              = as.integer(party == "Republican"),
    rep_x_partisan   = rep * any_partisan_period,
    rep_x_legperiod  = rep * is_leg_period
  )

# ---- 7) appendix calendar (optional) ----
bill_calendar <- bill_month_map %>%
  group_by(month) %>%
  summarise(n_bills = n(), bills = paste(sort(unique(bill_short)), collapse = " | "), .groups = "drop") %>%
  arrange(month)

# Save Data
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)

# Bundles (as you planned)
save(all_tweets_final_index, tweets_monthly, tweets_with_leg_flags,
     file = "data/processed/05_full_tweets_with_indices.RData",
     compress = "xz", compression_level = 9)

save(tweets_x_bill_long,
     file = "data/processed/05_long_tweets_by_bill.RData",
     compress = "xz", compression_level = 9)

# Legislation bundle (update)
existing <- load("data/processed/03_power_policy_historical.RData")
to_add <- c("legislation_timeline","power_status","bill_month_map","bills_per_month",
            "bill_calendar","overlap_table","overlap_targets",
            "pre_buffer_months","post_buffer_months")
save(list = unique(c(existing, to_add)),
     file = "data/processed/03_power_policy_historical.RData",
     compress = "xz", compression_level = 9)

# RDS singles (recommended in parallel)
saveRDS(tweets_with_leg_flags, "data/processed/tweets_with_leg_flags.rds")
saveRDS(bill_month_map,       "data/processed/bill_month_map.rds")
saveRDS(bill_calendar,        "data/processed/bill_calendar.rds")

# Tiny metadata
saveRDS(list(created=Sys.time(), pre_buf=pre_buffer_months, post_buf=post_buffer_months,
             n_tweets=nrow(all_tweets_final_index),
             n_leg_tweets=sum(tweets_with_leg_flags$is_leg_period==1L)),
        "data/processed/05_full_tweets_with_indices_meta.rds")



