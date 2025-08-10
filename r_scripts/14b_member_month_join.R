source("r_scripts/03a_setup.R")

# inputs
tweets_with_leg_flags <- readRDS("data/processed/tweets_with_leg_flags.rds")
load("data/processed/05_full_tweets_with_indices.RData")  # gives tweets_monthly

# build member_month_flags + phase_flags (your working code)
member_month_flags <- tweets_with_leg_flags %>%
  group_by(full_name, month) %>%
  summarise(
    leg_period = as.integer(any(is_leg_period == 1L)),
    multi_bill_month = as.integer(any(multi_bill_month == 1L)),
    any_partisan_period = as.integer(any(any_partisan_period == 1L)),
    mean_partisanship_score = mean(mean_partisanship_score, na.rm = TRUE),
    sum_deficit_bil = sum(sum_est_10yr_deficit_billion, na.rm = TRUE),
    any_IRA = as.integer(any(`any_active_IRA` == 1L, na.rm = TRUE)),
    any_CHIPS = as.integer(any(`any_active_CHIPS` == 1L, na.rm = TRUE)),
    any_CARES = as.integer(any(`any_active_CARES` == 1L, na.rm = TRUE)),
    any_PPP_HCE = as.integer(any(`any_active_PPP_HCE` == 1L, na.rm = TRUE)),
    .groups = "drop"
  )

phase_flags <- tweets_with_leg_flags %>%
  group_by(full_name, month) %>%
  summarise(
    pre_phase_only_mm  = as.integer(any(pre_phase_only  == TRUE)),
    post_phase_only_mm = as.integer(any(post_phase_only == TRUE)),
    passage_month_mm   = as.integer(any(passage_month   == TRUE)),
    .groups = "drop"
  )

tweets_monthly_with_leg <- tweets_monthly %>%
  left_join(member_month_flags, by = c("full_name","month")) %>%
  left_join(phase_flags,        by = c("full_name","month")) %>%
  mutate(
    across(c(leg_period, multi_bill_month, any_partisan_period,
             any_IRA, any_CHIPS, any_CARES, any_PPP_HCE,
             pre_phase_only_mm, post_phase_only_mm, passage_month_mm),
           ~ coalesce(., 0L)),
    mean_partisanship_score = if_else(leg_period == 1L, mean_partisanship_score, NA_real_),
    sum_deficit_bil = coalesce(sum_deficit_bil, 0),
    rep = as.integer(party_clean == "Republican"),
    same_pres  = as.integer(party_clean == president_party),
    same_sen   = as.integer(party_clean == senate_control),
    same_house = as.integer(party_clean == house_control),
    ps_tmp = same_pres + same_sen + same_house,
    power1 = as.integer(ps_tmp == 1L),
    power3 = as.integer(ps_tmp == 3L)
  ) %>% select(-ps_tmp, -same_pres, -same_sen, -same_house)

# ... your joins/mutate for tweets_monthly_with_leg ...

tweets_monthly_with_leg <- tweets_monthly_with_leg %>%
  mutate(
    covid_window = as.integer(any_CARES == 1L | any_PPP_HCE == 1L)
  )

saveRDS(tweets_monthly_with_leg, "data/processed/tweets_monthly_with_leg.rds")
