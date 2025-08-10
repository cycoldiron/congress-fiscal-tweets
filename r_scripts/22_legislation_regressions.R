library(dplyr)
library(lubridate)

# From tweet-level -> member-month flags
member_month_flags <- tweets_with_leg_flags %>%
  group_by(full_name, month) %>%
  summarise(
    leg_period              = as.integer(any(is_leg_period == 1L)),
    multi_bill_month        = as.integer(any(multi_bill_month == 1L)),
    any_partisan_period     = as.integer(any(any_partisan_period == 1L)),
    mean_partisanship_score = mean(mean_partisanship_score, na.rm = TRUE),
    sum_deficit_bil         = sum(sum_est_10yr_deficit_billion, na.rm = TRUE),
    any_IRA                 = as.integer(any(`any_active_IRA` == 1L, na.rm = TRUE)),
    any_CHIPS               = as.integer(any(`any_active_CHIPS` == 1L, na.rm = TRUE)),
    any_CARES               = as.integer(any(`any_active_CARES` == 1L, na.rm = TRUE)),
    any_PPP_HCE             = as.integer(any(`any_active_PPP_HCE` == 1L, na.rm = TRUE)),
    .groups = "drop"
  )

# Join to your existing member-month table and add power alignment
library(dplyr)

tweets_monthly_with_leg <- tweets_monthly %>%
  left_join(member_month_flags, by = c("full_name","month")) %>%
  mutate(
    across(c(leg_period, multi_bill_month, any_partisan_period,
             any_IRA, any_CHIPS, any_CARES, any_PPP_HCE),
           ~ coalesce(., 0L)),
    mean_partisanship_score = if_else(leg_period == 1L, mean_partisanship_score, NA_real_),
    sum_deficit_bil = coalesce(sum_deficit_bil, 0),
    
    # party & power
    rep = as.integer(party_clean == "Republican"),
    same_pres  = as.integer(party_clean == president_party),
    same_sen   = as.integer(party_clean == senate_control),
    same_house = as.integer(party_clean == house_control),
    power_score_calc = same_pres + same_sen + same_house,
    power1 = as.integer(power_score_calc == 1L),
    power3 = as.integer(power_score_calc == 3L),
    deficit_reducing = as.integer(sum_deficit_bil > 0)
  ) %>%
  select(-power_score_calc, -same_pres, -same_sen, -same_house)


phase_flags <- tweets_with_leg_flags %>%
  group_by(full_name, month) %>%
  summarise(
    pre_phase_only_mm  = as.integer(any(pre_phase_only  == TRUE)),
    post_phase_only_mm = as.integer(any(post_phase_only == TRUE)),
    passage_month_mm   = as.integer(any(passage_month   == TRUE)),
    .groups = "drop"
  )

tweets_monthly_with_leg <- tweets_monthly_with_leg %>%
  left_join(phase_flags, by = c("full_name","month")) %>%
  mutate(
    across(c(pre_phase_only_mm, post_phase_only_mm, passage_month_mm), ~coalesce(., 0L))
  )

library(dplyr)
# use your member–month table with leg flags
df_mm <- tweets_monthly_with_leg %>%
  mutate(
    prop_def = n_debt_tweets / pmax(n_tweets, 1),
    mp_z  = ifelse(is.na(mean_partisanship_score), 0, as.numeric(scale(mean_partisanship_score))),
    def_z = ifelse(is.na(sum_deficit_bil),        0, as.numeric(scale(sum_deficit_bil)))
  ) %>%
  filter(n_tweets > 0)  # drop zero-activity months


library(fixest)

m_mm_base <- feglm(
  prop_def ~
    rep + power1 + power3 +
    leg_period +
    any_partisan_period + mp_z:leg_period + def_z:leg_period +
    deficit_reducing +
    rep:leg_period + rep:any_partisan_period +
    rep:mp_z:leg_period + rep:def_z:leg_period + rep:deficit_reducing
  | full_name + month,                           # fixed effects here
  family = binomial("logit"),
  data   = df_mm,
  weights = ~ n_tweets,                          # binomial denominator
  vcov   = ~ full_name                           # cluster by member
)


m_mm_phase <- feglm(
  prop_def ~
    rep + power1 + power3 +
    pre_phase_only_mm + post_phase_only_mm + passage_month_mm +
    any_partisan_period + mp_z + def_z + deficit_reducing +
    rep:pre_phase_only_mm + rep:post_phase_only_mm + rep:passage_month_mm +
    rep:any_partisan_period + rep:mp_z + rep:def_z + rep:deficit_reducing
  | full_name + month,
  family = binomial("logit"),
  data   = df_mm,
  weights = ~ n_tweets,
  vcov   = ~ full_name
)

m_mm_ira_chips <- feglm(
  prop_def ~
    rep + power1 + power3 +
    leg_period + any_partisan_period + mp_z:leg_period + def_z:leg_period +
    any_IRA + any_CHIPS +
    rep:leg_period + rep:any_partisan_period +
    rep:mp_z:leg_period + rep:def_z:leg_period +
    rep:any_IRA + rep:any_CHIPS
  | full_name + month,
  family = binomial("logit"),
  data   = df_mm,
  weights = ~ n_tweets,
  vcov   = ~ full_name
)

library(broom)
or_table <- function(m) broom::tidy(m, conf.int = TRUE) |>
  mutate(OR = exp(estimate), CI_low = exp(conf.low), CI_high = exp(conf.high)) |>
  select(term, OR, CI_low, CI_high, p.value)

or_table(m_mm_base) |> head()





