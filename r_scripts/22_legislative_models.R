source("r_scripts/03a_setup.R")

df_mm <- readRDS("data/processed/tweets_monthly_with_leg.rds") %>%
  mutate(
    prop_def = n_debt_tweets / pmax(n_tweets, 1),
    mp_z  = ifelse(is.na(mean_partisanship_score), 0, as.numeric(scale(mean_partisanship_score))),
    def_z = ifelse(is.na(sum_deficit_bil),        0, as.numeric(scale(sum_deficit_bil)))
  ) %>% filter(n_tweets > 0)

m_mm_base <- feglm(
  prop_def ~ rep + power1 + power3 +
    leg_period + any_partisan_period + mp_z:leg_period + def_z:leg_period +
    deficit_reducing +
    rep:leg_period + rep:any_partisan_period +
    rep:mp_z:leg_period + rep:def_z:leg_period + rep:deficit_reducing
  | full_name + month,
  family = binomial("logit"), data = df_mm,
  weights = ~ n_tweets, vcov = ~ full_name
)

m_mm_phase <- feglm(
  prop_def ~ rep + power1 + power3 +
    pre_phase_only_mm + post_phase_only_mm + passage_month_mm +
    any_partisan_period + mp_z + def_z + deficit_reducing +
    rep:pre_phase_only_mm + rep:post_phase_only_mm + rep:passage_month_mm +
    rep:any_partisan_period + rep:mp_z + rep:def_z + rep:deficit_reducing
  | full_name + month,
  family = binomial("logit"), data = df_mm,
  weights = ~ n_tweets, vcov = ~ full_name
)

saveRDS(m_mm_base,  "models/m_mm_base.rds")
saveRDS(m_mm_phase, "models/m_mm_phase.rds")
