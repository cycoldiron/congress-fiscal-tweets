library(dplyr)
library(broom)
library(gt)

# Run the same basic logistic regression
m1 <- glm(is_debt ~ party,
          data   = all_tweets_final_index,
          family = binomial())

# Tidy results & format
m1_tab <- tidy(m1, conf.int = TRUE, exponentiate = TRUE) %>%
  filter(term != "(Intercept)") %>%  # remove intercept
  mutate(term = recode(term,
                       `partyRepublican` = "Republican (vs. Democratic)"),
         `95% CI` = paste0(round(conf.low, 3), " – ", round(conf.high, 3)),
         estimate = round(estimate, 3),
         p.value = ifelse(p.value < 0.001, "< 0.001", round(p.value, 3)),
         sig = case_when(
           p.value == "< 0.001" ~ "***",
           as.numeric(p.value) < 0.01  ~ "**",
           as.numeric(p.value) < 0.05  ~ "*",
           TRUE ~ ""
         )) %>%
  select(term, estimate, `95% CI`, p.value, sig)

# Build GT table
# Reorder columns so CI is after p-value
m1_tab <- tidy(m1, conf.int = TRUE, exponentiate = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(term = recode(term,
                       `partyRepublican` = "Republican (vs. Democratic)"),
         `95% CI` = paste0(round(conf.low, 3), " – ", round(conf.high, 3)),
         estimate = round(estimate, 3),
         p.value = ifelse(p.value < 0.001, "< 0.001", round(p.value, 3)),
         sig = case_when(
           p.value == "< 0.001" ~ "***",
           as.numeric(p.value) < 0.01  ~ "**",
           as.numeric(p.value) < 0.05  ~ "*",
           TRUE ~ ""
         )) %>%
  select(term, estimate, p.value, `95% CI`, sig)  # <- reorder here

# Build GT table with adjusted column widths
gt_m1 <- gt(m1_tab) %>%
  tab_header(
    title    = md("**Logistic Regression: Probability a Tweet Mentions the Deficit**"),
    subtitle = "Bivariate model testing the effect of party affiliation"
  ) %>%
  cols_label(
    term     = md("**Variable**"),
    estimate = md("**Odds Ratio**"),
    `95% CI` = md("**95% CI**"),
    p.value  = md("**p-value**"),
    sig      = md("**Signif.**")
  ) %>%
  tab_options(table.font.size = px(13)) %>%
  cols_width(
    term ~ px(220),
    estimate ~ px(130),
    p.value ~ px(130),
    `95% CI` ~ px(200),  # now after p-value
    sig ~ px(90)
  ) %>%
  tab_source_note(
    md("*Notes:* Odds ratios reported. Significance codes: *** p < 0.001, ** p < 0.01, * p < 0.05.")
  )

# Save PNG
gtsave(
  data     = gt_m1,
  filename = file.path(out_dir, "r1_party_only.png"),
  vwidth   = 1600,
  vheight  = 500,
  zoom     = 2
)
