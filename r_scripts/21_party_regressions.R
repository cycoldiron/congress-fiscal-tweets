# --- Packages ---
library(dplyr)
library(brglm2)
library(broom)
library(gt)

# --- Data: keep only feasible party × power combos (2017–2023) ---
model_df <- all_tweets_final_index %>%
  filter(!(party == "Democratic" & power_score == 2),
         !(party == "Republican" & power_score == 1)) %>%
  mutate(
    party       = factor(party, levels = c("Democratic", "Republican")),
    power_score = factor(power_score, levels = c(0, 1, 3))
  ) %>%
  droplevels()

# --- Model: Firth-style bias-reduced logit with interaction ---
m2 <- glm(
  is_debt ~ party * power_score,
  data   = model_df,
  family = binomial(),
  method = brglm2::brglmFit,
  type   = "AS_median"
)

# --- Tidy & format: stars appended to OR; keep CI; drop Intercept & 'Rep × Power 1' ---
tab_m2 <- tidy(m2, conf.int = TRUE, exponentiate = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term = dplyr::recode(term,
                         `partyRepublican`              = "Republican (vs. Democratic)",
                         `power_score1`                 = "Power: 1 chamber (vs. 0)",
                         `power_score3`                 = "Power: 3 chambers (vs. 0)",
                         `partyRepublican:power_score1` = "Republican × Power 1",
                         `partyRepublican:power_score3` = "Republican × Power 3",
                         .default = term
    ),
    stars = dplyr::case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ ""
    ),
    `Odds Ratio` = paste0(round(estimate, 3), " ", stars),
    `95% CI`     = paste0(round(conf.low, 3), " – ", round(conf.high, 3))
  ) %>%
  # remove the unestimable/unused interaction row explicitly
  filter(term != "Republican × Power 1") %>%
  select(term, `Odds Ratio`, `95% CI`)

# --- GT table: compact layout + bullet notes ---
gt_m2 <- gt(tab_m2) %>%
  tab_header(
    title    = md("**Deficit Tweeting by Party and Institutional Power**"),
    subtitle = md("Odds ratios from bias-reduced logistic regression (2017–2023)")
  ) %>%
  cols_label(
    term         = md("**Variable**"),
    `Odds Ratio` = md("**Odds Ratio**"),
    `95% CI`     = md("**95% CI**")
  ) %>%
  tab_options(table.font.size = px(12)) %>%
  cols_align(
    align = "right",
    columns = vars(`Odds Ratio`, `95% CI`)
  ) %>%
  cols_width(
    term ~ px(270),         # keep variable col tight
    `Odds Ratio` ~ px(140), # widen odds ratio col to push CI right
    `95% CI` ~ px(220)      # keep CI readable but spaced
  ) %>%
  tab_source_note(
    md("**Notes:**<br>• *Power alignment* = count of institutions held by the member’s party (presidency, Senate, House).<br>• **Main effects** compare Democrats at each power level to Democrats at 0.<br>• **Interactions** (e.g., *Republican × Power 3*) show the additional effect for Republicans at that power level.<br>• Some terms are omitted because those party × power scenarios never occurred during 2017–2023 (Democrats never had 2; Republicans never had 1).<br>*** p < 0.001, ** p < 0.01, * p < 0.05.")
  )



# --- Save PNG ---
out_dir <- "/Users/cycoldiron/Desktop/congress-fiscal-tweets/results"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

gtsave(
  data     = gt_m2,
  filename = file.path(out_dir, "r2_party_power.png"),
  vwidth   = 1500,
  vheight  = 560,
  zoom     = 2
)

# --- Predicted odds ratios table (PNG) ---------------------------------------
library(dplyr)
library(gt)

# ---------- Build contrasts (drop NA coefficients safely) ----------
# Feasible combos in 2017–2023
pred_df <- tibble::tribble(
  ~party,        ~power_score,
  "Democratic",  0,
  "Democratic",  1,
  "Democratic",  3,
  "Republican",  0,
  "Republican",  3
) %>%
  mutate(
    party       = factor(party, levels = c("Democratic", "Republican")),
    power_score = factor(power_score, levels = c(0, 1, 3))
  )

# Model matrix for these combos
X  <- model.matrix(~ party * power_score, data = pred_df)

# Coefs / vcov from your fitted model
beta_all <- coef(m2)
V_all    <- vcov(m2)

# Keep only estimable (non-NA) coefficients and align matrices
keep        <- !is.na(beta_all)
beta        <- beta_all[keep]
V           <- V_all[keep, keep, drop = FALSE]
X           <- X[, names(beta), drop = FALSE]

# Baseline = first row (Dem, Power 0) given pred_df ordering
x0 <- X[1, , drop = FALSE]
Xd <- X - matrix(rep(x0, nrow(X)), nrow = nrow(X), byrow = TRUE)

# Log-odds contrasts, SEs, p-values, then ORs & CIs
logOR <- as.vector(Xd %*% beta)
se    <- sqrt(rowSums((Xd %*% V) * Xd))  # diag(Xd V Xd^T)
z     <- logOR / se
pvals <- 2 * pnorm(abs(z), lower.tail = FALSE)

OR    <- exp(logOR)
lo    <- exp(logOR - 1.96 * se)
hi    <- exp(logOR + 1.96 * se)

stars <- case_when(
  pvals < 0.001 ~ "***",
  pvals < 0.01  ~ "**",
  pvals < 0.05  ~ "*",
  TRUE ~ ""
)

pred_out <- pred_df %>%
  mutate(
    Variable    = paste0(party, " — Power ", as.character(power_score)),
    `Odds Ratio`= paste0(round(OR, 3), " ", stars),
    `95% CI`    = paste0(round(lo, 3), " – ", round(hi, 3))
  ) %>%
  select(Variable, `Odds Ratio`, `95% CI`)

# ---------- Pretty GT table + save ----------
pred_gt <- gt(pred_out) %>%
  tab_header(
    title    = md("**Predicted Odds Ratios: Party × Institutional Power**"),
    subtitle = md("Relative to **Democrats — Power 0** (2017–2023)")
  ) %>%
  cols_label(
    Variable    = md("**Variable**"),
    `Odds Ratio`= md("**Odds Ratio**"),
    `95% CI`    = md("**95% CI**")
  ) %>%
  tab_options(table.font.size = px(12)) %>%
  cols_align(align = "right", columns = c(`Odds Ratio`, `95% CI`)) %>%
  cols_width(
    Variable ~ px(320),
    `Odds Ratio` ~ px(140),
    `95% CI` ~ px(230)
  ) %>%
  tab_source_note(
    md("**Notes:**<br>• Baseline = Democrats with 0 aligned institutions (presidency, Senate, House).<br>• ORs and CIs computed from linear contrasts of the fitted model (log-odds), then exponentiated.<br>• Stars: *** p < 0.001, ** p < 0.01, * p < 0.05.<br>• Feasible levels in 2017–2023: Dem = {0,1,3}; Rep = {0,3}.")
  )

out_dir <- "/Users/cycoldiron/Desktop/congress-fiscal-tweets/results"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

gtsave(
  data     = pred_gt,
  filename = file.path(out_dir, "r2_predicted_party_power.png"),
  vwidth   = 1500,
  vheight  = 560,
  zoom     = 2
)
