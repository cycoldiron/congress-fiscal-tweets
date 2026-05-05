# Twitter and the Politics of the Federal Deficit

**Cy Coldiron** · UC Santa Barbara, Department of Economics · `coldiron@ucsb.edu`

[LinkedIn](https://www.linkedin.com/in/cycoldiron/)

---

## Overview

This paper provides the first large-scale empirical analysis of how U.S. congressional members communicate about the **federal deficit** on Twitter. Using ~3.6 million tweets from 1,000+ congressional accounts between June 2017 and January 2023, I classify deficit-related tweets with a two-tier keyword method and estimate how tweeting rates vary across institutional, legislative, and fiscal conditions using logistic regression.

The core finding: **deficit rhetoric tracks political power, not fiscal reality.** Republicans tweet about the deficit five times more often when out of power (minority under an opposing president), while Democrats remain comparatively stable regardless of their power status. The fiscal magnitude of legislation — measured by its 10-year CBO score — has no measurable effect on tweeting rates.

---

## Key Findings

- **Republicans** increase deficit-related tweeting ~5× when moving from in-power to out-of-power contexts; **Democrats** show no comparable shift.
- **Legislative windows** amplify the partisan gap, but only after conditioning on who controls the presidency and Congress — and only when the bill is highly partisan. The fiscal scale of the bill is insignificant.
- **CBO fiscal magnitude** has no statistically significant effect on tweeting rates across any model specification.
- **Party leaders** mirror their caucuses: McConnell tweeted about the deficit at 1.9% (minority) vs. 0.05% (majority) — a 38× reversal. Schumer went the opposite direction (1.0% minority → 4.0% majority). Pelosi tweeted more in the minority (1.1%) than as Speaker (0.5%).
- Republicans exhibit **greater intra-party coordination** in messaging than Democrats; the GOP demonstrates cohesive, synchronized shifts while Democratic messaging is more heterogeneous.
- Deficit tweeting correlates positively with CPI inflation (r = .78), though this likely reflects co-occurring legislative activity in 2022 rather than genuine fiscal concern.

---

## Methodology

**Classification:** A two-tier Python keyword strategy applied to ~3.6M tweets:
- *Tier 1 (anchor terms):* Unambiguous phrases (e.g., *"national debt," "balanced budget," "fiscal responsibility"*) → flagged directly.
- *Tier 2 (contextual expansion):* Weak terms (e.g., *"debt"*) → flagged only when co-occurring with fiscal context terms (e.g., *"federal," "budget," "tax," "spending"*).
- Result: ~0.7% of all congressional tweets are classified as deficit-related.

**Model:** Logistic regression on a member–month panel:

$$\log\left(\frac{p_{it}}{1 - p_{it}}\right) = \alpha_i + \gamma_t + \beta X_{it}$$

- Member fixed effects ($\alpha_i$) absorb baseline partisan and individual differences.
- Month fixed effects ($\gamma_t$) absorb common shocks (e.g., COVID-19).
- Two-way clustered standard errors (by member and month).
- Coefficients reported as **odds ratios** relative to: Democrats outside legislative windows.

**Sequential specifications** (Regressions 1–3) progressively condition on presidential party, chamber control, and trifecta status.

**Partisanship score** (novel measure): absolute difference between Democratic and Republican vote-share support for each bill, normalized for regression use.

---

## Data Sources

| Source | Description |
|---|---|
| [Tweets of Congress](https://github.com/alexlitel/congresstweets) (Alex Litel) | Raw tweet JSON — Jun 2017–Jan 2023; ~3.6M tweets; 1,000+ accounts |
| [Congressional Tweet Automator](https://github.com/alexlitel/congressional-tweet-automator) | Member metadata (party, chamber, leadership roles) |
| U.S. Treasury / Federal Reserve | 10-yr interest rates, CPI inflation, federal debt levels |
| Congressional Budget Office (CBO) | 10-year fiscal impact estimates per bill |
| Congressional metadata (manual) | Majority/minority leadership flags, partisanship scores |

**Coverage:** 2017-06-21 → 2023-01-31 across 115th, 116th, and 117th Congresses. Committee and institutional accounts (e.g., `@HouseDemocrats`, `@SenateGOP`) are excluded — only accounts tied to individual, serving members are retained.

---

## Repository Structure

```
congress-fiscal-tweets/
│
├── python_scripts/
│   ├── parse_fiscal_tweets.py          ← Two-tier deficit tweet classifier (~3.6M tweets)
│   └── coding_example.qmd              ← Classification walkthrough/demo
│
├── r_scripts/                          ← 30+ scripts, numbered by pipeline stage
│   ├── 00–03a  Setup, packages, paths (entry point: 03a_setup.R)
│   ├── 04–09   Data cleaning, merges, power variables, economic data, tweet panel
│   ├── 10–12   Party/power deficit behavior, regression specs, summary stats
│   ├── 13–14b  Legislative windows pipeline (bill flags → member–month join)
│   ├── 15–17   Member deficit behavior, economic correlations
│   ├── 18–19   Leadership analysis, party time trends
│   ├── 20–22c  Regression models (22b) and publication tables (22c)
│   ├── 23–32   Extended analyses: inflation merges, in/out power figures,
│   │           debt growth, TCJA adjustment, updated time-series plots
│   └── utils_gt.R                      ← Shared gt table formatting utilities
│
├── data/
│   ├── raw/congress_115_116_117/       ← Raw tweet JSON by congressional session
│   ├── raw/congress_aggregated/        ← Aggregated raw tweet data
│   ├── processed/                      ← Cleaned .RData/.rds files (pipeline stages 01–06,
│   │                                      bill maps, monthly panels with legislative flags)
│   └── external/                       ← Economic CSVs/XLSXs: approval ratings,
│                                          deficit series, interest rates, CPI/inflation
│
├── models/                             ← Saved fitted model objects (.rds)
│   ├── m_mm_main.rds                   ← Main member–month logistic model
│   ├── m_mm_combo.rds                  ← Control-combination model
│   ├── m_mm_trifecta_sym.rds           ← Trifecta specification
│   ├── m_mm_legXpartisan.rds           ← Mechanism: partisanship × legislative window
│   ├── m_mm_legXdefz.rds               ← Mechanism: CBO fiscal scale
│   └── m_bin_*.rds                     ← Binary outcome variants
│
├── figures/
│   ├── summary/                        ← Paper-ready figures: deficit tweet share by party,
│   │                                      time-series with legislative overlays
│   ├── economic_indicators/            ← CPI vs. deficit share, interest rates,
│   │                                      debt overlays, tweet-volume plots
│   ├── leadership/                     ← Majority/Minority leader deficit tweeting comparisons
│   └── individuals/                    ← Member-level density plots, in/out power boxplots,
│                                          in-minus-out shift distributions
│
├── results/
│   ├── good_regression_tables/         ← Publication-ready regression tables (r3–r14)
│   ├── good_summary_tables/            ← Descriptive tables: tweet-level deficit share
│   │                                      by party and bill
│   └── extra_regression_tables/        ← Additional/exploratory regression outputs
│
├── section_drafts/
│   └── 01_all_sections/
│       ├── 01_coldiron_full_thesis.qmd ← Main manuscript (Quarto source)
│       └── 01_coldiron_full_thesis.pdf ← Compiled PDF output
│
├── screenshots/                        ← Tweet screenshot figures (AOC, McCarthy examples)
├── images/                             ← Supporting images (partisan score formula, etc.)
├── congress-fiscal-tweets.Rproj        ← RStudio project file
└── CLAUDE.md                           ← Project notes for AI-assisted development
```

---

## Reproduction

```bash
# 1. Load packages and paths
Rscript r_scripts/03a_setup.R

# 2. Build legislative windows and member–month panel
Rscript r_scripts/13b_legislative_windows_build.R
Rscript r_scripts/14b_member_month_join.R

# 3. Fit regression models
Rscript r_scripts/22b_legislative_models.R

# 4. Export tables and figures
Rscript r_scripts/22c_legislation_tables.R
```

To recompile the thesis PDF:
```bash
quarto render section_drafts/01_all_sections/01_coldiron_full_thesis.qmd
```

> **Note:** `knitr::include_graphics()` calls in the thesis use absolute paths pointing to `/Users/cycoldiron/Desktop/congress-fiscal-tweets/figures/...`. Update these if running on a different machine.

---

## Citation

If reusing the data or code, please cite this repository and the original data sources:

- **Tweets of Congress:** https://github.com/alexlitel/congresstweets
- **Congressional Tweet Automator:** https://github.com/alexlitel/congressional-tweet-automator

---

## License

MIT License.
