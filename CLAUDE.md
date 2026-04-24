# CLAUDE.md — Congress Fiscal Tweets

## Project Overview

**"Twitter and the Politics of the Federal Deficit"**
Author: Cy Coldiron · UC Santa Barbara, Department of Economics

This paper analyzes how U.S. congressional members tweet about the **federal deficit** from June 2017 to January 2023 (~3.6M tweets, 1,000+ accounts). The core finding: deficit rhetoric correlates with **political opportunity, not fiscal conditions** — Republicans tweet about the deficit five times more often when out of power; Democrats remain comparatively stable across majority/minority status.

The analytical pipeline goes: raw tweet JSON → Python classification → R data builds → logistic regression models → figures/tables → Quarto-rendered PDF thesis.

---

## Research Questions

1. Does deficit tweeting vary with **party power status** (presidency, chamber control, trifectas)?
2. Do **legislative windows** amplify partisan tweeting gaps?
3. Does **bill partisanship** (vote-share divergence) or **fiscal scale** (CBO estimates) drive these effects?
4. How do **party leaders** (Majority/Minority) compare to rank-and-file members?

---

## Key Findings

- Republicans tweet about the deficit ~5× more when out of power (minority under opposing president).
- Legislative windows further amplify the partisan gap, but only when the bill is highly partisan.
- **CBO fiscal scale has no significant effect** on tweeting rates — it's about politics, not fiscal reality.
- Senate Minority Leader McConnell shows the sharpest reversal (1.9% minority → 0.05% majority); Schumer goes the opposite direction (1.0% minority → 4.0% majority).
- The GOP shows cohesive, synchronized party-wide shifts in messaging; Democrats show more individual variation.

---

## Data

| Source | Description |
|---|---|
| [Tweets of Congress](https://github.com/alexlitel/congresstweets) (Alex Litel) | Raw tweet JSON, Jun 2017–Jan 2023 |
| Congressional Tweet Automator | Member metadata (party, chamber, leadership roles) |
| U.S. Treasury / Federal Reserve | 10-yr interest rates, CPI inflation, debt levels |
| Congressional Budget Office (CBO) | Fiscal impact estimates per bill |
| Congressional metadata (manual) | Majority/minority leadership flags, bill partisanship scores |

Coverage: **2017-06-21 → 2023-01-31** · ~3.6M tweets after removing committee/institutional accounts.

---

## Classification Method

Two-tier keyword strategy implemented in Python (`python_scripts/parse_fiscal_tweets.py`):

- **Tier 1 (anchor terms):** Unambiguous phrases like *"national debt," "balanced budget," "fiscal responsibility"* — flagged directly.
- **Tier 2 (contextual expansion):** Weak terms like *"debt"* flagged only when co-occurring with context terms like *"federal," "budget," "tax," "spending."*

Result: ~0.7% of all congressional tweets are classified as deficit-related.

---

## Modeling Approach

**Unit of analysis:** Member–month (each member × each calendar month).

**Dependent variable:** Binary — did the member tweet about the deficit in a given tweet? (share of tweets that are deficit-related).

**Model:** Logistic regression with:
- Member fixed effects (absorb baseline differences across legislators and parties)
- Month fixed effects (absorb common shocks like COVID-19)
- Two-way clustered standard errors (by member and month)

**Sequential model specifications** (Regressions 1–3) progressively add controls for presidential party and congressional composition.

Coefficients presented as **odds ratios** relative to: *Democrats outside legislative windows, under each model's institutional baseline.*

---

## Directory Structure

### `r_scripts/`
30+ R scripts numbered by pipeline stage:

| Range | Purpose |
|---|---|
| `00–03a` | Setup, package loading, paths (`03a_setup.R` is the entry point) |
| `04–09` | Data cleaning, merges, federal power variables, economic data, full tweet panel |
| `10–12` | Party/power deficit behavior, regression specifications, summary stats |
| `13–14b` | Legislative windows pipeline — bill flags (`13b`), member–month join (`14b`) |
| `15–17` | Member deficit behavior analysis, economic correlations |
| `18–19` | Leadership analysis, party time trends |
| `20–22c` | Regression models (`22b_legislative_models.R`) and publication tables (`22c_legislation_tables.R`) |
| `23–32` | Extended analyses: inflation data merges, in/out power figures, debt growth figures, TCJA adjustment, updated time-series plots, summary regression |
| `utils_gt.R` | Shared `gt` table formatting utilities |

### `python_scripts/`
- `parse_fiscal_tweets.py` — Two-tier deficit tweet classifier applied to ~3.6M tweets.
- `coding_example.qmd` / `.quarto_ipynb` — Classification walkthrough/demo.

### `data/`
| Subfolder | Contents |
|---|---|
| `raw/congress_115_116_117/` | Raw tweet data by congressional session |
| `raw/congress_aggregated/` | Aggregated raw tweet data |
| `processed/` | Cleaned `.RData`/`.rds` files at each pipeline stage (01–06, bill maps, monthly panels with legislative flags) |
| `external/` | Economic indicator CSVs/XLSXs: congressional approval ratings, deficit time series, 10-yr interest rates, CPI/inflation |

### `models/`
Saved fitted model objects (`.rds`):
- `m_mm_main.rds`, `m_mm_combo.rds`, `m_mm_trifecta_sym.rds` — Main member–month logistic models
- `m_mm_legXpartisan.rds`, `m_mm_legXdefz.rds` — Mechanism models (partisanship × legislative window; CBO scale)
- `m_bin_combo.rds`, `m_bin_trifecta_sym.rds` — Binary outcome variants
- Others: power, phase, minority-president, base specifications

### `figures/`
| Subfolder | Contents |
|---|---|
| `summary/` | Paper-ready figures: time-series of deficit tweet share by party, legislation overlays |
| `economic_indicators/` | CPI vs. deficit share, interest rates, debt overlays, tweet-volume plots |
| `leadership/` | Majority/Minority leader deficit tweeting comparisons |
| `individuals/` | Member-level density plots, in/out power boxplots, in-minus-out shift distributions |

### `results/`
| Subfolder | Contents |
|---|---|
| `good_regression_tables/` | Publication-ready regression tables (r3–r14) |
| `good_summary_tables/` | Descriptive summary tables (tweet-level deficit share by party/bill) |
| `extra_regression_tables/` | Additional/exploratory regression outputs |
| `regression specifications/` | Specification notes |
| `images/` | Supplemental result images |

### `section_drafts/`
Thesis manuscript organized by section:
| File/Folder | Contents |
|---|---|
| `01_all_sections/01_coldiron_full_thesis.qmd` | **Main manuscript** — current full thesis source |
| `01_all_sections/01_coldiron_full_thesis.pdf` | Compiled PDF output |
| `01_intro.qmd`, `02_lit_review.qmd`, etc. | Individual section drafts |
| `00_all_sections/` | Older combined drafts |
| `10_old/` | Archived older drafts |

### Other
- `screenshots/` — Tweet screenshot images used as figures in the paper (AOC, McCarthy examples).
- `images/` — Supporting images (e.g., partisan score formula).
- `congress-fiscal-tweets.Rproj` — RStudio project file.

---

## Reproduction

```bash
# 1. Load packages and paths
Rscript r_scripts/03a_setup.R

# 2. Build legislative windows and member-month panel
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

---

## Notes for Claude

- The thesis `.qmd` uses absolute paths for `knitr::include_graphics()` pointing to `/Users/cycoldiron/Desktop/congress-fiscal-tweets/figures/...` — keep these paths consistent when adding or renaming figures.
- Regression tables use the `gt` package with shared utilities in `utils_gt.R`.
- Always stage all modified files when committing, including figure outputs that change as side effects of running scripts.
