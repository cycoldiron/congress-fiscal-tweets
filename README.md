# Congress Fiscal Tweets

This project analyzes how members of the U.S. Congress talk about the **federal deficit** on Twitter (Jun 2017–Jan 2023).
It tracks how rhetoric shifts with **party control**, **major legislation**, **macroeconomic conditions**, and
**leadership status** (e.g., Majority/Minority Leaders).

---

## Overview

- Party differences in deficit tweeting and how gaps evolve during **legislative windows** (e.g., TCJA, IRA).
- How gaps change with **relative power** (presidency/chambers; trifectas; control combos).
- Whether **bill partisanship** and **fiscal scale** matter more **inside** legislative windows.
- How **party leaders** (Majority/Minority leaders) compare to the rest of their caucus over time.

---

##  What’s in this repo

**R scripts (`/r_scripts`)** – 20+ scripts grouped as:
- **00–12: data build & joins** (cleaning tweets, merging congressional & macro data, monthly panels).
- **13–15: legislative windows pipeline** (constructs bill/window flags and member–month dataset).
- **18–21: descriptive analyses** (leadership, time trends, party summaries).
- **22b & 22c: modeling & tables** (governance-context models and publication tables).

**Python scripts (`/python_scripts`)** – pre-processing and deficit classifiers for ~3.5M tweets.

**Figures & results**:
- `results/` – regression tables **r3–r14** (e.g., majority context, mechanisms, scenario matrices).
- `figures/summary/` – paper-ready figures;
- `figures/economic_indicators/` – CPI, rates, CBO, and tweet-volume overlays;
- `figures/leadership/` – Majority/Minority leader comparisons;
- `figures/individuals/` – member-level profiles.

---

## Key analyses (high level)

- **Majority context**: GOP–Dem gaps vary sharply with who governs. Trifecta & control-combo models
  show opposite patterns in legislative vs. non-legislative months.
- **Mechanisms**: Bill **partisanship** amplifies the GOP–Dem gap during legislative windows; **scale** is weaker.
- **Leadership**: Leadership accounts show distinct, more strategic timing relative to rank-and-file.

---

## Data

- Tweets from **Tweets of Congress** by Alex Litel; congressional metadata from the
  Congressional Tweet Automator. See links below.
- Time coverage: **2017-06-21 → 2023-01-31**; ~**3.5M** tweets; 1k+ accounts.

---

## Repro (abridged)

1. Run `r_scripts/03a_setup.R` to load packages/paths.
2. Build legislative windows & panel: `13b_legislative_windows_build.R`, then `14b_member_month_join.R`.
3. Fit models: `22b_legislative_models.R`.
4. Export tables/figures: `22c_legislation_tables.R`.

---

##  License & Attribution

- MIT License.
- Data & metadata from:
  - **Tweets of Congress**: https://github.com/alexlitel/congresstweets
  - **Congressional Tweet Automator**: https://github.com/alexlitel/congressional-tweet-automator
  
Please cite this repo and the originals when reusing the data or code.

---

## 👤 Author

**Cy Coldiron** · UC Santa Barbara — Statistics & Data Science / Economics

[LinkedIn](https://www.linkedin.com/in/cycoldiron/) · [Email](mailto:coldiron@ucsb.edu)
