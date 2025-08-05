# Congress Fiscal Tweets

This project analyzes how U.S. members of Congress discuss the **federal deficit** on Twitter from June 2017 to January 2023.  
It explores how deficit-related rhetoric shifts with **partisan control**, **major legislation**, and **macroeconomic conditions**.

---

## 🔍 Overview

This research investigates:

- How fiscal rhetoric changes based on **party affiliation** and **power status** (majority/minority)
- Whether messaging aligns with shifts in the **deficit**, **CBO projections**, and **economic policy**
- Patterns in **tweet volume**, **language**, and **messaging strategies** around key legislative events (TCJA, IRA etc)

---

## ⚙️ Features

- 🐍 Python-based classification of ~3.6M congressional tweets for deficit-related content  
- 📊 R-based data analysis and visualization pipeline  
- 🏛️ Integration of congressional metadata (party, chamber, leadership, power status)  
- 📈 Merged with external economic data (CBO estimates, interest rates, CPI, approval data)

---

## 📁 Project Structure

```text
data/
├── raw/                # Raw JSON files (2017–2023 daily tweets)
├── processed/          # Cleaned and merged .RData files

scripts/
├── R scripts for cleaning, analysis, and plotting

python_scripts/
├── Python scripts for tweet labeling and preprocessing

figures/
├── Final visualizations and charts

results/
├── Summary tables, regression outputs, publication material
```


---

## 📚 Data Provenance

This project uses tweet data originally collected by the open-source [**Tweets of Congress**](https://github.com/alexlitel/congresstweets) repository by Alex Litel.

- The source repo includes one JSON file per day from June 2017 through early 2023
- Tweets were posted by official, campaign, party, and committee accounts
- Original data collected via the [Congressional Tweet Automator](https://github.com/alexlitel/congressional-tweet-automator)
- Congressional metadata (e.g., account type, office status, name) sourced from the Automator’s `historical-users-filtered.json`

**Note**: This project filters and modifies the original data to include only members of Congress and applies additional labels and transformations for research use.

---

## 📅 Data Coverage

- **Time period**: June 21, 2017 – January 31, 2023  
- **Accounts monitored**: 1,000+ congressional and affiliated Twitter accounts  
- **Total tweets**: ~3.5 million  
- **Granularity**: Daily JSON files  

---

## 🛠️ Data Pipeline Summary

1. Extract and clean JSON files from `/data/`
2. Filter to member accounts using metadata from the Automator repo
3. Classify tweets as deficit-related using keyword and regex matching (Python)
4. Merge tweets with congressional and macroeconomic metadata (R)
5. Aggregate and visualize results using `ggplot2` and `gt`

---

## ⚖️ License & Attribution

- This project is open-source and available under the MIT License.
- Tweet data and congressional metadata are sourced from:
  - [`congresstweets`](https://github.com/alexlitel/congresstweets) (MIT License)
  - [`congressional-tweet-automator`](https://github.com/alexlitel/congressional-tweet-automator)

Portions of this repository are derived from the above sources in compliance with their open-source licenses.  
Please cite both this project and the original repositories if reusing or extending the analysis.

---

## 👤 Author 

**Cy Coldiron**  
UC Santa Barbara — Statistics & Data Science / Economics  
[LinkedIn](https://www.linkedin.com/in/cycoldiron/) |[Email](coldiron@ucsb.edu)


