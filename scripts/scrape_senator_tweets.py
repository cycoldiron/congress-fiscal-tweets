import pandas as pd
import subprocess

# Load your final spreadsheet
df = pd.read_excel("data/external/senators_final.xlsx")

# Congress start and end dates
congress_dates = {
    116: ("2019-01-03", "2021-01-03"),
    117: ("2021-01-03", "2023-01-03"),
    118: ("2023-01-03", "2025-01-03"),
    119: ("2025-01-03", "2027-01-03"),
}

# Loop through each senator
for _, row in df.iterrows():
    handle = row["twitter_handle"]
    congress = row["congress_number"]
    name = row["full_name"].replace(" ", "_")

    print(row[['full_name', 'twitter_handle', 'congress_number']])

    if pd.isna(handle) or congress not in congress_dates:
        continue
    else:
        print(f"Scraping: {handle}, Congress: {congress}")


    since, until = congress_dates[congress]
    out_file = f"data/raw/{name}_C{congress}.json"

    query = f'from:{handle} since:{since} until:{until}'
    command = f'snscrape --jsonl twitter-search "{query}" > \"{out_file}\"'

    print(f"Scraping {handle} for Congress {congress}")
    subprocess.run(command, shell=True)
