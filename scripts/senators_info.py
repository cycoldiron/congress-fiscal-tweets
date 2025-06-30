import pandas as pd

# Correct URLs for each Congress
congress_urls = {
    116: "https://en.wikipedia.org/wiki/List_of_United_States_senators_in_the_116th_Congress",
    117: "https://en.wikipedia.org/wiki/List_of_United_States_senators_in_the_117th_Congress",
    118: "https://en.wikipedia.org/wiki/List_of_United_States_senators_in_the_118th_Congress",
    119: "https://en.wikipedia.org/wiki/List_of_United_States_senators_in_the_119th_Congress",
}

all_senators = []

for congress, url in congress_urls.items():
    print(f"📥 Scraping Congress {congress} from: {url}")
    tables = pd.read_html(url)

    found = False
    for table in tables:
        if "Senator" in table.columns and "Party" in table.columns and "State" in table.columns:
            df = table[["Senator", "Party", "State"]].copy()
            df = df.rename(columns={
                "Senator": "full_name",
                "Party": "party",
                "State": "state"
            })
            df["congress_number"] = congress
            all_senators.append(df)
            found = True
            break

    if not found:
        print(f"⚠️ Could not find valid senator table for Congress {congress}")

# Combine all rows
senators_df = pd.concat(all_senators, ignore_index=True)

# Drop duplicate rows (some senators may appear in multiple congresses)
senators_df = senators_df.drop_duplicates(subset=["full_name", "congress_number"])

# Optional: sort the data
senators_df = senators_df.sort_values(by=["congress_number", "state", "full_name"])

# Save to file
output_path = "data/raw/senators_116_to_119.csv"
senators_df.to_csv(output_path, index=False)

print(f"✅ Senator table saved to: {output_path}")
