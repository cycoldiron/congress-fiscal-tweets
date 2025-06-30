import os
import json
import pandas as pd

# Expanded keyword list (all lowercased for case-insensitive matching)
keywords = [
    "national debt",
    "national deficit",
    "government debt",
    "government deficit",
    "federal debt",
    "federal deficit",
    "u.s. debt",
    "u.s. deficit",
    "debt ceiling",
    "budget deficit",
    "fiscal responsibility",
    "fiscal cliff",
    "balanced budget",
    "paygo"
]

data = []

# Loop through each tweet file
for file in os.listdir("data/raw"):
    if not file.endswith(".json"):
        continue

    full_path = os.path.join("data/raw", file)
    with open(full_path, "r") as f:
        tweets = [json.loads(line) for line in f]

    total = len(tweets)
    debt_count = sum(
        any(k in tweet.get("content", "").lower() for k in keywords)
        for tweet in tweets
    )

    name, congress_str = file.replace(".json", "").split("_C")
    data.append({
        "senator": name.replace("_", " "),
        "congress": int(congress_str),
        "num_of_tweets": total,
        "num_of_debt_tweets": debt_count
    })

# Save results
df = pd.DataFrame(data)
df.to_csv("data/processed/senator_keyword_summary.csv", index=False)

