import os
import json
import pandas as pd
from tqdm import tqdm

# Tier 1: Strong phrase-based keywords (high precision)
strong_keywords = [
    "national debt", "national deficit", "government debt", "government deficit",
    "federal debt", "federal deficit", "u.s. debt", "u.s. deficit",
    "us debt", "us deficit", "american debt", "american deficit",
    "debt ceiling", "budget deficit", "fiscal responsibility", "fiscal cliff",
    "paygo", "unsustainable debt", "reckless spending", "debt crisis",
    "interest on the debt", "interest payments", "balanced budget", "unpaid debt",
    "unpaid tax", "trillion-dollar debt", "trillion-dollar deficit",
    "burdening future generations", "mortgaging our future", "out-of-control spending", "runaway debt", "deficit"
]

# Tier 2: Weak keywords + context
weak_keywords = ["debt"]
context_words = [
    "spending", "federal", "budget", "tax", "government",
    "cut", "reduce", "increase", "decrease", "higher", "increased", 
    "increasing", "trillion", "reckless", "unsustainable", "burden", "ceiling", "cliff", "reduction", "billion"]

# Paths
raw_data_path = "raw_congress_tweets/data"
output_path = "data/debt_tweet_summary.csv"

records = []

for filename in tqdm(sorted(os.listdir(raw_data_path))):
    if filename.endswith(".json"):
        full_path = os.path.join(raw_data_path, filename)
        with open(full_path, "r") as f:
            try:
                tweets = json.load(f)
            except Exception as e:
                print(f"Error reading {filename}: {e}")
                continue

            for tweet in tweets:
                text = tweet.get("text", "").lower()
                screen_name = tweet.get("screen_name", "")
                tweet_date = tweet.get("time", "")[:7]  # YYYY-MM

                # Strong match
                is_strong_match = any(kw in text for kw in strong_keywords)

                # Weak match: contains debt/deficit + context
                contains_weak = any(wk in text for wk in weak_keywords)
                contains_context = any(ctx in text for ctx in context_words)
                is_weak_match = contains_weak and contains_context

                is_debt = int(is_strong_match or is_weak_match)

                records.append({
                    "screen_name": screen_name,
                    "is_debt": is_debt,
                    "tweet_date": tweet_date
                })

df = pd.DataFrame(records)
df.to_csv(output_path, index=False)
print(f"✅ Saved {len(df)} rows to {output_path}")

df = pd.read_csv(output_path)
print(df.head())
print(df["is_debt"].sum())
