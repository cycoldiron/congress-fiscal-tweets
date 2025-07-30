# 02_merge_user_metadata.R
# Purpose: Merge tweet data with Congress metadata using progressive matching
# Output: all_tweets_final (merged tweets with full_name and party info)

# === Load libraries ===
library(pacman)
p_load(tidyverse, writexl, readxl, janitor, fuzzyjoin, stringr)

# === Load cleaned tweet data ===
load("data/processed/01_cleandata.Rdata")

# === Assign full_name and party to known leadership accounts ===
tweet_raw_df_2 <- tweet_raw_df_2 %>%
  mutate(
    screen_name = tolower(screen_name),
    full_name = case_when(
      screen_name == "mcconnellpress" ~ "Mitch McConnell",
      screen_name == "leadermcconnell" ~ "Mitch McConnell",
      screen_name == "speakermccarthy" ~ "Kevin McCarthy",
      screen_name == "kevinomccarthy" ~ "Kevin McCarthy",
      screen_name == "pryan" ~ "Paul Ryan",
      TRUE ~ NA_character_
    ),
    party = case_when(
      screen_name %in% c("mcconnellpress", "leadermcconnell", "speakermccarthy", "kevinomccarthy", "pryan") ~ "Republican",
      TRUE ~ NA_character_
    )
  )

# === Load and clean metadata ===
congress_excel_info <- read_excel("data/external/congress_aggregated/all_congress_data.xlsx") %>%
  clean_names() %>%
  mutate(screen_name = tolower(screen_name)) %>%
  distinct(screen_name, .keep_all = TRUE)

# === Pre-clean screen_names for fuzzy matching ===
clean_screen <- function(name) {
  name %>% str_remove_all("rep|sen|for|vote|[0-9]+") %>% str_squish()
}

tweet_raw_df_2 <- tweet_raw_df_2 %>%
  mutate(screen_name_clean = clean_screen(screen_name))

congress_excel_info <- congress_excel_info %>%
  mutate(screen_name_clean = clean_screen(screen_name))

# === Exact match ===
tweet_merged_df <- tweet_raw_df_2 %>%
  left_join(congress_excel_info, by = "screen_name", suffix = c(".x", ".y")) %>%
  mutate(
    full_name = coalesce(full_name.x, full_name.y),
    party = coalesce(party.x, party.y),
    match_type = if_else(!is.na(full_name.y), "exact", NA_character_)
  )

# === Substring match ===
unmatched <- tweet_merged_df %>%
  filter(is.na(full_name)) %>%
  distinct(screen_name) %>%
  rename(tweet_screen_name = screen_name)

meta_screen_names <- congress_excel_info %>%
  distinct(screen_name, full_name, party) %>%
  rename(meta_screen_name = screen_name)

substring_matches <- expand_grid(unmatched, meta_screen_names) %>%
  filter(
    str_detect(meta_screen_name, fixed(tweet_screen_name)) |
      str_detect(tweet_screen_name, fixed(meta_screen_name))
  ) %>%
  group_by(tweet_screen_name) %>%
  slice(1) %>%
  ungroup() %>%
  rename(full_name.sub = full_name, party.sub = party)

merged_substring <- tweet_merged_df %>%
  left_join(substring_matches, by = c("screen_name" = "tweet_screen_name")) %>%
  mutate(
    full_name = coalesce(full_name, full_name.sub),
    party = coalesce(party, party.sub),
    match_type = if_else(is.na(match_type) & !is.na(full_name.sub), "substring", match_type)
  )

# === Levenshtein match ===
lev_unmatched <- merged_substring %>%
  filter(is.na(full_name)) %>%
  distinct(screen_name) %>%
  rename(tweet_screen_name = screen_name)

lev_matches <- lev_unmatched %>%
  filter(!is.na(tweet_screen_name)) %>%
  stringdist_left_join(
    meta_screen_names %>% filter(!is.na(meta_screen_name)),
    by = c("tweet_screen_name" = "meta_screen_name"),
    method = "lv", max_dist = 3, distance_col = "dist"
  ) %>%
  group_by(tweet_screen_name) %>%
  slice_min(order_by = dist, n = 1) %>%
  ungroup() %>%
  rename(full_name.lev = full_name, party.lev = party)

merged_lev <- merged_substring %>%
  left_join(lev_matches, by = c("screen_name" = "tweet_screen_name")) %>%
  mutate(
    full_name = coalesce(full_name, full_name.lev),
    party = coalesce(party, party.lev),
    match_type = if_else(is.na(match_type) & !is.na(full_name.lev), "levenshtein", match_type)
  )

# === Last name match ===
last_name_df <- congress_excel_info %>%
  mutate(last_name = word(full_name, -1)) %>%
  select(last_name, full_name, party) %>%
  rename(full_name.last = full_name, party.last = party)

last_name_matches <- merged_lev %>%
  filter(is.na(full_name)) %>%
  distinct(screen_name) %>%
  mutate(screen_name_lower = tolower(screen_name)) %>%
  crossing(last_name_df) %>%
  filter(str_detect(screen_name_lower, tolower(last_name))) %>%
  group_by(screen_name) %>%
  slice(1) %>%
  ungroup()

merged_last <- merged_lev %>%
  left_join(last_name_matches, by = "screen_name") %>%
  mutate(
    full_name = coalesce(full_name, full_name.last),
    party = coalesce(party, party.last),
    match_type = if_else(is.na(match_type) & !is.na(full_name.last), "last_name", match_type)
  )

# === Final cleanup: remove unmatched and save ===
final_merged_df <- merged_last %>%
  select(full_name, screen_name, is_debt, tweet_date, party, match_type)

# === Create final filtered datasets ===
non_exact_matches <- final_merged_df %>%
  filter(match_type != "exact") %>%
  distinct(screen_name, full_name, party, match_type) %>%
  arrange(match_type, screen_name)

all_matches <- final_merged_df %>%
  distinct(screen_name, full_name, party, match_type) %>%
  arrange(match_type, screen_name)

valid_matches <- final_merged_df %>%
  filter(!is.na(full_name))

multi_screenname <- valid_matches %>%
  distinct(full_name, screen_name) %>%
  count(full_name) %>%
  filter(n >= 2)

full_name_screenname_df <- valid_matches %>%
  filter(full_name %in% multi_screenname$full_name) %>%
  distinct(full_name, screen_name, match_type)

all_tweets_final <- final_merged_df %>%
  filter(as.Date(paste0(tweet_date, "-01")) < as.Date("2022-12-31")) %>%
  filter(full_name != "Angus King") %>%
  select(-match_type)

# === Save outputs ===
write_xlsx(full_name_screenname_df, "data/external/congress_aggregated/all_twitterhandle_data.xlsx")
write_xlsx(non_exact_matches, "data/external/congress_aggregated/non_exact_matches_for_review.xlsx")

save(all_tweets_final, non_exact_matches, all_matches, full_name_screenname_df,
     file = "data/processed/02_matched_congress_tweet.Rdata")
