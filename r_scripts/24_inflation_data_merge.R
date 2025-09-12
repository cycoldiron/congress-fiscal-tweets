# Install if you don't have it already
# install.packages("fredr")
# install.packages(c("dplyr", "lubridate"))

library(fredr)
library(dplyr)

# Use your API key
fredr_set_key("8b1fd78976384511dabc0aa3e6c27061")

# Pull CPI data (need 2016 for YoY calc)
cpi <- fredr(
  series_id = "CPIAUCSL",
  observation_start = as.Date("2016-01-01"),
  observation_end   = as.Date("2023-12-31"),
  frequency = "m"
)

# Compute YoY inflation and filter to June 2017 – Jan 2023
cpi <- cpi %>%
  mutate(
    cpi = value,
    inflation_yoy = (cpi / lag(cpi, 12) - 1) * 100
  ) %>%
  select(date, cpi, inflation_yoy) %>%
  filter(date >= as.Date("2017-06-01"),
         date <= as.Date("2023-01-31"))



library(dplyr)

# --- 0) Paths
file_path <- "/Users/cycoldiron/Desktop/congress-fiscal-tweets/data/processed/tweets_with_leg_flags.rds"
backup_path <- paste0(sub("\\.rds$", "", file_path), "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".bak.rds")

# --- 1) Load tweets + make sure date types match
tweets_with_leg <- readRDS(file_path)
orig_ncol <- ncol(tweets_with_leg)

tweets_with_leg <- tweets_with_leg %>%
  mutate(tweet_date = as.Date(tweet_date))   # ensure Date class for join

# --- 2) Ensure CPI exists & is Date-typed (from earlier step)
# cpi should have cols: date (Date), cpi, inflation_yoy
stopifnot(all(c("date","cpi","inflation_yoy") %in% names(cpi)))
stopifnot(inherits(cpi$date, "Date"))

# --- 3) Join (rename new cols explicitly to avoid accidental overwrite)
joined <- tweets_with_leg %>%
  left_join(
    cpi %>% rename(cpi_sa = cpi, inflation_yoy_sa = inflation_yoy),
    by = c("tweet_date" = "date")
  )

# --- 4) Sanity checks BEFORE saving
new_ncol <- ncol(joined)
message("# of columns before: ", orig_ncol, " | after join: ", new_ncol)

# Expect +2 columns from join
if (new_ncol != orig_ncol + 2) {
  stop("Join did not add two columns. Check date types/keys and cpi columns.")
}

# Check NA rate of the new vars
print(
  joined %>%
    summarise(
      n_rows = n(),
      na_cpi_sa = sum(is.na(cpi_sa)),
      na_infl_sa = sum(is.na(inflation_yoy_sa)),
      distinct_months = n_distinct(tweet_date)
    )
)

# Spot-check range & first/last months
print(
  joined %>%
    distinct(tweet_date, inflation_yoy_sa) %>%
    arrange(tweet_date) %>%
    head(6)
)
print(
  joined %>%
    distinct(tweet_date, inflation_yoy_sa) %>%
    arrange(desc(tweet_date)) %>%
    head(6)
)

# --- 5) Backup old file then save
if (file.exists(file_path)) file.copy(file_path, backup_path, overwrite = TRUE)

saveRDS(joined, file_path)
message("✅ Saved updated file to: ", normalizePath(file_path))

# --- 6) Reload to confirm persistence
rm(tweets_with_leg, joined); gc()
reloaded <- readRDS(file_path)

message("Reloaded ncol = ", ncol(reloaded))
stopifnot(all(c("cpi_sa","inflation_yoy_sa") %in% names(reloaded)))



