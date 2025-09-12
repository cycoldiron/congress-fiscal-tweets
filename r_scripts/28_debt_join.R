library(dplyr)
library(lubridate)

# ===== Paths (relative to project root) =====
monthly_path <- file.path("data", "processed", "tweets_monthly_with_leg.rds")
flags_path   <- file.path("data", "processed", "tweets_with_leg_flags.rds")

# ===== Load datasets =====
mm  <- readRDS(monthly_path)
raw <- readRDS(flags_path)

# Helper: last non-NA within a month
last_non_na <- function(x) { x <- x[!is.na(x)]; if (length(x)) tail(x, 1) else NA_real_ }

# ===== Build monthly debt series from raw flags file =====
# Prefer an existing Date 'month' column; otherwise derive from 'tweet_date'
debt_monthly <- raw %>%
  mutate(
    month = dplyr::coalesce(
      suppressWarnings(as.Date(month)),
      floor_date(as.Date(tweet_date), "month")
    )
  ) %>%
  group_by(month) %>%
  summarise(
    total_public_debt_outstanding = last_non_na(total_public_debt_outstanding),
    .groups = "drop"
  ) %>%
  arrange(month)

# ===== Ensure 'mm$month' is Date and join =====
mm_joined <- mm %>%
  mutate(month = as.Date(month)) %>%
  left_join(debt_monthly, by = "month")

# ===== Overwrite the monthly file =====
saveRDS(mm_joined, monthly_path)

# ===== Verify save & column presence =====
mm_check <- readRDS(monthly_path)

stopifnot("total_public_debt_outstanding" %in% names(mm_check))

# Quick sanity checks
cat("✅ Column added. Rows:", nrow(mm_check), "\n")
cat("NA count in total_public_debt_outstanding:",
    sum(is.na(mm_check$total_public_debt_outstanding)), "\n")

print(
  mm_check %>%
    arrange(desc(month)) %>%
    select(month, total_public_debt_outstanding) %>%
    head(6)
)
