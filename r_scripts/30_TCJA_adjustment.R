# ===== Setup =====
library(dplyr)
library(lubridate)

# Paths (adjust if needed)
monthly_path <- "/Users/cycoldiron/Desktop/congress-fiscal-tweets/data/processed/tweets_monthly_with_leg.rds"
flags_path   <- "/Users/cycoldiron/Desktop/congress-fiscal-tweets/data/processed/tweets_with_leg_flags.rds"

# ===== Helper function =====
add_tcja_flag <- function(df) {
  stopifnot("month" %in% names(df))
  
  # Coerce month to Date safely
  month_as_date <- function(x) {
    if (inherits(x, "Date")) return(x)
    if (inherits(x, c("POSIXct","POSIXt"))) return(as.Date(x))
    as.Date(lubridate::ymd(x))
  }
  
  df <- df %>% mutate(month = month_as_date(month))
  
  # TCJA months
  tcja_months <- as.Date(c("2017-11-01", "2017-12-01", "2018-01-01"))
  
  # Drop any old is_tcja column if it exists
  if ("is_tcja" %in% names(df)) {
    df <- df %>% select(-is_tcja)
  }
  
  # Add the new flag
  df <- df %>% mutate(any_tcja = as.integer(month %in% tcja_months))
  
  return(df)
}

# ===== Process monthly file =====
mm <- readRDS(monthly_path) %>% add_tcja_flag()
saveRDS(mm, monthly_path)

# ===== Process flags file =====
ff <- readRDS(flags_path) %>% add_tcja_flag()
saveRDS(ff, flags_path)

# ===== Optional sanity checks =====
cat("\n=== Monthly file (tweets_monthly_with_leg.rds) ===\n")
print(table(mm$any_tcja, useNA = "ifany"))
print(mm %>% filter(any_tcja == 1) %>% select(month, any_tcja))

cat("\n=== Flags file (tweets_with_leg_flags.rds) ===\n")
print(table(ff$any_tcja, useNA = "ifany"))
print(ff %>% filter(any_tcja == 1) %>% select(month, any_tcja))


