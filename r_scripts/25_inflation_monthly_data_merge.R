library(dplyr)
library(fredr)

# ---------- 0) Paths ----------
m_path <- "/Users/cycoldiron/Desktop/congress-fiscal-tweets/data/processed/tweets_monthly_with_leg.rds"

# ---------- 1) Get CPI + YoY inflation (once) ----------
fredr_set_key("8b1fd78976384511dabc0aa3e6c27061")

cpi <- fredr(
  series_id = "CPIAUCSL",
  observation_start = as.Date("2016-01-01"),   # need prior year for YoY
  observation_end   = as.Date("2023-12-31"),
  frequency = "m"
) %>%
  mutate(
    cpi_sa = value,
    inflation_yoy_sa = (cpi_sa / dplyr::lag(cpi_sa, 12) - 1) * 100
  ) %>%
  select(date, cpi_sa, inflation_yoy_sa) %>%
  filter(date >= as.Date("2017-06-01"), date <= as.Date("2023-01-31")) %>%
  rename(month = date)   # match key in monthly dataset

# ---------- 2) Load monthly dataset ----------
mm <- readRDS(m_path)
stopifnot("month" %in% names(mm), inherits(mm$month, "Date"))

orig_ncol <- ncol(mm)

# ---------- 3) Join CPI -> monthly ----------
mm2 <- mm %>%
  left_join(cpi, by = "month")

# ---------- 4) Checks ----------
message("# cols before: ", orig_ncol, " | after: ", ncol(mm2))
message("Distinct months in data: ", dplyr::n_distinct(mm2$month))

# NA check (should be zero)
na_cpi  <- sum(is.na(mm2$cpi_sa))
na_infl <- sum(is.na(mm2$inflation_yoy_sa))
message("NAs — cpi_sa: ", na_cpi, " | inflation_yoy_sa: ", na_infl)
stopifnot(na_cpi == 0, na_infl == 0)

# Window check
stopifnot(min(mm2$month) >= as.Date("2017-06-01"),
          max(mm2$month) <= as.Date("2023-01-01"))

# Quick spot checks
print(mm2 %>% distinct(month, inflation_yoy_sa) %>% arrange(month) %>% head(6))
print(mm2 %>% distinct(month, inflation_yoy_sa) %>% arrange(desc(month)) %>% head(6))

# ---------- 5) Save (overwrite existing file) ----------
saveRDS(mm2, m_path)
message("✅ Saved with inflation to: ", normalizePath(m_path))
