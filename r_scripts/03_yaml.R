library(yaml)
library(dplyr)
library(lubridate)
library(purrr)
library(stringr)
library(writexl)

# Load YAML if not already loaded
# yaml_data <- yaml::read_yaml("/mnt/data/legislators-historical.yaml")

start_target <- as.Date("2017-01-01")
end_target <- as.Date("2023-12-31")

filtered_members <- yaml_data %>%
  keep(~ any(map_lgl(.x$terms, function(term) {
    start_date <- as.Date(term$start)
    end_date <- as.Date(term$end)
    (start_date <= end_target) && (end_date >= start_target)
  })))

member_df <- map_dfr(filtered_members, function(person) {
  overlapping_term <- keep(person$terms, function(term) {
    start_date <- as.Date(term$start)
    end_date <- as.Date(term$end)
    (start_date <= end_target) && (end_date >= start_target)
  })[[1]]
  
  full_name <- person$name$official_full %>%
    str_replace_all("\\s[A-Z]\\.\\s", " ") %>%  # Replace middle initials (e.g., " G. ") with single space
    str_replace_all("\\s[A-Z]\\.", " ") %>%     # Replace end-position initials (e.g., " G.") with single space
    str_replace_all("\\s*Jr\\.", " ") %>%       # Replace "Jr." with a space
    str_squish()                                # Final cleanup
  
  party_clean <- ifelse(overlapping_term$party == "Democrat", "Democratic", overlapping_term$party)
  
  tibble(
    full_name = full_name,
    party = party_clean
  )
})

write_xlsx(member_df, path = "/Users/cycoldiron/Desktop/congress-fiscal-tweets/data/external/cleaned_congress_members.xlsx")
