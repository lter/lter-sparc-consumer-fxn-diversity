# this code is to see if NTL has matching year and date column
# Packages
library(readr)
library(dplyr)
library(lubridate)

# 1) Load the file
df <- read_csv("data/01_community_raw_data/ntl90_v11.csv", guess_max = 1e6)

# 2) Parse the date column robustly (add/remove formats as needed)
#    Common patterns: "YYYY-MM-DD", "YYYYMMDD", "MM/DD/YYYY", "DD/MM/YYYY", "YYYY"
df2 <- df %>%
  mutate(
    # Try multiple date orders; returns POSIXct. Coerce to Date after.
    date_parsed = parse_date_time(
      sample_date,
      orders = c("Y-m-d","Ymd","m/d/Y","d/m/Y","Y"),
      tz = "UTC",
      exact = FALSE
    ) %>% as.Date(),
    
    # 3) Extract year from parsed date
    year_from_date = year(date_parsed),
    
    # Make sure the 'year' column is numeric/integer for comparison
    year_numeric = suppressWarnings(as.integer(year4))  # keeps NA if non-numeric
  ) %>%
  mutate(matches = year_numeric == year_from_date)
