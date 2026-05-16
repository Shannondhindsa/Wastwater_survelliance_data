# =============================================================================
# 01_fetch_data.R
# Purpose: Load and clean SARS-CoV-2 wastewater surveillance data
# =============================================================================

library(tidyverse)
library(lubridate)
library(janitor)

# Load raw data
raw_path <- "data/NWSS_Public_SARS-CoV-2_Wastewater_Metric_Data_20260515.csv"

message("Reading wastewater data from: ", raw_path)
raw <- read_csv(raw_path, show_col_types = FALSE)

message("Records loaded: ", format(nrow(raw), big.mark = ","))

# Clean the data
ww_clean <- raw %>%
  clean_names() %>%
  mutate(
    date_start        = as_date(date_start),
    date_end          = as_date(date_end),
    ptc_15d           = as.numeric(ptc_15d),
    detect_prop_15d   = as.numeric(detect_prop_15d),
    percentile        = as.numeric(percentile),
    population_served = as.numeric(population_served)
  ) %>%
  filter(
    !is.na(date_end),
    !is.na(ptc_15d),
    ptc_15d  > -99,
    detect_prop_15d >= 0
  ) %>%
  select(
    date           = date_end,
    state          = wwtp_jurisdiction,
    county         = county_names,
    population     = population_served,
    pct_change_15d = ptc_15d,
    detect_prop    = detect_prop_15d,
    percentile
  )

message("Records after cleaning: ", format(nrow(ww_clean), big.mark = ","))
message("Date range: ", min(ww_clean$date), " to ", max(ww_clean$date))
message("States represented: ", length(unique(ww_clean$state)))

write_csv(ww_clean, "data/wastewater_clean.csv")
message("Saved: data/wastewater_clean.csv")