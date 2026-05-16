# =============================================================================
# 02_explore.R
# Purpose: Exploratory analysis with seasonal decomposition
# =============================================================================

library(tidyverse)
library(lubridate)
library(tsibble)
library(feasts)
library(scales)
library(zoo)

# Load cleaned data
ww <- read_csv("data/wastewater_clean.csv", show_col_types = FALSE)

# Aggregate to national weekly mean
national_weekly <- ww %>%
  mutate(week = floor_date(date, "week")) %>%
  group_by(week) %>%
  summarise(
    mean_pct_change = mean(pct_change_15d, na.rm = TRUE),
    mean_detect     = mean(detect_prop,    na.rm = TRUE),
    n_sites         = n(),
    .groups = "drop"
  ) %>%
  filter(n_sites >= 50)

# Plot 1: National trend
p_trend <- ggplot(national_weekly, aes(x = week, y = mean_pct_change)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_line(color = "#1f77b4", linewidth = 0.8) +
  geom_smooth(method = "loess", span = 0.15, se = FALSE,
              color = "#d62728", linewidth = 1) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  labs(
    title    = "National SARS-CoV-2 Wastewater Viral Load Trend",
    subtitle = "Weekly mean 15-day percent change across all reporting sites",
    x        = NULL,
    y        = "Mean % change (15-day)"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave("outputs/national_trend.png", p_trend,
       width = 10, height = 5, dpi = 300)

# Plot 2: STL Seasonal decomposition
ts_data <- national_weekly %>%
  as_tsibble(index = week) %>%
  fill_gaps(mean_pct_change = NA) %>%
  mutate(mean_pct_change = zoo::na.approx(mean_pct_change, na.rm = FALSE))

decomp <- ts_data %>%
  model(STL(mean_pct_change ~ season(window = "periodic"))) %>%
  components()

p_decomp <- autoplot(decomp) +
  labs(title = "STL Decomposition of National Wastewater Signal") +
  theme_minimal(base_size = 11)

ggsave("outputs/seasonal_decomposition.png", p_decomp,
       width = 10, height = 7, dpi = 300)

# Save aggregated series for the forecasting step
write_csv(national_weekly, "data/national_weekly.csv")
message("EDA complete. Plots saved to outputs/")
