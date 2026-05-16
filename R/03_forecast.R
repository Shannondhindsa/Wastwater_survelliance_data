# =============================================================================
# 03_forecast.R
# Purpose: Fit ARIMA model, backtest, and generate forecasts
# =============================================================================

library(tidyverse)
library(lubridate)
library(tsibble)
library(fable)
library(feasts)
library(zoo)

# Load weekly aggregated series
national_weekly <- read_csv("data/national_weekly.csv", show_col_types = FALSE)

ts_data <- national_weekly %>%
  as_tsibble(index = week) %>%
  fill_gaps(mean_pct_change = NA) %>%
  mutate(mean_pct_change = zoo::na.approx(mean_pct_change, na.rm = FALSE)) %>%
  filter(!is.na(mean_pct_change))

# Hold out last 12 weeks for backtesting
n_holdout  <- 12
train_data <- ts_data %>% slice(1:(n() - n_holdout))

# Fit auto-ARIMA
message("Fitting ARIMA model...")
fit <- train_data %>%
  model(arima = ARIMA(mean_pct_change))

fit %>% report()

# Backtest
backtest <- fit %>% forecast(h = n_holdout)

accuracy_metrics <- backtest %>%
  accuracy(ts_data) %>%
  select(.model, RMSE, MAE, MAPE)

write_csv(accuracy_metrics, "outputs/accuracy_metrics.csv")
print(accuracy_metrics)

p_backtest <- backtest %>%
  autoplot(ts_data) +
  labs(title = "ARIMA Backtest: 12-Week Out-of-Sample Forecast",
       subtitle = "Shaded bands show 80% and 95% prediction intervals",
       x = NULL, y = "Mean % change (15-day)") +
  theme_minimal(base_size = 12)

ggsave("outputs/backtest_plot.png", p_backtest,
       width = 10, height = 5, dpi = 300)

# Refit on ALL data and forecast 12 weeks ahead
final_fit <- ts_data %>% model(arima = ARIMA(mean_pct_change))
future_forecast <- final_fit %>% forecast(h = 12)

p_forecast <- future_forecast %>%
  autoplot(ts_data) +
  labs(title = "12-Week Forecast: National Wastewater SARS-CoV-2 Trend",
       subtitle = "ARIMA model fit on full historical series",
       x = NULL, y = "Mean % change (15-day)") +
  theme_minimal(base_size = 12)

ggsave("outputs/forecast_plot.png", p_forecast,
       width = 10, height = 5, dpi = 300)

message("Forecasting complete. See outputs/ folder.")