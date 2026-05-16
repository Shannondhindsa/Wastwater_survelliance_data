# =============================================================================
# app.R — Wastewater Forecasting Dashboard
# Author: Shannon Dhindsa
# =============================================================================

library(shiny)
library(bslib)
library(tidyverse)
library(lubridate)
library(tsibble)
library(fable)
library(feasts)
library(plotly)
library(DT)
library(zoo)
library(bsicons)

# Load data once at startup
ww          <- read_csv("data/wastewater_clean.csv", show_col_types = FALSE)
national    <- read_csv("data/national_weekly.csv",  show_col_types = FALSE)
accuracy_df <- read_csv("outputs/accuracy_metrics.csv", show_col_types = FALSE)

states <- sort(unique(ww$state))

# ----- UI -----
ui <- page_navbar(
  title = "Wastewater Surveillance Forecasting",
  theme = bs_theme(bootswatch = "flatly"),
  
  nav_panel(
    title = "Overview",
    layout_columns(
      col_widths = c(4, 4, 4),
      value_box(
        title = "Reporting States",
        value = textOutput("kpi_sites"),
        showcase = bs_icon("geo-alt-fill"),
        theme = "primary"
      ),
      value_box(
        title = "Latest Week",
        value = textOutput("kpi_date"),
        showcase = bs_icon("calendar-week"),
        theme = "info"
      ),
      value_box(
        title = "Backtest RMSE",
        value = textOutput("kpi_rmse"),
        showcase = bs_icon("graph-up"),
        theme = "success"
      )
    ),
    card(
      card_header("National Trend"),
      plotlyOutput("national_plot", height = "450px")
    )
  ),
  
  nav_panel(
    title = "State Explorer",
    layout_sidebar(
      sidebar = sidebar(
        selectInput("state", "Select state:",
                    choices = states, selected = states[1]),
        sliderInput("smoothing", "Smoothing window (weeks):",
                    min = 1, max = 12, value = 4)
      ),
      card(
        card_header("State-Level Trend"),
        plotlyOutput("state_plot", height = "500px")
      )
    )
  ),
  
  nav_panel(
    title = "Forecast",
    layout_sidebar(
      sidebar = sidebar(
        sliderInput("horizon", "Forecast horizon (weeks):",
                    min = 4, max = 26, value = 12, step = 2),
        sliderInput("ci_level", "Confidence level (%):",
                    min = 50, max = 95, value = 80, step = 5)
      ),
      card(
        card_header("ARIMA Forecast — National Wastewater Signal"),
        plotlyOutput("forecast_plot", height = "500px")
      ),
      card(
        card_header("Model Accuracy (on 12-week holdout)"),
        DTOutput("accuracy_table")
      )
    )
  ),
  
  nav_panel(
    title = "About",
    card(
      card_body(
        h3("About this dashboard"),
        p("This dashboard visualizes SARS-CoV-2 wastewater surveillance data
           from the CDC's National Wastewater Surveillance System (NWSS) and
           generates short-term forecasts using ARIMA models."),
        h4("Methods"),
        p("Forecasts use auto-ARIMA model selection via the fable package,
           with model order chosen by AICc."),
        h4("Author"),
        p("Shannon Dhindsa | Yale Epidemiologist and Data Analyst")
      )
    )
  )
)

# ----- Server -----
server <- function(input, output, session) {
  
  output$kpi_sites <- renderText({
    format(length(unique(ww$state)), big.mark = ",")
  })
  
  output$kpi_date <- renderText({
    format(max(national$week, na.rm = TRUE), "%b %d, %Y")
  })
  
  output$kpi_rmse <- renderText({
    round(accuracy_df$RMSE[1], 2)
  })
  
  output$national_plot <- renderPlotly({
    p <- ggplot(national, aes(x = week, y = mean_pct_change)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
      geom_line(color = "#1f77b4", linewidth = 0.8) +
      geom_smooth(method = "loess", span = 0.15, se = FALSE,
                  color = "#d62728") +
      labs(x = NULL, y = "Mean % change (15-day)") +
      theme_minimal()
    ggplotly(p)
  })
  
  state_data <- reactive({
    ww %>%
      filter(state == input$state) %>%
      mutate(week = floor_date(date, "week")) %>%
      group_by(week) %>%
      summarise(mean_pct = mean(pct_change_15d, na.rm = TRUE),
                .groups = "drop") %>%
      arrange(week) %>%
      mutate(smoothed = zoo::rollmean(mean_pct, k = input$smoothing,
                                      fill = NA, align = "right"))
  })
  
  output$state_plot <- renderPlotly({
    p <- ggplot(state_data(), aes(x = week)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
      geom_line(aes(y = mean_pct), color = "grey70", linewidth = 0.5) +
      geom_line(aes(y = smoothed), color = "#2ca02c", linewidth = 1) +
      labs(x = NULL, y = "Mean % change (15-day)",
           title = paste("Wastewater trend —", input$state)) +
      theme_minimal()
    ggplotly(p)
  })
  
  forecast_result <- reactive({
    ts_data <- national %>%
      as_tsibble(index = week) %>%
      fill_gaps(mean_pct_change = NA) %>%
      mutate(mean_pct_change = zoo::na.approx(mean_pct_change, na.rm = FALSE)) %>%
      filter(!is.na(mean_pct_change))
    
    fit <- ts_data %>% model(arima = ARIMA(mean_pct_change))
    
    fc <- fit %>%
      forecast(h = input$horizon) %>%
      hilo(level = input$ci_level) %>%
      unpack_hilo(cols = paste0(input$ci_level, "%")) %>%
      as_tibble()
    
    list(ts_data = ts_data, fc = fc)
  })
  
  output$forecast_plot <- renderPlotly({
    res <- forecast_result()
    
    ci_lower <- paste0(input$ci_level, "%_lower")
    ci_upper <- paste0(input$ci_level, "%_upper")
    
    p <- ggplot() +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
      geom_line(data = res$ts_data,
                aes(x = week, y = mean_pct_change), color = "grey40") +
      geom_ribbon(data = res$fc,
                  aes(x = week,
                      ymin = .data[[ci_lower]],
                      ymax = .data[[ci_upper]]),
                  fill = "#1f77b4", alpha = 0.25) +
      geom_line(data = res$fc,
                aes(x = week, y = .mean), color = "#1f77b4", linewidth = 1) +
      labs(x = NULL, y = "Mean % change (15-day)") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$accuracy_table <- renderDT({
    datatable(accuracy_df, options = list(dom = "t"), rownames = FALSE) %>%
      formatRound(columns = c("RMSE", "MAE", "MAPE"), digits = 2)
  })
}

shinyApp(ui, server)