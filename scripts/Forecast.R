library(shiny)
library(dplyr)
library(readr)
library(zoo)
library(forecast)
library(Metrics)
library(plotly)

# Define UI for the Shiny App
ui <- fluidPage(
  titlePanel("ARIMA Energy Consumption Forecast (Performance Metrics)"),
  sidebarLayout(
    sidebarPanel(
      # Country selection is based solely on the feature-engineered dataset.
      uiOutput("country_ui"),
      actionButton("runForecast", "Run Forecast")
    ),
    mainPanel(
      plotlyOutput("forecastPlot"),
      verbatimTextOutput("errorMetrics"),
      tableOutput("forecastTable")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Set the dataset folder path (modify if needed)
  dataset_folder <- "C:/git/Energy-Consumption-Forecasting/datasets"
  
  # Load only the feature-engineered dataset
  feature_data <- read_csv(file.path(dataset_folder, "feature_engineered_energy_data.csv"), 
                           show_col_types = FALSE)
  
  # Dynamically determine the energy consumption column from the feature-engineered dataset.
  possible_energy_columns <- c("energy_consumption", "primary_energy_consumption", 
                               "total_energy_consumption", "electricity_demand")
  energy_col <- intersect(possible_energy_columns, colnames(feature_data))
  if (length(energy_col) == 0) {
    stop("Error: No valid energy consumption column found in the dataset.")
  } else {
    energy_col <- energy_col[1]
    cat("Using column:", energy_col, "for energy consumption.\n")
  }
  
  # Update the country selection input based on the feature-engineered dataset.
  output$country_ui <- renderUI({
    # Remove rows with missing energy consumption values.
    df <- feature_data %>% filter(!is.na(.data[[energy_col]]))
    countries <- unique(df$country)
    selectInput("country", "Select Country:", choices = countries)
  })
  
  # Process data when "Run Forecast" is clicked.
  forecastData <- eventReactive(input$runForecast, {
    df <- feature_data %>% filter(!is.na(.data[[energy_col]]))
    country_data <- df %>% filter(country == input$country) %>% arrange(year)
    if (nrow(country_data) == 0) {
      stop(paste("No data available for", input$country))
    }
    list(data = country_data, energy_col = energy_col)
  })
  
  # Fit ARIMA with a Box–Cox transformation, forecast the next 20 years,
  # and compute error metrics.
  forecastResult <- reactive({
    req(forecastData())
    country_data <- forecastData()$data
    energy_col_local <- forecastData()$energy_col
    
    # Create a time series object from the energy consumption data.
    ts_series <- ts(country_data[[energy_col_local]], 
                    start = min(country_data$year), frequency = 1)
    
    # Determine optimal Box-Cox lambda (adjust if series has non-positive values)
    lambda <- BoxCox.lambda(ts_series)
    
    # Fit ARIMA (using an exhaustive search by disabling stepwise and approximation)
    arima_model <- auto.arima(ts_series, lambda = lambda, 
                              stepwise = FALSE, approximation = FALSE)
    
    # Forecast the next 20 years.
    forecast_horizon <- 20
    forecast_result <- forecast(arima_model, h = forecast_horizon)
    
    # Calculate error metrics on the fitted (training) data.
    fitted_values <- fitted(arima_model)
    rmse_value <- rmse(ts_series, fitted_values)
    mape_value <- mape(ts_series, fitted_values)
    mean_val <- mean(ts_series, na.rm = TRUE)
    
    list(forecast = forecast_result, rmse = rmse_value, mape = mape_value, 
         mean_val = mean_val, country_data = country_data, ts_series = ts_series, lambda = lambda)
  })
  
  # Display error metrics and check conditions:
  #   - RMSE must be less than 10% of the mean energy consumption.
  #   - MAPE must be below 15%.
  output$errorMetrics <- renderPrint({
    req(forecastResult())
    rmse_val <- forecastResult()$rmse
    mape_val <- forecastResult()$mape
    mean_val <- forecastResult()$mean_val
    
    cat("ARIMA Model RMSE:", rmse_val, "\n")
    cat("Mean Energy Consumption:", mean_val, "\n")
    if(rmse_val < 0.1 * mean_val) {
      cat("RMSE condition met: RMSE is below 10% of the mean.\n")
    } else {
      cat("Warning: RMSE condition NOT met: RMSE is above 10% of the mean.\n")
    }
    
    cat("ARIMA Model MAPE:", mape_val * 100, "%\n")
    if(mape_val < 0.15) {
      cat("MAPE condition met: MAPE is below 15%.\n")
    } else {
      cat("Warning: MAPE condition NOT met: MAPE is above 15%.\n")
    }
  })
  
  # Render an interactive Plotly forecast plot.
  output$forecastPlot <- renderPlotly({
    req(forecastResult())
    res <- forecastResult()
    fc <- res$forecast
    country_data <- res$country_data
    energy_col_local <- forecastData()$energy_col
    
    # Prepare the actual historical data.
    actual_df <- data.frame(
      Year = country_data$year,
      Energy = country_data[[energy_col_local]]
    )
    
    # Create a forecast data frame.
    forecast_years <- seq(max(country_data$year) + 1, 
                          max(country_data$year) + length(fc$mean))
    forecast_df <- data.frame(
      Year = forecast_years,
      Forecast = as.numeric(fc$mean),
      Lower80 = as.numeric(fc$lower[,1]),
      Upper80 = as.numeric(fc$upper[,1]),
      Lower95 = as.numeric(fc$lower[,2]),
      Upper95 = as.numeric(fc$upper[,2])
    )
    
    # Build the interactive Plotly plot.
    p <- plot_ly() %>%
      add_lines(data = actual_df, x = ~Year, y = ~Energy, 
                name = "Actual", line = list(color = "blue")) %>%
      add_lines(data = forecast_df, x = ~Year, y = ~Forecast, 
                name = "Forecast", line = list(color = "red", dash = "dash")) %>%
      add_ribbons(data = forecast_df, x = ~Year, ymin = ~Lower95, ymax = ~Upper95, 
                  name = "95% Confidence", fillcolor = "rgba(255, 0, 0, 0.2)",
                  line = list(color = "transparent")) %>%
      add_ribbons(data = forecast_df, x = ~Year, ymin = ~Lower80, ymax = ~Upper80, 
                  name = "80% Confidence", fillcolor = "rgba(255, 0, 0, 0.1)",
                  line = list(color = "transparent")) %>%
      layout(title = paste("Interactive ARIMA Forecast for", input$country),
             xaxis = list(title = "Year"),
             yaxis = list(title = energy_col_local))
    
    p
  })
  
  # Display a table with the forecast details.
  output$forecastTable <- renderTable({
    req(forecastResult())
    res <- forecastResult()
    fc <- res$forecast
    forecast_years <- seq(max(res$country_data$year) + 1, 
                          max(res$country_data$year) + length(fc$mean))
    forecast_df <- data.frame(
      Year = forecast_years,
      Forecast = as.numeric(fc$mean),
      Lower80 = as.numeric(fc$lower[,1]),
      Upper80 = as.numeric(fc$upper[,1]),
      Lower95 = as.numeric(fc$lower[,2]),
      Upper95 = as.numeric(fc$upper[,2])
    )
    forecast_df
  })
}

# Run the Shiny App
shinyApp(ui = ui, server = server)

