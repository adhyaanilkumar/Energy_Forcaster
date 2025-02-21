# Load required libraries
library(forecast)
library(dplyr)
library(tidyr)
library(readr)
library(shiny)
library(ggplot2)
library(zoo)  # For linear interpolation

# Set the dataset folder path
dataset_folder <- "C:/git/Energy-Consumption-Forecasting/datasets"
feature_engineered_path <- file.path(dataset_folder, "feature_engineered_energy_data.csv")

# Read the feature-engineered dataset
energy_data <- read_csv(feature_engineered_path, show_col_types = FALSE)

# Identify the correct energy consumption column dynamically
energy_col <- "primary_energy_consumption"

# Ensure 'year' is numeric for time series modeling
energy_data$year <- as.numeric(energy_data$year)

# Handle missing values using linear interpolation or mean imputation
energy_data$primary_energy_consumption <- energy_data %>%
  group_by(country) %>%
  mutate(
    primary_energy_consumption = if (all(is.na(primary_energy_consumption))) {
      # If all values are NA, impute with the mean for that country
      mean_value <- mean(primary_energy_consumption, na.rm = TRUE)
      primary_energy_consumption <- ifelse(is.na(primary_energy_consumption), mean_value, primary_energy_consumption)
    } else {
      # Otherwise, apply linear interpolation
      primary_energy_consumption <- na.approx(primary_energy_consumption, rule = 2)
    }
  ) %>%
  ungroup()

# Function to fit ARIMA model and forecast
forecast_energy <- function(df, country, energy_col, forecast_years = 10) {
  ts_data <- df %>% filter(country == !!country) %>% arrange(year)
  
  # Check for missing values in the time series data for the selected country
  if (any(is.na(ts_data[[energy_col]]))) {
    cat("Missing data detected for", country, "in", energy_col, "\n")
    return(NULL)  # Return NULL if there is missing data
  }
  
  # Create a time series object for energy consumption
  ts_series <- ts(ts_data[[energy_col]], start = min(ts_data$year), frequency = 1)
  
  # Check if the time series has sufficient data points for ARIMA
  if (length(ts_series) < 2) {
    cat("Insufficient data for ARIMA model in", country, "\n")
    return(NULL)  # Return NULL if not enough data points
  }
  
  # Fit ARIMA model
  arima_model <- auto.arima(ts_series)
  arima_forecast <- forecast(arima_model, h = forecast_years)
  
  return(arima_forecast)
}

# Forecast for selected countries
forecast_results <- list()
countries <- unique(energy_data$country)
for (country in countries) {
  forecast_results[[country]] <- forecast_energy(energy_data, country, energy_col)
}

# Define UI for the app
ui <- fluidPage(
  titlePanel("Energy Consumption Forecasting and Comparison"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("country1", "Select Country 1", choices = countries),
      selectInput("country2", "Select Country 2", choices = countries),
      selectInput("energy_type", "Select Energy Type", choices = c("Primary Energy Consumption", "Renewables Consumption"))
    ),
    
    mainPanel(
      plotOutput("forecastPlot"),
      textOutput("modelExplanation")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Reactive expression to get forecast data
  forecast_data <- reactive({
    country1_forecast <- forecast_results[[input$country1]]
    country2_forecast <- forecast_results[[input$country2]]
    
    if (is.null(country1_forecast) || is.null(country2_forecast)) {
      return(NULL)  # Return NULL if forecasts are not available
    }
    
    data1 <- data.frame(
      Year = seq(2021, 2030),
      Forecast = country1_forecast$mean,
      Country = input$country1
    )
    
    data2 <- data.frame(
      Year = seq(2021, 2030),
      Forecast = country2_forecast$mean,
      Country = input$country2
    )
    
    combined_data <- rbind(data1, data2)
    return(combined_data)
  })
  
  # Plot the forecast
  output$forecastPlot <- renderPlot({
    forecast_data <- forecast_data()
    
    # Check if forecast data is NULL or empty
    if (is.null(forecast_data) || nrow(forecast_data) == 0) {
      return(NULL)  # Return nothing if no valid forecast data
    }
    
    # Remove rows with NA values in 'Forecast' column before plotting
    forecast_data_clean <- forecast_data %>%
      filter(!is.na(Forecast))
    
    # If after cleaning there's no data, show a message instead of plotting
    if (nrow(forecast_data_clean) == 0) {
      return(NULL)  # Return nothing if no clean data available
    }
    
    # Create the plot
    ggplot(forecast_data_clean, aes(x = Year, y = Forecast, color = Country)) +
      geom_line() +
      labs(title = paste("Energy Consumption Forecast (2021-2030)"),
           x = "Year", y = input$energy_type) +
      theme_minimal()
  })
  
  # Display machine learning model explanation
  output$modelExplanation <- renderText({
    explanation <- paste(
      "The following models were used to forecast energy consumption:\n\n",
      "1. **ARIMA (AutoRegressive Integrated Moving Average):**\n",
      "   ARIMA is a statistical method for time series forecasting. It captures trends, seasonality, and patterns in historical data, and predicts future values based on these patterns.\n",
      "   The model combines three components:\n",
      "   - **AR (AutoRegressive):** Uses past values to predict future values.\n",
      "   - **I (Integrated):** Differencing to make the series stationary.\n",
      "   - **MA (Moving Average):** Uses past forecast errors.\n\n",
      "2. **Exponential Smoothing (ETS):**\n",
      "   ETS is a forecasting method where the model assigns exponentially decreasing weights to past observations. More recent data points are given higher weights. The model adjusts for trends and seasonality.\n\n",
      "These models help predict future energy consumption by learning from past patterns in the data, offering valuable insights for planning and forecasting future energy needs."
    )
    return(explanation)
  })
}

# Run the app
shinyApp(ui = ui, server = server)


