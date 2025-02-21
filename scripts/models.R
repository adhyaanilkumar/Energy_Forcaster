# Load required libraries
library(tidyr)
library(dplyr)
library(readr)
library(forecast)  # ARIMA & Exponential Smoothing
library(caret)  # Data splitting & evaluation
library(randomForest)  # Random Forest
library(xgboost)  # XGBoost
library(Metrics)  # RMSE Calculation

# Set dataset folder path
dataset_folder <- "C:/git/Energy-Consumption-Forecasting/datasets"

# Check if the feature-engineered dataset exists
feature_engineered_path <- file.path(dataset_folder, "feature_engineered_energy_data.csv")
merged_data_path <- file.path(dataset_folder, "merged_energy_data.csv")

if (file.exists(feature_engineered_path)) {
  cat("Using Feature-Engineered Dataset\n")
  energy_data <- read_csv(feature_engineered_path, show_col_types = FALSE)
} else {
  cat("Feature-Engineered Dataset Not Found. Using Merged Dataset\n")
  energy_data <- read_csv(merged_data_path, show_col_types = FALSE)
}

# Identify the correct energy consumption column dynamically
possible_energy_columns <- c("energy_consumption", "primary_energy_consumption", "total_energy_consumption", "electricity_demand")
energy_col <- intersect(possible_energy_columns, colnames(energy_data))

if (length(energy_col) == 0) {
  stop("Error: No valid energy consumption column found in the dataset.")
} else {
  energy_col <- energy_col[1]  # Use the first matching column
  cat("Using column:", energy_col, "for energy consumption.\n")
}

# Identify the correct renewable energy column dynamically
possible_renewable_columns <- c("renewables_consumption", "renewables_electricity", "renewables_share_energy", "renewables_share_elec", "other_renewable_consumption")
renewable_col <- intersect(possible_renewable_columns, colnames(energy_data))

if (length(renewable_col) == 0) {
  stop("Error: No valid renewable energy column found in the dataset.")
} else {
  renewable_col <- renewable_col[1]  # Use the first matching column
  cat("Using column:", renewable_col, "for renewable energy.\n")
}

# Ensure 'year' is numeric for time series modeling
energy_data$year <- as.numeric(energy_data$year)

# Select relevant columns
energy_data <- energy_data %>%
  select(year, country, all_of(energy_col), gdp, all_of(renewable_col), population) %>%
  filter(!is.na(.data[[energy_col]]))  # Remove missing values

# Convert 'country' to a factor
energy_data$country <- as.factor(energy_data$country)

# ---- 4.1 TIME SERIES MODELS ----

# Function to fit and evaluate ARIMA & Exponential Smoothing models
fit_time_series_models <- function(df, country) {
  ts_data <- df %>% filter(country == !!country) %>% arrange(year)
  
  # Convert energy consumption to a time series object
  ts_series <- ts(ts_data[[energy_col]], start = min(ts_data$year), frequency = 1)
  
  # ARIMA Model
  arima_model <- auto.arima(ts_series)
  arima_forecast <- forecast(arima_model, h = 5)
  
  # Exponential Smoothing Model
  ets_model <- ets(ts_series)
  ets_forecast <- forecast(ets_model, h = 5)
  
  # Calculate RMSE
  actuals <- ts_series
  arima_rmse <- rmse(actuals, fitted(arima_model))
  ets_rmse <- rmse(actuals, fitted(ets_model))
  
  cat("\nCountry:", country)
  cat("\nARIMA RMSE:", arima_rmse)
  cat("\nExponential Smoothing RMSE:", ets_rmse, "\n")
  
  return(list(arima_rmse = arima_rmse, ets_rmse = ets_rmse))
}

# Apply function for a specific country (Modify as needed)
time_series_results <- fit_time_series_models(energy_data, "United States")

# ---- 4.2 MACHINE LEARNING MODELS ----

# Data Preprocessing for ML Models
ml_data <- energy_data %>%
  select(-country) %>%  # Remove categorical variables
  drop_na()  # Remove missing values

# Split data into train and test sets
set.seed(42)
trainIndex <- createDataPartition(ml_data[[energy_col]], p = 0.8, list = FALSE)
train_data <- ml_data[trainIndex, ]
test_data <- ml_data[-trainIndex, ]

# Prepare training and testing datasets
train_x <- train_data %>% select(-all_of(energy_col))
train_y <- train_data[[energy_col]]
test_x <- test_data %>% select(-all_of(energy_col))
test_y <- test_data[[energy_col]]

# RANDOM FOREST MODEL
rf_model <- randomForest(x = train_x, y = train_y, ntree = 100)
rf_pred <- predict(rf_model, test_x)

# RMSE for Random Forest
rf_rmse <- rmse(test_y, rf_pred)
cat("\nRandom Forest RMSE:", rf_rmse)

# XGBOOST MODEL
xgb_model <- xgboost(data = as.matrix(train_x), label = train_y, nrounds = 100, objective = "reg:squarederror")
xgb_pred <- predict(xgb_model, as.matrix(test_x))

# RMSE for XGBoost
xgb_rmse <- rmse(test_y, xgb_pred)
cat("\nXGBoost RMSE:", xgb_rmse)

# ---- 4.3 MODEL VALIDATION ----

# Combine all model performances
model_performance <- data.frame(
  Model = c("ARIMA", "Exponential Smoothing", "Random Forest", "XGBoost"),
  RMSE = c(time_series_results$arima_rmse, time_series_results$ets_rmse, rf_rmse, xgb_rmse)
)

print(model_performance)

# Save model performance as CSV
write.csv(model_performance, file.path(dataset_folder, "model_performance.csv"), row.names = FALSE)
cat("\nModel performance saved to 'model_performance.csv'")
