# Load required libraries
library(dplyr)
library(readr)
library(caret)
library(randomForest)
library(xgboost)
library(Metrics)
library(forecast)
library(scales)

# Set dataset path
dataset_folder <- "C:/git/Energy-Consumption-Forecasting/datasets"
merged_data_path <- file.path(dataset_folder, "merged_energy_data.csv")

# Load dataset
energy_data <- read_csv(merged_data_path, show_col_types = FALSE)

# Select relevant columns
selected_columns <- c("country", "year", "primary_energy_consumption", "gdp", "population", "renewables_consumption")
energy_data <- energy_data %>% select(any_of(selected_columns))

# Drop columns with excessive missing values (>50%)
missing_summary <- colMeans(is.na(energy_data)) * 100
high_missing_cols <- names(missing_summary[missing_summary > 50])
energy_data <- energy_data %>% select(-any_of(high_missing_cols))

# Remove outliers (cap extreme values)
energy_data <- energy_data %>%
  mutate(primary_energy_consumption = pmin(primary_energy_consumption, quantile(primary_energy_consumption, 0.99, na.rm = TRUE))) %>%
  mutate(primary_energy_consumption = pmax(primary_energy_consumption, quantile(primary_energy_consumption, 0.01, na.rm = TRUE)))

# Fill missing values using median for numerical columns
energy_data <- energy_data %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), median(., na.rm = TRUE), .)))

# Normalize numerical features using Min-Max Scaling
normalize <- function(x) { (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) }
energy_data <- energy_data %>%
  mutate(across(where(is.numeric), normalize))

# Increase dataset size (use up to 50,000 rows)
set.seed(42)
energy_data <- energy_data %>% sample_n(min(nrow(energy_data), 50000))

# Save reduced dataset for ML processing
reduced_data_path <- file.path(dataset_folder, "reduced_energy_data.csv")
write_csv(energy_data, reduced_data_path)
cat("Reduced dataset saved:", reduced_data_path, "\n")

# --- TIME SERIES MODELING (ARIMA & EXPONENTIAL SMOOTHING) ---
energy_data <- read_csv(reduced_data_path, show_col_types = FALSE)

# Ensure 'year' column is numeric
energy_data <- energy_data %>% mutate(year = as.numeric(year))

# Select country for time series analysis
selected_country <- "United States"
ts_data <- energy_data %>% filter(country == selected_country) %>% arrange(year)

if (nrow(ts_data) < 10) {
  cat("Not enough data points for time series analysis. Skipping ARIMA & Exponential Smoothing.\n")
  arima_rmse <- NA
  ets_rmse <- NA
} else {
  ts_series <- ts(ts_data$primary_energy_consumption, start = min(ts_data$year), frequency = 1)
  
  # --- ARIMA Model (Manually Tuned) ---
  arima_model <- Arima(ts_series, order = c(2,1,2))  
  arima_forecast <- forecast(arima_model, h = 5)
  
  # --- Exponential Smoothing Model (Fixed for non-seasonal data) ---
  ets_model <- ets(ts_series)  # Automatically selects best ETS model
  ets_forecast <- forecast(ets_model, h = 5)
  
  # Calculate RMSE
  actuals <- ts_series
  arima_rmse <- rmse(actuals, fitted(arima_model))
  ets_rmse <- rmse(actuals, fitted(ets_model))
}

cat("\nARIMA RMSE:", arima_rmse, "\n")
cat("Exponential Smoothing RMSE:", ets_rmse, "\n")

# --- MACHINE LEARNING MODELING (Random Forest & XGBoost) ---
target_col <- "primary_energy_consumption"

if (!target_col %in% colnames(energy_data)) {
  stop("Error: No valid energy consumption column found.")
}

ml_data <- energy_data %>% select(-c(country, year), -any_of(high_missing_cols))

set.seed(42)
trainIndex <- createDataPartition(ml_data[[target_col]], p = 0.8, list = FALSE)
train_data <- ml_data[trainIndex, ]
test_data <- ml_data[-trainIndex, ]

train_x <- train_data %>% select(-target_col)
train_y <- train_data[[target_col]]
test_x <- test_data %>% select(-target_col)
test_y <- test_data[[target_col]]

# --- Random Forest Model (Optimized) ---
rf_model <- randomForest(x = train_x, y = train_y, ntree = 200, maxnodes = 30)  
rf_pred <- predict(rf_model, test_x)
rf_rmse <- rmse(test_y, rf_pred)

# --- XGBoost Model (Tuned for better accuracy) ---
xgb_model <- xgboost(data = as.matrix(train_x), label = train_y, nrounds = 150, max_depth = 6, eta = 0.05, objective = "reg:squarederror")
xgb_pred <- predict(xgb_model, as.matrix(test_x))
xgb_rmse <- rmse(test_y, xgb_pred)

# Save Model Performance Results
model_performance <- data.frame(
  Model = c("ARIMA", "Exponential Smoothing", "Random Forest", "XGBoost"),
  RMSE = c(arima_rmse, ets_rmse, rf_rmse, xgb_rmse)
)

performance_file <- file.path(dataset_folder, "model_performance.csv")
write_csv(model_performance, performance_file)

cat("\nModel performance saved to:", performance_file, "\n")


