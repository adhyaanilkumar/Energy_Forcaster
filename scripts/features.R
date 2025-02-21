# Load required libraries
library(dplyr)
library(readr)
library(lubridate)
library(zoo)
library(scales)

# Set dataset path
dataset_folder <- "C:/git/Energy-Consumption-Forecasting/datasets"
merged_data_path <- file.path(dataset_folder, "merged_energy_data.csv")

# Read dataset
merged_data <- read_csv(merged_data_path, show_col_types = FALSE)

# Ensure 'year' column is numeric
merged_data <- merged_data %>% mutate(year = as.numeric(year))

# Sort data by country and year to maintain time order
merged_data <- merged_data %>% arrange(country, year)

# ---- Creating Derived Features ----

# 1️⃣ **Lagged Energy Consumption (1-year lag)**
merged_data <- merged_data %>%
  group_by(country) %>%
  mutate(lagged_energy = lag(primary_energy_consumption, order_by = year)) %>%
  ungroup()

# 2️⃣ **Moving Average (3-year window)**
merged_data <- merged_data %>%
  group_by(country) %>%
  mutate(moving_avg_energy = rollmean(primary_energy_consumption, k = 3, fill = NA, align = "right")) %>%
  ungroup()

# 3️⃣ **Year-over-Year Growth (%)**
merged_data <- merged_data %>%
  group_by(country) %>%
  mutate(yoy_growth = (primary_energy_consumption - lag(primary_energy_consumption, 1)) / lag(primary_energy_consumption, 1)) %>%
  ungroup()

# 4️⃣ **Decade Indicator**
merged_data <- merged_data %>%
  mutate(decade = floor(year / 10) * 10)

# 5️⃣ **Seasonal Indicators**
merged_data <- merged_data %>%
  mutate(month = month(as.Date(paste(year, "01", "01", sep = "-"))),
         is_winter = ifelse(month %in% c(12, 1, 2), 1, 0),
         is_summer = ifelse(month %in% c(6, 7, 8), 1, 0))

# 6️⃣ **Weather-Based Features (Only if Temperature Data Exists)**
if ("average_temperature" %in% colnames(merged_data)) {
  merged_data <- merged_data %>%
    mutate(temp_anomaly = average_temperature - mean(average_temperature, na.rm = TRUE),
           HDD = ifelse(average_temperature < 18, 18 - average_temperature, 0),
           CDD = ifelse(average_temperature > 24, average_temperature - 24, 0))
} else {
  # Create missing columns as NA if temperature data is unavailable
  merged_data$temp_anomaly <- NA
  merged_data$HDD <- NA
  merged_data$CDD <- NA
}

# 7️⃣ **Energy Efficiency Features**
merged_data <- merged_data %>%
  mutate(energy_intensity = primary_energy_consumption / gdp,
         energy_per_capita = primary_energy_consumption / population,
         renewable_share = renewables_consumption / primary_energy_consumption)

# 8️⃣ **Rolling Aggregations**
merged_data <- merged_data %>%
  group_by(country) %>%
  mutate(rolling_avg_5yr = rollmean(primary_energy_consumption, k = 5, fill = NA, align = "right"),
         rolling_std_5yr = rollapply(primary_energy_consumption, width = 5, FUN = sd, fill = NA, align = "right")) %>%
  ungroup()

# ---- Scaling and Normalizing Data ----

# Min-Max Scaling Function
min_max_scale <- function(x) {
  if (all(is.na(x))) return(x)  # If all values are NA, return as is
  return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

# Z-score Normalization Function
z_score_norm <- function(x) {
  if (all(is.na(x))) return(x)  # If all values are NA, return as is
  return((x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE))
}

# Define all possible numeric columns
numeric_columns <- c("primary_energy_consumption", "gdp", "population", "fossil_fuel_consumption",
                     "renewables_consumption", "energy_per_gdp", "lagged_energy", "moving_avg_energy", 
                     "yoy_growth", "energy_intensity", "energy_per_capita", "renewable_share",
                     "rolling_avg_5yr", "rolling_std_5yr", "average_temperature", "temp_anomaly", "HDD", "CDD")

# Keep only columns that exist in the dataset
numeric_columns <- numeric_columns[numeric_columns %in% colnames(merged_data)]

# Apply Scaling and Normalization only to available columns
merged_data_scaled <- merged_data %>%
  mutate(across(all_of(numeric_columns), min_max_scale, .names = "scaled_{.col}"),
         across(all_of(numeric_columns), z_score_norm, .names = "normalized_{.col}"))

# Save the transformed dataset
scaled_data_path <- file.path(dataset_folder, "feature_engineered_energy_data.csv")
write_csv(merged_data_scaled, scaled_data_path)

cat("Feature Engineering Completed! File saved at:", scaled_data_path, "\n")

