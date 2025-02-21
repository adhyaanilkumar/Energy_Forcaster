# Load required libraries
library(dplyr)
library(tidyr)
library(readr)

# Set the dataset folder path
dataset_folder <- "C:/git/Energy-Consumption-Forecasting/datasets"

# List of datasets to clean
datasets <- list(
  "wb_gdp_data.csv",
  "wb_renewable_energy.csv",
  "wb_population_data.csv",
  "wb_energy_use.csv",
  "kaggle_energy_data.csv",
  "kaggle_weather.csv"
)

# Function to clean datasets
clean_data <- function(file_name) {
  # Construct full file path
  file_path <- file.path(dataset_folder, file_name)
  
  # Read the dataset
  df <- read_csv(file_path, show_col_types = FALSE)
  
  # 1. Remove duplicate rows
  df <- df %>% distinct()
  
  # 2. Handle missing values
  df <- df %>% mutate(across(everything(), ~ifelse(is.na(.), NA, .)))  # Preserve structure
  
  # 3. Convert numeric columns to correct format
  numeric_cols <- sapply(df, is.character) & grepl("^[0-9]+(\\.[0-9]+)?$", df, perl=TRUE)
  df[numeric_cols] <- lapply(df[numeric_cols], as.numeric)
  
  # 4. Save cleaned dataset
  cleaned_file_path <- file.path(dataset_folder, paste0("cleaned_", file_name))
  write_csv(df, cleaned_file_path)
  
  cat(paste("Cleaned file saved:", cleaned_file_path, "\n"))
}

# Loop through each dataset and clean it
for (file in datasets) {
  clean_data(file)
}

cat("All datasets have been cleaned and saved.\n")
