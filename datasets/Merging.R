# Load required libraries
library(dplyr)
library(tidyr)
library(readr)

# Set the dataset folder path
dataset_folder <- "C:/git/Energy-Consumption-Forecasting/datasets"

# List of cleaned datasets to merge
datasets <- list(
  "cleaned_wb_gdp_data.csv",
  "cleaned_wb_renewable_energy.csv",
  "cleaned_wb_population_data.csv",
  "cleaned_wb_energy_use.csv",
  "cleaned_kaggle_energy_data.csv",
  "cleaned_kaggle_weather.csv"
)

# Function to check and standardize column names
read_and_prepare <- function(file_name) {
  file_path <- file.path(dataset_folder, file_name)
  df <- read_csv(file_path, show_col_types = FALSE)
  
  # Standardize column names
  colnames(df) <- tolower(gsub("\\s+", "_", colnames(df)))  # Convert to lowercase and replace spaces with "_"
  
  # Identify possible variations of country and year columns
  possible_country_names <- c("country", "country_name", "nation")
  possible_year_names <- c("year", "yr", "fiscal_year")
  
  # Rename columns if necessary
  for (col in possible_country_names) {
    if (col %in% colnames(df)) {
      colnames(df)[colnames(df) == col] <- "country"
      break
    }
  }
  
  for (col in possible_year_names) {
    if (col %in% colnames(df)) {
      colnames(df)[colnames(df) == col] <- "year"
      break
    }
  }
  
  # Check if 'country' and 'year' exist
  if (!"country" %in% colnames(df) | !"year" %in% colnames(df)) {
    warning(paste("Skipping", file_name, "- Missing 'country' or 'year' column"))
    return(NULL)  # Skip this dataset
  }
  
  return(df)
}

# Read and prepare all datasets
data_list <- lapply(datasets, read_and_prepare)

# Remove NULL entries (datasets without 'country' or 'year')
data_list <- data_list[!sapply(data_list, is.null)]

# Merge datasets using full join
if (length(data_list) > 0) {
  merged_data <- Reduce(function(x, y) full_join(x, y, by = c("country", "year")), data_list)
  
  # Handle missing values
  merged_data <- merged_data %>% mutate(across(everything(), ~ifelse(is.na(.), NA, .)))
  
  # Save the merged dataset
  merged_file_path <- file.path(dataset_folder, "merged_energy_data.csv")
  write_csv(merged_data, merged_file_path)
  
  cat("Merged dataset saved at:", merged_file_path, "\n")
} else {
  cat("No datasets could be merged due to missing 'country' or 'year' columns.\n")
}

