# Load required libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(readr)

# Set dataset path
dataset_folder <- "C:/git/Energy-Consumption-Forecasting/datasets"
merged_data_path <- file.path(dataset_folder, "merged_energy_data.csv")

# Read dataset
merged_data <- read_csv(merged_data_path, show_col_types = FALSE)

# Extract unique country names
country_list <- unique(merged_data$country)

# Define available energy types for trends visualization
energy_types <- c("primary_energy_consumption", "fossil_fuel_consumption", 
                  "renewables_consumption", "oil_consumption", "nuclear_consumption")

# Select relevant correlation factors
correlation_factors <- c("gdp", "population", "fossil_fuel_consumption", 
                         "renewables_consumption", "energy_per_gdp", 
                         "average_temperature")

# Remove factors that do not exist in the dataset
correlation_factors <- correlation_factors[correlation_factors %in% colnames(merged_data)]

# Define UI for Shiny app
ui <- fluidPage(
  titlePanel("📊 Interactive Energy Consumption Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      h3("🔹 Task 1.1: Energy Consumption Trends"),
      selectInput("selected_country", "🌍 Select Primary Country:", choices = country_list, selected = "United States"),
      selectInput("compare_country", "🔄 Compare with Another Country (Optional):", choices = c("None", country_list), selected = "None"),
      selectInput("selected_energy", "⚡ Select Energy Type:", choices = energy_types, selected = "primary_energy_consumption"),
      
      hr(),
      h3("🔹 Task 1.2: Correlation Analysis"),
      selectInput("selected_var", "📌 Select a factor to compare with Energy Consumption:", choices = correlation_factors, selected = "gdp"),
      textOutput("corr_value"),
      
      hr(),
      h4("📈 Interpretation of Correlation Values:"),
      p("• **Above 0.7 (or below -0.7):** Strong correlation (major impact)"),
      p("• **Between 0.4 and 0.7 (or -0.4 to -0.7):** Moderate correlation"),
      p("• **Below 0.4:** Weak or no significant relationship")
    ),
    
    mainPanel(
      h3("📊 Energy Consumption Trends (Task 1.1)"),
      plotOutput("trendPlot"),
      
      hr(),
      
      h3("📈 Energy Consumption Correlation Analysis (Task 1.2)"),
      plotOutput("scatterPlot"),
      
      hr(),
      
      h3("📘 How Correlation is Calculated"),
      p("The correlation coefficient (r) measures the strength and direction of a relationship between two variables. It is calculated using Pearson’s correlation formula:"),
      
      tags$pre("r = Σ [(Xi - X̄)(Yi - Ȳ)] / sqrt(Σ (Xi - X̄)² * Σ (Yi - Ȳ)²)"),
      
      p("Where:"),
      p("• Xi, Yi = Individual data points for variables X and Y."),
      p("• X̄, Ȳ = Mean of X and Y."),
      p("• Σ = Summation across all observations."),
      
      p("Correlation values range from **-1 to +1**:"),
      p("• **+1** → Perfect positive correlation (as X increases, Y increases)."),
      p("• **0** → No correlation (X and Y are independent)."),
      p("• **-1** → Perfect negative correlation (as X increases, Y decreases).")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Task 1.1: Energy Consumption Trends Visualization (With Comparison)
  output$trendPlot <- renderPlot({
    
    # Filter dataset for selected country
    country_data <- merged_data %>%
      filter(country == input$selected_country & !is.na(.data[[input$selected_energy]])) %>%
      arrange(year) %>%
      mutate(Country = input$selected_country)  # Add identifier column
    
    # If a second country is selected, add it to the dataset
    if (input$compare_country != "None") {
      compare_data <- merged_data %>%
        filter(country == input$compare_country & !is.na(.data[[input$selected_energy]])) %>%
        arrange(year) %>%
        mutate(Country = input$compare_country)  # Add identifier column
      
      # Combine both datasets
      country_data <- bind_rows(country_data, compare_data)
    }
    
    # Create time series plot with comparison
    ggplot(country_data, aes(x = year, y = .data[[input$selected_energy]], color = Country)) +
      geom_line(size = 1) +
      geom_point(size = 2, alpha = 0.6) +
      labs(title = paste("Energy Consumption Trends:", input$selected_energy),
           x = "Year",
           y = input$selected_energy,
           color = "Country") +
      theme_minimal()
  })
  
  # Task 1.2: Correlation Analysis Scatter Plot
  output$scatterPlot <- renderPlot({
    
    # Ensure selected variable exists
    if (input$selected_var %in% colnames(merged_data)) {
      
      # Remove NA values
      clean_data <- merged_data %>%
        filter(!is.na(primary_energy_consumption), !is.na(.data[[input$selected_var]]))
      
      # Scatter plot with regression line
      ggplot(clean_data, aes_string(x = input$selected_var, y = "primary_energy_consumption")) +
        geom_point(alpha = 0.5) +
        geom_smooth(method = "lm", color = "blue") +
        labs(title = paste("Energy Consumption vs", input$selected_var),
             x = input$selected_var,
             y = "Primary Energy Consumption (TWh)") +
        theme_minimal()
    }
  })
  
  # Display correlation value with explanation
  output$corr_value <- renderText({
    if (input$selected_var %in% colnames(merged_data)) {
      
      clean_data <- merged_data %>%
        filter(!is.na(primary_energy_consumption), !is.na(.data[[input$selected_var]]))
      
      corr_val <- cor(clean_data$primary_energy_consumption, clean_data[[input$selected_var]], use = "complete.obs")
      
      # Interpretation
      if (corr_val > 0.7) {
        interpretation <- "Strong positive correlation - The factor has a significant positive impact on energy consumption."
      } else if (corr_val < -0.7) {
        interpretation <- "Strong negative correlation - The factor significantly reduces energy consumption."
      } else if (abs(corr_val) > 0.4) {
        interpretation <- "Moderate correlation - The factor somewhat affects energy consumption."
      } else {
        interpretation <- "Weak or no correlation - This factor does not strongly impact energy consumption."
      }
      
      paste("📊 Correlation:", round(corr_val, 3), "-", interpretation)
      
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)



