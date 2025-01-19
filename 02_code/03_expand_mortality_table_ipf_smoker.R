 
# Function to adjust mortality rates by age, and expand by smoker status using IPF (repeat for each gender)
expand_mortality_table_ipf_smoker <- function(data, smoker_hazard, non_smoker_hazard, subtitle = "") {
  # Validate inputs
  # Check if the required columns exist in the dataset
  if (!all(c("country", "gender", "age", "deaths") %in% colnames(data)) || 
      !any(c("insured_rates", "population_rates") %in% colnames(data))) {
    stop("The input dataframe must contain the columns: 'country', 'gender', 'age', 'deaths', and either 'insured_rates' or 'population_rates'.")
  }
 
  # Prepare seed array and targets for IPF (deaths)
  seed <- array(1, dim = c(2, nrow(data)))  # Initial seed for IPF
  target_row <- c(Yes = smoker_hazard, No = non_smoker_hazard)  # Smoker and non-smoker proportions
  target_col <- data$deaths  # Total deaths per age
  
  # Define target data and dimensions
  target_data <- list(target_row, target_col)
  target_dims <- list(1, 2)
  
  # Run IPF
  ipf_result <- Ipfp(seed, target_dims, target_data, na.target = TRUE)
  
  # Extract adjusted deaths for smokers and non-smokers
  adjusted_data <- data %>%
    mutate(
      Smoker_Deaths = ipf_result$x.hat[1, ],
      Non_Smoker_Deaths = ipf_result$x.hat[2, ]
    ) %>%
    # Transform into long format
    pivot_longer(
      cols = c(Smoker_Deaths, Non_Smoker_Deaths),
      names_to = "smoker",
      values_to = "adjusted_deaths"
    ) %>%
    mutate(
      smoker = ifelse(smoker == "Smoker_Deaths", "Yes", "No"), 
      rates = adjusted_deaths / 50000
    ) %>%
    select(country, gender, age, smoker, rates)
    
  data$rates <- if ("insured_rates" %in% colnames(data)) {
    data$insured_rates
  } else {
    data$population_rates
  }
  
  gg <-
    ggplot(adjusted_data,
           aes(
             x = age,
             y = rates,
             color = smoker,
             linetype = gender
           )) +
    geom_line(size = 1) +
    geom_line(
      data = data,
      aes(x = age, y = rates, linetype = gender),
      color = "grey",
      size = 1
    ) +
    labs(
      title = "Mortality Rates by Smoking Status and Gender",
      subtitle = subtitle,
      x = "Age",
      y = "Mortality Rates",
      color = "Smoker Status",
      linetype = "Gender"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      axis.title = element_text(size = 12),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 10)
    )
  gg
  
  # Return both the results and the country code
  return(list(adjusted_data = adjusted_data, plot = gg))
}
 
 
 
