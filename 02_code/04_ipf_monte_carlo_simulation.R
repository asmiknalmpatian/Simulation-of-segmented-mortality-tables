
# Functions ---------------------------------------------------------------

run_ipf_monte_carlo_simulation <- function(country_code, population_file, deaths_file, states_file, basis_table, smoker_data_input, year = 2019, age_range = 20:75, iterations = 10) {
  
  # Load data 
  pop_data <- read.table(population_file, header = TRUE, skip = 2) %>% 
    filter(Year == year, Age %in% age_range)
  
  deaths_data <- read.table(deaths_file, header = TRUE, skip = 2) %>% 
    filter(Year == year, Age %in% age_range)
  
  state_data <- read_excel(states_file)
  
  basis_data <- read_delim(basis_table, delim = ";", escape_double = FALSE, trim_ws = TRUE, 
                            locale = locale(encoding = 'ISO-8859-1', decimal_mark = ","))  

  # Gender distribution
  male_pop <- (sum(pop_data$Male) / sum(pop_data$Total)) * 100
  female_pop <- (sum(pop_data$Female) / sum(pop_data$Total)) * 100 
  
  # Gender-age distribution
  pop_data$Female_distribution <- (pop_data$Female / sum(pop_data$Female)) * 100
  pop_data$Male_distribution <- (pop_data$Male / sum(pop_data$Male)) * 100
  deaths_data$Female_distribution <- (deaths_data$Female / sum(deaths_data$Female)) * 100
  deaths_data$Male_distribution <- (deaths_data$Male / sum(deaths_data$Male)) * 100
  
  # Smoker distribution
  # Calculate smoker and non-smoker percentages for men and women 
  # Combine into a table
  smoker_data <- data.frame( 
    Yes = c(smoker_data_input["Male",], smoker_data_input["Female",]),
    No = c(100 - smoker_data_input["Male",], 100 - smoker_data_input["Female",] )  
  )
  rownames(smoker_data) <- c("Male", "Female")
  
   
  create_ipfp <- function(sex = "Male", smoker_data, pop_data, state_data, iter = 50, tol = 1e-5) {
    smoker <- setNames((as.numeric(smoker_data[sex, ]) / sum(as.numeric(smoker_data[sex, ])))*100, colnames(smoker_data))
    age <- setNames(pop_data[[paste(sex, "distribution", sep = "_")]], pop_data$Age)
    states <- setNames(((state_data$population / sum(state_data$population)) * 100), state_data$state)
    target <- list(smoker, age, states)
    descript <- list(1, 2, 3)
    names <- list(names(smoker), names(age), names(states))
    weight_init <- array(1, c(length(smoker), length(age), length(states)), dimnames = names)
    result <- Ipfp(weight_init, descript, target, iter = iter, print = TRUE, tol = tol)
    return(result)
  }
  
  # Generate IPFP results for both genders
  result_male <- create_ipfp(sex = "Male", smoker_data = smoker_data, pop_data = pop_data, state_data = state_data)
  result_female <- create_ipfp(sex = "Female", smoker_data = smoker_data, pop_data = pop_data, state_data = state_data)
  
  # Convert IPFP results to tidy format
  convert_to_tidy_df <- function(result) {
    x_hat <- result$x.hat
    tidy_data <- data.frame()
    for (region in dimnames(x_hat)[[3]]) {
      for (sex in dimnames(x_hat)[[1]]) {
        for (age in as.numeric(dimnames(x_hat)[[2]])) {
          tidy_data <- rbind(tidy_data, data.frame(
            smoker = sex,
            age = age,
            state = region,
            distribution = x_hat[sex, as.character(age), region]
          ))
        }
      }
    }
    return(tidy_data)
  }
  
  result_male_tidy <- convert_to_tidy_df(result_male)
  result_female_tidy <- convert_to_tidy_df(result_female)
  
  # Normalize Female distribution
  scaling_factor <- male_pop / female_pop
  result_female_tidy$distribution <- result_female_tidy$distribution * scaling_factor
  
  # Combine Male and Female Data
  result_male_tidy$gender <- "M"
  result_female_tidy$gender <- "F"
  combined_df <- rbind(result_male_tidy, result_female_tidy)
  combined_df$distribution <- (combined_df$distribution / sum(combined_df$distribution)) * 100
  
  if("smoker" %in% colnames(basis_data)) {
    # Merge with mortality rates
    combined_df_with_rates <- combined_df %>% 
      inner_join(basis_data, by = c("gender", "age", "smoker"))
  } else {
    # Merge with mortality rates
    combined_df_with_rates <- combined_df %>% 
      inner_join(basis_data, by = c("gender", "age"))
  }
  
  
  combined_df_with_rates$rates <- if("insured_rates" %in% colnames(combined_df_with_rates)) {
    combined_df_with_rates$insured_rates
  } else if("population_rates" %in% colnames(combined_df_with_rates)) {
    combined_df_with_rates$population_rates
  } else {
    combined_df_with_rates$rates
  }
  
  combined_df_with_rates$population <- combined_df_with_rates$distribution * 1000000
  combined_df_with_rates$lambda <- combined_df_with_rates$population * combined_df_with_rates$rates
  
  # Monte Carlo Simulation
  simulate_deaths <- function(data, iterations) {
    results <- data %>%
      rowwise() %>%
      mutate(simulated_deaths = list(rpois(iterations, lambda = lambda))) %>%
      ungroup() %>%
      tidyr::unnest(simulated_deaths) %>%
      mutate(iteration = rep(1:iterations, each = nrow(data)))
    return(results)
  }
  
  
  # Add country information to results
  simulation_results <- simulate_deaths(data = combined_df_with_rates, iterations)
  simulation_results$country <- country_code
  
  simulation_results <- simulation_results %>% 
    mutate(simulated_rates = simulated_deaths / population) %>% 
    select(iteration, country, state, age, gender, smoker, population, simulated_deaths, simulated_rates)
    
  # Aggregate and Analyze Results
  aggregated_results <- simulation_results %>%
    group_by(smoker, gender, state, age) %>%
    summarize(
      mean_rates = mean(simulated_deaths / population),
      sd_deaths = sd(simulated_deaths / population),
      total_deaths = sum(simulated_deaths / population),
      lower_ci = quantile(simulated_deaths / population, 0.025),
      upper_ci = quantile(simulated_deaths / population, 0.975),
      .groups = "drop"
    )
  
  # Visualization
  gg <- ggplot(aggregated_results, aes(x = age, y = mean_rates, color = smoker, group = smoker)) +
    geom_line(size = 1) +
    geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = smoker), alpha = 0.2) +
    facet_wrap(~gender + state, scales = "free_y") +
    labs(
      title = paste("Simulated mortality rates for", country_code),
      x = "Age",
      y = "Simulated mortality rates",
      color = "smoker",
      fill = "smoker"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      strip.text = element_text(size = 12)
    ) 
  
  
  # Return both the results and the country code
  return(list(simulation_results = simulation_results, plot = gg, country_code = country_code))
}

combine_country_analysis <- function(...) {
  # Collect all input arguments (list of country analysis results)
  country_results <- list(...)
  
  # Initialize empty data frame for combined results
  combined_results <- data.frame()
  
  # Loop through each country's result and add a Country column
  for (res in country_results) {
    country_code <- res$country_code
    data <- res$simulation_results 
    combined_results <- rbind(combined_results, data)
  }
  
  return(combined_results)
}
 
