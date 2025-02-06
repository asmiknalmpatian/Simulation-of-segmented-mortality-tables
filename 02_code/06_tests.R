# Packages required -------------------------------------------------------

library(testthat)
library(dplyr)
library(readxl)

# Load required datasets ----
setwd("D:/UNI/06_Promotion/4_Paper_Simulation/Simulation-of-segmented-mortality-tables")


simulation_results <- read_csv("./03_results/simulated_results_aggregated.csv")
population_mortality <- read_csv("./01_data/population_mortality/population_mortality.csv") 

insured_rates <- rbind(
  read_delim("./01_data/de_input/de_dav_rates.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE, 
             locale = locale(encoding = 'ISO-8859-1', decimal_mark = ",")) ,
  read_delim("./01_data/ita_input/ita_rates_smoker.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE, 
             locale = locale(encoding = 'ISO-8859-1', decimal_mark = ",")) %>% 
    rename(insured_rates = rates),
  read_delim("./01_data/che_input/che_rates_smoker_ins.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE, 
             locale = locale(encoding = 'ISO-8859-1', decimal_mark = ",")) %>% 
    rename(insured_rates =rates)
)

final_results <- simulation_results %>%
  mutate(country = ifelse(country == "Germany (insured)", "DE", ifelse(country == "Italy (insured)", "ITA", "CHE"))) %>% 
  left_join(population_mortality, by = c("country", "age", "gender")) %>%
  left_join(insured_rates, by = c("country", "age", "gender", "smoker")) %>%
  mutate(
    simulated_deaths = simulated_population * simulated_rates,
    population_deaths = simulated_population * population_rates,
    insured_deaths = simulated_population * insured_rates
  )

# Define expected values for testing
expected_smoker_proportions <- list(
  DE = list(M = 27, F = 20.8),
  ITA = list(M = 27.8, F = 16.4),
  CHE = list(M = 31, F = 23.3)
)

expected_state_proportions_files <- list(
  DE = "./01_data/de_input/germany_states.xlsx",
  ITA = "./01_data/ita_input/italy_states.xlsx",
  CHE = "./01_data/che_input/swiss_states.xlsx"
)

expected_hazard_ratios <- list(
  ITA = list(smoker = 0.014, non_smoker = 0.01),
  CHE = list(smoker = 0.01099, non_smoker = 0.00286)
)

# Test 1: Basic unit tests ----

test_that("Column completeness checks", {
  # Check that the expected columns exist in final_results
  required_cols <- c("country", "age", "gender", "smoker", "simulated_rates", 
                     "population_rates", "insured_rates", "simulated_deaths", 
                     "population_deaths", "insured_deaths", "simulated_population")
  expect_true(all(required_cols %in% colnames(final_results)))
})

test_that("Rate calculations are accurate", {
  # Test a specific age-gender-smoker group for known simulated rate
  sample_result <- final_results %>%
    filter(country == "ITA", age == 30, gender == "M", smoker == "Yes")
  
  expect_equal(
    sample_result$simulated_deaths / sample_result$simulated_population,
    sample_result$simulated_rates,
    tolerance = 1e-6
  )
})

test_that("Proportions add up to 100%", {
  # Verify population proportions by gender add up to 100% for each country
  proportions <- final_results %>%
    group_by(country) %>%
    summarize(total_population = sum(simulated_population), .groups = "drop") %>%
    group_by(country) %>%
    mutate(proportion = 100 * total_population / sum(total_population))
  
  expect_true(all(abs(proportions$proportion - 100) < 1e-6))
})


test_that("Data integrity is maintained", {
  # Ensure no negative values exist in the critical numeric columns
  numeric_cols <- c("simulated_population", "simulated_deaths", 
                    "population_deaths", "insured_deaths")
  
  for (col in numeric_cols) {
    expect_true(all(final_results[[col]] >= 0))
  }
})



# Test 2: Smoker and Non-Smoker Proportions by Gender ----
test_that("Smoker and non-smoker proportions by gender are correct", {
  # Calculate proportions
  proportions <- final_results %>%
    group_by(country, gender, smoker) %>%
    summarize(total_population = sum(simulated_population), .groups = "drop") %>%
    group_by(country, gender) %>%
    mutate(proportion = 100 * (total_population / sum(total_population)))
  
  for (c in names(expected_smoker_proportions)) {
    for (g in c("M", "F")) {
      smoker_prop <- proportions %>%
        filter(country == c, gender == g, smoker == "Yes") %>%
        pull(proportion)
      
      expect_equal(
        smoker_prop,
        expected_smoker_proportions[[c]][[g]],
        tolerance = 1e-1, # Allow a small margin of error
        info = paste("Mismatch for smoker proportion in", c, "for gender", g)
      )
    }
  }
})

# Test 3: State Proportions by Country ----

test_that("State proportions match expected datasets", {
  # Load expected data for each country and compare proportions
  for (c in names(expected_state_proportions_files)) {
    expected_data <- read_excel(expected_state_proportions_files[[c]])
    
    calculated_proportions <- final_results %>%
      filter(country == c) %>%
      group_by(state) %>%
      summarize(total_population = sum(simulated_population), .groups = "drop") %>%
      mutate(proportion = 100 * (total_population / sum(total_population))) %>%
      select(state, proportion)
    
    # Match state-level proportions
    for (s in expected_data$state) {
      calculated_prop <- calculated_proportions %>%
        filter(state == s) %>%
        pull(proportion)
      
      expected_prop <- expected_data %>%
        filter(state == s) %>%
        pull(population) %>%
        {. / sum(expected_data$population) * 100}
      
      expect_equal(
        calculated_prop,
        expected_prop,
        tolerance = 1e-1,
        info = paste("Mismatch for state proportion in", c, "for state", s)
      )
    }
  }
})

# Test 4: Hazard Ratios Between Smokers and Non-Smokers ----

test_that("Hazard ratios between smokers and non-smokers match expectations", {
  # Calculate rates for smokers and non-smokers by country and gender
  proportions <- final_results %>%
    filter(!is.na(smoker)) %>%  # Exclude missing smoker status
    group_by(country, gender, smoker) %>%
    summarize(
      Total_Deaths = sum(simulated_deaths, na.rm = TRUE),
      Total_Population = sum(simulated_population, na.rm = TRUE),
      Total_Rate = Total_Deaths / Total_Population,
      .groups = "drop"
    ) %>%
    pivot_wider(
      names_from = smoker,
      values_from = Total_Rate,
      names_prefix = "Smoker_",
      values_fill = list(Total_Rate = NA)  # Keep NA for validation
    ) %>%  
    mutate(Smoker_Yes = ifelse(is.na(Smoker_Yes), 0, Smoker_Yes),
           Smoker_No = ifelse(is.na(Smoker_No), 0, Smoker_No)) %>% 
    group_by(country, gender) %>% 
    summarize(Smoker_Yes = sum(Smoker_Yes), 
              Smoker_No = sum(Smoker_No) 
    )  %>%
    mutate(Hazard_Ratio =Smoker_Yes / Smoker_No)
  proportions
  
  # Compare hazard ratios to expected values for each country and gender
  for (c in names(expected_hazard_ratios)) {
    for (g in c("M", "F")) {
      calculated_ratio <- proportions %>%
        filter(country == c, gender == g) %>%
        pull(Hazard_Ratio)
      
      expected_ratio <- expected_hazard_ratios[[c]]$smoker / expected_hazard_ratios[[c]]$non_smoker
      
      expect_equal(
        calculated_ratio,
        expected_ratio,
        tolerance = 1e-1,
        info = paste("Mismatch for hazard ratio in", c, "for gender", g)
      )
    }
  }
})

