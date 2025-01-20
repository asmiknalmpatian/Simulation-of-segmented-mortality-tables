
# Packages required -------------------------------------------------------

library(tidyverse)  # Includes ggplot2, dplyr, tidyr, readr, readxl
library(demography)
library(StMoMo)
library(mgcv)       # For GAM modeling
library(mipfp)      # For Iterative Proportional Fitting
library(countrycode)
library(readxl)
library(testthat)
options(warn=-1)

# Load all functions ------------------------------------------------------

source("02_code/01_get_population_mortality.R")
source("02_code/02_population2insured.R")
source("02_code/03_expand_mortality_table_ipf_smoker.R")
source("02_code/04_ipf_monte_carlo_simulation.R")
source("02_code/05_evaluation.R")
 
setwd("D:/UNI/06_Promotion/4_Paper_Simulation/Simulation-of-segmented-mortality-tables")

# Application of functions ------------------------------------------------

# 1. Germany (insured) --------------------------------------------------------------

result_de <- run_ipf_monte_carlo_simulation(
  country_code = "Germany (insured)",
  population_file = "./01_data/population_mortality/hmd/DEUTNP/population.txt",
  deaths_file = "./01_data/population_mortality/hmd/DEUTNP/deaths.txt",
  states_file = "./01_data/de_input/germany_states.xlsx",
  basis_table = "./01_data/de_input/de_dav_rates.csv",
  smoker_data_input = data.frame(
    Yes = c(27, 20.8), 
    row.names = c("Male", "Female")
  ),
  year = 2019,
  age_range = 20:75,
  iterations = 50
)

print(result_de$country_code)
head(result_de$simulation_results)
print(result_de$plot)

# 2. Italy (insured) ----------------------------------------------------------------

# 1. Data load

ita_ania_rates <- read_delim("./01_data/ita_input/ita_ania_rates.csv", ",", escape_double = FALSE, trim_ws = TRUE) %>% 
  filter(age %in% 20:80) %>% 
  mutate(deaths = insured_rates * 100000)

# 2. Extend insured mortality table from age-gender to age-gender-smoker distinction

ita_rates_smoker <- expand_mortality_table_ipf_smoker(data = ita_ania_rates,  
                                                          smoker_hazard = 0.014, 
                                                          non_smoker_hazard = 0.01, 
                                                          subtitle = "Italian, insured population, source: ANIA")

head(ita_rates_smoker$adjusted_data)
print(ita_rates_smoker$plot)

write.table(ita_rates_smoker$adjusted_data, "./01_data/ita_input/ita_rates_smoker.csv", sep = ";", row.names = FALSE, col.names = TRUE, quote = FALSE, dec = ",") 

# 3. Run simulation with IPF-extended population and Monte Carlo
 
result_ita <- run_ipf_monte_carlo_simulation(
  country_code = "Italy (insured)",
  population_file = "./01_data/population_mortality/hmd/ITA/population.txt",
  deaths_file = "./01_data/population_mortality/hmd/ITA/deaths.txt",
  states_file = "./01_data/ita_input/italy_states.xlsx",
  basis_table = "./01_data/ita_input/ita_rates_smoker.csv",
  smoker_data_input = data.frame(
    Yes = c(27.8, 16.4),
    row.names = c("Male", "Female")
  ),
  year = 2019,
  age_range = 20:75,
  iterations = 50
) 

print(result_ita$country_code)
head(result_ita$simulation_results)
print(result_ita$plot)

# 3. Switzerland (population) ----------------------------------------------------------

# 1. Data load: Get population data. Before adding new countries, store required input data from HMD or WHO.

population_mortality_che <- get_population_mortality(country_codes = c("DEUTNP", "ITA", "CHE"), 
                                                 results_file = "./01_data/population_mortality/population_mortality.csv") %>% 
  filter(country == "CHE", age %in% 20:80) %>%  
  mutate(deaths = population_rates * 100000)

# 2. Extend insured mortality table from age-gender to age-gender-smoker distinction

che_rates_smoker_pop <- expand_mortality_table_ipf_smoker(data = population_mortality_che,
                                                         smoker_hazard = 0.01099, 
                                                         non_smoker_hazard = 0.00286, 
                                                         subtitle = "Swiss, overall population, source = HMD")

head(che_rates_smoker_pop$adjusted_data)
print(che_rates_smoker_pop$plot)

write.table(che_rates_smoker_pop$adjusted_data, "./01_data/che_input/che_rates_smoker_pop.csv", sep = ";", row.names = FALSE, col.names = TRUE, quote = FALSE, dec = ",") 
 
# 3. Run simulation with IPF-extended population and Monte Carlo

result_che_pop <- run_ipf_monte_carlo_simulation(
  country_code = "Switzerland (population)",
  population_file = "./01_data/population_mortality/hmd/CHE/population.txt",
  deaths_file = "./01_data/population_mortality/hmd/CHE/deaths.txt",
  states_file = "./01_data/che_input/swiss_states.xlsx",
  basis_table = "./01_data/che_input/che_rates_smoker_pop.csv",   
  smoker_data_input = data.frame(
    Yes = c(31, 23.3), 
    row.names = c("Male", "Female")
  ),
  year = 2019,
  age_range = 20:75,
  iterations = 50
)
 
print(result_che_pop$country_code)
head(result_che_pop$simulation_results)
print(result_che_pop$plot)


# 4. Switzerland (insured*) ----------------------------------------------------------


# 1. Transform the basic mortality table from CHE to from insured to population data. Assumption: CHE and DE have similar ratio between insured and overall population mortality.

pred_insured_mortality_che <- population2insured(
  target_country = "CHE",  # Specify the target country
  source_country = "DE", # Specify the assumption of similar ratio insured vs. overall population mortality
  population_mortality_path = "./01_data/population_mortality/population_mortality.csv"
)

print(pred_insured_mortality_che$plot)

# 2. Extend insured mortality table from age-gender to age-gender-smoker distinction

che_rates_smoker_ins <- expand_mortality_table_ipf_smoker(data = pred_insured_mortality_che$pred,
                                                          smoker_hazard = 0.01099, 
                                                          non_smoker_hazard = 0.00286, 
                                                          subtitle = "Swiss, predicted insured, source = HMD & DAV*")

head(che_rates_smoker_ins$adjusted_data)
print(che_rates_smoker_ins$plot)

write.table(che_rates_smoker_ins$adjusted_data, "./01_data/che_input/che_rates_smoker_ins.csv", sep = ";", row.names = FALSE, col.names = TRUE, quote = FALSE, dec = ",") 

# 3. Run simulation with IPF-extended population and Monte Carlo

result_che_ins <- run_ipf_monte_carlo_simulation(
  country_code = "Switzerland (insured*)",
  population_file = "./01_data/population_mortality/hmd/CHE/population.txt",
  deaths_file = "./01_data/population_mortality/hmd/CHE/deaths.txt",
  states_file = "./01_data/che_input/swiss_states.xlsx",
  basis_table = "./01_data/che_input/che_rates_smoker_ins.csv",   
  smoker_data_input = data.frame(
    Yes = c(31, 23.3), 
    row.names = c("Male", "Female")
  ),
  year = 2019,
  age_range = 20:75,
  iterations = 50
)

print(result_che_ins$country_code)
head(result_che_ins$simulation_results)
print(result_che_ins$plot)

# Save combined results ------------------------------------------------------------

simulated_results_full <- combine_country_analysis(result_de, result_ita, result_che_pop)
write_csv(simulated_results_full, "./03_results/simulated_results_full.csv")
write_csv(simulated_results_full, "./04_app/data/simulated_results_full.csv")

combined_results <- simulated_results_full %>% 
  group_by(country, age, gender, smoker, state) %>%
  summarize(
    simulated_deaths = mean(simulated_deaths),
    simulated_population = mean(population),
    simulated_rates = (simulated_deaths / simulated_population),
    .groups = "drop"
  )
write_csv(combined_results, "./03_results/simulated_results_aggregated.csv")
 

# Visual evaluation ----------------------------------------------------------------

plot_country_rates(country_name = "DE", data = final_results, aggregate_by_smoker = T)
plot_country_rates(country_name = "ITA", data = final_results, aggregate_by_smoker = T)
plot_country_rates(country_name = "CHE", data = final_results, aggregate_by_smoker = T)


# Tests -------------------------------------------------------------------

test_file("02_code/06_tests.R")
 
