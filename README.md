# Simulated Mortality Tables

We provide simulated mortality tables for the insured population in Germany, Italy, and Switzerland, segmented by Age, Gender, Smoker status, and State. The data adheres to high privacy standards, going beyond basic segmentation. The code allows for reproduction and extension of simulations to other countries and segmentation variables.

## Objective

The overall objective is to create a robust framework for simulating and analyzing mortality rates, accounting for demographic factors and smoker status, and to provide insights into the differences between population and insured mortality rates in different countries. This can be valuable for actuarial analysis, insurance product development, and demographic studies.

## Outcomes

The simulated dataset can be found in the `03_results` folder and dynamically accessed in the [dashboard](https://advancedmortalitymodeling.shinyapps.io/simulate_mortality_tables_v1/).

## Summary of Functions

### `get_population_mortality`

Processes mortality data for specified countries, differentiates between Human Mortality Database (HMD) and World Health Organization (WHO) data sources, and calculates mortality rates using models. Saves the results and plots.

### `population2insured`

Converts population mortality rates to insured mortality rates for a target country using source country data, fits models, and generates plots comparing actual population rates with predicted insured rates.

### `expand_mortality_table_ipf_smoker`

Adjusts mortality rates by age and gender, expands them by smoker status using Iterative Proportional Fitting (IPF), and generates plots for smoker-specific rates. Extensible to other variables beyond smoker status.

### `run_ipf_monte_carlo_simulation`

Runs a Monte Carlo simulation to estimate mortality rates using IPF, adjusts for demographic distributions (age, gender, smoker status), and generates simulated mortality data and visualizations.

### `plot_country_rates`

Visualizes mortality rates for a specified country, comparing simulated rates, population rates, and insured rates. Aggregates data by smoker status if needed.

### Test Suite

Validates the integrity and accuracy of the data processing and simulation results by checking column completeness, rate calculations, proportions, and consistency of smoker and state proportions, as well as hazard ratios.

---

## Inputs and Outputs

| **Function**                     | **Inputs**                                                                                                           | **Outputs**                                                                                                                                                                                                                                      |
|-----------------------------------|-----------------------------------------------------------------------------------------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `get_population_mortality`        | - `country_codes`: Vector of country codes to process. <br> - `results_file`: Path to the CSV file for results.         | - Returns a data frame of combined mortality data. <br> - Saves mortality rates and plots for each country in specified directories.                                                                                                           |
| `population2insured`              | - `target_country`: Target country for insured rates. <br> - `source_country`: Reference country. <br> - `population_mortality_path`: Path to mortality data. | - Returns a list containing: <br> - `plot`: Visualizing rate comparisons. <br> - `model_summary`: Summary of fitted model. <br> - `pred`: Data frame of predicted insured rates for the target country.                                          |
| `expand_mortality_table_ipf_smoker` | - `data`: Data frame with mortality data. <br> - `smoker_hazard`: Smoker hazard ratio. <br> - `non_smoker_hazard`: Non-smoker hazard ratio. <br> - `subtitle`: Subtitle for the plot (optional). | - Returns a list containing: <br> - `adjusted_data`: Mortality rates by smoker status. <br> - `plot`: Visualizing smoker-specific rates.                                                                                                        |
| `run_ipf_monte_carlo_simulation`  | - `country_code`: Country code. <br> - `population_file`: Path to population data. <br> - `deaths_file`: Path to deaths data. <br> - `states_file`: Path to state data. <br> - `basis_table`: Basis mortality rates. <br> - `smoker_data_input`: Smoker distribution. <br> - `year`: Year (default 2019). <br> - `age_range`: Age range (default 20-75). <br> - `iterations`: Number of iterations (default 10). | - Returns a list containing: <br> - `simulation_results`: Simulated mortality results. <br> - `plot`: Visualizing simulated mortality rates. <br> - `country_code`: Used country code.                                                           |
| `plot_country_rates`              | - `country_name`: Name of the country. <br> - `data`: Mortality data frame. <br> - `aggregate_by_smoker`: Aggregate by smoker (default TRUE).                 | - Returns a ggplot object visualizing mortality rates by age, gender, and smoker status for the specified country.                                                                                                                             |
| Test Suite                        | - Global variables and data frames (`final_results`, `simulation_results`, etc.).                                       | - Performs tests to validate data integrity and accuracy, providing feedback through assertions.                                                                                                        |

---

### Notes

- Ensure the `03_results` folder is accessible for outputs.
- Extend and modify functions as needed for additional segmentation variables or countries.
- For detailed usage, refer to the code comments or reach out to the repository maintainers.

