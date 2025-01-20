# Simulated mortality tables <img src="logo.png" align="right" width="200"/>
 
We provide simulated mortality tables for the insured population in Germany, Italy, and Switzerland, segmented by age, gender, smoker status, and state. The data adheres to high privacy standards, going beyond basic segmentation. The code allows for reproduction and extension of simulations to other countries and segmentation variables.

## Objective

The overall objective is to create a robust framework for simulating and analyzing mortality rates, accounting for demographic factors and smoker status, and to provide insights into the differences between population and insured mortality rates in different countries. This can be valuable for actuarial analysis, insurance product development, and demographic studies.

## Outcomes

The simulated dataset can be found in the `03_results` folder and dynamically accessed in the [dashboard](https://advancedmortalitymodeling.shinyapps.io/simulate_mortality_tables_v1/).

---
## Overview of main functions

| **Purpose**                                                                                                                                                                                                                       | **Inputs**                                                                                                                                                                                   | **Outputs**                                                                                                                                                                                                                      |
|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Process mortality data for specified countries, calculate rates using HMD/WHO data sources, and save results and plots.                                                                                                          | - `country_codes`: Vector of country codes to process. <br> - `results_file`: Path to the CSV file for results.                                                                                | - Data frame of combined mortality data. <br> - Saved mortality rates and plots for each country in specified directories.                                                                                                      |
| Convert population mortality rates to insured mortality rates for a target country using source country data, fit models, and generate plots comparing population and insured rates.                                              | - `target_country`: Target country for insured rates. <br> - `source_country`: Reference country. <br> - `population_mortality_path`: Path to mortality data.                                 | - Returns a list: <br> - `plot`: Visualizing rate comparisons. <br> - `model_summary`: Summary of fitted model. <br> - `pred`: Predicted insured rates for the target country.                                                  |
| Adjust mortality rates by age and gender, expand them by smoker status using Iterative Proportional Fitting (IPF), and generate smoker-specific rate plots.                                                                      | - `data`: Data frame with mortality data. <br> - `smoker_hazard`: Smoker hazard ratio. <br> - `non_smoker_hazard`: Non-smoker hazard ratio. <br> - `subtitle`: Subtitle for the plot (optional). | - Returns a list: <br> - `adjusted_data`: Mortality rates by smoker status. <br> - `plot`: Visualizing smoker-specific rates.                                                                                                  |
| Run a Monte Carlo simulation to estimate mortality rates using IPF, adjust for demographic distributions (age, gender, smoker status), and generate simulated mortality data and visualizations.                                  | - `country_code`: Country code. <br> - `population_file`: Path to population data. <br> - `deaths_file`: Path to deaths data. <br> - `states_file`: Path to state data. <br> - `basis_table`: Basis mortality rates. <br> - `smoker_data_input`: Smoker distribution. <br> - `year`: Year (default 2019). <br> - `age_range`: Age range (default 20-75). <br> - `iterations`: Number of iterations (default 10). | - Returns a list: <br> - `simulation_results`: Simulated mortality results. <br> - `plot`: Visualizing simulated mortality rates. <br> - `country_code`: Used country code.                                                     |
| Visualize mortality rates for a specified country, comparing simulated rates, population rates, and insured rates, with an option to aggregate by smoker status.                                                                 | - `country_name`: Name of the country. <br> - `data`: Mortality data frame. <br> - `aggregate_by_smoker`: Aggregate by smoker (default TRUE).                                                  | - Returns a ggplot object visualizing mortality rates by age, gender, and smoker status for the specified country.                                                                                                             |
| Validate data integrity and accuracy through assertions, ensuring column completeness, rate calculations, proportions, and consistency of smoker and state proportions and hazard ratios.                                         | - Global variables and data frames (`final_results`, `simulation_results`, etc.).                                                                                                              | - Performs tests to validate data integrity and accuracy, providing feedback through assertions.                                                                                                                              |

---

## Notes

- Ensure the `03_results` folder is accessible for outputs.
- R version used is 4.4.2.
- Extend and modify functions as needed for additional segmentation variables or countries.
- The insured mortality tables used in the study are:
  - **Germany**: [Raucher- und Nichtrauchersterbetafeln für Lebensversicherungen mit Todesfallcharakter](https://aktuar.de/de/wissen/fachinformationen/detail/raucher-und-nichtrauchersterbetafeln-fuer-lebensversicherungen-mit-todesfallcharakter/)
  - **Italy**: [Studio ANIA su nuove basi demografiche](https://www.ordineattuari.it/articoli/news/2014/2/studio-ania-su-nuove-basi-demografiche/)
- For detailed usage, refer to the code comments or reach out to the repository maintainers.














