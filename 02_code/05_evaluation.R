# Load required datasets ----
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

# Visualize all rates --------
# Do simulated rates vs. population (HMD) vs. insured (DAV, ANIA) look plausible?

plot_country_rates <- function(country_name, data, aggregate_by_smoker = TRUE) {
  # Filter for the selected country
  plot_data <- data %>%
    filter(country == country_name) %>%
    mutate(smoker = if (aggregate_by_smoker) smoker else "All") %>%
    group_by(country, age, gender, smoker) %>%
    summarize(
      Population = sum(simulated_population),
      simulated_deaths = sum(simulated_deaths),
      population_deaths = sum(population_deaths),
      insured_deaths = sum(insured_deaths),
      .groups = "drop"
    ) %>%
    filter(age %in% 20:70)
  
  # Calculate rates
  plot_data <- plot_data %>%
    mutate(
      simulated_rates = simulated_deaths / Population,
      population_rates = population_deaths / Population,
      insured_rates = insured_deaths / Population
    )
  
  # Reshape data for plotting
  data_long <- plot_data %>%
    pivot_longer(
      cols = c(simulated_rates, population_rates, insured_rates),
      names_to = "Rate_Type",
      values_to = "Rate"
    )
  
  # Dynamically set facets
  facet_formula <- if (aggregate_by_smoker) ~ smoker + gender else ~ gender 
  
  # Generate the plot
  ggplot(data_long, aes(x = age, y = Rate, group = Rate_Type, linetype = Rate_Type, color = Rate_Type)) +
    geom_line(size = 1) +
    facet_wrap(facet_formula, ncol = 2) +  # Use facet_wrap with 2 columns
    scale_linetype_manual(
      values = c(
        "simulated_rates" = "dashed",   # Dashed for simulated_rates
        "population_rates" = "solid",  # Solid for population_rates
        "insured_rates" = "dotted"    # Dotted for insured_rates
      )
    ) +
    scale_color_manual(
      values = c(
        "simulated_rates" = "orangered3",   # Blue for simulated_rates
        "population_rates" = "gray33",  # Red for population_rates
        "insured_rates" = "royalblue2"    # Green for insured_rates
      )
    ) +
    labs(
      title = paste("Rates by Age, Gender,", 
                    if (aggregate_by_smoker) "and Smoker Status" else "Aggregated by Gender and Age", 
                    "in", country_name),
      x = "Age",
      y = "Mortality rates",
      color = "Rate type",
      linetype = "Rate type"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10)
    )
}