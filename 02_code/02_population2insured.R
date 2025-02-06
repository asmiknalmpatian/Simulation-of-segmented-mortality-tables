  
# Function ----------------------------------------------------------------

population2insured <- function(
    target_country, source_country, population_mortality_path, output_path
) {
  # Load and prepare population mortality data
  population_mortality <- read_csv(population_mortality_path) %>% 
    filter(country %in% c("DE", "ITA", target_country))
  
  # Load and blend DAV rates (combine smoker and non-smoker)
  dav_rates_blended <- read_delim(
    "./01_data/de_input/de_dav_rates.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE,
    locale = locale(encoding = 'ISO-8859-1', decimal_mark = ",")
  ) %>%
    # mutate(insured_rates = insured_rates * ifelse(smoker == "Yes", 0.239, (1 - 0.239))) %>% # smoker prevalence
    group_by(country, age, gender, smoker) %>%
    summarize(insured_rates = sum(insured_rates), .groups = "drop") 
  # 
  # # Load ANIA rates
  ania_rates_original <- read_delim("01_data/ita_input/ita_rates_smoker.csv", 
                                    delim = ";", escape_double = FALSE, trim_ws = TRUE,
                                    locale = locale(encoding = 'ISO-8859-1', decimal_mark = ",")) %>% 
    select(country, age, gender, smoker,  rates) %>% rename(insured_rates = rates)
  
  # Combine rates
  insured_data <- bind_rows(dav_rates_blended, ania_rates_original)
  
  # Expand population data for target country to include smoker categories
  population_mortality_expanded <- population_mortality %>%
    tidyr::crossing(smoker = c("Yes", "No"))
  
  # Merge datasets
  hmd_plus_insured_data <- population_mortality_expanded %>%
    left_join(insured_data, by = c("country", "age", "gender", "smoker")) %>%  
    mutate(
      gender = as.factor(gender),
      age_gender = as.factor(interaction(age, gender)),
      smoker_gender = as.factor(interaction(smoker, gender)),
      insured_deaths = insured_rates * 100000,
      population_deaths = population_rates * 100000,
      population = 100000
    ) %>%
    filter(age %in% 20:75)
  
  # Fit the model using the dynamic formula
  mod <- gam(
    round(insured_deaths) ~ s(age, k = 20) + s(round(population_deaths), by = smoker_gender, k = 20),
    data = hmd_plus_insured_data %>% filter(country %in% c(source_country)),
    family = poisson(link = "log"),
    offset = log(population)
  )
  
  # Predict insured rates for all countries
  hmd_plus_insured_data <- hmd_plus_insured_data %>%
    mutate(predicted_insured_rates = as.numeric(predict(mod, newdata = ., type = "response"))) 
  
  # Generate the plot for all countries
  p <- hmd_plus_insured_data %>%
    filter(country %in% c(target_country, source_country)) %>% 
    ggplot(aes(x = age)) +
    geom_line(aes(y = insured_rates, color = "Insured Rates", linetype = "Actual"), size = 1) +
    geom_line(aes(y = population_rates, color = "Population Rates", linetype = "Actual"), size = 1) +
    geom_line(aes(y = predicted_insured_rates, color = "Predicted Insured Rates", linetype = "Predicted"), size = 1) +
    facet_wrap(country ~ smoker_gender, ncol = 4) +
    scale_color_manual(
      name = "Rate Type",
      values = c(
        "Predicted Insured Rates" = "blue",
        "Insured Rates" = "red",
        "Population Rates" = "black"
      )
    ) +
    scale_linetype_manual(
      name = "Line Type",
      values = c(
        "Predicted" = "dashed",
        "Actual" = "solid"
      )
    ) +
    labs(
      title = "Rates by Age for All Countries",
      x = "Age",
      y = "Rates"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.box = "vertical",
      legend.title = element_text(face = "bold")
    )
  
  p
  
  # Filter predictions for the target country
  target_country_predictions <- hmd_plus_insured_data %>%
    filter(country == target_country) %>%
    rename(rates = predicted_insured_rates) %>%  
    select(country, gender, age, smoker, rates) 
  
  write.table(target_country_predictions, output_path, sep = ";", row.names = FALSE, col.names = TRUE, quote = FALSE, dec = ",") 
  
  
  # Return the plot and model summary
  list(
    plot = p,
    model_summary = summary(mod),
    pred = target_country_predictions
  )
}