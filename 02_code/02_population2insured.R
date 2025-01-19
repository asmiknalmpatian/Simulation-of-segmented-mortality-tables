
# Function ----------------------------------------------------------------

population2insured <- function(
  target_country, source_country, population_mortality_path
) {
  # Load and prepare population mortality data
  population_mortality <- read_csv(population_mortality_path) %>% 
    filter(country %in% c("DE", "ITA", target_country))
  
  # Load and blend DAV rates (combine smoker and non-smoker)
  dav_rates_blended <- read_delim(
    "./01_data/de_input/de_dav_rates.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE,
    locale = locale(encoding = 'ISO-8859-1', decimal_mark = ",")
  ) %>%
    mutate(insured_rates = insured_rates * ifelse(smoker == "Yes", 0.239, (1 - 0.239))) %>% # smoker prevalence
    group_by(country, age, gender) %>%
    summarize(insured_rates = sum(insured_rates), .groups = "drop") 
  
  # Load ANIA rates
  ania_rates_original <- read_csv("./01_data/ita_input/ita_ania_rates.csv") %>% 
    select(country, age, gender, insured_rates)  
  
  # Combine rates
  insured_data <- bind_rows(dav_rates_blended, ania_rates_original) 
  
  # Merge datasets
  hmd_plus_insured_data <- population_mortality %>%
    left_join(insured_data, by = c("country", "age", "gender")) %>%  
    mutate(
      gender = as.factor(gender),
      age_gender = as.factor(interaction(age, gender)),
      insured_deaths = insured_rates * 100000,
      population_deaths = population_rates * 100000,
      population = 100000
    ) %>%
    filter(age %in% 20:75)
 
  # Fit the model using the dynamic formula
  mod <- gam(
    round(insured_deaths) ~ s(age, k = 20) + s(round(population_deaths), by = gender, k = 20),
    data = hmd_plus_insured_data %>% filter(country %in% c(source_country)),
    family = poisson(link = "log"),
    offset = log(population)
  )
  
  # Predict insured rates for all countries
  hmd_plus_insured_data <- hmd_plus_insured_data %>%
    mutate(predicted_insured_rates = as.numeric(predict(mod, newdata = ., type = "response")))
  
  # Filter predictions for the target country
  target_country_predictions <- hmd_plus_insured_data %>%
    filter(country == target_country) %>%
    select(country, age, gender, predicted_insured_rates) %>%
    rename(insured_rates = predicted_insured_rates) %>% 
    mutate(deaths = insured_rates * 100000)
  
  #write_csv(x = target_country_predictions, file = output_path)
  
  # Generate the plot for all countries
  p <- hmd_plus_insured_data %>%
    ggplot(aes(x = age)) +
    geom_line(aes(y = insured_rates, color = "Insured Rates", linetype = "Actual"), size = 1) +
    geom_line(aes(y = population_rates, color = "Population Rates", linetype = "Actual"), size = 1) +
    geom_line(aes(y = predicted_insured_rates, color = "Predicted Insured Rates", linetype = "Predicted"), size = 1) +
    facet_wrap(gender ~ country, ncol = 3) +
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
  
  # Return the plot and model summary
  list(
    plot = p,
    model_summary = summary(mod),
    pred = target_country_predictions
  )
}
 
