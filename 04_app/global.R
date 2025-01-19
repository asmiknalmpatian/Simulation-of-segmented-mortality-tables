library(tidyverse)

# Load the dataset
simulation_results <- read_csv("./data/simulation_results.csv") %>%
  mutate(SimulatedRates = SimulatedDeaths / Population)

# Optional: Print a message to confirm the data was loaded
message("Dataset loaded successfully!")