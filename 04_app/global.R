library(tidyverse)
library(shiny) 
library(DT) 

# Load the dataset 
simulation_results <- read_csv("./data/simulated_results_full.csv") 

# Optional: Print a message to confirm the data was loaded
message("Dataset loaded successfully!")