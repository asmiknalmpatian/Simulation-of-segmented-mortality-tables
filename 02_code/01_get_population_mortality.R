 get_population_mortality <- function(country_codes, results_file) { 
  
  
  if (file.exists(results_file)) {
    # Read the existing data
    existing_data <- read.csv(results_file)
    
    # Ensure there is existing data and filter country codes not in the existing data
    if (nrow(existing_data) > 0) { 
      
      # Filter country_codes to include only those not present in existing_data$country
      country_codes <- country_codes[!country_codes %in% existing_data$country]
      
      # If "DE" is in existing data, remove it explicitly (optional step based on logic)
      if ("DE" %in% existing_data$country) {
        country_codes <- country_codes[country_codes != "DEUTNP"]
      }
      
      
    }
    
    # If no valid country codes remain after filtering, stop processing
    if (length(country_codes) == 0) {
      message("All countries already exist in population_mortality.csv. No new country codes to process. ")
      return(existing_data)

    } else {
      # Print out the countries that already exist
      message("The following country codes will be added. Rest already exists in the results file. ", paste(country_codes, collapse = ", "))
      
    }
  }
  
  # Function to extract mortality rates and create a data frame
  extract_mortality_rates <- function(forecast, gender, country_code) {
    rates <- forecast$rates
    age <- forecast$ages
    year <- forecast$years
    country <- rep(country_code, length(age))
    gender_vector <- rep(gender, length(age))
    
    data.frame(country = country, age = age, gender = gender_vector, rates = rates)
  }
  
  # 1. Check if HMD or WHO
  
  # Loop over each country code
  for (country_code in country_codes) {
    
    if (country_code %in% c("AUS", "AUT", "BLR", "BEL", "BGR", "CAN", "CHL", "CZE", "DNK", 
                            "EST", "FIN", "FRATNP", "FRACNP", "DEUTNP", "DEUTE", "DEUTW", "GRC", "HUN", "ISL", 
                            "IRL", "ISR", "ITA", "JPN", "LVA", "LTU", "LUX", "NLD", "NZL_NP", "NZL_MA", "NZL_NM", 
                            "NOR", "POL", "PRT", "RUS", "SVK", "SVN", "ESP", "SWE", "CHE", "TWN", "GBR_NP", 
                            "GBRTENW", "GBRCENW", "GBR_SCO", "GBR_NIR", "USA", "UKR")) {
      print(paste("Processing HMD data for", country_code))

      # Create the country-specific subfolder if it doesn't exist
      dir.create(file.path("./01_data/population_mortality/hmd", country_code), recursive = TRUE, showWarnings = FALSE)
      
      # 2. Load the dataset(s) ----
      
      # Read the deaths data from the text file, skipping the first two lines
      deaths <- read.table(paste0("./01_data/population_mortality/hmd/", country_code, "/deaths.txt"), header = TRUE, skip = 2)  
      
      # Replace '110+' with '999' and convert Age to integer
      deaths$Age <- as.integer(gsub("110\\+", "999", deaths$Age))
      
      # Filter the data for ages 0 to 75
      deaths <- deaths[deaths$Age %in% 0:75, ]
      
      # Check if the most recent year is greater than or equal to 2019
      if (max(deaths$Year) >= 2019) {
        # Filter the data for the years 2009-2019
        deaths <- subset(deaths, Year >= 2009 & Year <= 2019)
      } else {
        # Take the latest 10 years of data
        recent_years <- sort(unique(deaths$Year), decreasing = TRUE)[1:10]
        deaths <- subset(deaths, Year %in% recent_years)
      }
      deaths_female <- deaths[, c("Year", "Age", "Female")]
      deaths_male <- deaths[, c("Year", "Age", "Male")]
      
      # Assume population data is in a similar format and read it
      pop <- read.table(paste0("./01_data/population_mortality/hmd/", country_code, "/population.txt"), header = TRUE, skip = 2) 
      pop$Age <- as.integer(gsub("110\\+", "999", pop$Age))
      pop <- pop[pop$Age %in% 0:75, ]
      pop <- subset(pop, Year %in% deaths$Year)
      pop$Year <- as.integer(pop$Year)
      pop_list <- list(
        male = {pop_male <- pop[, c("Year", "Age", "Male")]; colnames(pop_male)[3] <- "Population"; pop_male},
        female = {pop_female <- pop[, c("Year", "Age", "Female")]; colnames(pop_female)[3] <- "Population"; pop_female}
      )
      
      # Initialize a list to store the results
      results <- list()
      
      # Loop over genders
      for (gender in c("Male", "Female")) {
        # Calculate mortality rates
        mortality <- deaths
        mortality$Rate <- deaths[[gender]] / pop[[gender]] 
        
        # When saving files, include the country code in the path and filename
        popfile_gender <- paste0("./01_data/population_mortality/hmd/", country_code, "/population_", tolower(gender),".txt")
        write.table(pop_list[[tolower(gender)]], popfile_gender, row.names = FALSE)
        
        # Prepare the data for StMoMo
        mortality_data <- mortality %>%
          select(Year, Age, Rate) %>%
          rename(Population = Rate)
        
        # Write the mortality rates to text files
        mortality_filename <- paste0("./01_data/population_mortality/hmd/", country_code, "/mortality_", tolower(gender), ".txt")
        write.table(mortality_data, mortality_filename, row.names = FALSE)
        
        
        # 3. If HMD run Lee Carter for both genders ----
        
        # Fit the Lee-Carter model
        demog_data <- read.demogdata(file = mortality_filename, popfile = popfile_gender, type = "mortality", label = gender, skip = 0, popskip = 0)
        stmomo_data <- StMoMoData(demog_data, series = "population")
        LC_fit <- fit(lc(), data = stmomo_data, ages.fit = 0:75, years.fit = demog_data$years)
        LC_for <- forecast(LC_fit, h = 1)
        
        # Store the results
        results[[gender]] <- list(LC_fit = LC_fit, LC_for = LC_for)
        
        # Plot the results
        plot(log(LC_for$rates), main = paste("Log Mortality Rates for", gender))
        
      }
      
      # Access the results for males and females
      male_results <- results[["Male"]]
      female_results <- results[["Female"]]
      
      # Extract mortality rates for both genders
      male_mortality <- extract_mortality_rates(male_results$LC_for, "Male", country_code)
      female_mortality <- extract_mortality_rates(female_results$LC_for, "Female", country_code)
      
      # Fit a GAM to the transformed rates
      male_gam <- gam(log(rates) ~ s(age, k = 15), data = male_mortality)
      female_gam <- gam(log(rates) ~ s(age, k = 15), data = female_mortality)
      
      # Predictions for individual ages
      ages <- 0:75
      log_male_predictions <- predict(male_gam, newdata = data.frame(age = ages))
      log_female_predictions <- predict(female_gam, newdata = data.frame(age = ages))
      
      # Back-transform predictions
      male_predictions <- exp(log_male_predictions)
      female_predictions <- exp(log_female_predictions)
      
      
      # Create dataframes for males and females with the country code
      male_mortality <- data.frame(country = country_code, age = ages, gender = rep("Male", length(ages)), population_rates = male_predictions)
      female_mortality <- data.frame(country = country_code, age = ages, gender = rep("Female", length(ages)), population_rates = female_predictions)
      
      
      # Save the plots in the country-specific 'hmd' folder with the country code as a suffix
      jpeg_filename <- paste0("./01_data/population_mortality/hmd/", country_code, "/mortality_", "male", "_", country_code, ".jpeg")
      jpeg(jpeg_filename)
      plot(log(male_mortality$population_rates), type = "l", main = paste("Log Mortality Rates for", gender, "in", country_code))
      dev.off()
      
      
      # Save the plots in the country-specific 'hmd' folder with the country code as a suffix
      jpeg_filename <- paste0("./01_data/population_mortality/hmd/", country_code, "/mortality_", "female", "_", country_code, ".jpeg")
      jpeg(jpeg_filename)
      plot(log(female_mortality$population_rates), type = "l", main = paste("Log Mortality Rates for", gender, "in", country_code))
      dev.off()
      
      # Define the files to keep
      files_to_keep <- c("deaths.txt", "population.txt", 
                         paste0("mortality_male_", country_code, ".jpeg"), 
                         paste0("mortality_female_", country_code, ".jpeg"))
      
      # List all files in the country-specific subfolder
      all_files <- list.files(file.path("./01_data/population_mortality/hmd", country_code), full.names = TRUE)
      
      # Determine which files to delete (all files that are not in the 'files_to_keep' list)
      files_to_delete <- setdiff(all_files, file.path(file.path("./01_data/population_mortality/hmd", country_code), files_to_keep))
      
      # Remove the files
      file.remove(files_to_delete)
      
      
    } else {
      print(paste("Processing WHO data for", country_code))
      
      # Create the country-specific subfolder if it doesn't exist
      dir.create(file.path("./01_data/population_mortality/who", country_code), recursive = TRUE, showWarnings = FALSE)
      
      
      # 2. Load the dataset(s) ----
      dataset <- read.csv(paste0('./01_data/population_mortality/who/', country_code, '/data.csv'), check.names = FALSE, skip = 1) 
      dataset <- dataset[-1] # drop first column 
      colnames(dataset) <- c("Age", "Male","Female")
      head(dataset)
      
      # Assuming you have a dataframe 'dataset' as given in the previous messages
      age_groups <- dataset$Age
      male_rates <- dataset$Male
      female_rates <- dataset$Female
      
      
      # 3. If WHO run GAM ----
      
      # Calculate midpoints of the age groups
      age_midpoints <- c(0.5, # for <1 year
                         seq(2.5, 72.5, by=5)) # for other age groups assuming uniform distribution
      
      # Transform rates using the log function to ensure positivity
      log_male_rates <- log(male_rates)
      log_female_rates <- log(female_rates)
      
      # Fit a GAM to the transformed rates
      male_gam <- gam(log_male_rates ~ s(age_midpoints, k = 15))
      female_gam <- gam(log_female_rates ~ s(age_midpoints, k = 15))
      
      # Predictions for individual ages
      ages <- 0:75
      log_male_predictions <- predict(male_gam, newdata = data.frame(age_midpoints = ages))
      log_female_predictions <- predict(female_gam, newdata = data.frame(age_midpoints = ages))
      
      # Back-transform predictions
      male_predictions <- exp(log_male_predictions)
      female_predictions <- exp(log_female_predictions)
      
      # Save the plots in the country-specific 'who' folder with the country code as a suffix
      jpeg(paste0("./01_data/population_mortality/who/", country_code, "/mortality_males_gam_", country_code, ".jpeg"))
      plot(ages, log(male_predictions), type = "l", main = paste("Mortality Rates for Males (GAM) in", country_code), xlab = "Age", ylab = "Mortality Rates", col = "blue")
      points(age_midpoints, log(male_rates), col = "red", pch = 16) # Add real values to the plot
      dev.off()
      
      jpeg(paste0("./01_data/population_mortality/who/", country_code, "/mortality_females_gam_", country_code, ".jpeg"))
      plot(ages, log(female_predictions), type = "l", main = paste("Mortality Rates for Females (GAM) in", country_code), xlab = "Age", ylab = "Mortality Rates", col = "blue")
      points(age_midpoints, log(female_rates), col = "red", pch = 16) # Add real values to the plot
      dev.off()
      
      # Create dataframes for males and females with the country code
      male_mortality <- data.frame(country = country_code, age = ages, gender = rep("Male", length(ages)), population_rates = male_predictions)
      female_mortality <- data.frame(country = country_code, age = ages, gender = rep("Female", length(ages)), population_rates = female_predictions)
      
    }
    
    # Combine male and female mortality rates
    combined_mortality <- rbind(male_mortality, female_mortality)
    combined_mortality$country <- ifelse(combined_mortality$country == "DEUTNP", "DE", combined_mortality$country) 
    
    
    combined_mortality <- combined_mortality %>% 
      mutate(gender = ifelse(gender == "Male", "M", "F"))
    
    # Check if the file exists and append the data if it does
    if (file.exists(results_file)) {
      # Read the existing data
      existing_data <- read.csv(results_file)
      # Check if the country_code already exists in the existing data
      if (!(country_code %in% existing_data$country)) {
        # Combine the new data with the existing data
        population_mortality <- rbind(existing_data, combined_mortality) 
        # Write the combined data to the CSV file
        write.csv(population_mortality, results_file, row.names = FALSE)
      } else {
        # If the country_code exists, print a message and do not append the data
        message("Data for country_code ", country_code, " already exists in the file.")
      }
    } else {
      # If the file doesn't exist, write the new data to the CSV file 
      population_mortality <- combined_mortality
      write.csv(population_mortality, results_file, row.names = FALSE)
    }
  } 

  
  return(population_mortality)
  
}