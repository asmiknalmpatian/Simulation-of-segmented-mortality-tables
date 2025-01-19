library(shiny)
library(tidyverse)
library(DT)

function(input, output, session) {
  # Update state choices dynamically based on selected country
  observeEvent(input$country, {
    available_states <- simulation_results %>%
      filter(Country_code == input$country) %>%
      pull(State) %>%
      unique() %>%
      sort()
    
    # Add "All States" as an option
    available_states <- c("All States", available_states)
    updateSelectInput(session, "state", choices = available_states, selected = "All States")
  })
  
  # Reactive dataset based on user inputs
  filtered_data <- reactive({
    req(input$country, input$age, input$gender, input$smoker)
    
    data <- simulation_results %>%
      filter(
        Country_code == input$country,
        Age >= input$age[1],
        Age <= input$age[2],
        Gender %in% input$gender,
        Smoker %in% input$smoker
      )
    
    # Filter by state only if a specific state is selected
    if (input$state != "All States") {
      data <- data %>% filter(State == input$state)
    } else {
      data <- data %>%
        group_by(Age, Smoker, Gender, Iteration) %>%
        summarize(
          SimulatedDeaths = sum(SimulatedDeaths, na.rm = TRUE),
          Population = sum(Population, na.rm = TRUE),
          SimulatedRates = ifelse(Population > 0, SimulatedDeaths / Population, 0),  # Avoid division by zero
          .groups = "drop"
        )
    }
    
    return(data)
  })
  
  # Generate plot
  output$deathPlot <- renderPlot({
    aggregated_data <- filtered_data() %>%
      group_by(Age, Smoker, Gender) %>%
      summarize(
        MeanRates = mean(SimulatedRates, na.rm = TRUE),
        LowerCI = quantile(SimulatedRates, 0.025, na.rm = TRUE),
        UpperCI = quantile(SimulatedRates, 0.975, na.rm = TRUE),
        .groups = "drop"
      )
    
    p <- ggplot(aggregated_data, aes(x = Age, y = MeanRates, color = Smoker, linetype = Gender, group = interaction(Smoker, Gender))) +
      geom_line(size = 1) +
      labs(
        title = "Simulated Mortality Rates",
        x = "Age",
        y = "Mean Simulated Mortality Rates",
        color = "Smoker",
        linetype = "Gender"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    if (input$show_ci == "yes") {
      p <- p +
        geom_ribbon(aes(ymin = LowerCI, ymax = UpperCI, fill = Smoker, group = interaction(Smoker, Gender)), alpha = 0.2, linetype = 0) +
        guides(fill = guide_legend(title = "Smoker 95%-CI"))
    }
    
    return(p)
  })
  
  # Show the table with data used for the plot (without CI)
  output$dataTable <- DT::renderDataTable({
 
      filtered_data() %>%
      group_by(Age, Smoker, Gender) %>%
      summarize(
        `Sim. Rate` = round(mean(SimulatedRates, na.rm = TRUE), 6),
        `Lower CI` = round(quantile(SimulatedRates, 0.025, na.rm = TRUE), 6),
        `Upper CI` = round(quantile(SimulatedRates, 0.975, na.rm = TRUE), 6),
        .groups = "drop"
      ) %>% 
      DT::datatable(
        options = list(
          scrollY = "300px",  # Limit the height of the table to 300px, enabling scrolling
          paging = TRUE,      # Enable pagination
          pageLength = 10     # Display 10 rows per page
        )
      )
  })
  
  # Allow the user to download the table as a CSV
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(Sys.Date(), input$country, input$state, "mortality_table.csv", sep = "_")
    },
    content = function(file) {
      write_csv(filtered_data() %>%
                  select(Age, Smoker, Gender, SimulatedDeaths, Population, SimulatedRates), file)  # Exclude CI columns here too
    }
  )
  
  # Download handler for plot as JPG
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste(Sys.Date(), input$country, input$state, "mortality_graph.jpg", sep = "_")
    },
    content = function(file) {
      # Save the plot as JPG
      ggsave(file, plot = last_plot(), device = "jpeg", width = 8, height = 6, dpi = 300)
    }
  )
}