

function(input, output, session) {
  # Update state choices dynamically based on selected country
  observeEvent(input$country, {
    available_states <- simulation_results %>%
      filter(country == input$country) %>%
      pull(state) %>%
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
        country == input$country,
        age >= input$age[1],
        age <= input$age[2],
        gender %in% input$gender,
        smoker %in% input$smoker
      )
    
    # Filter by state only if a specific state is selected
    if (input$state != "All States") {
      data <- data %>% filter(state == input$state)
    } else {
      data <- data %>%
        group_by(age, smoker, gender, iteration) %>% # Sum across states
        summarize(
          simulated_deaths = sum(simulated_deaths),
          population = sum(population),
          simulated_rates = simulated_deaths / population, 
          .groups = "drop"
        )
    }
    
    return(data)
  })
  
  # Generate plot
  output$deathPlot <- renderPlot({
    aggregated_data <- filtered_data() %>%
      group_by(age, smoker, gender) %>%
      summarize(
        mean_rates = mean(simulated_deaths / population), # Average across iterations
        sd_deaths = sd(simulated_deaths / population),
        total_deaths = sum(simulated_deaths / population),
        lower_ci = quantile(simulated_deaths / population, 0.025),
        upper_ci = quantile(simulated_deaths / population, 0.975),
        .groups = "drop"
      )
    
    p <- ggplot(aggregated_data, aes(x = age, y = mean_rates, color = smoker, linetype = gender, group = interaction(smoker, gender))) +
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
        geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = smoker, group = interaction(smoker, gender)), alpha = 0.2, linetype = 0) +
        guides(fill = guide_legend(title = "Smoker 95%-CI"))
    }
    
    return(p)
  })
  
  # Show the table with data used for the plot (without CI)
  output$dataTable <- DT::renderDataTable({
    
    filtered_data() %>%
      group_by(age, smoker, gender) %>%
      summarize(
        simulated_rates = round(mean(simulated_rates, na.rm = TRUE), 6),
        lower_ci = round(quantile(simulated_rates, 0.025, na.rm = TRUE), 6),
        upper_ci = round(quantile(simulated_rates, 0.975, na.rm = TRUE), 6),
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
                  select(age, smoker, gender, simulated_deaths, population, simulated_rates), file)  # Exclude CI columns here too
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