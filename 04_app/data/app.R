library(shiny)
library(tidyverse)




# Define UI

# Define UI
ui <- fluidPage(
  # Add custom CSS styling
  tags$head(
    tags$style(HTML("
                    body { font-family: Arial, sans-serif; background-color: #f9f9f9; color: #333; }
                    .title { text-align: center; color: #0073e6; margin-bottom: 20px; }
                    .sidebar { background-color: #f1f1f1; padding: 15px; border-radius: 5px; }
                    .main { padding: 15px; }
                    "))
  ),
  
  # Title
  div(class = "title", h1("Monte Carlo Simulation: Mortality Analysis")),
  
  # Layout with Sidebar and Main Panel
  fluidRow(
    column(3, 
           div(class = "sidebar",
               h4("Filter Options"),
               selectInput("country", "Select Country:", choices = unique(simulation_results$Country_code), selected = "Germany (insured)"),
               selectInput("state", "Select State:", choices = NULL), # Dynamically loaded states
               sliderInput("age", "Select Age Range:", min = min(simulation_results$Age), max = max(simulation_results$Age), value = c(30, 70)),
               selectInput("gender", "Select Gender(s):", choices = unique(simulation_results$Gender), selected = unique(simulation_results$Gender), multiple = TRUE),
               selectInput("smoker", "Select Smoking Status:", choices = unique(simulation_results$Smoker), selected = unique(simulation_results$Smoker), multiple = TRUE),
               radioButtons("show_ci", "Show Confidence Interval:", choices = c("With CI" = "yes", "Without CI" = "no"), selected = "no")
           )
    ),
    column(9,
           div(class = "main", plotOutput("deathPlot"))
    )
  )
)

# Define Server logic
server <- function(input, output, session) {
  # 
  # # Load the dataset inside the server
  # simulation_results <- read_csv("./simulation_results.csv") %>%
  #   mutate(SimulatedRates = SimulatedDeaths / Population)
  
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
          SimulatedRates = SimulatedDeaths / Population,
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
}

# Run the app
shinyApp(ui = ui, server = server)