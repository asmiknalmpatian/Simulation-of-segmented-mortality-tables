fluidPage(
  # Add custom CSS styling
  tags$head(
    tags$style(HTML("
                    body { font-family: Arial, sans-serif; background-color: #f9f9f9; color: #333; }
                    .title { text-align: center; color: #0073e6; margin-bottom: 20px; }
                    .sidebar { background-color: #f1f1f1; padding: 15px; border-radius: 5px; }
                    .main { padding: 15px; }
                    .data-table { max-height: 400px; overflow-y: scroll; }
                    "))
  ),
  
  # Title
  div(class = "title", h1("Monte Carlo Simulation: Mortality Analysis")),
  
  # Layout with Sidebar and Main Panel
  fluidRow(
    column(3, 
           div(class = "sidebar",
               h4("Filter Options"),
               selectInput("country", "Select Country:", choices = unique(simulation_results$country), selected = "Germany (insured)"),
               selectInput("state", "Select State:", choices = NULL), # Dynamically loaded states
               sliderInput("age", "Select Age Range:", min = min(simulation_results$age), max = max(simulation_results$age), value = c(30, 70)),
               selectInput("gender", "Select Gender:", choices = unique(simulation_results$gender), selected = unique(simulation_results$gender), multiple = TRUE),
               selectInput("smoker", "Select Smoking Status:", choices = unique(simulation_results$smoker), selected = unique(simulation_results$smoker), multiple = TRUE),
               radioButtons("show_ci", "Show Confidence Interval:", choices = c("With 95%-CI" = "yes", "Without 95%-CI" = "no"), selected = "no")
           )
    ),
    column(9,
           div(class = "main", 
               plotOutput("deathPlot"),
               
               # Table for data
               DTOutput("dataTable"),
               
               # Download buttons
               downloadButton("downloadData", "Download table as .csv"),
               downloadButton("downloadPlot", "Download graph as .jpg")
           )
    )
  )
)