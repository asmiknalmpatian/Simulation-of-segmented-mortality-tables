# Deploy app
setwd("D:/UNI/06_Promotion/4_Paper_Simulation/Simulation-of-segmented-mortality-tables")

library(rsconnect)
rsconnect::setAccountInfo(name='...', 
                          token='...', 
                          secret='...')
 
shiny::runApp('./04_app') 

rsconnect::deployApp('./04_app', appName = "simulate_mortality_tables_v1")

# rsconnect::terminateApp(appName = "simulate_mortality_tables_v4")
# rsconnect::showLogs(appName = "simulate_mortality_tables_v1")
# rsconnect::listBundleFiles('D:/UNI/06_Promotion/4_Paper_Simulation/app')
 
