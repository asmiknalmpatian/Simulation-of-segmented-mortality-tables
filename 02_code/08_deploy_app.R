# Deploy app

library(rsconnect)
rsconnect::setAccountInfo(name='advancedmortalitymodeling', 
                          token='AA7AA516DC0139A5F6E1D162C84EAA1C', 
                          secret='ddZLiS5ik1gqy1oNLvtJh2/hImh6MzeGWymTZ9KY')
 
shiny::runApp('D:/UNI/06_Promotion/4_Paper_Simulation/app') 

rsconnect::deployApp('D:/UNI/06_Promotion/4_Paper_Simulation/app', appName = "SimMortalityTables_v9")

# rsconnect::terminateApp(appName = "SimMortalityTables_v9")
# rsconnect::showLogs(appName = "SimMortalityTables_v9")
# rsconnect::listBundleFiles('D:/UNI/06_Promotion/4_Paper_Simulation/app')
 
