library(dotenv)
# You can control your local app development via environment variables.
# You can define things like input-data, output-data etc.
# Per default your environment is defined in `/.env`
load_dot_env()
# provide common stuff
source("src/common/logger.R")
source("src/common/runtime_configuration.R")
source("src/common/runtime_configuration.R")
clearRecentOutput()

# Lets simulate running your app on MoveApps
source("src/moveapps.R")
options(shiny.host = "0.0.0.0")
options(shiny.port = 3838)
shinyApp(ui, server, enableBookmarking = "server")

