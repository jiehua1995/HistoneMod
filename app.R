# app.R
source("functions.R")
depends_check()
source("ui.R")
source("server.R")
shinyApp(ui, server)
