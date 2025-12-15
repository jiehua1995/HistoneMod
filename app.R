# app.R

# This is a Shiny web application. 

# Before running the app, make sure all dependencies are installed.
# Option 1: Using renv
# renv::restore()
# Option 2: Using the depends_check function
# source("functions.R")
# depends_check()

# You can run the application by clicking the 'Run App' button above in RStudio.

source("ui.R")
source("server.R")
shinyApp(ui, server)
