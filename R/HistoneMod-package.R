#' HistoneMod package
#'
#' Provides a Shiny application and reusable helper functions for quantitative
#' analysis of histone post-translational modifications from peptide-level CSV
#' exports.
#'
#' @keywords internal
#' @import dplyr
#' @import ggplot2
#' @import tidyr
#' @importFrom DT DTOutput datatable renderDT
#' @importFrom ggsignif geom_signif
#' @importFrom shiny HTML NS actionButton actionLink addResourcePath br
#' @importFrom shiny checkboxInput conditionalPanel div downloadButton
#' @importFrom shiny downloadHandler fileInput fluidPage h4 h5 icon
#' @importFrom shiny incProgress mainPanel modalButton modalDialog moduleServer
#' @importFrom shiny need numericInput observeEvent plotOutput reactive
#' @importFrom shiny reactiveVal removeModal renderImage renderPlot renderUI
#' @importFrom shiny req resourcePaths runApp selectInput shinyApp showModal
#' @importFrom shiny sidebarLayout sidebarPanel sliderInput span tabPanel
#' @importFrom shiny tabsetPanel tagList tags uiOutput validate withProgress
#' @importFrom shinyalert shinyalert useShinyalert
#' @importFrom shinycssloaders withSpinner
#' @importFrom shinyjs runjs useShinyjs
#' @importFrom shinyWidgets pickerInput updatePickerInput
#' @importFrom grDevices bmp dev.off jpeg pdf png postscript setEPS svg tiff
#' @importFrom graphics par plot.new text
#' @importFrom grid gpar grid.draw grid.layout grid.lines grid.newpage
#' @importFrom grid grid.raster grid.rect grid.text popViewport pushViewport
#' @importFrom grid unit viewport
#' @importFrom stats aov prcomp sd t.test
#' @importFrom utils capture.output combn packageDescription tail
"_PACKAGE"
