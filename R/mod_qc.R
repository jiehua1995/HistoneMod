##### Shiny module: quality control #####
# UI and server for sample/peptide quality metrics.

#' QC module UI
#'
#' @param id Module id.
#'
#' @return A `shiny.tag.list` with the module UI.
#' @keywords internal
qc_ui <- function(id) {
  ns <- shiny::NS(id)

  tagList(
    div(
      class = "row",
      div(
        class = "col-md-6",
        div(class = "plot-container", withSpinner(plotOutput(ns("missingness"), height = "380px"))),
        div(class = "text-end",
          actionButton(ns("export_missingness"), "Download Plot", icon = icon("download"),
                       class = "btn btn-primary btn-sm"))
      ),
      div(
        class = "col-md-6",
        div(class = "plot-container", withSpinner(plotOutput(ns("cv"), height = "380px"))),
        div(class = "text-end",
          actionButton(ns("export_cv"), "Download Plot", icon = icon("download"),
                       class = "btn btn-primary btn-sm"))
      )
    ),
    div(
      class = "row",
      div(
        class = "col-md-6",
        h5(style = "font-weight: 600; color: #1e293b;", "Sample QC"),
        div(class = "plot-container", DTOutput(ns("sample_table")))
      ),
      div(
        class = "col-md-6",
        h5(style = "font-weight: 600; color: #1e293b;", "Peptide QC"),
        div(class = "plot-container", DTOutput(ns("feature_table")))
      )
    ),
    div(
      class = "text-end mb-2",
      downloadButton(ns("download_qc"), "Download QC tables (CSV)",
                     class = "btn btn-outline-secondary btn-sm")
    ),
    hr(),
    tabsetPanel(
      tabPanel(
        "Sample correlation",
        div(class = "plot-container mt-2", withSpinner(plotOutput(ns("sample_correlation"), height = "480px"))),
        div(class = "text-end",
          actionButton(ns("export_correlation"), "Download Plot", icon = icon("download"),
                       class = "btn btn-primary btn-sm"))
      ),
      tabPanel(
        "Missingness map",
        div(class = "plot-container mt-2", withSpinner(plotOutput(ns("missingness_tile"), height = "480px"))),
        div(class = "text-end",
          actionButton(ns("export_missingness_tile"), "Download Plot", icon = icon("download"),
                       class = "btn btn-primary btn-sm"))
      ),
      tabPanel(
        "RT consistency",
        div(class = "plot-container mt-2", withSpinner(plotOutput(ns("rt_consistency"), height = "480px"))),
        div(class = "text-end",
          actionButton(ns("export_rt"), "Download Plot", icon = icon("download"),
                       class = "btn btn-primary btn-sm"))
      )
    )
  )
}

#' QC module server
#'
#' @param id Module id.
#' @param data_reactive Reactive returning the processed data frame.
#' @param value_col Reactive returning the name of the numeric value column.
#' @param on_export Callback used to open the export dialog.
#'
#' @return Invisibly `NULL`.
#' @keywords internal
qc_server <- function(id, data_reactive,
                      value_col = shiny::reactive("Percentage"),
                      on_export = function(plot, prefix, width = 8, height = 6) invisible(NULL)) {
  shiny::moduleServer(id, function(input, output, session) {
    qc <- shiny::reactive({
      data <- data_reactive()
      shiny::req(data)
      rt_col <- if ("Peptide.Retention.Time" %in% names(data)) "Peptide.Retention.Time" else NULL
      qc_summary(data, value_col = value_col(), rt_col = rt_col)
    })

    missingness_plot <- shiny::reactive(plot_qc_missingness(qc()))
    cv_plot <- shiny::reactive(plot_qc_cv(qc()))

    shiny::observeEvent(input$export_missingness, {
      on_export(missingness_plot(), "QC_missingness", width = 9, height = 5)
    })
    shiny::observeEvent(input$export_cv, {
      on_export(cv_plot(), "QC_cv", width = 9, height = 5)
    })

    output$missingness <- renderPlot({
      missingness_plot()
    })

    output$cv <- renderPlot({
      cv_plot()
    })

    correlation_plot <- shiny::reactive({
      data <- data_reactive()
      shiny::req(data)
      plot_sample_correlation(data, value_col = value_col())
    })

    missingness_tile_plot <- shiny::reactive({
      data <- data_reactive()
      shiny::req(data)
      plot_missingness_tile(data, value_col = value_col())
    })

    rt_plot <- shiny::reactive({
      data <- data_reactive()
      shiny::req(data)
      plot_rt_consistency(data)
    })

    shiny::observeEvent(input$export_correlation, {
      on_export(correlation_plot(), "QC_sample_correlation", width = 8, height = 7)
    })
    shiny::observeEvent(input$export_missingness_tile, {
      on_export(missingness_tile_plot(), "QC_missingness_tile", width = 10, height = 6)
    })
    shiny::observeEvent(input$export_rt, {
      on_export(rt_plot(), "QC_rt_consistency", width = 10, height = 6)
    })

    output$sample_correlation <- renderPlot({
      p <- correlation_plot()
      grid::grid.newpage()
      grid::grid.draw(p$gtable)
    })

    output$missingness_tile <- renderPlot({
      missingness_tile_plot()
    })

    output$rt_consistency <- renderPlot({
      rt_plot()
    })

    output$sample_table <- renderDT({
      datatable(qc()$sample_qc, options = list(pageLength = 10, scrollX = TRUE))
    })

    output$feature_table <- renderDT({
      datatable(qc()$feature_qc, options = list(pageLength = 10, scrollX = TRUE))
    })

    output$download_qc <- downloadHandler(
      filename = function() paste0("HistoneMod_QC_", Sys.Date(), ".csv"),
      content = function(file) {
        result <- qc()
        utils::write.csv(result$sample_qc, file, row.names = FALSE)
      }
    )

    invisible(NULL)
  })
}
