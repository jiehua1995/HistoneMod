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
        div(class = "plot-container", withSpinner(plotOutput(ns("missingness"), height = "380px")))
      ),
      div(
        class = "col-md-6",
        div(class = "plot-container", withSpinner(plotOutput(ns("cv"), height = "380px")))
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
    )
  )
}

#' QC module server
#'
#' @param id Module id.
#' @param data_reactive Reactive returning the processed data frame.
#' @param value_col Reactive returning the name of the numeric value column.
#'
#' @return Invisibly `NULL`.
#' @keywords internal
qc_server <- function(id, data_reactive,
                      value_col = shiny::reactive("Percentage")) {
  shiny::moduleServer(id, function(input, output, session) {
    qc <- shiny::reactive({
      data <- data_reactive()
      shiny::req(data)
      rt_col <- if ("Peptide.Retention.Time" %in% names(data)) "Peptide.Retention.Time" else NULL
      qc_summary(data, value_col = value_col(), rt_col = rt_col)
    })

    output$missingness <- renderPlot({
      plot_qc_missingness(qc())
    })

    output$cv <- renderPlot({
      plot_qc_cv(qc())
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
