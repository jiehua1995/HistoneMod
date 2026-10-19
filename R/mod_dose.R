##### Shiny module: dose-response #####
# UI and server for dose-response curve fitting.

#' Dose-response module UI
#'
#' @param id Module id.
#'
#' @return A `shiny.tag.list`.
#' @keywords internal
dose_ui <- function(id) {
  ns <- shiny::NS(id)

  tagList(
    div(
      class = "mb-3",
      div(
        class = "row",
        div(class = "col-md-4", selectInput(ns("dose_col"), "Dose column", choices = NULL)),
        div(class = "col-md-4", selectInput(ns("protein"), "Protein", choices = NULL)),
        div(class = "col-md-2", numericInput(ns("max_features"), "Max features", value = 12, min = 1, max = 48, step = 1)),
        div(class = "col-md-2", br(), actionButton(ns("run"), "Fit curves", class = "btn btn-primary"))
      )
    ),
    div(class = "plot-container", withSpinner(plotOutput(ns("curves"), height = "560px"))),
    div(class = "row",
      div(class = "col-md-7", div(class = "plot-container", DTOutput(ns("fit_table")))),
      div(class = "col-md-5", div(class = "plot-container", DTOutput(ns("curve_table"))))
    ),
    div(class = "text-end mb-2",
      downloadButton(ns("download_fits"), "Download fits (CSV)", class = "btn btn-outline-secondary btn-sm")
    )
  )
}

#' Dose-response module server
#'
#' @param id Module id.
#' @param data_reactive Reactive returning the processed data frame.
#' @param value_col Reactive returning the numeric value column.
#'
#' @return Invisibly `NULL`.
#' @keywords internal
dose_server <- function(id, data_reactive,
                        value_col = shiny::reactive("Percentage")) {
  shiny::moduleServer(id, function(input, output, session) {
    numeric_columns <- shiny::reactive({
      data <- data_reactive()
      shiny::req(data)
      candidates <- setdiff(names(data), c(
        "Replicate.Name", "Group", "Protein.Name", "Peptide.Note",
        "Peptide.Sequence", value_col()
      ))
      candidates[vapply(data[candidates], is.numeric, logical(1))]
    })

    shiny::observeEvent(numeric_columns(), {
      choices <- numeric_columns()
      if (length(choices) == 0) return()
      shiny::updateSelectInput(session, "dose_col", choices = choices, selected = choices[1])
    }, ignoreInit = FALSE)

    proteins <- shiny::reactive({
      data <- data_reactive()
      shiny::req(data)
      sort(unique(as.character(data$Protein.Name)))
    })

    shiny::observeEvent(proteins(), {
      choices <- proteins()
      if (length(choices) == 0) return()
      shiny::updateSelectInput(session, "protein", choices = choices, selected = choices[1])
    }, ignoreInit = FALSE)

    fits <- shiny::eventReactive(input$run, {
      data <- data_reactive()
      shiny::validate(shiny::need(!is.null(data) && nrow(data) > 0, "No data available."))
      shiny::validate(shiny::need(
        !is.null(input$dose_col) && nzchar(input$dose_col),
        "Choose a numeric dose column."
      ))

      subset <- data[as.character(data$Protein.Name) == input$protein, , drop = FALSE]
      shiny::validate(shiny::need(nrow(subset) > 0, "No data for this protein."))

      features <- unique(as.character(subset$Peptide.Note))
      max_features <- as.integer(input$max_features)
      if (length(features) > max_features) features <- features[seq_len(max_features)]
      subset <- subset[as.character(subset$Peptide.Note) %in% features, , drop = FALSE]

      fit_dose_response(
        subset,
        dose_col = input$dose_col,
        value_col = value_col(),
        group_cols = c("Protein.Name", "Peptide.Note")
      )
    })

    output$curves <- renderPlot({
      data <- data_reactive()
      shiny::req(data)
      plot_dose_response(
        data[as.character(data$Protein.Name) == input$protein, , drop = FALSE],
        fits = fits(),
        dose_col = input$dose_col,
        value_col = value_col(),
        group_cols = c("Protein.Name", "Peptide.Note")
      )
    })

    output$fit_table <- renderDT({
      datatable(fits(), options = list(pageLength = 10, scrollX = TRUE))
    })

    output$curve_table <- renderDT({
      datatable(fits()[, intersect(
        c("Protein.Name", "Peptide.Note", "model", "ec50", "r_squared", "n", "converged"),
        names(fits())
      ), drop = FALSE], options = list(pageLength = 10, scrollX = TRUE))
    })

    output$download_fits <- downloadHandler(
      filename = function() paste0("HistoneMod_dose_fits_", Sys.Date(), ".csv"),
      content = function(file) {
        utils::write.csv(fits(), file, row.names = FALSE)
      }
    )

    invisible(NULL)
  })
}
