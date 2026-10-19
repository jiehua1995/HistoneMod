##### Shiny module: differential analysis #####
# UI and server for the global group-comparison view (volcano plot + table).

#' Differential analysis module UI
#'
#' @param id Module id.
#'
#' @return A `shiny.tag.list` with the module UI.
#' @keywords internal
differential_ui <- function(id) {
  ns <- shiny::NS(id)

  tagList(
    div(
      class = "mb-3",
      div(
        class = "row",
        div(class = "col-md-3", selectInput(ns("reference_group"), "Reference group", choices = NULL)),
        div(class = "col-md-3", selectInput(
          ns("transform"), "Data transform",
          choices = c("CLR" = "clr", "None" = "none"), selected = "clr"
        )),
        div(class = "col-md-3", checkboxInput(ns("within_protein"), "Adjust within protein", value = FALSE))
      ),
      div(
        class = "row",
        div(class = "col-md-3", checkboxInput(ns("all_pairs"), "All pairwise comparisons", value = TRUE)),
        div(class = "col-md-3", numericInput(ns("min_n"), "Minimum n per group", value = 2, min = 1, step = 1)),
        div(class = "col-md-3", numericInput(ns("alpha"), "Significance threshold", value = 0.05, min = 0.001, max = 0.5, step = 0.01)),
        div(
          class = "col-md-3",
          br(),
          actionButton(ns("run"), "Run differential analysis", class = "btn btn-primary")
        )
      )
    ),
    div(
      class = "row",
      div(
        class = "col-md-7",
        div(class = "plot-container", withSpinner(plotOutput(ns("volcano"), height = "520px")))
      ),
      div(
        class = "col-md-5",
        div(class = "plot-container", DTOutput(ns("table")))
      )
    ),
    div(
      class = "text-end mb-2",
      downloadButton(ns("download_csv"), "Download results (CSV)", class = "btn btn-outline-secondary btn-sm")
    )
  )
}

#' Differential analysis module server
#'
#' @param id Module id.
#' @param data_reactive Reactive returning the processed data frame.
#' @param value_col Reactive returning the name of the numeric value column.
#' @param group_order Reactive returning an optional group order vector.
#'
#' @return Invisibly `NULL`.
#' @keywords internal
differential_server <- function(id, data_reactive,
                                value_col = shiny::reactive("Percentage"),
                                group_order = shiny::reactive(NULL)) {
  shiny::moduleServer(id, function(input, output, session) {
    group_choices <- shiny::reactive({
      data <- data_reactive()
      shiny::req(data)
      groups <- unique(as.character(data$Group))
      if (length(groups) == 0) return(character(0))
      order <- group_order()
      group_levels(groups, order)
    })

    shiny::observeEvent(group_choices(), {
      choices <- group_choices()
      if (length(choices) == 0) return()
      shiny::updateSelectInput(session, "reference_group", choices = choices, selected = choices[1])
    }, ignoreInit = FALSE)

    results <- shiny::eventReactive(input$run, {
      data <- data_reactive()
      shiny::validate(shiny::need(!is.null(data) && nrow(data) > 0, "No data available for differential analysis."))

      groups <- unique(as.character(data$Group))
      shiny::validate(shiny::need(length(groups) >= 2, "Differential analysis needs at least two groups."))

      comparisons <- NULL
      if (!isTRUE(input$all_pairs)) {
        reference <- input$reference_group
        shiny::validate(shiny::need(
          !is.null(reference) && nzchar(reference),
          "Choose a reference group or enable all pairwise comparisons."
        ))
        others <- setdiff(groups, reference)
        comparisons <- lapply(others, function(g) c(reference, g))
      }

      differential_analysis(
        data,
        value_col = value_col(),
        transform = input$transform,
        comparisons = comparisons,
        alpha = input$alpha,
        within_protein = isTRUE(input$within_protein),
        min_n = as.integer(input$min_n)
      )
    })

    output$volcano <- renderPlot({
      res <- results()
      shiny::validate(shiny::need(nrow(res) > 0, "No comparisons were returned."))
      plot_volcano(res, alpha = input$alpha)
    })

    output$table <- renderDT({
      res <- results()
      shiny::req(nrow(res) > 0)
      datatable(res, options = list(pageLength = 10, scrollX = TRUE))
    })

    output$download_csv <- downloadHandler(
      filename = function() paste0("HistoneMod_differential_", Sys.Date(), ".csv"),
      content = function(file) {
        res <- results()
        shiny::req(nrow(res) > 0)
        utils::write.csv(res, file, row.names = FALSE)
      }
    )

    invisible(NULL)
  })
}
