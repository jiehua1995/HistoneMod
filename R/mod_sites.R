##### Shiny module: site occupancy and cross-talk #####
# Site-level views built on parse_peptide_sites()/compute_site_occupancy()/
# compute_crosstalk(). These use the raw MS1 table because unmodified states
# are required for occupancy.

#' Site analysis module UI
#'
#' @param id Module id.
#'
#' @return A `shiny.tag.list`.
#' @keywords internal
sites_ui <- function(id) {
  ns <- shiny::NS(id)

  tagList(
    tabsetPanel(
      tabPanel(
        "Occupancy",
        div(
          class = "mb-3",
          div(
            class = "row",
            div(class = "col-md-4", selectInput(ns("occ_protein"), "Protein", choices = NULL)),
            div(class = "col-md-3", checkboxInput(ns("occ_backbone"), "Per peptide backbone", value = FALSE)),
            div(class = "col-md-3", checkboxInput(ns("occ_require_un"), "Require unmodified state", value = FALSE)),
            div(class = "col-md-2", br(), actionButton(ns("run_occupancy"), "Calculate", class = "btn btn-primary"))
          )
        ),
        div(class = "plot-container", withSpinner(plotOutput(ns("occupancy_plot"), height = "480px"))),
        div(class = "row",
          div(class = "col-md-8", div(class = "plot-container", DTOutput(ns("occupancy_table")))),
          div(class = "col-md-4", br(), downloadButton(ns("download_occupancy"), "Download occupancy (CSV)",
                                                      class = "btn btn-outline-secondary btn-sm"))
        )
      ),
      tabPanel(
        "Cross-talk",
        div(
          class = "mb-3",
          div(
            class = "row",
            div(class = "col-md-4", selectInput(ns("ct_protein"), "Protein", choices = NULL)),
            div(class = "col-md-3", selectInput(ns("ct_type"), "View",
              choices = c("Pattern frequencies" = "patterns", "Co-occurrence heatmap" = "cooccurrence"),
              selected = "patterns"
            )),
            div(class = "col-md-3", numericInput(ns("ct_top"), "Top patterns", value = 10, min = 1, max = 40, step = 1)),
            div(class = "col-md-2", br(), actionButton(ns("run_crosstalk"), "Calculate", class = "btn btn-primary"))
          )
        ),
        div(class = "plot-container", withSpinner(plotOutput(ns("crosstalk_plot"), height = "480px"))),
        div(class = "row",
          div(class = "col-md-8", div(class = "plot-container", DTOutput(ns("crosstalk_table")))),
          div(class = "col-md-4", br(), downloadButton(ns("download_crosstalk"), "Download cross-talk (CSV)",
                                                      class = "btn btn-outline-secondary btn-sm"))
        )
      )
    )
  )
}

#' Site analysis module server
#'
#' @param id Module id.
#' @param ms1_reactive Reactive returning the raw MS1 table.
#' @param sample_reactive Reactive returning the sample table.
#' @param group_order Reactive returning an optional group order vector.
#'
#' @return Invisibly `NULL`.
#' @keywords internal
sites_server <- function(id, ms1_reactive, sample_reactive,
                         group_order = shiny::reactive(NULL)) {
  shiny::moduleServer(id, function(input, output, session) {
    proteins <- shiny::reactive({
      data <- ms1_reactive()
      shiny::req(data)
      sort(unique(as.character(data$Protein.Name)))
    })

    shiny::observeEvent(proteins(), {
      choices <- proteins()
      if (length(choices) == 0) return()
      shiny::updateSelectInput(session, "occ_protein", choices = choices, selected = choices[1])
      shiny::updateSelectInput(session, "ct_protein", choices = choices, selected = choices[1])
    }, ignoreInit = FALSE)

    occupancy <- shiny::eventReactive(input$run_occupancy, {
      ms1 <- ms1_reactive()
      sample <- sample_reactive()
      shiny::validate(shiny::need(!is.null(ms1) && !is.null(sample), "Upload data first."))
      shiny::validate(shiny::need(
        !is.null(input$occ_protein) && nzchar(input$occ_protein),
        "Choose a protein."
      ))

      tryCatch(
        compute_site_occupancy(
          ms1,
          sample,
          backbone_detail = isTRUE(input$occ_backbone),
          require_unmodified = isTRUE(input$occ_require_un)
        ),
        error = function(e) {
          shiny::validate(shiny::need(FALSE, conditionMessage(e)))
        }
      )
    })

    occupancy_filtered <- shiny::reactive({
      data <- occupancy()
      shiny::req(data)
      if ("Protein.Name" %in% names(data)) {
        data <- data[as.character(data$Protein.Name) == input$occ_protein, , drop = FALSE]
      }
      data
    })

    output$occupancy_plot <- renderPlot({
      plot_occupancy(occupancy_filtered(), group_order = group_order())
    })

    output$occupancy_table <- renderDT({
      datatable(occupancy_filtered(), options = list(pageLength = 10, scrollX = TRUE))
    })

    output$download_occupancy <- downloadHandler(
      filename = function() paste0("HistoneMod_occupancy_", Sys.Date(), ".csv"),
      content = function(file) {
        utils::write.csv(occupancy_filtered(), file, row.names = FALSE)
      }
    )

    crosstalk <- shiny::eventReactive(input$run_crosstalk, {
      ms1 <- ms1_reactive()
      sample <- sample_reactive()
      shiny::validate(shiny::need(!is.null(ms1) && !is.null(sample), "Upload data first."))

      tryCatch(
        compute_crosstalk(
          ms1,
          protein = input$ct_protein,
          sample = sample
        ),
        error = function(e) {
          shiny::validate(shiny::need(FALSE, conditionMessage(e)))
        }
      )
    })

    output$crosstalk_plot <- renderPlot({
      plot_crosstalk(
        crosstalk(),
        type = input$ct_type,
        top_n = as.integer(input$ct_top),
        group_order = group_order()
      )
    })

    output$crosstalk_table <- renderDT({
      result <- crosstalk()
      table <- if (identical(input$ct_type, "patterns")) result$patterns else result$cooccurrence
      datatable(table, options = list(pageLength = 10, scrollX = TRUE))
    })

    output$download_crosstalk <- downloadHandler(
      filename = function() paste0("HistoneMod_crosstalk_", Sys.Date(), ".csv"),
      content = function(file) {
        result <- crosstalk()
        table <- if (identical(input$ct_type, "patterns")) result$patterns else result$cooccurrence
        utils::write.csv(table, file, row.names = FALSE)
      }
    )

    invisible(NULL)
  })
}
