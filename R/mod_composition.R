##### Shiny module: composition and derived metrics #####
# UI and server for stacked modification-state bars and user-defined metrics.

#' Composition module UI
#'
#' @param id Module id.
#'
#' @return A `shiny.tag.list` with the module UI.
#' @keywords internal
composition_ui <- function(id) {
  ns <- shiny::NS(id)

  tagList(
    div(
      class = "mb-3",
      div(
        class = "row",
        div(class = "col-md-4", selectInput(ns("protein"), "Protein", choices = NULL)),
        div(class = "col-md-3", checkboxInput(ns("summarise"), "Average by group", value = TRUE)),
        div(class = "col-md-3", selectInput(
          ns("palette"), "Colour palette",
          choices = c(
            "Viridis" = "viridis", "Magma" = "magma", "Plasma" = "plasma",
            "Inferno" = "inferno", "Cividis" = "cividis", "Rocket" = "rocket",
            "Mako" = "mako", "Turbo" = "turbo"
          ),
          selected = "viridis"
        ))
      )
    ),
    div(class = "plot-container", withSpinner(plotOutput(ns("composition"), height = "520px"))),
    div(
      class = "text-end mb-2",
      actionButton(ns("export_composition"), "Download Plot", icon = icon("download"),
                   class = "btn btn-primary btn-sm"),
      downloadButton(ns("download_composition"), "Download composition table (CSV)",
                     class = "btn btn-outline-secondary btn-sm")
    ),
    hr(),
    h5(style = "font-weight: 600; color: #1e293b;", "Derived modification metrics"),
    p(
      style = "color: #64748b; font-size: 13px;",
      "One metric per line: name = numerator regex / denominator regex. ",
      "Use \"all\" as the denominator to divide by every measured peptide."
    ),
    textAreaInput(
      ns("specs"), NULL, rows = 4, width = "100%",
      placeholder = "K4me3_share = K4_me3 / K4_"
    ),
    div(
      class = "row",
      div(class = "col-md-6", withSpinner(plotOutput(ns("metrics_plot"), height = "360px"))),
      div(class = "col-md-6", div(class = "plot-container", DTOutput(ns("metrics_table"))))
    ),
    div(
      class = "text-end mb-2",
      actionButton(ns("export_metrics"), "Download Plot", icon = icon("download"),
                   class = "btn btn-primary btn-sm"),
      downloadButton(ns("download_metrics"), "Download metrics (CSV)",
                     class = "btn btn-outline-secondary btn-sm")
    ),
    hr(),
    h5(style = "font-weight: 600; color: #1e293b;", "Mark class summary"),
    checkboxInput(ns("by_class"), "Summarise peptide values by annotated mark class", value = FALSE),
    shiny::conditionalPanel(
      condition = "input.by_class == true",
      ns = ns,
      div(class = "row",
        div(class = "col-md-7", withSpinner(plotOutput(ns("class_plot"), height = "380px"))),
        div(class = "col-md-5", div(class = "plot-container", DTOutput(ns("class_table"))))
      ),
      div(
        class = "text-end mb-2",
        actionButton(ns("export_class"), "Download Plot", icon = icon("download"),
                     class = "btn btn-primary btn-sm"),
        downloadButton(ns("download_class"), "Download mark classes (CSV)",
                       class = "btn btn-outline-secondary btn-sm")
      )
    )
  )
}

#' Composition module server
#'
#' @param id Module id.
#' @param data_reactive Reactive returning the processed data frame.
#' @param value_col Reactive returning the name of the numeric value column.
#' @param group_order Reactive returning an optional group order vector.
#' @param on_export Callback used to open the export dialog.
#'
#' @return Invisibly `NULL`.
#' @keywords internal
composition_server <- function(id, data_reactive,
                               value_col = shiny::reactive("Percentage"),
                               group_order = shiny::reactive(NULL),
                               on_export = function(plot, prefix, width = 8, height = 6) invisible(NULL)) {
  shiny::moduleServer(id, function(input, output, session) {
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

    composition_data <- shiny::reactive({
      data <- data_reactive()
      shiny::req(data, input$protein)
      data <- data[as.character(data$Protein.Name) == input$protein, , drop = FALSE]
      plot_composition(
        data,
        protein = input$protein,
        value_col = value_col(),
        group_order = group_order(),
        palette = input$palette,
        summarise = isTRUE(input$summarise)
      )
    })

    shiny::observeEvent(input$export_composition, {
      on_export(composition_data(), "Composition", width = 10, height = 6)
    })

    output$composition <- renderPlot({
      composition_data()
    })

    output$download_composition <- downloadHandler(
      filename = function() paste0("HistoneMod_composition_", Sys.Date(), ".csv"),
      content = function(file) {
        data <- data_reactive()
        shiny::req(data)
        keep <- c("Replicate.Name", "Group", "Protein.Name", "Peptide.Note", value_col())
        keep <- intersect(keep, names(data))
        utils::write.csv(data[, keep, drop = FALSE], file, row.names = FALSE)
      }
    )

    metric_specs <- shiny::reactive({
      text <- input$specs
      if (is.null(text) || !nzchar(trimws(text))) return(NULL)
      lines <- trimws(strsplit(text, "\n", fixed = TRUE)[[1]])
      lines <- lines[nzchar(lines)]
      if (length(lines) == 0) return(NULL)

      parsed <- lapply(lines, function(line) {
        parts <- strsplit(line, "=", fixed = TRUE)[[1]]
        if (length(parts) != 2) return(NULL)
        ratio <- strsplit(parts[2], "/", fixed = TRUE)[[1]]
        if (length(ratio) != 2) return(NULL)
        data.frame(
          metric = trimws(parts[1]),
          numerator = trimws(ratio[1]),
          denominator = trimws(ratio[2]),
          stringsAsFactors = FALSE
        )
      })
      parsed <- parsed[!vapply(parsed, is.null, logical(1))]
      if (length(parsed) == 0) return(NULL)
      do.call(rbind, parsed)
    })

    metrics <- shiny::reactive({
      specs <- metric_specs()
      data <- data_reactive()
      if (is.null(specs) || is.null(data) || nrow(data) == 0) return(NULL)
      tryCatch(
        compute_metrics(data, specs, value_col = value_col()),
        error = function(e) NULL
      )
    })

    metrics_plot_object <- shiny::reactive({
      met <- metrics()
      shiny::validate(shiny::need(!is.null(met) && nrow(met) > 0, "Enter at least one valid metric."))
      met$Group <- factor(met$Group, levels = group_levels(met$Group, group_order()))
      agg <- stats::aggregate(
        met$value,
        by = list(Group = met$Group, metric = met$metric),
        FUN = function(v) mean(v, na.rm = TRUE)
      )
      names(agg)[3] <- "mean_value"
      ggplot(agg, aes(x = Group, y = mean_value, fill = Group)) +
        geom_col(position = position_dodge(width = 0.9)) +
        facet_wrap(~metric, scales = "free_y") +
        labs(x = NULL, y = "Metric value", title = NULL) +
        theme_minimal(base_family = "sans") +
        theme(
          panel.grid = element_blank(),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none"
        ) +
        scale_fill_viridis_d()
    })

    shiny::observeEvent(input$export_metrics, {
      on_export(metrics_plot_object(), "Composition_metrics", width = 9, height = 5)
    })

    output$metrics_plot <- renderPlot({
      metrics_plot_object()
    })

    output$metrics_table <- renderDT({
      met <- metrics()
      shiny::req(!is.null(met))
      datatable(met, options = list(pageLength = 10, scrollX = TRUE))
    })

    output$download_metrics <- downloadHandler(
      filename = function() paste0("HistoneMod_metrics_", Sys.Date(), ".csv"),
      content = function(file) {
        met <- metrics()
        shiny::req(!is.null(met))
        utils::write.csv(met, file, row.names = FALSE)
      }
    )

    class_summary <- shiny::reactive({
      data <- data_reactive()
      shiny::req(data)
      if (!isTRUE(input$by_class)) return(NULL)

      sites <- tryCatch(parse_peptide_sites(data), error = function(e) NULL)
      if (is.null(sites) || nrow(sites) == 0) return(NULL)

      data$row_id <- seq_len(nrow(data))
      joined <- merge(sites, data, by = "row_id", suffixes = c("", ".data"))
      annotated <- tryCatch(annotate_sites(joined), error = function(e) joined)

      tryCatch(
        summarise_by_class(annotated, value_col = value_col(), group_col = "Group"),
        error = function(e) NULL
      )
    })

    class_plot_object <- shiny::reactive({
      summary <- class_summary()
      shiny::validate(shiny::need(
        !is.null(summary) && nrow(summary) > 0,
        "No annotated sites to summarise."
      ))
      plot_mark_classes(summary, group_order = group_order())
    })

    shiny::observeEvent(input$export_class, {
      on_export(class_plot_object(), "Mark_classes", width = 9, height = 5)
    })

    output$class_plot <- renderPlot({
      class_plot_object()
    })

    output$class_table <- renderDT({
      summary <- class_summary()
      shiny::req(!is.null(summary))
      datatable(summary, options = list(pageLength = 10, scrollX = TRUE))
    })

    output$download_class <- downloadHandler(
      filename = function() paste0("HistoneMod_mark_classes_", Sys.Date(), ".csv"),
      content = function(file) {
        summary <- class_summary()
        shiny::req(!is.null(summary))
        utils::write.csv(summary, file, row.names = FALSE)
      }
    )

    invisible(NULL)
  })
}
