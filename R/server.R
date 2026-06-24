##### Shiny Server Logic #####
# This file wires the UI to computation: validation, reactivity, plot rendering,
# export modals (with fixed-size preview + zoom), and PDF report generation.

#' HistoneMod Shiny Server Function
#'
#' Implements the server-side logic for the HistoneMod application, including
#' file validation, percentage calculation, plotting, data export, and PDF
#' report generation.
#'
#' @param input,output,session Standard Shiny server function arguments.
#'
#' @return No return value. Called for its side effects in a Shiny app.
#' @export
histonemod_server <- function(input, output, session) {

  ##### Export Settings (Persisted Across Modals) #####
  # CUSTOMIZE: Default export width/height/dpi for each plot type.
  # These are updated live while the export modal is open.
  # Persist export settings so switching modals (e.g., Zoom) doesn't reset user choices
  export_settings_pca <- reactiveVal(list(format = "png", width = 8, height = 6, dpi = 300))
  export_settings_hm <- reactiveVal(list(format = "png", width = 10, height = 8, dpi = 300))
  export_settings_bar <- reactiveVal(list(format = "png", width = 8, height = 6, dpi = 300))

  ##### Small Helpers (Input Picking / Numeric Safety) #####
  pick_val <- function(input_val, stored_val, default_val) {
    if (!is.null(input_val) && length(input_val) > 0 && !identical(input_val, "")) return(input_val)
    if (!is.null(stored_val) && length(stored_val) > 0 && !identical(stored_val, "")) return(stored_val)
    default_val
  }

  pick_num <- function(input_val, stored_val, default_val) {
    x <- suppressWarnings(as.numeric(input_val))
    if (length(x) == 1 && is.finite(x) && !is.na(x) && x > 0) return(x)
    y <- suppressWarnings(as.numeric(stored_val))
    if (length(y) == 1 && is.finite(y) && !is.na(y) && y > 0) return(y)
    default_val
  }

  clamp_num <- function(x, min_val, max_val) {
    if (length(x) != 1 || !is.finite(x) || is.na(x)) return(min_val)
    max(min_val, min(max_val, x))
  }

  # Use current on-screen plot size (CSS pixels) to derive a sensible default export size in inches.
  # Assumes 96 CSS pixels per inch (browser standard). Results are rounded to integers.
  # CUSTOMIZE: If your deployment environment uses a different CSS pixel density,
  # update the 96 px/in assumption here.
  get_screen_inches <- function(output_id, default_width_in, default_height_in) {
    w_px <- suppressWarnings(as.numeric(session$clientData[[paste0("output_", output_id, "_width")]]))
    h_px <- suppressWarnings(as.numeric(session$clientData[[paste0("output_", output_id, "_height")]]))

    w_in <- default_width_in
    h_in <- default_height_in
    if (length(w_px) == 1 && is.finite(w_px) && !is.na(w_px) && w_px > 0) {
      w_in <- round(w_px / 96)
    }
    if (length(h_px) == 1 && is.finite(h_px) && !is.na(h_px) && h_px > 0) {
      h_in <- round(h_px / 96)
    }

    list(
      width = clamp_num(w_in, 1, 20),
      height = clamp_num(h_in, 1, 20)
    )
  }

  ##### Export Modal: Keep Stored Settings In Sync #####
  # Keep reactive settings in sync with inputs while the export modal is open
  observeEvent(input$export_format_pca, {
    s <- export_settings_pca(); s$format <- input$export_format_pca; export_settings_pca(s)
  }, ignoreInit = TRUE)
  observeEvent(input$export_width_pca, {
    val <- suppressWarnings(as.numeric(input$export_width_pca))
    if (length(val) == 1 && is.finite(val) && !is.na(val) && val > 0) {
      s <- export_settings_pca(); s$width <- val; export_settings_pca(s)
    }
  }, ignoreInit = TRUE)
  observeEvent(input$export_height_pca, {
    val <- suppressWarnings(as.numeric(input$export_height_pca))
    if (length(val) == 1 && is.finite(val) && !is.na(val) && val > 0) {
      s <- export_settings_pca(); s$height <- val; export_settings_pca(s)
    }
  }, ignoreInit = TRUE)
  observeEvent(input$export_dpi_pca, {
    val <- suppressWarnings(as.numeric(input$export_dpi_pca))
    if (length(val) == 1 && is.finite(val) && !is.na(val) && val > 0) {
      s <- export_settings_pca(); s$dpi <- val; export_settings_pca(s)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$export_format_hm, {
    s <- export_settings_hm(); s$format <- input$export_format_hm; export_settings_hm(s)
  }, ignoreInit = TRUE)
  observeEvent(input$export_width_hm, {
    val <- suppressWarnings(as.numeric(input$export_width_hm))
    if (length(val) == 1 && is.finite(val) && !is.na(val) && val > 0) {
      s <- export_settings_hm(); s$width <- val; export_settings_hm(s)
    }
  }, ignoreInit = TRUE)
  observeEvent(input$export_height_hm, {
    val <- suppressWarnings(as.numeric(input$export_height_hm))
    if (length(val) == 1 && is.finite(val) && !is.na(val) && val > 0) {
      s <- export_settings_hm(); s$height <- val; export_settings_hm(s)
    }
  }, ignoreInit = TRUE)
  observeEvent(input$export_dpi_heatmap, {
    val <- suppressWarnings(as.numeric(input$export_dpi_heatmap))
    if (length(val) == 1 && is.finite(val) && !is.na(val) && val > 0) {
      s <- export_settings_hm(); s$dpi <- val; export_settings_hm(s)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$export_format_bar, {
    s <- export_settings_bar(); s$format <- input$export_format_bar; export_settings_bar(s)
  }, ignoreInit = TRUE)
  observeEvent(input$export_width_bar, {
    val <- suppressWarnings(as.numeric(input$export_width_bar))
    if (length(val) == 1 && is.finite(val) && !is.na(val) && val > 0) {
      s <- export_settings_bar(); s$width <- val; export_settings_bar(s)
    }
  }, ignoreInit = TRUE)
  observeEvent(input$export_height_bar, {
    val <- suppressWarnings(as.numeric(input$export_height_bar))
    if (length(val) == 1 && is.finite(val) && !is.na(val) && val > 0) {
      s <- export_settings_bar(); s$height <- val; export_settings_bar(s)
    }
  }, ignoreInit = TRUE)
  observeEvent(input$export_dpi_barplot, {
    val <- suppressWarnings(as.numeric(input$export_dpi_barplot))
    if (length(val) == 1 && is.finite(val) && !is.na(val) && val > 0) {
      s <- export_settings_bar(); s$dpi <- val; export_settings_bar(s)
    }
  }, ignoreInit = TRUE)

  ##### Export Modals (Preview + Zoom) #####
  # CUSTOMIZE: Supported formats and DPI presets are defined inside these modal builders.
  show_export_pca_modal <- function() {
    s <- export_settings_pca()
    showModal(modalDialog(
      title = div(style="font-size: 20px; font-weight: 600; color: #1e293b;",
                  icon("image"), " Export PCA Plot"),
      size = "m",
      easyClose = TRUE,
      div(class="p-3",
          uiOutput("export_preview_panel_pca"),
          div(class="mb-3",
              tags$label(class="form-label fw-bold", "Format"),
              selectInput("export_format_pca", NULL,
                          choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg", "JPEG" = "jpeg",
                                      "TIFF" = "tiff", "EPS" = "eps", "BMP" = "bmp"),
                    selected = pick_val(input$export_format_pca, s$format, "png"))
          ),
          div(class="row",
              div(class="col-md-6 mb-3",
                  tags$label(class="form-label fw-bold", "Width (inches)"),
                numericInput("export_width_pca", NULL, value = pick_num(input$export_width_pca, s$width, 8), min = 1, max = 20, step = 0.5)
              ),
              div(class="col-md-6 mb-3",
                  tags$label(class="form-label fw-bold", "Height (inches)"),
                numericInput("export_height_pca", NULL, value = pick_num(input$export_height_pca, s$height, 6), min = 1, max = 20, step = 0.5)
              )
          ),
          div(class="mb-3",
              tags$label(class="form-label fw-bold", "DPI (for PNG/JPEG/TIFF/BMP)"),
              selectInput("export_dpi_pca", NULL,
                          choices = c("300" = 300,
                                      "600" = 600,
                                      "900" = 900,
                                      "1200" = 1200,
                                      "1500" = 1500,
                                      "2400" = 2400),
                    selected = as.character(pick_num(input$export_dpi_pca, s$dpi, 300)))
          )
      ),
      footer = tagList(
        modalButton("Cancel"),
        downloadButton("download_pca_plot", "Download")
      )
    ))
  }

  show_export_heatmap_modal <- function() {
    s <- export_settings_hm()
    showModal(modalDialog(
      title = div(style="font-size: 20px; font-weight: 600; color: #1e293b;",
                  icon("image"), " Export Heatmap"),
      size = "m",
      easyClose = TRUE,
      div(class="p-3",
          uiOutput("export_preview_panel_heatmap"),
          div(class="mb-3",
              tags$label(class="form-label fw-bold", "Format"),
              selectInput("export_format_hm", NULL,
                          choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg", "JPEG" = "jpeg",
                                      "TIFF" = "tiff", "EPS" = "eps", "BMP" = "bmp"),
                    selected = pick_val(input$export_format_hm, s$format, "png"))
          ),
          div(class="row",
              div(class="col-md-6 mb-3",
                  tags$label(class="form-label fw-bold", "Width (inches)"),
                numericInput("export_width_hm", NULL, value = pick_num(input$export_width_hm, s$width, 10), min = 1, max = 20, step = 0.5)
              ),
              div(class="col-md-6 mb-3",
                  tags$label(class="form-label fw-bold", "Height (inches)"),
                numericInput("export_height_hm", NULL, value = pick_num(input$export_height_hm, s$height, 8), min = 1, max = 20, step = 0.5)
              )
          ),
          div(class="mb-3",
              tags$label(class="form-label fw-bold", "DPI (for PNG/JPEG/TIFF/BMP)"),
              selectInput("export_dpi_heatmap", NULL,
                          choices = c("300" = 300,
                                      "600" = 600,
                                      "900" = 900,
                                      "1200" = 1200,
                                      "1500" = 1500,
                                      "2400" = 2400),
                    selected = as.character(pick_num(input$export_dpi_heatmap, s$dpi, 300)))
          )
      ),
      footer = tagList(
        modalButton("Cancel"),
        downloadButton("download_heatmap_plot", "Download")
      )
    ))
  }

  show_export_barplot_modal <- function() {
    s <- export_settings_bar()
    showModal(modalDialog(
      title = div(style="font-size: 20px; font-weight: 600; color: #1e293b;",
                  icon("image"), " Export Barplots"),
      size = "m",
      easyClose = TRUE,
      div(class="p-3",
          uiOutput("export_preview_panel_barplot"),
          div(class="mb-3",
              tags$label(class="form-label fw-bold", "Format"),
              selectInput("export_format_bar", NULL,
                          choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg", "JPEG" = "jpeg",
                                      "TIFF" = "tiff", "EPS" = "eps", "BMP" = "bmp"),
                    selected = pick_val(input$export_format_bar, s$format, "png"))
          ),
          div(class="row",
              div(class="col-md-6 mb-3",
                  tags$label(class="form-label fw-bold", "Width (inches)"),
                numericInput("export_width_bar", NULL, value = pick_num(input$export_width_bar, s$width, 8), min = 1, max = 20, step = 0.5)
              ),
              div(class="col-md-6 mb-3",
                  tags$label(class="form-label fw-bold", "Height (inches)"),
                numericInput("export_height_bar", NULL, value = pick_num(input$export_height_bar, s$height, 6), min = 1, max = 20, step = 0.5)
              )
          ),
          div(class="mb-3",
              tags$label(class="form-label fw-bold", "DPI (for PNG/JPEG/TIFF/BMP)"),
              selectInput("export_dpi_barplot", NULL,
                          choices = c("300" = 300,
                                      "600" = 600,
                                      "900" = 900,
                                      "1200" = 1200,
                                      "1500" = 1500,
                                      "2400" = 2400),
                    selected = as.character(pick_num(input$export_dpi_barplot, s$dpi, 300)))
          )
      ),
      footer = tagList(
        modalButton("Cancel"),
        downloadButton("download_barplot", "Download")
      )
    ))
  }

  ##### Error Rendering Helper #####
  # Used when plot rendering fails: draws a diagnostic panel into the plot area.
  # CUSTOMIZE: Reduce detail if you prefer shorter error outputs.
  plot_error_panel <- function(e) {
    msg <- conditionMessage(e)
    cls <- paste(class(e), collapse = ", ")
    call_txt <- tryCatch(deparse(conditionCall(e)), error = function(err) NA_character_)

    trace_txt <- tryCatch({
      if (requireNamespace("rlang", quietly = TRUE)) {
        utils::capture.output(print(rlang::trace_back()))
      } else {
        utils::capture.output(print(utils::tail(sys.calls(), 50)))
      }
    }, error = function(err) {
      utils::capture.output(print(utils::tail(sys.calls(), 50)))
    })

    details <- paste(c(
      paste0("Error: ", msg),
      paste0("Class: ", cls),
      paste0("Call: ", call_txt),
      "",
      "Trace:",
      trace_txt
    ), collapse = "\n")

    graphics::plot.new()
    graphics::par(mar = c(1, 1, 1, 1))
    graphics::text(0, 1, adj = c(0, 1), labels = details, cex = 0.75)
    invisible(NULL)
  }
  
  ##### App State (Reactive Values) #####
  ms1_data <- reactiveVal(NULL)
  sample_data <- reactiveVal(NULL)
  filtered_data <- reactiveVal(NULL)
  current_plot <- reactiveVal(NULL)
  current_plot_type <- reactiveVal(NULL)

  ##### Version Footer (GitHub Release Check) #####
  latest_version_val <- reactiveVal(NA_character_)
  observeEvent(TRUE, {
    latest_version_val(tryCatch(get_latest_release_version_cached(GITHUB_REPO), error = function(e) NA_character_))
  }, once = TRUE)

  output$footer_line <- renderUI({
    current_version <- normalize_version(APP_VERSION)
    latest_version <- latest_version_val()

    repo_url <- paste0("https://github.com/", GITHUB_REPO)

    version_html <- if(is.na(current_version)) {
      "Version: unknown"
    } else if(!is.na(latest_version) && !identical(latest_version, current_version)) {
      paste0(
        'Version: <a href="', repo_url, '" target="_blank" style="color:#3b82f6;">', current_version, '</a>',
        ' | New version available: <a href="', repo_url, '" target="_blank" style="color:#3b82f6;">', latest_version, '</a>'
      )
    } else {
      paste0('Version: <a href="', repo_url, '" target="_blank" style="color:#3b82f6;">', current_version, '</a>')
    }

    copyright_part <- 'Powered by <a href="https://www.molekularbiologie.abi.med.uni-muenchen.de/personen/imhof_group/hua/index.html" target="_blank" style="color:#3b82f6;">Jie Hua</a>, <a href="https://www.molekularbiologie.abi.med.uni-muenchen.de/personen/imhof_group/borso/index.html" target="_blank" style="color:#3b82f6;">Dr. Marco Borso</a> and <a href="https://www.molekularbiologie.abi.med.uni-muenchen.de/personen/imhof_group/bozdag/index.html" target="_blank" style="color:#3b82f6;">Beyza Bozdağ</a>. Copyright © <a href="https://www.molekularbiologie.abi.med.uni-muenchen.de/personen/imhof_group/index.html" target="_blank" style="color:#3b82f6;">Imhof Group</a>'

    tags$p(
      class = "text-center mb-0",
      style = "color: #64748b; font-size: 13px;",
      HTML(paste0(copyright_part, " . ", version_html))
    )
  })
  
  ##### Demo Data (Used in Help Popups) #####
  # Demo data
  demo_ms1 <- data.frame(
    Replicate.Name       = c("5A_rep1", "5A_rep2"),
    Isotope.Label.Type   = c("light", "light"),
    Protein.Name         = c("H3_3-8", "H3_3-8"),
    Total.Area.MS1.Sum   = c(11572408320, 11156388864),
    Peptide.Note         = c("H3_3_8_K4_un", "H3_3_8_K9_ac")
  )
  
  
  demo_sample <- data.frame(
    Replicate.Name = c("5A_rep1", "5A_rep2"),
    Group = c("5A","5A"),
    Replicate.No = c(1,2)
  )
  
  ##### Sidebar: Validation Status UI #####
  # Validation status display
  output$validation_status <- renderUI({
    ms1_valid <- !is.null(input$ms1_file)
    sample_valid <- !is.null(input$sample_file)
    
    if(ms1_valid && sample_valid) {
      div(
        class = "validation-badge",
        style = "background: #d1fae5; color: #065f46; border: 1px solid #a7f3d0;",
        icon("check-circle"),
        span("Files validated successfully")
      )
    } else if(ms1_valid || sample_valid) {
      div(
        class = "validation-badge",
        style = "background: #fed7aa; color: #92400e; border: 1px solid #fcd34d;",
        icon("exclamation-circle"),
        span("Please upload both files")
      )
    } else {
      div(
        class = "validation-badge",
        style = "background: #e2e8f0; color: #475569; border: 1px solid #cbd5e1;",
        icon("info-circle"),
        span("Waiting for file upload")
      )
    }
  })
  
  ##### Tab: Data Preview (Conditional UI) #####
  # Preview content conditional UI
  output$preview_content <- renderUI({
    if(is.null(input$ms1_file) && is.null(input$sample_file)) {
      div(class = "plot-container text-center", style="padding: 60px 20px;",
        icon("upload", style="font-size: 48px; color: #94a3b8; margin-bottom: 16px;"),
        h4(style="color: #64748b; font-weight: 500;", "Please upload MS1 and Sample files"),
        p(style="color: #94a3b8; font-size: 14px;", "Upload your data files using the sidebar to view previews")
      )
    } else {
      tagList(
        div(class = "mb-4",
          h4(style = "font-weight: 600; color: #1e293b; margin-bottom: 16px;", 
             icon("table"), " MS1 File Preview"),
          div(class = "plot-container",
            DTOutput("ms1_preview")
          )
        ),
        div(class = "mb-4",
          h4(style = "font-weight: 600; color: #1e293b; margin-bottom: 16px;", 
             icon("table"), " Sample File Preview"),
          div(class = "plot-container",
            DTOutput("sample_preview")
          )
        )
      )
    }
  })
  
  ##### Data Preview Tables #####
  # MS1 file preview
  output$ms1_preview <- renderDT({
    req(input$ms1_file)
    validation <- validate_ms1_file(input$ms1_file$datapath)
    if(!validation$valid) {
      shinyalert("Invalid MS1 File", validation$message, type="error")
      return(NULL)
    }
    datatable(head(validation$data, 100), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Sample file preview
  output$sample_preview <- renderDT({
    req(input$sample_file)
    validation <- validate_sample_file(input$sample_file$datapath)
    if(!validation$valid) {
      shinyalert("Invalid Sample File", validation$message, type="error")
      return(NULL)
    }
    datatable(validation$data, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  ##### File Upload Initialization #####
  # Init the select sections after uploading
  observeEvent(list(input$ms1_file, input$sample_file), {
    req(input$ms1_file, input$sample_file)
    
    # Validate files
    ms1_validation <- validate_ms1_file(input$ms1_file$datapath)
    sample_validation <- validate_sample_file(input$sample_file$datapath)
    
    if(!ms1_validation$valid) {
      shinyalert("Invalid MS1 File", ms1_validation$message, type="error")
      return()
    }
    
    if(!sample_validation$valid) {
      shinyalert("Invalid Sample File", sample_validation$message, type="error")
      return()
    }
    
    ms1 <- ms1_validation$data
    sample <- sample_validation$data
    
    ms1_data(ms1)
    sample_data(sample)
    
    updatePickerInput(session, "select_peptides",
                      choices=unique(ms1$Peptide.Note),
                      selected=unique(ms1$Peptide.Note))
    updatePickerInput(session, "select_samples",
                      choices=unique(sample$Replicate.Name),
                      selected=unique(sample$Replicate.Name))
    updatePickerInput(session, "select_protein",
                      choices=unique(ms1$Protein.Name),
                      selected=unique(ms1$Protein.Name)[1])
    
    # Effect
    
    runjs("
    const duration = 3000;
    const animationEnd = Date.now() + duration;
    const defaults = { startVelocity: 30, spread: 360, ticks: 60, zIndex: 999 };

    function randomInRange(min, max) {
      return Math.random() * (max - min) + min;
    }

    const interval = setInterval(function() {
      const timeLeft = animationEnd - Date.now();
      if (timeLeft <= 0) {
        return clearInterval(interval);
      }
      const particleCount = 50 * (timeLeft / duration);
      confetti(Object.assign({}, defaults, { particleCount, origin: { x: randomInRange(0, 1), y: Math.random() - 0.2 } }));
    }, 250);
  ")
    
    
  })
  
  ##### Help / Info Popups #####
  # Alerts
  observeEvent(input$ms1_info, {
    shinyalert("MS1 CSV table:",
      HTML(paste0(
        "<div style='text-align:left; max-width:100%;'>",
        "Please upload a comma-delimited CSV file with at least these columns, like:<br>",
        "<div style='overflow-x:auto; max-width:100%;'>",
        "<table style='border-collapse:collapse; width:100%; border:1px solid #94a3b8; table-layout:fixed;'>",
        "<tr>",
        "<th style='border:1px solid #94a3b8; padding:4px 6px; white-space:normal; word-break:break-word;'>Protein Name</th>",
        "<th style='border:1px solid #94a3b8; padding:4px 6px; white-space:normal; word-break:break-word;'>Peptide Note</th>",
        "<th style='border:1px solid #94a3b8; padding:4px 6px; white-space:normal; word-break:break-word;'>Replicate Name</th>",
        "<th style='border:1px solid #94a3b8; padding:4px 6px; white-space:normal; word-break:break-word;'>Total Area MS1 Sum</th>",
        "<th style='border:1px solid #94a3b8; padding:4px 6px; white-space:normal; word-break:break-word;'>Isotope Label Type</th>",
        "</tr>",
        paste0(
          "<tr>",
          "<td style='border:1px solid #94a3b8; padding:4px 6px; white-space:normal; word-break:break-word;'>", demo_ms1$Protein.Name, "</td>",
          "<td style='border:1px solid #94a3b8; padding:4px 6px; white-space:normal; word-break:break-word;'>", demo_ms1$Peptide.Note, "</td>",
          "<td style='border:1px solid #94a3b8; padding:4px 6px; white-space:normal; word-break:break-word;'>", demo_ms1$Replicate.Name, "</td>",
          "<td style='border:1px solid #94a3b8; padding:4px 6px; white-space:normal; word-break:break-word;'>", demo_ms1$Total.Area.MS1.Sum, "</td>",
          "<td style='border:1px solid #94a3b8; padding:4px 6px; white-space:normal; word-break:break-word;'>", demo_ms1$Isotope.Label.Type, "</td>",
          "</tr>",
          collapse = ""
        ),
        "</table>",
        "</div>",
        "</div>"
      )),
               html=TRUE
    )
  })
  
  observeEvent(input$sample_info, {
    shinyalert("Sample CSV table:",
               HTML(paste0(
                 "<div style='text-align:left; max-width:100%;'>",
                 "Please upload a comma-delimited CSV file with at least these columns, like:<br>",
                 "<div style='overflow-x:auto; max-width:100%;'>",
                 "<table style='border-collapse:collapse; width:100%; border:1px solid #94a3b8; table-layout:fixed;'>",
                 "<tr>",
                 "<th style='border:1px solid #94a3b8; padding:4px 6px; white-space:normal; word-break:break-word;'>Replicate Name</th>",
                 "<th style='border:1px solid #94a3b8; padding:4px 6px; white-space:normal; word-break:break-word;'>Group</th>",
                 "<th style='border:1px solid #94a3b8; padding:4px 6px; white-space:normal; word-break:break-word;'>Replicate No</th>",
                 "</tr>",
                 paste0(
                   "<tr>",
                   "<td style='border:1px solid #94a3b8; padding:4px 6px; white-space:normal; word-break:break-word;'>", demo_sample$Replicate.Name, "</td>",
                   "<td style='border:1px solid #94a3b8; padding:4px 6px; white-space:normal; word-break:break-word;'>", demo_sample$Group, "</td>",
                   "<td style='border:1px solid #94a3b8; padding:4px 6px; white-space:normal; word-break:break-word;'>", demo_sample$Replicate.No, "</td>",
                   "</tr>",
                   collapse = ""
                 ),
                 "</table>",
                 "</div>",
                 "</div>"
               )),
               html=TRUE
    )
  })
  
  observeEvent(input$peptide_info, {
    shinyalert("Peptides Selection",
               "Here you can select the peptides to display (all selected by default).")
  })
  
  observeEvent(input$sample_select_info, {
    shinyalert("Samples Selection",
               "Here you can select the samples to display (all selected by default).")
  })
  
  observeEvent(input$exclude_info, {
    shinyalert("Exclude unmodified peptides",
               "Checking this option will exclude unmodified peptides.(name those peptides end with \'un\'. For example: H3_3_8_K4_un or H3_9_17_K8K14_unun ")
  })
  
  
  # Filter the data
  filtered_data_reactive <- reactive({
    req(ms1_data(), sample_data())
    df <- percentage_calculation(
      ms1_data(), sample_data(),
      exclude_un = input$exclude_un,
      selected_peptides = input$select_peptides,
      selected_samples = input$select_samples
    )
    filtered_data(df)
    ##### Data Pipeline (Reactive Filtered Data) #####
    df
  })
  
  # PCA
  pca_plot_reactive <- reactive({
    req(filtered_data_reactive())
    data <- filtered_data_reactive()
    
    # Check duplicates
    dup_check <- data %>% count(Replicate.Name, Peptide.Note) %>% filter(n > 1)
    shiny::validate(
      shiny::need(nrow(dup_check) == 0, "Check your data, it is wrong or it has duplicated ids. No PCA available now")
    )
    
    ##### PCA Tab Logic #####
    plot_pca(data, show_ellipse = input$show_ellipse, color_palette = input$pca_palette)
  })
  
  output$pca_plot <- renderPlot({ 
    tryCatch({
      p <- pca_plot_reactive()
      current_plot(p)
      current_plot_type("pca")
      print(p)
      invisible(NULL)
    }, error = function(e) {
      plot_error_panel(e)
    })
  })

  ##### Export Preview Images (PCA) #####
  # CUSTOMIZE: `max_px` controls preview speed/quality.
  output$export_preview_pca <- renderImage({
    req(pca_plot_reactive())
    s <- export_settings_pca()
    width_in <- pick_num(input$export_width_pca, s$width, 8)
    height_in <- pick_num(input$export_height_pca, s$height, 6)

    export_dpi <- pick_num(input$export_dpi_pca, s$dpi, 300)
    if(!is.finite(export_dpi) || export_dpi <= 0) export_dpi <- 300

    # Keep preview fast while preserving aspect ratio
    max_px <- 1600
    preview_dpi <- min(export_dpi, floor(max_px / max(width_in, height_in)))
    if(!is.finite(preview_dpi) || preview_dpi < 72) preview_dpi <- min(export_dpi, 150)

    outfile <- tempfile(fileext = ".png")
    tryCatch({
      ggplot2::ggsave(
        filename = outfile,
        plot = pca_plot_reactive(),
        width = width_in,
        height = height_in,
        dpi = preview_dpi,
        device = "png"
      )
    }, error = function(e) {
      grDevices::png(outfile, width = 8, height = 4, units = "in", res = 150)
      plot_error_panel(e)
      grDevices::dev.off()
    })

    list(src = outfile, contentType = "image/png", alt = "PCA export preview")
  }, deleteFile = TRUE)

  output$export_preview_pca_zoom <- renderImage({
    req(pca_plot_reactive())
    s <- export_settings_pca()
    width_in <- pick_num(input$export_width_pca, s$width, 8)
    height_in <- pick_num(input$export_height_pca, s$height, 6)

    export_dpi <- pick_num(input$export_dpi_pca, s$dpi, 300)
    if(!is.finite(export_dpi) || export_dpi <= 0) export_dpi <- 300

    max_px <- 3200
    preview_dpi <- min(export_dpi, floor(max_px / max(width_in, height_in)))
    if(!is.finite(preview_dpi) || preview_dpi < 72) preview_dpi <- min(export_dpi, 150)

    outfile <- tempfile(fileext = ".png")
    tryCatch({
      ggplot2::ggsave(
        filename = outfile,
        plot = pca_plot_reactive(),
        width = width_in,
        height = height_in,
        dpi = preview_dpi,
        device = "png"
      )
    }, error = function(e) {
      grDevices::png(outfile, width = 8, height = 4, units = "in", res = 150)
      plot_error_panel(e)
      grDevices::dev.off()
    })

    list(src = outfile, contentType = "image/png", alt = "PCA export preview (zoom)")
  }, deleteFile = TRUE)

  output$export_preview_panel_pca <- renderUI({
    div(class = "mb-3",
        div(class = "d-flex justify-content-between align-items-center mb-2",
            tags$label(class="form-label fw-bold mb-0", "Preview"),
            actionButton(
              "export_preview_zoom_pca",
              "Zoom",
              icon = icon("search-plus"),
              class = "btn btn-outline-secondary btn-sm"
            )
        ),
        div(
          class = "export-preview-frame export-preview-frame--fixed",
          imageOutput("export_preview_pca", height = "100%")
        )
    )
  })
  
  ##### Download Handler (PCA) #####
  # Export PCA plot
  observeEvent(input$export_pca, {
    req(pca_plot_reactive())
    # Default width/height reflect what the user sees on screen (rounded inches)
    s <- export_settings_pca()
    dims <- get_screen_inches("pca_plot", default_width_in = 8, default_height_in = 6)
    export_settings_pca(list(
      format = pick_val(input$export_format_pca, s$format, "png"),
      width = dims$width,
      height = dims$height,
      dpi = pick_num(input$export_dpi_pca, s$dpi, 300)
    ))
    show_export_pca_modal()
  })

  observeEvent(input$export_preview_zoom_pca, {
    req(pca_plot_reactive())

    # snapshot current choices before switching modals
    s <- export_settings_pca()
    s$format <- pick_val(input$export_format_pca, s$format, "png")
    s$width <- pick_num(input$export_width_pca, s$width, 8)
    s$height <- pick_num(input$export_height_pca, s$height, 6)
    s$dpi <- pick_num(input$export_dpi_pca, s$dpi, 300)
    export_settings_pca(s)

    showModal(modalDialog(
      title = div(style="font-size: 20px; font-weight: 600; color: #1e293b;",
                  icon("search-plus"), "Preview (Zoom)"),
      size = "l",
      easyClose = FALSE,
      div(class = "p-3",
          div(class = "export-preview-frame export-preview-frame--zoom",
              imageOutput("export_preview_pca_zoom", height = "100%"))
      ),
      footer = tagList(
        actionButton("export_zoom_back_pca", "Back", class = "btn btn-secondary")
      )
    ))
  })

  observeEvent(input$export_zoom_back_pca, {
    req(pca_plot_reactive())
    removeModal()
    show_export_pca_modal()
  })
  
  output$download_pca_plot <- downloadHandler(
    filename = function() {
      s <- export_settings_pca()
      fmt <- pick_val(input$export_format_pca, s$format, "png")
      paste0("PCA_plot_", Sys.Date(), ".", fmt)
    },
    content = function(file) {
      req(pca_plot_reactive())
      s <- export_settings_pca()
      fmt <- pick_val(input$export_format_pca, s$format, "png")
      width_in <- pick_num(input$export_width_pca, s$width, 8)
      height_in <- pick_num(input$export_height_pca, s$height, 6)
      dpi_val <- pick_num(input$export_dpi_pca, s$dpi, 300)

      # snapshot for future opens
      export_settings_pca(list(format = fmt, width = width_in, height = height_in, dpi = dpi_val))

      tryCatch({
        ggplot2::ggsave(
          filename = file,
          plot = pca_plot_reactive(),
          width = width_in,
          height = height_in,
          dpi = dpi_val,
          device = fmt
        )
        removeModal()
      }, error = function(e) {
        shinyalert::shinyalert(
          "Download failed",
          paste0("Could not export the plot.\n\n", conditionMessage(e)),
          type = "error"
        )
      })
    }
  )
  
  ##### Heatmap Tab Logic #####
  # ------------------------
  # Heatmap
  # ------------------------
  heatmap_plot_reactive <- reactive({
    req(filtered_data_reactive())
    data <- filtered_data_reactive()
    
    # Check duplicates
    dup_check <- data %>% count(Replicate.Name, Peptide.Note) %>% filter(n > 1)
    shiny::validate(shiny::need(nrow(dup_check) == 0, "Check your data, it is wrong or it has duplicated ids. No Heatmap available now"))
    
    plot_heatmap(data, cluster_rows = input$cluster_rows, cluster_cols = input$cluster_cols, color_palette = input$heatmap_palette)
  })
  
  output$heatmap_plot <- renderPlot({ 
    tryCatch({
      p <- heatmap_plot_reactive()
      current_plot(p)
      current_plot_type("heatmap")
      grid::grid.newpage()
      grid::grid.draw(p$gtable)
      invisible(NULL)
    }, error = function(e) {
      plot_error_panel(e)
    })
  })

  ##### Export Preview Images (Heatmap) #####
  # CUSTOMIZE: `max_px` controls preview speed/quality.
  output$export_preview_heatmap <- renderImage({
    req(heatmap_plot_reactive())
    s <- export_settings_hm()
    width_in <- pick_num(input$export_width_hm, s$width, 10)
    height_in <- pick_num(input$export_height_hm, s$height, 8)

    export_dpi <- pick_num(input$export_dpi_heatmap, s$dpi, 300)
    if(!is.finite(export_dpi) || export_dpi <= 0) export_dpi <- 300

    max_px <- 1600
    preview_dpi <- min(export_dpi, floor(max_px / max(width_in, height_in)))
    if(!is.finite(preview_dpi) || preview_dpi < 72) preview_dpi <- min(export_dpi, 150)

    outfile <- tempfile(fileext = ".png")
    tryCatch({
      grDevices::png(outfile, width = width_in, height = height_in, units = "in", res = preview_dpi)
      p <- heatmap_plot_reactive()
      grid::grid.newpage()
      grid::grid.draw(p$gtable)
      grDevices::dev.off()
    }, error = function(e) {
      tryCatch(grDevices::dev.off(), error = function(err) NULL)
      grDevices::png(outfile, width = 8, height = 4, units = "in", res = 150)
      plot_error_panel(e)
      grDevices::dev.off()
    })

    list(src = outfile, contentType = "image/png", alt = "Heatmap export preview")
  }, deleteFile = TRUE)

  output$export_preview_heatmap_zoom <- renderImage({
    req(heatmap_plot_reactive())
    s <- export_settings_hm()
    width_in <- pick_num(input$export_width_hm, s$width, 10)
    height_in <- pick_num(input$export_height_hm, s$height, 8)

    export_dpi <- pick_num(input$export_dpi_heatmap, s$dpi, 300)
    if(!is.finite(export_dpi) || export_dpi <= 0) export_dpi <- 300

    max_px <- 3200
    preview_dpi <- min(export_dpi, floor(max_px / max(width_in, height_in)))
    if(!is.finite(preview_dpi) || preview_dpi < 72) preview_dpi <- min(export_dpi, 150)

    outfile <- tempfile(fileext = ".png")
    tryCatch({
      grDevices::png(outfile, width = width_in, height = height_in, units = "in", res = preview_dpi)
      p <- heatmap_plot_reactive()
      grid::grid.newpage()
      grid::grid.draw(p$gtable)
      grDevices::dev.off()
    }, error = function(e) {
      tryCatch(grDevices::dev.off(), error = function(err) NULL)
      grDevices::png(outfile, width = 8, height = 4, units = "in", res = 150)
      plot_error_panel(e)
      grDevices::dev.off()
    })

    list(src = outfile, contentType = "image/png", alt = "Heatmap export preview (zoom)")
  }, deleteFile = TRUE)

  output$export_preview_panel_heatmap <- renderUI({
    div(class = "mb-3",
        div(class = "d-flex justify-content-between align-items-center mb-2",
            tags$label(class="form-label fw-bold mb-0", "Preview"),
            actionButton(
              "export_preview_zoom_heatmap",
              "Zoom",
              icon = icon("search-plus"),
              class = "btn btn-outline-secondary btn-sm"
            )
        ),
        div(
          class = "export-preview-frame export-preview-frame--fixed",
          imageOutput("export_preview_heatmap", height = "100%")
        )
    )
  })
  
  # Export Heatmap
  observeEvent(input$export_heatmap, {
    req(heatmap_plot_reactive())
    s <- export_settings_hm()
    dims <- get_screen_inches("heatmap_plot", default_width_in = 10, default_height_in = 8)
    export_settings_hm(list(
      format = pick_val(input$export_format_hm, s$format, "png"),
      width = dims$width,
      height = dims$height,
      dpi = pick_num(input$export_dpi_heatmap, s$dpi, 300)
    ))
    show_export_heatmap_modal()
  })

  observeEvent(input$export_preview_zoom_heatmap, {
    req(heatmap_plot_reactive())

    s <- export_settings_hm()
    s$format <- pick_val(input$export_format_hm, s$format, "png")
    s$width <- pick_num(input$export_width_hm, s$width, 10)
    s$height <- pick_num(input$export_height_hm, s$height, 8)
    s$dpi <- pick_num(input$export_dpi_heatmap, s$dpi, 300)
    export_settings_hm(s)

    showModal(modalDialog(
      title = div(style="font-size: 20px; font-weight: 600; color: #1e293b;",
                  icon("search-plus"), "Preview (Zoom)"),
      size = "l",
      easyClose = FALSE,
      div(class = "p-3",
          div(class = "export-preview-frame export-preview-frame--zoom",
              imageOutput("export_preview_heatmap_zoom", height = "100%"))
      ),
      footer = tagList(
        actionButton("export_zoom_back_heatmap", "Back", class = "btn btn-secondary")
      )
    ))
  })

  observeEvent(input$export_zoom_back_heatmap, {
    req(heatmap_plot_reactive())
    removeModal()
    show_export_heatmap_modal()
  })
  
  ##### Download Handler (Heatmap) #####
  output$download_heatmap_plot <- downloadHandler(
    filename = function() {
      s <- export_settings_hm()
      fmt <- pick_val(input$export_format_hm, s$format, "png")
      paste0("Heatmap_", Sys.Date(), ".", fmt)
    },
    content = function(file) {
      req(heatmap_plot_reactive())

      s <- export_settings_hm()
      fmt <- pick_val(input$export_format_hm, s$format, "png")
      width_in <- pick_num(input$export_width_hm, s$width, 10)
      height_in <- pick_num(input$export_height_hm, s$height, 8)
      dpi_val <- pick_num(input$export_dpi_heatmap, s$dpi, 300)
      export_settings_hm(list(format = fmt, width = width_in, height = height_in, dpi = dpi_val))

      export_plot <- function() {
        p <- heatmap_plot_reactive()
        grid::grid.newpage()
        grid::grid.draw(p$gtable)
        invisible(NULL)
      }

      tryCatch({
        if(fmt %in% c("pdf", "svg", "eps")) {
          if(fmt == "pdf") {
            grDevices::pdf(file, width = width_in, height = height_in)
          } else if(fmt == "svg") {
            grDevices::svg(file, width = width_in, height = height_in)
          } else {
            grDevices::setEPS()
            grDevices::postscript(file, width = width_in, height = height_in)
          }
          export_plot()
          grDevices::dev.off()
        } else {
          # For raster formats
          device_func <- switch(fmt,
                                "png" = grDevices::png,
                                "jpeg" = grDevices::jpeg,
                                "tiff" = grDevices::tiff,
                                "bmp" = grDevices::bmp,
                                grDevices::png)
          device_func(file, width = width_in, height = height_in,
                      units = "in", res = dpi_val)
          export_plot()
          grDevices::dev.off()
        }
        removeModal()
      }, error = function(e) {
        tryCatch(grDevices::dev.off(), error = function(err) NULL)
        shinyalert::shinyalert(
          "Download failed",
          paste0("Could not export the plot.\n\n", conditionMessage(e)),
          type = "error"
        )
      })
    }
  )
  
  ##### Barplot Tab Logic #####
  # Barplot - update peptide choices when protein changes
  observeEvent(input$select_protein, {
    req(filtered_data_reactive(), input$select_protein)
    data <- filtered_data_reactive()
    peptides <- unique(data$Peptide.Note[data$Protein.Name == input$select_protein])
    updatePickerInput(session, "select_peptide_barplot", choices = peptides, selected = peptides[1])
  })
  
  # Generate single barplot
  current_barplot <- reactive({
    req(filtered_data_reactive(), input$select_protein, input$select_peptide_barplot)
    data <- filtered_data_reactive()
    y_limits <- NULL
    if(isFALSE(input$barplot_y_auto)) {
      y_limits <- input$barplot_y_range
    }
    plot_barplot_single(
      data,
      input$select_protein,
      input$select_peptide_barplot,
      add_signif = input$add_signif,
      color_palette = input$barplot_palette,
      y_limits = y_limits
    )
  })
  
  output$barplot_single <- renderPlot({
    current_barplot()
  })

  ##### Export Preview Images (Barplot) #####
  # CUSTOMIZE: `max_px` controls preview speed/quality.
  output$export_preview_barplot <- renderImage({
    req(current_barplot())
    s <- export_settings_bar()
    width_in <- pick_num(input$export_width_bar, s$width, 8)
    height_in <- pick_num(input$export_height_bar, s$height, 6)

    export_dpi <- pick_num(input$export_dpi_barplot, s$dpi, 300)
    if(!is.finite(export_dpi) || export_dpi <= 0) export_dpi <- 300

    max_px <- 1600
    preview_dpi <- min(export_dpi, floor(max_px / max(width_in, height_in)))
    if(!is.finite(preview_dpi) || preview_dpi < 72) preview_dpi <- min(export_dpi, 150)

    outfile <- tempfile(fileext = ".png")
    tryCatch({
      ggplot2::ggsave(
        filename = outfile,
        plot = current_barplot(),
        width = width_in,
        height = height_in,
        dpi = preview_dpi,
        device = "png"
      )
    }, error = function(e) {
      grDevices::png(outfile, width = 8, height = 4, units = "in", res = 150)
      plot_error_panel(e)
      grDevices::dev.off()
    })

    list(src = outfile, contentType = "image/png", alt = "Barplot export preview")
  }, deleteFile = TRUE)

  output$export_preview_barplot_zoom <- renderImage({
    req(current_barplot())
    s <- export_settings_bar()
    width_in <- pick_num(input$export_width_bar, s$width, 8)
    height_in <- pick_num(input$export_height_bar, s$height, 6)

    export_dpi <- pick_num(input$export_dpi_barplot, s$dpi, 300)
    if(!is.finite(export_dpi) || export_dpi <= 0) export_dpi <- 300

    max_px <- 3200
    preview_dpi <- min(export_dpi, floor(max_px / max(width_in, height_in)))
    if(!is.finite(preview_dpi) || preview_dpi < 72) preview_dpi <- min(export_dpi, 150)

    outfile <- tempfile(fileext = ".png")
    tryCatch({
      ggplot2::ggsave(
        filename = outfile,
        plot = current_barplot(),
        width = width_in,
        height = height_in,
        dpi = preview_dpi,
        device = "png"
      )
    }, error = function(e) {
      grDevices::png(outfile, width = 8, height = 4, units = "in", res = 150)
      plot_error_panel(e)
      grDevices::dev.off()
    })

    list(src = outfile, contentType = "image/png", alt = "Barplot export preview (zoom)")
  }, deleteFile = TRUE)

  output$export_preview_panel_barplot <- renderUI({
    div(class = "mb-3",
        div(class = "d-flex justify-content-between align-items-center mb-2",
            tags$label(class="form-label fw-bold mb-0", "Preview"),
            actionButton(
              "export_preview_zoom_barplot",
              "Zoom",
              icon = icon("search-plus"),
              class = "btn btn-outline-secondary btn-sm"
            )
        ),
        div(
          class = "export-preview-frame export-preview-frame--fixed",
          imageOutput("export_preview_barplot", height = "100%")
        )
    )
  })
  
  # Export Barplot
  observeEvent(input$export_barplot, {
    req(current_barplot())
    s <- export_settings_bar()
    dims <- get_screen_inches("barplot_single", default_width_in = 8, default_height_in = 6)
    export_settings_bar(list(
      format = pick_val(input$export_format_bar, s$format, "png"),
      width = dims$width,
      height = dims$height,
      dpi = pick_num(input$export_dpi_barplot, s$dpi, 300)
    ))
    show_export_barplot_modal()
  })

  observeEvent(input$export_preview_zoom_barplot, {
    req(current_barplot())

    s <- export_settings_bar()
    s$format <- pick_val(input$export_format_bar, s$format, "png")
    s$width <- pick_num(input$export_width_bar, s$width, 8)
    s$height <- pick_num(input$export_height_bar, s$height, 6)
    s$dpi <- pick_num(input$export_dpi_barplot, s$dpi, 300)
    export_settings_bar(s)

    showModal(modalDialog(
      title = div(style="font-size: 20px; font-weight: 600; color: #1e293b;",
                  icon("search-plus"), "Preview (Zoom)"),
      size = "l",
      easyClose = FALSE,
      div(class = "p-3",
          div(class = "export-preview-frame export-preview-frame--zoom",
              imageOutput("export_preview_barplot_zoom", height = "100%"))
      ),
      footer = tagList(
        actionButton("export_zoom_back_barplot", "Back", class = "btn btn-secondary")
      )
    ))
  })

  observeEvent(input$export_zoom_back_barplot, {
    req(current_barplot())
    removeModal()
    show_export_barplot_modal()
  })
  
  ##### Download Handler (Barplot) #####
  output$download_barplot <- downloadHandler(
    filename = function() {
      peptide_name <- gsub("[^A-Za-z0-9_-]", "_", input$select_peptide_barplot)
      s <- export_settings_bar()
      fmt <- pick_val(input$export_format_bar, s$format, "png")
      paste0("Barplot_", input$select_protein, "_", peptide_name, "_", Sys.Date(), ".", fmt)
    },
    content = function(file) {
      req(current_barplot())
      s <- export_settings_bar()
      fmt <- pick_val(input$export_format_bar, s$format, "png")
      width_in <- pick_num(input$export_width_bar, s$width, 8)
      height_in <- pick_num(input$export_height_bar, s$height, 6)
      dpi_val <- pick_num(input$export_dpi_barplot, s$dpi, 300)
      export_settings_bar(list(format = fmt, width = width_in, height = height_in, dpi = dpi_val))

      tryCatch({
        ggplot2::ggsave(
          filename = file,
          plot = current_barplot(),
          width = width_in,
          height = height_in,
          dpi = dpi_val,
          device = fmt
        )
        removeModal()
      }, error = function(e) {
        shinyalert::shinyalert(
          "Download failed",
          paste0("Could not export the plot.\n\n", conditionMessage(e)),
          type = "error"
        )
      })
    }
  )
  
  ##### Tab: Table #####
  # Data table
  output$data_table <- renderDT({
    req(filtered_data_reactive())
    datatable(filtered_data_reactive())
  })
  
  ##### Download: Wide Table (CSV) #####
  # Download
  output$download_filtered <- downloadHandler(
    filename = function() { paste0("Wide_Format_Table_", Sys.Date(), ".csv") },
    content = function(file) {
      df <- filtered_data_reactive()
      wide <- df %>%
        select(Replicate.Name, Peptide.Note, Percentage) %>%
        pivot_wider(names_from = Peptide.Note, values_from = Percentage, values_fill=0)
      write.csv(wide, file, row.names=FALSE)
    }
  )

  ##### Download: PDF Quick Report #####
  # CUSTOMIZE: The `pdf_cfg` list (below) is the central place to tweak page size,
  # margins, plot target sizes, and cover layout.
  output$download_plots_pdf <- downloadHandler(
    filename = function() {
      paste0("HistoneMod_Report_", Sys.Date(), ".pdf")
    },
    contentType = "application/pdf",
    content = function(file) {
      df_for_counts <- filtered_data_reactive()
      pca_plot_obj <- pca_plot_reactive()
      req(df_for_counts)
      req(pca_plot_obj)

      ##### PDF Layout Knobs (Manual Configuration) #####
      # CUSTOMIZE: Edit these values to control text/image sizes and positions.
      pdf_cfg <- list(
        # A4 landscape (PPT-like)
        page_width_in = 11.69,
        page_height_in = 8.27,

        # Page header/footer
        page_title_fontsize = 16,
        page_footer_fontsize = 12,
        page_footer_y_npc = 0.040,

        # Main content viewport (keeps margins consistent)
        content_width_npc = 0.92,
        content_height_npc = 0.78,

        # Plot placement: target size in inches (will shrink if too large)
        pca_width_in = 10,
        pca_height_in = 6,
        heatmap_width_in = 18,
        heatmap_height_in = 12,
        heatmap_font_scale = 1.5,

        # Barplot page layout + each single barplot size
        barplot_ncols = 3,
        barplot_nrows = 2,
        barplot_plots_per_page = 6,
        barplot_single_width_in = 15,
        barplot_single_height_in = 12,
        barplot_font_scale = 3,

        # Cover: logo
        cover_logo_x_npc = 0.08,
        cover_logo_y_npc = 0.90,
        cover_logo_width_in = 1.10,
        cover_logo_height_in = 1.10,

        # Cover: title
        cover_title = "Histone PTM Quantification Report",
        cover_title_x_npc = 0.50,
        cover_title_y_npc = 0.60,
        cover_title_fontsize = 32,

        # Cover: body text
        cover_lines_x_npc = 0.12,
        cover_lines_y_npc = 0.40,
        cover_lines_fontsize = 12,
        cover_lines_lineheight = 1.2,
        cover_lines = c(
          paste0("Tool: HistoneMod (v", normalize_version(APP_VERSION), ")"),
          paste0("Link: https://github.com/", GITHUB_REPO),
          paste0("Date: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
          "Device: (edit me)",
          ""
        )
      )

      as_pos_num <- function(x, default) {
        val <- suppressWarnings(as.numeric(x))
        if (!is.finite(val) || is.na(val) || val <= 0) default else val
      }

      clean_chr_vec <- function(x) {
        if (is.null(x)) return(character(0))
        x <- as.character(x)
        x <- x[!is.na(x)]
        x <- x[nzchar(trimws(x))]
        unique(x)
      }

      build_barplot_plan <- function(df) {
        proteins <- sort(clean_chr_vec(df$Protein.Name))

        ncols <- suppressWarnings(as.integer(pdf_cfg$barplot_ncols))
        nrows <- suppressWarnings(as.integer(pdf_cfg$barplot_nrows))
        if (!is.finite(ncols) || is.na(ncols) || ncols <= 0) ncols <- 2L
        if (!is.finite(nrows) || is.na(nrows) || nrows <= 0) nrows <- 3L

        plots_per_page <- suppressWarnings(as.integer(pdf_cfg$barplot_plots_per_page))
        if (!is.finite(plots_per_page) || is.na(plots_per_page) || plots_per_page <= 0) {
          plots_per_page <- ncols * nrows
        }

        chunks_by_protein <- list()
        page_count <- 0L

        for (protein in proteins) {
          peptides <- clean_chr_vec(df$Peptide.Note[df$Protein.Name == protein])
          peptides <- sort(peptides)
          if (length(peptides) == 0) next
          chunks <- split(peptides, ceiling(seq_along(peptides) / plots_per_page))
          chunks_by_protein[[protein]] <- chunks
          page_count <- page_count + length(chunks)
        }

        list(
          proteins = names(chunks_by_protein),
          chunks_by_protein = chunks_by_protein,
          page_count = page_count
        )
      }

      barplot_plan <- build_barplot_plan(df_for_counts)
      barplot_page_count <- barplot_plan$page_count
      total_pages <- 4 + barplot_page_count
      current_page <- 0L

      page_width_in <- pdf_cfg$page_width_in
      page_height_in <- pdf_cfg$page_height_in

      draw_page_title <- function(title_text) {
        grid::grid.text(
          label = title_text,
          x = 0.06,
          y = 0.975,
          just = c("left", "top"),
          gp = grid::gpar(fontsize = pdf_cfg$page_title_fontsize, fontface = "bold", col = "#0f172a")
        )
        grid::grid.lines(x = c(0.05, 0.95), y = c(0.945, 0.945), gp = grid::gpar(col = "#cbd5e1", lwd = 1))
      }

      draw_page_footer <- function(page_idx, total_pages) {
        grid::grid.text(
          label = paste0("Page ", page_idx, "/", total_pages),
          x = 0.5,
          y = pdf_cfg$page_footer_y_npc,
          gp = grid::gpar(fontsize = pdf_cfg$page_footer_fontsize, col = "black")
        )
      }

      next_page <- function(title_text = NULL, body_expr) {
        current_page <<- current_page + 1L
        grid::grid.newpage()
        if (!is.null(title_text)) draw_page_title(title_text)
        if (!is.null(body_expr)) {
          tryCatch(
            {
              body_expr()
            },
            error = function(e) {
              with_full_content_viewport({
                grid::grid.text(
                  paste0("Page failed: ", conditionMessage(e)),
                  x = 0.02,
                  y = 0.98,
                  just = c("left", "top"),
                  gp = grid::gpar(fontsize = 12, col = "#0f172a")
                )
              })
            }
          )
        }
        draw_page_footer(current_page, total_pages)
        invisible(NULL)
      }

      with_full_content_viewport <- function(expr) {
        vp <- grid::viewport(
          x = 0.5,
          y = 0.50,
          width = pdf_cfg$content_width_npc,
          height = pdf_cfg$content_height_npc,
          just = "center"
        )
        grid::pushViewport(vp)
        on.exit(grid::popViewport(), add = TRUE)
        force(expr)
        invisible(NULL)
      }

      # Fit a target (w,h) in inches into the content area, keeping aspect ratio.
      # Returns a scale factor (<= 1) that indicates how much we had to shrink.
      with_inch_box_in_content <- function(target_w_in, target_h_in, render_fn) {
        w_in <- as_pos_num(target_w_in, 8)
        h_in <- as_pos_num(target_h_in, 6)

        content_w_in <- page_width_in * pdf_cfg$content_width_npc
        content_h_in <- page_height_in * pdf_cfg$content_height_npc
        shrink <- max(w_in / content_w_in, h_in / content_h_in, 1)
        draw_w_in <- w_in / shrink
        draw_h_in <- h_in / shrink
        scale <- 1 / shrink

        with_full_content_viewport({
          grid::pushViewport(
            grid::viewport(
              width = grid::unit(draw_w_in, "in"),
              height = grid::unit(draw_h_in, "in"),
              just = "center"
            )
          )
          on.exit(grid::popViewport(), add = TRUE)
          render_fn(scale)
        })

        invisible(NULL)
      }

      cover_page <- function() {
        grid::grid.rect(gp = grid::gpar(col = NA, fill = "white"))

        # Logo (optional) - keep original aspect ratio
        logo_path <- histonemod_logo_path()
        if (file.exists(logo_path) && requireNamespace("png", quietly = TRUE)) {
          img <- tryCatch(png::readPNG(logo_path), error = function(e) NULL)
          if (!is.null(img)) {
            ar <- nrow(img) / ncol(img)
            if (!is.finite(ar) || is.na(ar) || ar <= 0) ar <- 1
            max_w_in <- suppressWarnings(as.numeric(pdf_cfg$cover_logo_width_in))
            max_h_in <- suppressWarnings(as.numeric(pdf_cfg$cover_logo_height_in))
            if (!is.finite(max_w_in) || is.na(max_w_in) || max_w_in <= 0) max_w_in <- 1
            if (!is.finite(max_h_in) || is.na(max_h_in) || max_h_in <= 0) max_h_in <- 1

            # Fit within max_w_in x max_h_in without changing aspect ratio
            w_by_h <- max_h_in / ar
            logo_w_in <- min(max_w_in, w_by_h)
            logo_h_in <- logo_w_in * ar
            grid::grid.raster(
              img,
              x = pdf_cfg$cover_logo_x_npc,
              y = pdf_cfg$cover_logo_y_npc,
              width = grid::unit(logo_w_in, "in"),
              height = grid::unit(logo_h_in, "in"),
              just = c("left", "center")
            )
          }
        }

        # Main title
        grid::grid.text(
          pdf_cfg$cover_title,
          x = pdf_cfg$cover_title_x_npc,
          y = pdf_cfg$cover_title_y_npc,
          gp = grid::gpar(fontsize = pdf_cfg$cover_title_fontsize, fontface = "bold", col = "#0f172a")
        )

        # Cover body text (manual, but Device is auto)
        host_name <- Sys.info()[["nodename"]]
        os_info <- paste(Sys.info()[["sysname"]], Sys.info()[["release"]])
        device_line <- paste0(
          "Device: ",
          ifelse(is.na(host_name) || !nzchar(host_name), "unknown", host_name),
          " (", os_info, ")"
        )

        r_line <- paste0("R: ", R.version.string)

        cover_lines <- pdf_cfg$cover_lines
        if (is.null(cover_lines) || length(cover_lines) == 0) cover_lines <- character(0)

        # Remove Notes line(s) if present
        cover_lines <- cover_lines[!grepl("^Notes\\s*:", cover_lines)]

        # Remove R line(s) if present, then insert the current R version.
        cover_lines <- cover_lines[!grepl("^R(\\s+version)?\\s*:", cover_lines, ignore.case = TRUE)]
        date_idx <- grep("^Date\\s*:", cover_lines)
        if (length(date_idx) > 0) {
          cover_lines <- append(cover_lines, values = r_line, after = date_idx[1])
        } else {
          link_idx <- grep("^Link\\s*:", cover_lines)
          if (length(link_idx) > 0) {
            cover_lines <- append(cover_lines, values = r_line, after = link_idx[1])
          } else {
            cover_lines <- c(cover_lines, r_line)
          }
        }

        device_idx <- grep("^Device\\s*:", cover_lines)
        if (length(device_idx) > 0) {
          cover_lines[device_idx] <- device_line
        } else {
          date_idx <- grep("^Date\\s*:", cover_lines)
          if (length(date_idx) > 0) {
            insert_at <- date_idx[1]
            cover_lines <- append(cover_lines, values = device_line, after = insert_at)
          } else {
            cover_lines <- c(cover_lines, device_line)
          }
        }

        grid::grid.text(
          paste(cover_lines, collapse = "\n"),
          x = pdf_cfg$cover_lines_x_npc,
          y = pdf_cfg$cover_lines_y_npc,
          just = c("left", "top"),
          gp = grid::gpar(fontsize = pdf_cfg$cover_lines_fontsize, col = "#0f172a", lineheight = pdf_cfg$cover_lines_lineheight)
        )
      }

      data_source_page <- function() {
        ms1_name <- if(!is.null(input$ms1_file)) input$ms1_file$name else NA_character_
        sample_name <- if(!is.null(input$sample_file)) input$sample_file$name else NA_character_

        df <- df_for_counts
        group_n <- if("Group" %in% names(df)) length(unique(df$Group)) else NA_integer_

        y_axis_mode <- if(isTRUE(input$barplot_y_auto)) "Auto" else "Manual"
        y_axis_range <- if(isTRUE(input$barplot_y_auto)) NA_character_ else paste0("[", paste(input$barplot_y_range, collapse = ", "), "]")

        heading_gp <- grid::gpar(fontsize = 13, fontface = "bold", col = "#0f172a")
        item_gp <- grid::gpar(fontsize = 11, fontface = "plain", col = "#0f172a")

        y <- 0.90
        x0 <- 0.08

        grid::grid.text("Input files", x = x0, y = y, just = c("left", "top"), gp = heading_gp)
        y <- y - 0.045
        grid::grid.text(paste0("• MS1: ", ms1_name), x = x0 + 0.02, y = y, just = c("left", "top"), gp = item_gp)
        y <- y - 0.032
        grid::grid.text(paste0("• Sample: ", sample_name), x = x0 + 0.02, y = y, just = c("left", "top"), gp = item_gp)

        y <- y - 0.055
        grid::grid.text("Dataset overview", x = x0, y = y, just = c("left", "top"), gp = heading_gp)
        y <- y - 0.045
        grid::grid.text(paste0("• Proteins: ", length(unique(df$Protein.Name))), x = x0 + 0.02, y = y, just = c("left", "top"), gp = item_gp)
        y <- y - 0.032
        grid::grid.text(paste0("• Peptides: ", length(unique(df$Peptide.Note))), x = x0 + 0.02, y = y, just = c("left", "top"), gp = item_gp)
        y <- y - 0.032
        grid::grid.text(paste0("• Samples: ", length(unique(df$Replicate.Name))), x = x0 + 0.02, y = y, just = c("left", "top"), gp = item_gp)
        y <- y - 0.032
        grid::grid.text(paste0("• Groups: ", if(!is.na(group_n)) group_n else "(not available)"), x = x0 + 0.02, y = y, just = c("left", "top"), gp = item_gp)

        y <- y - 0.055
        grid::grid.text("Key settings", x = x0, y = y, just = c("left", "top"), gp = heading_gp)
        y <- y - 0.045
        grid::grid.text(paste0("• Exclude unmodified peptides: ", if(isTRUE(input$exclude_un)) "Yes" else "No"), x = x0 + 0.02, y = y, just = c("left", "top"), gp = item_gp)
        y <- y - 0.032
        grid::grid.text(paste0("• PCA palette: ", input$pca_palette), x = x0 + 0.02, y = y, just = c("left", "top"), gp = item_gp)
        y <- y - 0.032
        grid::grid.text(paste0("• Heatmap palette: ", input$heatmap_palette), x = x0 + 0.02, y = y, just = c("left", "top"), gp = item_gp)
        y <- y - 0.032
        grid::grid.text(paste0("• Heatmap clustering (rows/cols): ", if(isTRUE(input$cluster_rows)) "On" else "Off", " / ", if(isTRUE(input$cluster_cols)) "On" else "Off"), x = x0 + 0.02, y = y, just = c("left", "top"), gp = item_gp)
        y <- y - 0.032
        grid::grid.text(paste0("• Barplot palette: ", input$barplot_palette), x = x0 + 0.02, y = y, just = c("left", "top"), gp = item_gp)
        y <- y - 0.032
        grid::grid.text(paste0("• Barplot significance stars: ", if(isTRUE(input$add_signif)) "On" else "Off"), x = x0 + 0.02, y = y, just = c("left", "top"), gp = item_gp)
        y <- y - 0.032
        grid::grid.text(paste0("• Barplot Y-axis mode: ", y_axis_mode), x = x0 + 0.02, y = y, just = c("left", "top"), gp = item_gp)
        if(!is.na(y_axis_range)) {
          y <- y - 0.032
          grid::grid.text(paste0("• Barplot Y-axis range: ", y_axis_range), x = x0 + 0.02, y = y, just = c("left", "top"), gp = item_gp)
        }
      }

      pca_page <- function() {
        with_inch_box_in_content(pdf_cfg$pca_width_in, pdf_cfg$pca_height_in, function(scale) {
          print(pca_plot_obj, newpage = FALSE)
        })
      }

      heatmap_page <- function() {
        with_inch_box_in_content(pdf_cfg$heatmap_width_in, pdf_cfg$heatmap_height_in, function(scale) {
          heatmap_fs <- as_pos_num(pdf_cfg$heatmap_font_scale, 1) * scale
          fontsize <- max(4, round(12 * heatmap_fs))
          p <- plot_heatmap(
            df_for_counts,
            cluster_rows = input$cluster_rows,
            cluster_cols = input$cluster_cols,
            color_palette = input$heatmap_palette,
            fontsize = fontsize
          )
          grid::grid.draw(p$gtable)
        })
      }

      barplot_pages <- function() {
        df <- df_for_counts
        proteins <- barplot_plan$proteins
        ncols <- suppressWarnings(as.integer(pdf_cfg$barplot_ncols))
        nrows <- suppressWarnings(as.integer(pdf_cfg$barplot_nrows))
        if (!is.finite(ncols) || is.na(ncols) || ncols <= 0) ncols <- 2L
        if (!is.finite(nrows) || is.na(nrows) || nrows <= 0) nrows <- 3L

        # Target size per single barplot (inches), shrink-to-fit inside each cell
        target_w_in <- as_pos_num(pdf_cfg$barplot_single_width_in, 5)
        target_h_in <- as_pos_num(pdf_cfg$barplot_single_height_in, 2.4)

        max_cell_w_in <- (page_width_in * pdf_cfg$content_width_npc) / ncols
        max_cell_h_in <- (page_height_in * pdf_cfg$content_height_npc) / nrows
        shrink <- max(target_w_in / max_cell_w_in, target_h_in / max_cell_h_in, 1)
        draw_w_in <- target_w_in / shrink
        draw_h_in <- target_h_in / shrink

        # Use the pre-built plan for progress reporting
        progress_step <- 0.45 / max(1, barplot_plan$page_count)

        y_limits <- NULL
        if (isFALSE(input$barplot_y_auto)) {
          y_limits <- input$barplot_y_range
        }

        for (protein in proteins) {
          chunks <- barplot_plan$chunks_by_protein[[protein]]
          protein_total_pages <- length(chunks)
          if (protein_total_pages == 0) next

          for (page_idx in seq_along(chunks)) {
            next_page(paste0("Barplots: ", protein, " (", page_idx, "/", protein_total_pages, ")"), function() {
              with_full_content_viewport({
                grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrows, ncols)))
                on.exit(grid::popViewport(), add = TRUE)

                page_peptides <- chunks[[page_idx]]
                for (k in seq_along(page_peptides)) {
                  r <- ceiling(k / ncols)
                  c <- ((k - 1) %% ncols) + 1
                  peptide <- page_peptides[[k]]

                  # Cell viewport from the layout, then an inner fixed-size viewport in inches
                  grid::pushViewport(grid::viewport(layout.pos.row = r, layout.pos.col = c))
                  inner_vp <- grid::viewport(
                    width = grid::unit(draw_w_in, "in"),
                    height = grid::unit(draw_h_in, "in"),
                    just = "center"
                  )
                  grid::pushViewport(inner_vp)

                  cell_scale <- 1 / shrink
                  font_scale <- as_pos_num(pdf_cfg$barplot_font_scale, 1) * cell_scale

                  tryCatch({
                    p <- plot_barplot_single(
                      df,
                      protein_name = protein,
                      peptide_name = peptide,
                      add_signif = input$add_signif,
                      color_palette = input$barplot_palette,
                      y_limits = y_limits,
                      font_scale = font_scale,
                      title_text = peptide
                    )
                    print(p, newpage = FALSE)
                  }, error = function(e) {
                    grid::grid.text(
                      paste0("Failed: ", peptide, "\n", conditionMessage(e)),
                      x = 0.02,
                      y = 0.98,
                      just = c("left", "top"),
                      gp = grid::gpar(fontsize = 9, col = "#0f172a")
                    )
                  })

                  grid::popViewport()
                  grid::popViewport()
                }
              })
            })

            shiny::incProgress(progress_step, detail = paste0("Barplots: ", protein, " (", page_idx, "/", protein_total_pages, ")"))
          }
        }
      }

      # Generate PDF
      grDevices::pdf(file = file, width = pdf_cfg$page_width_in, height = pdf_cfg$page_height_in, onefile = TRUE)
      on.exit(tryCatch(grDevices::dev.off(), error = function(e) NULL), add = TRUE)

      tryCatch({
        shiny::withProgress(message = "Generating PDF report...", value = 0, {
          shiny::incProgress(0.05, detail = "Cover")
          next_page(NULL, cover_page)

          shiny::incProgress(0.10, detail = "Data source")
          next_page("Data Source", data_source_page)

          shiny::incProgress(0.15, detail = "PCA")
          next_page("PCA", pca_page)

          shiny::incProgress(0.15, detail = "Heatmap")
          next_page("Heatmap", heatmap_page)

          shiny::incProgress(0.05, detail = "Barplots")
          barplot_pages()

          shiny::incProgress(0.05, detail = "Done")
        })
      }, error = function(e) {
        # If anything fails, write an error page to the PDF instead of returning an HTML error.
        next_page("Report generation failed", function() {
          with_full_content_viewport({
            msg <- paste(
              c(
                "An error occurred while generating the PDF report.",
                "",
                paste0("Error: ", conditionMessage(e))
              ),
              collapse = "\n"
            )
            grid::grid.text(
              msg,
              x = 0.02,
              y = 0.98,
              just = c("left", "top"),
              gp = grid::gpar(fontsize = 12, col = "#0f172a")
            )
          })
        })
      })
    }
  )
}
