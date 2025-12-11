server <- function(input, output, session) {
  ms1_data <- reactiveVal(NULL)
  sample_data <- reactiveVal(NULL)
  filtered_data <- reactiveVal(NULL)
  current_plot <- reactiveVal(NULL)
  current_plot_type <- reactiveVal(NULL)
  
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
  
  # Alerts
  observeEvent(input$ms1_info, {
    shinyalert("MS1 CSV Description:",
HTML("Please upload a comma-delimited CSV file with at least these columns, like: <br>" %>%
paste0("<table border='1' cellpadding='4'><tr><th>Protein Name</th><th>Peptide Note</th><th>Replicate Name</th><th>Total Area MS1 Sum</th><th>Isotope Label Type</th></tr>",
                        paste0("<tr><td>", demo_ms1$Protein.Name, "</td><td>", demo_ms1$Peptide.Note, "</td><td>", demo_ms1$Replicate.Name, "</td><td>", demo_ms1$Total.Area.MS1.Sum, "</td><td>", demo_ms1$Isotope.Label.Type, "</td></tr>", collapse=""),
                        "</table>"
                      )),
               html=TRUE
    )
  })
  
  observeEvent(input$sample_info, {
    shinyalert("Sample CSV Description",
               HTML("Please upload a comma-delimited CSV file with at least these columns, like: <br>" %>%
                      paste0(
                        "<table border='1' cellpadding='4'><tr><th>Replicate Name</th><th>Group</th><th>Replicate No</th></tr>",
                        paste0("<tr><td>", demo_sample$Replicate.Name, "</td><td>", demo_sample$Group, "</td><td>", demo_sample$Replicate.No, "</td></tr>", collapse=""),
                        "</table>"
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
    df
  })
  
  # PCA
  pca_plot_reactive <- reactive({
    req(filtered_data_reactive())
    data <- filtered_data_reactive()
    
    # Check duplicates
    dup_check <- data %>% count(Replicate.Name, Peptide.Note) %>% filter(n > 1)
    validate(
      need(nrow(dup_check) == 0, "Check your data, it is wrong or it has duplicated ids. No PCA available now")
    )
    
    plot_pca(data, show_ellipse = input$show_ellipse, color_palette = input$pca_palette)
  })
  
  output$pca_plot <- renderPlot({ 
    p <- pca_plot_reactive()
    current_plot(p)
    current_plot_type("pca")
    p
  })
  
  # Export PCA plot
  observeEvent(input$export_pca, {
    req(pca_plot_reactive())
    showModal(modalDialog(
      title = div(style="font-size: 20px; font-weight: 600; color: #1e293b;", 
                  icon("image"), " Export PCA Plot"),
      size = "m",
      easyClose = TRUE,
      div(class="p-3",
        div(class="mb-3",
          tags$label(class="form-label fw-bold", "Format"),
          selectInput("export_format_pca", NULL,
                      choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg", "JPEG" = "jpeg", 
                                  "TIFF" = "tiff", "EPS" = "eps", "BMP" = "bmp"),
                      selected = "png")
        ),
        div(class="row",
          div(class="col-md-6 mb-3",
            tags$label(class="form-label fw-bold", "Width (inches)"),
            numericInput("export_width_pca", NULL, value = 8, min = 1, max = 20, step = 0.5)
          ),
          div(class="col-md-6 mb-3",
            tags$label(class="form-label fw-bold", "Height (inches)"),
            numericInput("export_height_pca", NULL, value = 6, min = 1, max = 20, step = 0.5)
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
                      selected = 300)
        )
      ),
      footer = tagList(
        modalButton("Cancel"),
        downloadButton("download_pca_plot", "Download")
      )
    ))
  })
  
  output$download_pca_plot <- downloadHandler(
    filename = function() {
      paste0("PCA_plot_", Sys.Date(), ".", input$export_format_pca)
    },
    content = function(file) {
      ggsave(file, plot = pca_plot_reactive(), 
             width = input$export_width_pca, 
             height = input$export_height_pca, 
             dpi = as.numeric(input$export_dpi_pca),
             device = input$export_format_pca)
      removeModal()
    }
  )
  
  # ------------------------
  # Heatmap
  # ------------------------
  heatmap_plot_reactive <- reactive({
    req(filtered_data_reactive())
    data <- filtered_data_reactive()
    
    # Check duplicates
    dup_check <- data %>% count(Replicate.Name, Peptide.Note) %>% filter(n > 1)
    validate(need(nrow(dup_check) == 0, "Check your data, it is wrong or it has duplicated ids. No Heatmap available now"))
    
    plot_heatmap(data, cluster_rows = input$cluster_rows, cluster_cols = input$cluster_cols, color_palette = input$heatmap_palette)
  })
  
  output$heatmap_plot <- renderPlot({ 
    p <- heatmap_plot_reactive()
    current_plot(p)
    current_plot_type("heatmap")
    p
  })
  
  # Export Heatmap
  observeEvent(input$export_heatmap, {
    req(heatmap_plot_reactive())
    showModal(modalDialog(
      title = div(style="font-size: 20px; font-weight: 600; color: #1e293b;", 
                  icon("image"), " Export Heatmap"),
      size = "m",
      easyClose = TRUE,
      div(class="p-3",
        div(class="mb-3",
          tags$label(class="form-label fw-bold", "Format"),
          selectInput("export_format_hm", NULL,
                      choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg", "JPEG" = "jpeg",
                                  "TIFF" = "tiff", "EPS" = "eps", "BMP" = "bmp"),
                      selected = "png")
        ),
        div(class="row",
          div(class="col-md-6 mb-3",
            tags$label(class="form-label fw-bold", "Width (inches)"),
            numericInput("export_width_hm", NULL, value = 10, min = 1, max = 20, step = 0.5)
          ),
          div(class="col-md-6 mb-3",
            tags$label(class="form-label fw-bold", "Height (inches)"),
            numericInput("export_height_hm", NULL, value = 8, min = 1, max = 20, step = 0.5)
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
                      selected = 300)
        )
      ),
      footer = tagList(
        modalButton("Cancel"),
        downloadButton("download_heatmap_plot", "Download")
      )
    ))
  })
  
  output$download_heatmap_plot <- downloadHandler(
    filename = function() {
      paste0("Heatmap_", Sys.Date(), ".", input$export_format_hm)
    },
    content = function(file) {
      if(input$export_format_hm %in% c("pdf", "svg", "eps")) {
        if(input$export_format_hm == "pdf") {
          pdf(file, width = input$export_width_hm, height = input$export_height_hm)
        } else if(input$export_format_hm == "svg") {
          svg(file, width = input$export_width_hm, height = input$export_height_hm)
        } else {
          setEPS()
          postscript(file, width = input$export_width_hm, height = input$export_height_hm)
        }
        print(heatmap_plot_reactive())
        dev.off()
      } else {
        # For raster formats
        device_func <- switch(input$export_format_hm,
                              "png" = png,
                              "jpeg" = jpeg,
                              "tiff" = tiff,
                              "bmp" = bmp,
                              png)
        device_func(file, width = input$export_width_hm, height = input$export_height_hm, 
                    units = "in", res = as.numeric(input$export_dpi_heatmap))
        print(heatmap_plot_reactive())
        dev.off()
      }
      removeModal()
    }
  )
  
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
    plot_barplot_single(data, input$select_protein, input$select_peptide_barplot, add_signif=input$add_signif, color_palette=input$barplot_palette)
  })
  
  output$barplot_single <- renderPlot({
    current_barplot()
  })
  
  # Export Barplot
  observeEvent(input$export_barplot, {
    req(current_barplot())
    showModal(modalDialog(
      title = div(style="font-size: 20px; font-weight: 600; color: #1e293b;", 
                  icon("image"), " Export Barplots"),
      size = "m",
      easyClose = TRUE,
      div(class="p-3",
        div(class="mb-3",
          tags$label(class="form-label fw-bold", "Format"),
          selectInput("export_format_bar", NULL,
                      choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg", "JPEG" = "jpeg",
                                  "TIFF" = "tiff", "EPS" = "eps", "BMP" = "bmp"),
                      selected = "png")
        ),
        div(class="row",
          div(class="col-md-6 mb-3",
            tags$label(class="form-label fw-bold", "Width (inches)"),
            numericInput("export_width_bar", NULL, value = 8, min = 1, max = 20, step = 0.5)
          ),
          div(class="col-md-6 mb-3",
            tags$label(class="form-label fw-bold", "Height (inches)"),
            numericInput("export_height_bar", NULL, value = 6, min = 1, max = 20, step = 0.5)
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
                      selected = 300)
        )
      ),
      footer = tagList(
        modalButton("Cancel"),
        downloadButton("download_barplot", "Download")
      )
    ))
  })
  
  output$download_barplot <- downloadHandler(
    filename = function() {
      peptide_name <- gsub("[^A-Za-z0-9_-]", "_", input$select_peptide_barplot)
      paste0("Barplot_", input$select_protein, "_", peptide_name, "_", Sys.Date(), ".", input$export_format_bar)
    },
    content = function(file) {
      ggsave(file, plot = current_barplot(), 
             width = input$export_width_bar, 
             height = input$export_height_bar, 
             dpi = as.numeric(input$export_dpi_barplot),
             device = input$export_format_bar)
      removeModal()
    }
  )
  
  # Data table
  output$data_table <- renderDT({
    req(filtered_data_reactive())
    datatable(filtered_data_reactive())
  })
  
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
}