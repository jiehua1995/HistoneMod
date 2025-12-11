server <- function(input, output, session) {
  ms1_data <- reactiveVal(NULL)
  sample_data <- reactiveVal(NULL)
  filtered_data <- reactiveVal(NULL)
  
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
  
  # Init the select sections after uploading
  observeEvent(list(input$ms1_file, input$sample_file), {
    req(input$ms1_file, input$sample_file)
    ms1 <- read.csv(input$ms1_file$datapath)
    sample <- read.csv(input$sample_file$datapath)
    
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
               "Checking this option will exclude unmodified peptides.")
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
    
    # 检查重复
    dup_check <- data %>% count(Replicate.Name, Peptide.Note) %>% filter(n > 1)
    validate(
      need(nrow(dup_check) == 0, "Check your data, it is wrong or it has duplicated ids. No PCA available now")
    )
    
    plot_pca(data)
  })
  
  output$pca_plot <- renderPlot({ pca_plot_reactive() })
  
  # ------------------------
  # Heatmap
  # ------------------------
  heatmap_plot_reactive <- reactive({
    req(filtered_data_reactive())
    data <- filtered_data_reactive()
    
    # 检查重复
    dup_check <- data %>% count(Replicate.Name, Peptide.Note) %>% filter(n > 1)
    validate(need(nrow(dup_check) == 0, "Check your data, it is wrong or it has duplicated ids. No Heatmap available now"))
    
    plot_heatmap(data)
  })
  
  output$heatmap_plot <- renderPlot({ heatmap_plot_reactive() })
  
  # Barplot
  output$barplot_ui <- renderUI({
    req(filtered_data_reactive(), input$select_protein)
    plots <- plot_barplot(filtered_data_reactive(), input$select_protein, add_signif=input$add_signif)
    plot_output_list <- lapply(names(plots), function(n) {
      plotOutput(paste0("barplot_", n), height="400px")
    })
    for(i in seq_along(plots)) {
      local({
        my_i <- i
        plotname <- paste0("barplot_", names(plots)[my_i])
        output[[plotname]] <- renderPlot({ plots[[my_i]] })
      })
    }
    do.call(tagList, plot_output_list)
  })
  
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