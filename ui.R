ui <- fluidPage(
  
  
  shinyalert::useShinyalert(force = TRUE),
  
  # Activate the shinyjs
  shinyjs::useShinyjs(),
  
  tags$head(
    # Tailwind CSS
    tags$script(src = "https://cdn.tailwindcss.com"),
    # Confetti effect
    tags$script(src = "https://cdn.jsdelivr.net/npm/canvas-confetti@1.6.0/dist/confetti.browser.min.js"),
    # Custom styles
    tags$style(HTML("
      body { background: #f8fafc; font-family: 'Segoe UI', sans-serif; }
      .plot-container { 
        position: relative; 
        background: white; 
        border-radius: 12px; 
        padding: 20px; 
        box-shadow: 0 2px 8px rgba(0,0,0,0.08);
        margin-bottom: 24px;
      }
      .download-btn {
        position: absolute;
        top: 16px;
        right: 16px;
        z-index: 10;
        background: #3b82f6;
        color: white;
        border: none;
        border-radius: 8px;
        padding: 8px 16px;
        cursor: pointer;
        font-size: 14px;
        font-weight: 500;
        display: flex;
        align-items: center;
        gap: 6px;
        transition: all 0.2s;
      }
      .download-btn:hover {
        background: #2563eb;
        box-shadow: 0 4px 12px rgba(59, 130, 246, 0.4);
      }
      .sidebar-section {
        background: white;
        padding: 20px;
        border-radius: 12px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.08);
        margin-bottom: 16px;
      }
      .main-title {
        font-size: 28px;
        font-weight: 700;
        color: #1e293b;
        margin-bottom: 8px;
      }
      .validation-badge {
        display: inline-flex;
        align-items: center;
        gap: 8px;
        padding: 8px 16px;
        border-radius: 8px;
        font-size: 14px;
        font-weight: 500;
      }
      .tab-content { padding: 24px; }
      footer { background: white !important; border-top: 1px solid #e2e8f0; }
    "))
  ),
  tags$div(id = "confetti-container", style="position:fixed;top:0;left:0;width:100%;height:100%;pointer-events:none;z-index:9999;"),
  
  div(class = "container-fluid px-4 py-4",
    div(class = "main-title", "Histone PTM Quantification"),
    div(style = "color: #64748b; font-size: 14px; margin-bottom: 24px;",
      "Quantitative analysis of histone post-translational modifications"
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      class = "px-3",
      # Upload section
      div(class = "sidebar-section",
        h5(style="font-weight: 600; color: #1e293b; margin-bottom: 16px;", 
           icon("upload"), " Upload Files"),
        fileInput("ms1_file", label = tagList(
          span(style="font-weight: 600; color: #334155; font-size: 14px;", "MS1 File"),
          actionLink("ms1_info", label = NULL, icon = icon("question-circle"),
                     style="background-color: #e2e8f0; border-radius:50%; padding:4px 6px; color:#475569; margin-left:8px; font-size:12px;")
        ), accept=".csv"),
        fileInput("sample_file", label = tagList(
          span(style="font-weight: 600; color: #334155; font-size: 14px;", "Sample File"),
          actionLink("sample_info", label = NULL, icon = icon("question-circle"),
                     style="background-color: #e2e8f0; border-radius:50%; padding:4px 6px; color:#475569; margin-left:8px; font-size:12px;")
        ), accept=".csv")
      ),
      
      # Selection section
      div(class = "sidebar-section",
        h5(style="font-weight: 600; color: #1e293b; margin-bottom: 16px;", 
           icon("filter"), " Data Selection"),
        pickerInput("select_peptides", label = tagList(
          span(style="font-weight: 600; color: #334155; font-size: 14px;", "Peptide Modifications"),
          actionLink("peptide_info", label = NULL, icon = icon("question-circle"),
                     style="background-color: #e2e8f0; border-radius:50%; padding:4px 6px; color:#475569; margin-left:8px; font-size:12px;")
        ), choices=NULL, multiple=TRUE, options=list(`actions-box`=TRUE)),
        pickerInput("select_samples", label = tagList(
          span(style="font-weight: 600; color: #334155; font-size: 14px;", "Samples"),
          actionLink("sample_select_info", label = NULL, icon = icon("question-circle"),
                     style="background-color: #e2e8f0; border-radius:50%; padding:4px 6px; color:#475569; margin-left:8px; font-size:12px;")
        ), choices=NULL, multiple=TRUE, options=list(`actions-box`=TRUE))
      ),
      
      # Options section
      div(class = "sidebar-section",
        h5(style="font-weight: 600; color: #1e293b; margin-bottom: 16px;", 
           icon("cog"), " Options"),
        checkboxInput("exclude_un", label = tagList(
          span(style="font-weight: 600; color: #334155; font-size: 14px;", "Exclude unmodified peptides"),
          actionLink("exclude_info", label = NULL, icon = icon("question-circle"),
                     style="background-color: #e2e8f0; border-radius:50%; padding:4px 6px; color:#475569; margin-left:8px; font-size:12px;")
        ), value = TRUE)
      ),
      
      # Validation status
      div(class = "sidebar-section",
        uiOutput("validation_status")
      )
    ),
    
    # Main content area
    mainPanel(
      tabsetPanel(
        tabPanel("Data Preview",
                 div(class = "p-3",
                   uiOutput("preview_content")
                 )
        ),
        tabPanel("PCA",
                 div(class = "mb-3",
                   div(class = "row",
                     div(class = "col-md-6",
                       checkboxInput("show_ellipse", "Show 95% confidence ellipses", value = TRUE),
                       div(style="color: #64748b; font-size: 13px; margin-left: 24px; margin-top: -8px;",
                         icon("info-circle"), " Requires ≥4 samples per group to display ellipses")
                     ),
                     div(class = "col-md-6",
                       selectInput("pca_palette", "Color palette",
                                   choices = c("Viridis" = "viridis", "Magma" = "magma", 
                                               "Plasma" = "plasma", "Inferno" = "inferno",
                                               "Cividis" = "cividis", "Rocket" = "rocket",
                                               "Mako" = "mako", "Turbo" = "turbo"),
                                   selected = "viridis")
                     )
                   )
                 ),
                 div(class = "text-end mb-2",
                   actionButton("export_pca", "Download Plot", 
                                icon = icon("download"),
                                class = "btn btn-primary",
                                style = "background-color: #3b82f6; border: none;")
                 ),
                 div(class = "plot-container",
                   withSpinner(plotOutput("pca_plot", height="600px"))
                 )
        ),
        tabPanel("Heatmap",
                 div(class = "mb-3",
                   div(class = "row",
                     div(class = "col-md-4",
                       checkboxInput("cluster_rows", "Cluster peptides (rows)", value = TRUE)
                     ),
                     div(class = "col-md-4",
                       checkboxInput("cluster_cols", "Cluster samples (columns)", value = TRUE)
                     ),
                     div(class = "col-md-4",
                       selectInput("heatmap_palette", "Color palette",
                                   choices = c("Viridis" = "viridis", "Magma" = "magma", 
                                               "Plasma" = "plasma", "Inferno" = "inferno",
                                               "Cividis" = "cividis", "Rocket" = "rocket",
                                               "Mako" = "mako", "Turbo" = "turbo"),
                                   selected = "viridis")
                     )
                   )
                 ),
                 div(class = "text-end mb-2",
                   actionButton("export_heatmap", "Download Plot", 
                                icon = icon("download"),
                                class = "btn btn-primary",
                                style = "background-color: #3b82f6; border: none;")
                 ),
                 div(class = "plot-container",
                   withSpinner(plotOutput("heatmap_plot", height="600px"))
                 )
        ),
        tabPanel("Barplot",
                 div(class = "mb-3",
                   div(class = "row",
                     div(class = "col-md-4",
                       pickerInput("select_protein", "Select Protein", choices=NULL)
                     ),
                     div(class = "col-md-4",
                       pickerInput("select_peptide_barplot", "Select Peptide", choices=NULL)
                     ),
                     div(class = "col-md-4",
                       selectInput("barplot_palette", "Color palette",
                                   choices = c("Viridis" = "viridis", "Magma" = "magma", 
                                               "Plasma" = "plasma", "Inferno" = "inferno",
                                               "Cividis" = "cividis", "Rocket" = "rocket",
                                               "Mako" = "mako", "Turbo" = "turbo"),
                                   selected = "viridis")
                     )
                   ),
                   checkboxInput("add_signif", "Add significance stars", value=TRUE)
                 ),
                 div(class = "text-end mb-2",
                   actionButton("export_barplot", "Download Plot", 
                                icon = icon("download"),
                                class = "btn btn-primary",
                                style = "background-color: #3b82f6; border: none;")
                 ),
                 div(class = "plot-container",
                   withSpinner(plotOutput("barplot_single", height="500px"))
                 )
        ),
        tabPanel("Table",
                 div(class = "plot-container", style="max-height: 600px; overflow-y: auto;",
                   DTOutput("data_table")
                 )
        ),
        tabPanel("DownloadData",
                 br(), br(), br(),
                 div(
                   style = "max-width:600px; margin:auto; padding:30px; background:#f8f9fa; 
             border-radius:15px; text-align:center; box-shadow:0 4px 15px rgba(0,0,0,0.1);",
                   
                   # Title
                   tags$div(
                     #icon("file-download", lib="font-awesome", style="font-size:50px; color:#28a745;"),
                     h3("Download Your Processed Data"),
                     p("Wide format table ready for analysis/sharing")
                   ),
                   
                   # Download button
                   downloadButton("download_filtered", "Download Wide Format Table",
                                  style="color:white; background-color:#28a745; font-size:20px; 
                          padding:15px 30px; border-radius:12px; border:none; margin-top:20px;"),
                   
                   # Download status message
                   br(), br(),
                   uiOutput("download_status")
                 )
        )
      )
    )
  ),
  
  tags$footer(
    class = "mt-5 py-4",
    style = "position:fixed; bottom:0; width:100%; background:white; border-top: 1px solid #e2e8f0;",
    tags$p(
      class = "text-center mb-0",
      style = "color: #64748b; font-size: 13px;",
      HTML('Powered by <a href="https://www.molekularbiologie.abi.med.uni-muenchen.de/personen/imhof_group/hua/index.html" target="_blank" style="color:#3b82f6;">Jie Hua</a> and <a href="https://www.molekularbiologie.abi.med.uni-muenchen.de/personen/imhof_group/borso/index.html" target="_blank" style="color:#3b82f6;">Dr. Marco Borso</a>. Copyright © <a href="https://www.molekularbiologie.abi.med.uni-muenchen.de/personen/imhof_group/index.html" target="_blank" style="color:#3b82f6;">Imhof Group</a>')
    )
  )
)