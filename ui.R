ui <- fluidPage(
  
  shinyalert::useShinyalert(force = TRUE),
  
  # Activate the shinyjs
  useShinyjs(),
  tags$head(
    tags$script(src = "https://cdn.jsdelivr.net/npm/canvas-confetti@1.6.0/dist/confetti.browser.min.js")
  ),
  tags$div(id = "confetti-container", style="position:fixed;top:0;left:0;width:100%;height:100%;pointer-events:none;z-index:9999;"),
  
  
  titlePanel("Histone PTM Quantification"),
  
  sidebarLayout(
    # Sidebar
    sidebarPanel(
      # MS1 table upload
      fileInput("ms1_file", label = tagList(
        "Upload MS1 File",
        actionLink("ms1_info", label = NULL, icon = icon("question-circle"),
                   style="background-color: #d3d3d3; border-radius:50%; padding:3px 3px; color:black; margin-left:5px;")
      ), accept=".csv"),
      
      # Sample table upload
      fileInput("sample_file", label = tagList(
        "Upload Sample File",
        actionLink("sample_info", label = NULL, icon = icon("question-circle"),style="background-color: #d3d3d3; border-radius:50%; padding:3px 3px; color:black; margin-left:5px;")
      ), accept=".csv"),
      
      # Modification selection
      pickerInput("select_peptides", label = tagList(
        "Select Peptide Modifications",
        actionLink("peptide_info", label = NULL, icon = icon("question-circle"),style="background-color: #d3d3d3; border-radius:50%; padding:3px 3px; color:black; margin-left:5px;")
      ), choices=NULL, multiple=TRUE, options=list(`actions-box`=TRUE)),
      
      # Sample selection
      pickerInput("select_samples", label = tagList(
        "Select Samples",
        actionLink("sample_select_info", label = NULL, icon = icon("question-circle"),style="background-color: #d3d3d3; border-radius:50%; padding:3px 3px; color:black; margin-left:5px;")
      ), choices=NULL, multiple=TRUE, options=list(`actions-box`=TRUE)),
      
      # Exclude unmodified checkbox
      checkboxInput("exclude_un", label = tagList(
        "Exclude unmodified peptides",
        actionLink("exclude_info", label = NULL, icon = icon("question-circle"),style="background-color: #d3d3d3; border-radius:50%; padding:3px 3px; color:black; margin-left:5px;")
      ), value = TRUE)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("PCA",
                 withSpinner(plotOutput("pca_plot", height="600px")),
        ),
        tabPanel("Heatmap",
                 withSpinner(plotOutput("heatmap_plot", height="600px")),
        ),
        tabPanel("Barplot",
                 pickerInput("select_protein", "Select Protein", choices=NULL),
                 checkboxInput("add_signif", "Add significance stars", value=TRUE),
                 withSpinner(uiOutput("barplot_ui", height="600px"))),
        tabPanel("Table", DTOutput("data_table")),
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
                   
                   # 纯文字下载按钮
                   downloadButton("download_filtered", "Download Wide Format Table",
                                  style="color:white; background-color:#28a745; font-size:20px; 
                          padding:15px 30px; border-radius:12px; border:none; margin-top:20px;"),
                   
                   # 下载状态提示
                   br(), br(),
                   uiOutput("download_status")
                 )
        )
        
        
      )
    )
  ),
  
  tags$footer(
    tags$p(
      HTML('Powered by <a href="https://www.molekularbiologie.abi.med.uni-muenchen.de/personen/imhof_group/hua/index.html" target="_blank">Jie Hua</a> and <a href="https://www.molekularbiologie.abi.med.uni-muenchen.de/personen/imhof_group/borso/index.html" target="_blank">Dr. Marco Borso</a>. Copyright © <a href="https://www.molekularbiologie.abi.med.uni-muenchen.de/personen/imhof_group/index.html" target="_blank">Imhof Group</a>'),
      style="text-align:center; color: grey; padding:10px;"
    ),
    style="position:fixed; bottom:0; width:100%; background:#f8f9fa;"
  )
  
)