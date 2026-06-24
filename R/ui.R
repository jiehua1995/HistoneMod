##### HistoneMod Shiny UI #####
# This file wraps the application UI as a package function.

# Internal Shiny UI function used by `histonemod_app()`.
histonemod_ui <- function() {
  .register_histonemod_resources()

  ##### Shiny UI (Layout + Styling) #####
  # This file defines the visible UI: page structure, sidebar controls, tabs,
  # and CSS used by the export-preview frames.
  #
  # CUSTOMIZE: App title/subtitle/logo, default plot heights, and CSS live here.

  ##### Top-Level Page Container #####
  fluidPage(
  
  
    shinyalert::useShinyalert(force = TRUE),
  
    # Activate the shinyjs
    shinyjs::useShinyjs(),
  
    ##### Global Head Includes (CSS/JS) #####
    # CUSTOMIZE: If you need offline usage, replace CDN assets with local files.
    tags$head(
      # Tailwind CSS (CDN)
      tags$script(src = "https://cdn.tailwindcss.com"),
      # Confetti effect (CDN)
      tags$script(src = "https://cdn.jsdelivr.net/npm/canvas-confetti@1.6.0/dist/confetti.browser.min.js"),
      ##### Custom Styles #####
      # CUSTOMIZE: Update sizes/spacing here (avoid changing IDs/classes used in server.R).
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
        .well,
        form.well,
        .col-sm-4 > .well,
        .col-sm-4 > form.well,
        .col-md-4 > .well,
        .col-md-4 > form.well {
          background: transparent !important;
          border: none !important;
          box-shadow: none !important;
          padding: 0 !important;
          margin-bottom: 0 !important;
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
        .header-brand {
          display: flex;
          align-items: flex-start;
          gap: 16px;
        }
        .app-logo {
          height: 78px;
          width: auto;
          display: block;
          flex: 0 0 auto;
        }
        .tab-content { padding: 24px; }
        footer { background: white !important; border-top: 1px solid #e2e8f0; }

        ##### Export Preview Frames #####
        /* Fixed frame size; image fits without resizing the frame */
        /* CUSTOMIZE: adjust max-width/height to change preview frame size */
        .export-preview-frame {
          width: 100%;
          max-width: 520px;
          margin: 0 auto;
          background: white;
          border: 1px solid #e2e8f0;
          border-radius: 8px;
          overflow: hidden;
        }
        .export-preview-frame--fixed {
          height: 260px;
        }
        .export-preview-frame--zoom {
          height: 70vh;
          max-height: 720px;
          max-width: 100%;
        }
        .export-preview-frame .shiny-image-output {
          width: 100%;
          height: 100% !important;
        }
        .export-preview-frame img {
          width: 100%;
          height: 100%;
          display: block;
          object-fit: contain;
        }
      "))
    ),
    tags$div(id = "confetti-container", style="position:fixed;top:0;left:0;width:100%;height:100%;pointer-events:none;z-index:9999;"),
  
    ##### Header Brand (Logo + Title) #####
    # CUSTOMIZE: Replace `www/logo.png` and update the title/subtitle text.
    div(class = "container-fluid px-4 py-4",
      div(class = "header-brand",
        tags$img(src = "HistoneMod-assets/logo.png", class = "app-logo", alt = "Logo"),
        div(
          div(class = "main-title", "Histone PTM Quantification"),
          div(style = "color: #64748b; font-size: 14px; margin-bottom: 24px;",
            "Quantitative analysis of histone post-translational modifications"
          )
        )
      )
    ),
  
    ##### Sidebar + Main Panels #####
    div(class = "container-fluid px-4",
      div(class = "row",
        div(class = "col-sm-4",
        ##### Sidebar: Upload Files #####
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
          ), accept=".csv"),
          div(style = "display:flex; gap:8px; flex-wrap:wrap; margin-top:12px;",
            actionButton("load_demo_data", "Load Demo Data", icon = icon("flask"), class = "btn btn-outline-primary btn-sm"),
            actionButton("open_demo_copy_modal", "Copy Demo Files", icon = icon("folder-open"), class = "btn btn-outline-secondary btn-sm")
          ),
          div(
            style = "margin-top:10px; color:#64748b; font-size:12px; line-height:1.5;",
            "Bundled demo data are anonymized and numerically randomized examples derived from the structure of real histone modification tables. Only four representative modification states are retained."
          )
        ),
      
        ##### Sidebar: Data Selection #####
        div(class = "sidebar-section",
          h5(style="font-weight: 600; color: #1e293b; margin-bottom: 16px;", 
             icon("filter"), " Data Selection"),
          shinyWidgets::pickerInput("select_peptides", label = tagList(
            span(style="font-weight: 600; color: #334155; font-size: 14px;", "Peptide Modifications"),
            actionLink("peptide_info", label = NULL, icon = icon("question-circle"),
                       style="background-color: #e2e8f0; border-radius:50%; padding:4px 6px; color:#475569; margin-left:8px; font-size:12px;")
          ), choices=NULL, multiple=TRUE, options=list(`actions-box`=TRUE)),
          shinyWidgets::pickerInput("select_samples", label = tagList(
            span(style="font-weight: 600; color: #334155; font-size: 14px;", "Samples"),
            actionLink("sample_select_info", label = NULL, icon = icon("question-circle"),
                       style="background-color: #e2e8f0; border-radius:50%; padding:4px 6px; color:#475569; margin-left:8px; font-size:12px;")
          ), choices=NULL, multiple=TRUE, options=list(`actions-box`=TRUE))
        ),
      
        ##### Sidebar: Analysis Options #####
        div(class = "sidebar-section",
          h5(style="font-weight: 600; color: #1e293b; margin-bottom: 16px;", 
             icon("cog"), " Options"),
          checkboxInput("exclude_un", label = tagList(
            span(style="font-weight: 600; color: #334155; font-size: 14px;", "Exclude unmodified peptides"),
            actionLink("exclude_info", label = NULL, icon = icon("question-circle"),
                       style="background-color: #e2e8f0; border-radius:50%; padding:4px 6px; color:#475569; margin-left:8px; font-size:12px;")
          ), value = TRUE)
        ),
      
        ##### Sidebar: Validation Status #####
        div(class = "sidebar-section",
          uiOutput("validation_status")
        )
        ),
    
      ##### Main Content: Tabs #####
      div(class = "col-sm-8",
        tabsetPanel(
          ##### Tab: Data Preview #####
          tabPanel("Data Preview",
                   div(class = "p-3",
                     uiOutput("preview_content")
                   )
          ),
          ##### Tab: PCA #####
          # CUSTOMIZE: Default plot height is set in `plotOutput(..., height="600px")`.
          tabPanel("PCA",
                   div(class = "mb-3",
                     div(class = "row",
                       div(class = "col-md-6",
                         checkboxInput("show_ellipse", "Show 95% confidence ellipses", value = TRUE),
                         div(style="color: #64748b; font-size: 13px; margin-left: 24px; margin-top: -8px;",
                          icon("info-circle"), " Ellipses can be toggled on or off; groups with fewer than 4 samples are skipped.")
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
          ##### Tab: Heatmap #####
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
          ##### Tab: Barplot #####
          tabPanel("Barplot",
                   div(class = "mb-3",
                     div(class = "row",
                       div(class = "col-md-4",
                         shinyWidgets::pickerInput("select_protein", "Select Protein", choices=NULL)
                       ),
                       div(class = "col-md-4",
                         shinyWidgets::pickerInput("select_peptide_barplot", "Select Peptide", choices=NULL)
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
                     checkboxInput("add_signif", "Add significance stars", value=TRUE),
                     checkboxInput("barplot_y_auto", "Y-axis: Auto", value = TRUE),
                     conditionalPanel(
                       condition = "input.barplot_y_auto == false",
                       div(class = "row",
                         div(class = "col-md-6",
                           sliderInput(
                             "barplot_y_range",
                             "Y-axis range",
                             min = 0,
                             max = 100,
                             value = c(0, 100),
                             step = 1
                           )
                         )
                       )
                     )
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
          ##### Tab: Table #####
          tabPanel("Table",
                   div(class = "plot-container", style="max-height: 600px; overflow-y: auto;",
                     DTOutput("data_table")
                   )
          ),
          ##### Tab: Download Data / Report #####
          # CUSTOMIZE: Button labels and the panel styling live here.
          tabPanel("Download Data",
                   br(), br(), br(),
                   div(
                     style = "max-width:600px; margin:auto; padding:30px; background:#f8f9fa; 
               border-radius:15px; text-align:center; box-shadow:0 4px 15px rgba(0,0,0,0.1);",
                   
                     # Download button
                     downloadButton("download_filtered", "Download Wide Format Table",
                                    style="color:white; background-color:#28a745; font-size:20px; 
                        padding:15px 30px; border-radius:12px; border:none; margin-top:20px; width:100%; display:block;"),

                       br(),
                       downloadButton("download_plots_pdf", "Generate a Quick Report",
                          style="color:white; background-color:#3b82f6; font-size:20px; 
                        padding:15px 30px; border-radius:12px; border:none; margin-top:16px; width:100%; display:block;"),
                   
                     # Download status message
                     br(), br(),
                     uiOutput("download_status")
                   )
          )
        )
      )
      )
    ),
  
    ##### Footer #####
    tags$footer(
      class = "mt-5 py-4",
      style = "position:fixed; bottom:0; width:100%; background:white; border-top: 1px solid #e2e8f0;",
      uiOutput("footer_line")
    )
  )
}
