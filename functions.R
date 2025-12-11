# ------------------------
# 0. install required packages
# ------------------------

depends_check <- function() {
  required_packages <- c(
    "dplyr","tidyr","ggplot2","ggrepel","ggsignif","viridis",
    "pheatmap","shiny","shinyWidgets","shinycssloaders","shinyjs","shinyalert","DT"
  )
  
  if (!requireNamespace("pak", quietly = TRUE)) {
    install.packages("pak")
  }
  library(pak)
  
  # Install packages
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      pak::pkg_install(pkg)
    }
  }
  
  # Load required packages
  for (pkg in required_packages) {
    library(pkg, character.only = TRUE)
  }
}




# ------------------------
# 1. percentage_calculation
# ------------------------
percentage_calculation <- function(ms1, sample,
                                   exclude_un = TRUE,
                                   selected_peptides = NULL,
                                   selected_samples = NULL,
                                   max_percentage = 100) {
  # Replace space with dot
  colnames(ms1) <- gsub("\\s+", ".", colnames(ms1))
  colnames(sample) <- gsub("\\s+", ".", colnames(sample))

  # Make sure the replicate name is character
  ms1$Replicate.Name <- as.character(ms1$Replicate.Name)
  sample$Replicate.Name <- as.character(sample$Replicate.Name)
  
  # Merge the tables
  df <- merge(ms1, sample, by = "Replicate.Name")
  
  # Only keep light peptides
  df <- df %>% filter(Isotope.Label.Type == "light")
  
  # Calculate the percentage
  df <- df %>%
    group_by(Replicate.Name, Protein.Name) %>%
    mutate(Total.Area.MS1.Sum = sum(Total.Area.MS1, na.rm=TRUE)) %>%
    ungroup() %>%
    mutate(Percentage = Total.Area.MS1 / Total.Area.MS1.Sum * 100) %>%
    filter(Percentage <= max_percentage)
  
  
  
  # Exclude unmodified
  if(exclude_un) {
    df <- df %>% filter({
      last_mod <- sub("^.*_", "", Peptide.Note)
      !grepl("^un(un)*$", last_mod, ignore.case = TRUE)
    })
  }
  
  
  # Filte peptide modifications
  if(!is.null(selected_peptides)) {
    df <- df %>% filter(Peptide.Note %in% selected_peptides)
  }
  
  # Filter samples
  if(!is.null(selected_samples)) {
    df <- df %>% filter(Replicate.Name %in% selected_samples)
  }
  
  
  return(df)
}

# ------------------------
# 2. PCA plot
# ------------------------
plot_pca <- function(data_merge) {

  colnames(data_merge) <- gsub("\\s+", ".", colnames(data_merge))
  

  wide <- data_merge %>%
    select(Replicate.Name, Peptide.Note, Percentage) %>%
    pivot_wider(names_from = Peptide.Note, values_from = Percentage, values_fill = 0)
  
  if (nrow(wide) == 0) return(ggplot() + ggtitle("No data for PCA"))
  
  numeric_mat <- wide %>% select(-Replicate.Name) %>% dplyr::select(where(is.numeric))
  sds <- apply(numeric_mat, 2, sd, na.rm = TRUE)
  keep_cols <- names(sds)[sds > 0]
  
  if (length(keep_cols) < 2) {
    return(ggplot() + geom_text(aes(0, 0, label = "Not enough variable peptides for PCA")) + theme_void())
  }
  
  pca_mat <- numeric_mat[, keep_cols, drop = FALSE]
  
  # PCA
  pca_res <- prcomp(pca_mat, center = TRUE, scale. = TRUE)
  scores <- as.data.frame(pca_res$x)
  
  # Keep Replicate.Name
  scores$Replicate.Name <- wide$Replicate.Name
  
  # Group
  if ("Group" %in% colnames(data_merge)) {
    scores <- left_join(scores, data_merge %>% select(Replicate.Name, Group) %>% distinct(),
                        by = "Replicate.Name")
    scores$Group <- factor(scores$Group)
  } else {
    scores$Group <- factor("All")
  }
  
  variance_explained <- summary(pca_res)$importance[2, ]
  
  ggplot(scores, aes(x = PC1, y = PC2, color = Group)) +
    geom_point(size = 6) +
    geom_text_repel(aes(label = Replicate.Name), size = 6, max.overlaps = Inf) +
    labs(title = "PCA of Replicates",
         x = paste0("PC1 (", round(variance_explained[1]*100, 1), "%)"),
         y = paste0("PC2 (", round(variance_explained[2]*100, 1), "%)")) +
    theme_minimal() +
    scale_color_viridis_d()
}


# ------------------------
# 3. Heatmap plot
# ------------------------
plot_heatmap <- function(data_merge, cluster_rows=TRUE, cluster_cols=TRUE) {
  colnames(data_merge) <- gsub("\\s+", ".", colnames(data_merge))
  
  heat_data <- data_merge %>%
    select(Replicate.Name, Peptide.Note, Percentage) %>%
    pivot_wider(names_from = Replicate.Name, values_from = Percentage, values_fill=0) %>%
    arrange(Peptide.Note)
  
  if(ncol(heat_data) <= 1) stop("Not enough data for heatmap")
  
  rownames_mat <- heat_data$Peptide.Note
  heat_mat <- as.matrix(heat_data %>% select(-Peptide.Note))
  rownames(heat_mat) <- rownames_mat
  
  # Plot heatmap
  pheatmap::pheatmap(heat_mat,
                     cluster_rows=cluster_rows, cluster_cols=cluster_cols,
                     color=viridis::viridis(100), border_color="grey60",
                     fontsize_row=10, fontsize_col=10,
                     main="Peptide Percentage Heatmap")
}


# ------------------------
# 4. Barplot per protein
# ------------------------
plot_barplot <- function(data_merge, protein_name=NULL, add_signif=TRUE) {
  colnames(data_merge) <- gsub("\\s+", ".", colnames(data_merge))
  
  df <- data_merge
  if(!is.null(protein_name)) df <- df %>% filter(Protein.Name == protein_name)
  
  peptides <- unique(df$Peptide.Note)
  plots <- list()
  
  for(peptide in peptides) {
    df_sub <- df %>% filter(Peptide.Note == peptide)
    
    summary_data <- df_sub %>%
      group_by(Group) %>%
      summarise(mean_percentage = mean(Percentage, na.rm=TRUE),
                sd_percentage = sd(Percentage, na.rm=TRUE),
                n=n(),
                se = sd_percentage / sqrt(n),
                .groups="drop") %>%
      arrange(Group)
    
    y_base <- max(summary_data$mean_percentage + ifelse(is.na(summary_data$se), 0, summary_data$se), na.rm=TRUE)
    step <- ifelse(y_base==0,0.5,y_base*0.12)
    
    p <- ggplot(summary_data, aes(x=Group, y=mean_percentage, fill=Group)) +
      geom_col(position=position_dodge(width=0.9)) +
      geom_errorbar(aes(ymin=mean_percentage - se, ymax=mean_percentage + se),
                    width=0.2, position=position_dodge(width=0.9)) +
      labs(title=paste0("Peptide: ", peptide), y="Percentage", x="Group") +
      theme_minimal() +
      theme(axis.title = element_text(size = 12),axis.text = element_text(size=12),
        axis.text.x = element_text(angle=45, hjust=1), legend.position="none") +
      scale_fill_viridis_d()
    
    # only p < 0.05
    if(add_signif && length(unique(df_sub$Group))>=2) {
      comps <- combn(unique(df_sub$Group), 2, simplify=FALSE)
      ypos <- y_base + step
      annotations <- c()
      signif_comps <- list()
      
      for(cmp in comps) {
        subdf <- df_sub %>% filter(Group %in% cmp)
        pval <- tryCatch(t.test(Percentage ~ Group, data=subdf)$p.value, error=function(e) NA)
        if(!is.na(pval) && pval < 0.05) { 
          signif_comps[[length(signif_comps)+1]] <- cmp
          if(pval < 0.001) ann <- "***"
          else if(pval < 0.01) ann <- "**"
          else ann <- "*"
          annotations <- c(annotations, ann)
          ypos <- ypos + step
        }
      }
      
      # Add significance labels
      if(length(signif_comps) > 0) {
        p <- p + geom_signif(comparisons = signif_comps,
                             annotations = annotations,
                             y_position = seq(y_base+step, by=step, length.out=length(signif_comps)),
                             tip_length = 0.02)
      }
    }
    
    plots[[peptide]] <- p
  }
  
  return(plots)
}

# ------------------------
# 5. Downlod the picture
# ------------------------

# Download_UI
downloadPlotUI <- function(id, label = "Download Plot") {
  ns <- NS(id)
  downloadButton(ns("download"), label)
}

# Download_server
downloadPlotServer <- function(id, plot_reactive, filename_prefix = "plot") {
  moduleServer(id, function(input, output, session) {
    output$download <- downloadHandler(
      filename = function() { paste0(filename_prefix, "_", Sys.Date(), ".png") },
      content = function(file) {
        p <- plot_reactive()
        ggsave(file, plot = p, width = 8, height = 6, dpi = 300)
      }
    )
  })
}
