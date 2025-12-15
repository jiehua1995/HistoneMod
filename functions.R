# ------------------------
# 0. Prepare environment
# ------------------------

depends_check <- function() {
  required_packages <- c(
    "dplyr","tidyr","ggplot2","ggrepel","ggsignif","viridis","pheatmap","shiny","shinyWidgets","shinycssloaders","shinyjs","shinyalert","DT"
  )
  # Install pak if not already installed
  if (!requireNamespace("pak", quietly = TRUE)) {
    install.packages("pak")
  }
  library(pak)
  # Install required packages if not already installed
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
# File Validation Functions
# ------------------------
validate_ms1_file <- function(file_path) {
  tryCatch({
    df <- read.csv(file_path)
    required_cols <- c("Protein.Name", "Peptide.Note", "Replicate.Name", 
                       "Total.Area.MS1", "Isotope.Label.Type")
    
    # Convert column names
    colnames(df) <- gsub("\\s+", ".", colnames(df))
    
    missing_cols <- required_cols[!required_cols %in% colnames(df)]
    
    if(length(missing_cols) > 0) {
      return(list(
        valid = FALSE,
        message = paste("Missing required columns:", paste(missing_cols, collapse=", "))
      ))
    }
    
    # Check for empty data
    if(nrow(df) == 0) {
      return(list(valid = FALSE, message = "File is empty"))
    }
    
    return(list(valid = TRUE, message = "Valid MS1 file", data = df))
  }, error = function(e) {
    return(list(valid = FALSE, message = paste("Error reading file:", e$message)))
  })
}

validate_sample_file <- function(file_path) {
  tryCatch({
    df <- read.csv(file_path)
    required_cols <- c("Replicate.Name", "Group", "Replicate.No")
    
    # Convert column names
    colnames(df) <- gsub("\\s+", ".", colnames(df))
    
    missing_cols <- required_cols[!required_cols %in% colnames(df)]
    
    if(length(missing_cols) > 0) {
      return(list(
        valid = FALSE,
        message = paste("Missing required columns:", paste(missing_cols, collapse=", "))
      ))
    }
    
    # Check for empty data
    if(nrow(df) == 0) {
      return(list(valid = FALSE, message = "File is empty"))
    }
    
    return(list(valid = TRUE, message = "Valid Sample file", data = df))
  }, error = function(e) {
    return(list(valid = FALSE, message = paste("Error reading file:", e$message)))
  })
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
plot_pca <- function(data_merge, show_ellipse = TRUE, color_palette = "viridis") {

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
  
  # Select color scale
  color_scale <- switch(color_palette,
                        "viridis" = scale_color_viridis_d(),
                        "magma" = scale_color_viridis_d(option = "magma"),
                        "plasma" = scale_color_viridis_d(option = "plasma"),
                        "inferno" = scale_color_viridis_d(option = "inferno"),
                        "cividis" = scale_color_viridis_d(option = "cividis"),
                        "rocket" = scale_color_viridis_d(option = "rocket"),
                        "mako" = scale_color_viridis_d(option = "mako"),
                        "turbo" = scale_color_viridis_d(option = "turbo"),
                        scale_color_viridis_d())
  
  p <- ggplot(scores, aes(x = PC1, y = PC2, color = Group)) +
    geom_point(size = 6) +
    geom_text_repel(aes(label = Replicate.Name), size = 6, max.overlaps = Inf) +
    labs(title = "PCA of Replicates",
         x = paste0("PC1 (", round(variance_explained[1]*100, 1), "%)"),
         y = paste0("PC2 (", round(variance_explained[2]*100, 1), "%)")) +
    theme_minimal() +
    color_scale
  
  # Add confidence ellipses if requested and enough samples per group
  if(show_ellipse) {
    group_counts <- table(scores$Group)
    if(all(group_counts >= 4)) {
      # Suppress warnings about too few points
      p <- p + suppressWarnings(
        stat_ellipse(aes(color = Group), level = 0.95, linetype = 2)
      )
    }
  }
  
  return(p)
}


# ------------------------
# 3. Heatmap plot
# ------------------------
plot_heatmap <- function(data_merge, cluster_rows=TRUE, cluster_cols=TRUE, color_palette="viridis") {
  colnames(data_merge) <- gsub("\\s+", ".", colnames(data_merge))
  
  heat_data <- data_merge %>%
    select(Replicate.Name, Peptide.Note, Percentage) %>%
    pivot_wider(names_from = Replicate.Name, values_from = Percentage, values_fill=0)
  
  # Sort by name if not clustering
  if(!cluster_rows) {
    heat_data <- heat_data %>% arrange(Peptide.Note)
  }
  
  if(ncol(heat_data) <= 1) stop("Not enough data for heatmap")
  
  rownames_mat <- heat_data$Peptide.Note
  heat_mat <- as.matrix(heat_data %>% select(-Peptide.Note))
  rownames(heat_mat) <- rownames_mat
  
  # Sort columns by name if not clustering
  if(!cluster_cols) {
    heat_mat <- heat_mat[, order(colnames(heat_mat)), drop=FALSE]
  }
  
  # Select color palette
  colors <- switch(color_palette,
                   "viridis" = viridis::viridis(100),
                   "magma" = viridis::magma(100),
                   "plasma" = viridis::plasma(100),
                   "inferno" = viridis::inferno(100),
                   "cividis" = viridis::cividis(100),
                   "rocket" = viridis::rocket(100),
                   "mako" = viridis::mako(100),
                   "turbo" = viridis::turbo(100),
                   viridis::viridis(100))
  
  # Plot heatmap
  pheatmap::pheatmap(heat_mat,
                     cluster_rows=cluster_rows, cluster_cols=cluster_cols,
                     color=colors, border_color="grey60",
                     fontsize_row=10, fontsize_col=10,
                     main="Peptide Percentage Heatmap")
}


# ------------------------
# 4. Barplot per protein (single peptide)
# ------------------------
plot_barplot_single <- function(data_merge, protein_name, peptide_name, add_signif=TRUE, color_palette="viridis") {
  colnames(data_merge) <- gsub("\\s+", ".", colnames(data_merge))
  
  df <- data_merge
  if(!is.null(protein_name)) df <- df %>% filter(Protein.Name == protein_name)
  df_sub <- df %>% filter(Peptide.Note == peptide_name)
  
  if(nrow(df_sub) == 0) {
    return(ggplot() + 
           geom_text(aes(0, 0, label = "No data available"), size = 6) + 
           theme_void())
  }
  
  summary_data <- df_sub %>%
    group_by(Group) %>%
    summarise(mean_percentage = mean(Percentage, na.rm=TRUE),
              sd_percentage = sd(Percentage, na.rm=TRUE),
              n=n(),
              se = sd_percentage / sqrt(n),
              .groups="drop") %>%
    arrange(Group)
  
  # Calculate y-axis range first to accommodate significance brackets
  y_base <- max(summary_data$mean_percentage + summary_data$se, na.rm=TRUE)
  
  # Select fill scale
  fill_scale <- switch(color_palette,
                       "viridis" = scale_fill_viridis_d(),
                       "magma" = scale_fill_viridis_d(option = "magma"),
                       "plasma" = scale_fill_viridis_d(option = "plasma"),
                       "inferno" = scale_fill_viridis_d(option = "inferno"),
                       "cividis" = scale_fill_viridis_d(option = "cividis"),
                       "rocket" = scale_fill_viridis_d(option = "rocket"),
                       "mako" = scale_fill_viridis_d(option = "mako"),
                       "turbo" = scale_fill_viridis_d(option = "turbo"),
                       scale_fill_viridis_d())
  
  p <- ggplot(summary_data, aes(x=Group, y=mean_percentage, fill=Group)) +
    geom_col(position=position_dodge(width=0.9)) +
    geom_errorbar(aes(ymin=mean_percentage - se, ymax=mean_percentage + se),
                  width=0.2, position=position_dodge(width=0.9)) +
    labs(title=paste0("Peptide: ", peptide_name), y="Percentage", x="Group") +
    theme_minimal() +
    theme(axis.title = element_text(size = 12),axis.text = element_text(size=12),
      axis.text.x = element_text(angle=45, hjust=1), legend.position="none",
      plot.title = element_text(size=12)) +
    fill_scale
  
  # Add ANOVA p-value to top-right corner if significance testing is enabled
  if(add_signif && length(unique(df_sub$Group)) >= 2) {
    anova_pval <- tryCatch({
      if(length(unique(df_sub$Group)) == 2) {
        # Use t-test for 2 groups
        t.test(Percentage ~ Group, data=df_sub)$p.value
      } else {
        # Use ANOVA for 3+ groups
        aov_result <- aov(Percentage ~ Group, data=df_sub)
        summary(aov_result)[[1]]["Pr(>F)"][1,1]
      }
    }, error=function(e) NA)
    
    if(!is.na(anova_pval)) {
      anova_text <- if(length(unique(df_sub$Group)) == 2) {
        sprintf("t-test p = %.4f", anova_pval)
      } else {
        sprintf("ANOVA p = %.4f", anova_pval)
      }
      p <- p + annotate("text", x=Inf, y=Inf, label=anova_text,
                       hjust=1.1, vjust=1.5, size=3.5, fontface="plain")
    }
  }
  
  # Add significance stars (only p < 0.05)
  y_max <- y_base * 1.2  # Default extension
  if(add_signif && length(unique(df_sub$Group))>=2) {
    comps <- combn(unique(df_sub$Group), 2, simplify=FALSE)
    step <- y_base * 0.08
    ypos <- y_base + step
    annotations <- c()
    signif_comps <- list()
    y_positions <- c()
    
    for(cmp in comps) {
      subdf <- df_sub %>% filter(Group %in% cmp)
      pval <- tryCatch(t.test(Percentage ~ Group, data=subdf)$p.value, error=function(e) NA)
      if(!is.na(pval) && pval < 0.05) { 
        signif_comps[[length(signif_comps)+1]] <- cmp
        if(pval < 0.001) ann <- "***"
        else if(pval < 0.01) ann <- "**"
        else ann <- "*"
        annotations <- c(annotations, ann)
        y_positions <- c(y_positions, ypos)
        ypos <- ypos + step
      }
    }
    
    # Add significance labels
    if(length(signif_comps) > 0) {
      p <- p + geom_signif(comparisons = signif_comps,
                           annotations = annotations,
                           y_position = y_positions,
                           tip_length = 0.02)
      # Extend y-axis to show all significance brackets
      y_max <- max(y_positions) + step * 1.5
    }
  }
  
  # Set y-axis limits after calculating significance positions
  p <- p + ylim(0, y_max)
  
  return(p)
}

# ------------------------
# 4b. Barplot per protein (legacy - all peptides)
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
    
    # Extend y-axis by 20% to give more space
    y_max <- max(summary_data$mean_percentage + summary_data$se, na.rm=TRUE) * 1.2
    
    p <- ggplot(summary_data, aes(x=Group, y=mean_percentage, fill=Group)) +
      geom_col(position=position_dodge(width=0.9)) +
      geom_errorbar(aes(ymin=mean_percentage - se, ymax=mean_percentage + se),
                    width=0.2, position=position_dodge(width=0.9)) +
      labs(title=paste0("Peptide: ", peptide), y="Percentage", x="Group") +
      theme_minimal() +
      theme(axis.title = element_text(size = 12),axis.text = element_text(size=12),
        axis.text.x = element_text(angle=45, hjust=1), legend.position="none",
        plot.title = element_text(size=12)) +
      scale_fill_viridis_d() +
      ylim(0, y_max)
    
    # Add ANOVA p-value to top-right corner if significance testing is enabled
    if(add_signif && length(unique(df_sub$Group)) >= 2) {
      anova_pval <- tryCatch({
        if(length(unique(df_sub$Group)) == 2) {
          # Use t-test for 2 groups
          t.test(Percentage ~ Group, data=df_sub)$p.value
        } else {
          # Use ANOVA for 3+ groups
          aov_result <- aov(Percentage ~ Group, data=df_sub)
          summary(aov_result)[[1]]["Pr(>F)"][1,1]
        }
      }, error=function(e) NA)
      
      if(!is.na(anova_pval)) {
        anova_text <- if(length(unique(df_sub$Group)) == 2) {
          sprintf("t-test p = %.4f", anova_pval)
        } else {
          sprintf("ANOVA p = %.4f", anova_pval)
        }
        p <- p + annotate("text", x=Inf, y=Inf, label=anova_text,
                         hjust=1.1, vjust=1.5, size=3.5, fontface="plain")
      }
    }
    
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
