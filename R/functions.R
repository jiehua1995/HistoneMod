##### HistoneMod: Shared Functions #####
# This file contains reusable helpers for: dependency setup, input validation,
# data processing, plotting (PCA/Heatmap/Barplot), and version/download helpers.
# It is sourced by both ui.R and server.R.

##### Dependencies & Environment Setup #####
# CUSTOMIZE: Add/remove packages in `required_packages` if your app evolves.
# ------------------------
# 0. Prepare environment
# ------------------------

#' Check Whether HistoneMod Dependencies Are Available
#'
#' Validates that the packages declared in `DESCRIPTION` are installed and
#' available to the current R session. This is mainly a diagnostic helper; when
#' HistoneMod is installed normally, these dependencies should already be pulled
#' in automatically.
#'
#' @return Invisibly returns `TRUE` when all required packages are available.
#'   Throws an error listing missing packages otherwise.
#' @export
depends_check <- function() {
  desc_fields <- tryCatch(
    utils::packageDescription("HistoneMod", fields = c("Depends", "Imports")),
    error = function(e) NULL
  )

  required_packages <- character(0)
  if (!is.null(desc_fields)) {
    fields <- unname(unlist(desc_fields, use.names = FALSE))
    fields <- fields[!is.na(fields)]
    parsed <- trimws(unlist(strsplit(paste(fields, collapse = ","), ",")))
    parsed <- sub("\\s*\\(.*\\)$", "", parsed)
    parsed <- parsed[nzchar(parsed)]
    required_packages <- setdiff(unique(parsed), "R")
  }

  if (length(required_packages) == 0) {
    required_packages <- c(
      "DT", "dplyr", "ggplot2", "ggrepel", "ggsignif", "httr",
      "jsonlite", "pheatmap", "png", "shiny", "shinyalert",
      "shinycssloaders", "shinyjs", "shinyWidgets", "svglite",
      "tidyr", "viridis"
    )
  }

  missing_packages <- required_packages[
    !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
  ]

  if (length(missing_packages) > 0) {
    stop(
      "Missing required packages: ",
      paste(missing_packages, collapse = ", "),
      ". Reinstall HistoneMod so its Imports are installed automatically.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

##### Input File Validation #####
# CUSTOMIZE: Required column names live in `required_cols` below.
# ------------------------
# File Validation Functions
# ------------------------
#' Validate an MS1 CSV File
#'
#' Checks that an MS1 CSV export can be read, contains the required columns, and
#' is not empty.
#'
#' @param file_path Path to the MS1 CSV file.
#'
#' @return A list with at least `valid` and `message`. When validation succeeds,
#'   the returned list also contains the parsed data frame in `data`.
#' @export
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

#' Validate a Sample Annotation CSV File
#'
#' Checks that a sample annotation CSV file can be read, contains the required
#' columns, and is not empty.
#'
#' @param file_path Path to the sample CSV file.
#'
#' @return A list with at least `valid` and `message`. When validation succeeds,
#'   the returned list also contains the parsed data frame in `data`.
#' @export
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

##### Data Processing: Percentage Calculation #####
# CUSTOMIZE: `exclude_un`, `max_percentage`, and the peptide "un/unun" rule
# are the most common knobs to tweak.
# ------------------------
# 1. percentage_calculation
# ------------------------
#' Calculate Relative Peptide Percentages
#'
#' Merges MS1 and sample tables, keeps light peptides, computes within-protein
#' relative abundances, and optionally filters unmodified peptides, peptide
#' selections, and sample selections.
#'
#' @param ms1 A data frame containing peptide-level MS1 measurements.
#' @param sample A data frame containing sample metadata.
#' @param exclude_un Logical; if `TRUE`, excludes peptide names ending in
#'   `"un"` or `"unun"`.
#' @param selected_peptides Optional character vector of peptide names to keep.
#' @param selected_samples Optional character vector of sample names to keep.
#' @param max_percentage Numeric upper bound used to filter implausible
#'   percentage values.
#'
#' @return A processed data frame with merged metadata and a `Percentage`
#'   column.
#' @export
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
    # CUSTOMIZE: This rule treats trailing "_un" or "_unun" (case-insensitive)
    # as unmodified peptides.
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

##### Plotting: PCA #####
# CUSTOMIZE: Point/text sizes and theme settings are in `plot_pca()`.
# ------------------------
# 2. PCA plot
# ------------------------
#' Plot PCA Scores
#'
#' Builds a PCA score plot from the processed HistoneMod percentage table.
#'
#' @param data_merge A processed data frame containing at least
#'   `Replicate.Name`, `Peptide.Note`, and `Percentage`.
#' @param show_ellipse Logical; if `TRUE`, draws 95 percent confidence ellipses
#'   for groups with enough samples.
#' @param color_palette Character palette name. Supported values are
#'   `"viridis"`, `"magma"`, `"plasma"`, `"inferno"`, `"cividis"`,
#'   `"rocket"`, `"mako"`, and `"turbo"`.
#'
#' @return A `ggplot` object.
#' @export
plot_pca <- function(data_merge, show_ellipse = TRUE, color_palette = "viridis") {

  colnames(data_merge) <- gsub("\\s+", ".", colnames(data_merge))

  if ("Replicate.Name" %in% colnames(data_merge)) {
    data_merge$Replicate.Name <- as.character(data_merge$Replicate.Name)
  }
  if ("Peptide.Note" %in% colnames(data_merge)) {
    data_merge$Peptide.Note <- as.character(data_merge$Peptide.Note)
  }
  if ("Group" %in% colnames(data_merge)) {
    data_merge$Group <- as.character(data_merge$Group)
  }


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
  scores$Replicate.Name <- as.character(wide$Replicate.Name)
  
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
    ggrepel::geom_text_repel(aes(label = Replicate.Name),
                             size = 6,
                             max.overlaps = Inf,
                             family = "sans") +
    labs(
      title = NULL,
      x = paste0("PC1 (", round(variance_explained[1] * 100, 1), "%)"),
      y = paste0("PC2 (", round(variance_explained[2] * 100, 1), "%)")
    ) +
    theme_minimal(base_family = "sans") +
    theme(
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8, linejoin = "mitre"),
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 14),
      legend.title = element_text(size = 16),
      legend.text = element_text(size = 14)
    ) +
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


##### Plotting: Heatmap (pheatmap) #####
# CUSTOMIZE: Font sizes are controlled by `fontsize`, `fontsize_row`, `fontsize_col`.
# The PDF report may pass scaled values here to avoid overlap.
# ------------------------
# 3. Heatmap plot
# ------------------------
#' Plot a Heatmap
#'
#' Converts the processed HistoneMod percentage table into a peptide-by-sample
#' matrix and renders a heatmap with `pheatmap`.
#'
#' @param data_merge A processed data frame containing at least
#'   `Replicate.Name`, `Peptide.Note`, and `Percentage`.
#' @param cluster_rows Logical; whether peptides should be clustered.
#' @param cluster_cols Logical; whether samples should be clustered.
#' @param color_palette Character palette name. Supported values are
#'   `"viridis"`, `"magma"`, `"plasma"`, `"inferno"`, `"cividis"`,
#'   `"rocket"`, `"mako"`, and `"turbo"`.
#' @param fontsize Base font size passed to `pheatmap`.
#' @param fontsize_row Optional row label font size.
#' @param fontsize_col Optional column label font size.
#'
#' @return A `pheatmap` result object.
#' @export
plot_heatmap <- function(
  data_merge,
  cluster_rows = TRUE,
  cluster_cols = TRUE,
  color_palette = "viridis",
  fontsize = 12,
  fontsize_row = NULL,
  fontsize_col = NULL
) {
  colnames(data_merge) <- gsub("\\s+", ".", colnames(data_merge))

  if ("Replicate.Name" %in% colnames(data_merge)) {
    data_merge$Replicate.Name <- as.character(data_merge$Replicate.Name)
  }
  if ("Peptide.Note" %in% colnames(data_merge)) {
    data_merge$Peptide.Note <- as.character(data_merge$Peptide.Note)
  }
  
  heat_data <- data_merge %>%
    select(Replicate.Name, Peptide.Note, Percentage) %>%
    pivot_wider(names_from = Replicate.Name, values_from = Percentage, values_fill=0)
  
  # Sort by name if not clustering
  if(!cluster_rows) {
    heat_data <- heat_data %>% arrange(Peptide.Note)
  }
  
  if(ncol(heat_data) <= 1) stop("Not enough data for heatmap")
  
  rownames_mat <- as.character(heat_data$Peptide.Note)
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

  fontsize <- suppressWarnings(as.numeric(fontsize))
  if (!is.finite(fontsize) || is.na(fontsize) || fontsize <= 0) fontsize <- 12
  fontsize <- max(4, min(24, fontsize))

  if (is.null(fontsize_row)) fontsize_row <- fontsize
  if (is.null(fontsize_col)) fontsize_col <- fontsize
  fontsize_row <- suppressWarnings(as.numeric(fontsize_row))
  fontsize_col <- suppressWarnings(as.numeric(fontsize_col))
  if (!is.finite(fontsize_row) || is.na(fontsize_row) || fontsize_row <= 0) fontsize_row <- fontsize
  if (!is.finite(fontsize_col) || is.na(fontsize_col) || fontsize_col <= 0) fontsize_col <- fontsize
  fontsize_row <- max(3, min(24, fontsize_row))
  fontsize_col <- max(3, min(24, fontsize_col))

  # Plot heatmap in the simplest/original way (pheatmap draws as a side effect).
  p <- pheatmap::pheatmap(
    heat_mat,
    cluster_rows = cluster_rows,
    cluster_cols = cluster_cols,
    color = colors,
    fontsize = fontsize,
    fontsize_row = fontsize_row,
    fontsize_col = fontsize_col,
    border_color = "grey50",
    angle_col = 45,
    main = "",
    silent = TRUE
  )

  p
}


##### Plotting: Barplot (Single Protein + Single Peptide) #####
# CUSTOMIZE: The key knob for PDF readability is `font_scale`.
# For Shiny on-screen plots, keep `font_scale = 1`.
# ------------------------
# 4. Barplot per protein (single peptide)
# ------------------------
#' Plot a Single-Peptide Barplot
#'
#' Summarises peptide percentages by group and renders a single barplot with
#' optional significance testing.
#'
#' @param data_merge A processed data frame returned by
#'   [percentage_calculation()].
#' @param protein_name Optional protein identifier used to subset the data.
#' @param peptide_name Peptide identifier to plot.
#' @param add_signif Logical; whether to add significance labels.
#' @param color_palette Character palette name. Supported values are
#'   `"viridis"`, `"magma"`, `"plasma"`, `"inferno"`, `"cividis"`,
#'   `"rocket"`, `"mako"`, and `"turbo"`.
#' @param y_limits Optional numeric vector of length 2 for the y-axis range.
#' @param font_scale Numeric multiplier used to scale plot text.
#' @param title_text Optional custom plot title.
#'
#' @return A `ggplot` object.
#' @export
plot_barplot_single <- function(
  data_merge,
  protein_name,
  peptide_name,
  add_signif = TRUE,
  color_palette = "viridis",
  y_limits = NULL,
  font_scale = 1,
  title_text = NULL
) {
  colnames(data_merge) <- gsub("\\s+", ".", colnames(data_merge))

  font_scale <- suppressWarnings(as.numeric(font_scale))
  if (!is.finite(font_scale) || is.na(font_scale) || font_scale <= 0) font_scale <- 1
  font_scale <- max(0.2, min(2.5, font_scale))

  # ggplot text `size` is in mm; theme text sizes are in pt.
  # Convert pt -> mm so annotation text can visually match axis tick text.
  pt_to_mm <- function(pt) pt / 2.845276
  axis_tick_pt <- 14
  axis_tick_mm <- pt_to_mm(axis_tick_pt) * font_scale
  
  df <- data_merge
  if(!is.null(protein_name)) df <- df %>% filter(Protein.Name == protein_name)
  df_sub <- df %>% filter(Peptide.Note == peptide_name)
  
  if(nrow(df_sub) == 0) {
    return(ggplot() + 
           geom_text(aes(0, 0, label = "No data available"), size = 6 * font_scale) + 
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
    labs(title = title_text, y = "Percentage", x = "Group") +
    theme_minimal(base_family = "sans") +
    theme(
      panel.grid = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
      axis.title = element_text(size = 16 * font_scale),
      axis.text = element_text(size = 14 * font_scale),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none",
      # Keep the peptide label visually consistent with x-axis tick font size.
      plot.title = element_text(size = 14 * font_scale, face = "plain", hjust = 0)
    ) +
    fill_scale
  
  # Add ANOVA p-value to top-right corner if significance testing is enabled
  anova_present <- FALSE
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
      anova_present <- TRUE
      anova_text <- if(length(unique(df_sub$Group)) == 2) {
        sprintf("t-test p = %.6f", anova_pval)
      } else {
        sprintf("ANOVA p = %.6f", anova_pval)
      }
      p <- p + annotate("text", x=Inf, y=Inf, label=anova_text,
                       hjust=1.1, vjust=1.5, size = axis_tick_mm * 1, fontface="plain")
    }
  }
  
  # Add significance stars (only p < 0.05)
  y_max <- y_base * 1.5  # Default extension
  if(add_signif && length(unique(df_sub$Group))>=2) {
    # CUSTOMIZE: If you want a different significance threshold, change 0.05.
    comps <- combn(unique(df_sub$Group), 2, simplify=FALSE)
    # Keep the first bracket closer to the bars
    step <- if (is.finite(y_base) && y_base > 0) y_base * 0.1 else 0.5
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
                           tip_length = 0.02,
                           textsize = 6 * font_scale,
                           # Move stars down closer to the bracket line
                           vjust = 0.7)
      # Extend y-axis to show all significance brackets
      y_max <- max(y_positions) + step * 1.5

      # Leave extra room above the highest bracket for the ANOVA label
      if (isTRUE(anova_present)) {
        y_max <- y_max + step * 1.2
      }
    }
  }
  
  # Set y-axis limits after calculating significance positions
  auto_y_max <- if(is.finite(y_max) && y_max > 0) y_max else 1
  plot_ylim <- if(is.null(y_limits) || length(y_limits) != 2 || any(!is.finite(y_limits))) {
    c(0, auto_y_max)
  } else {
    as.numeric(y_limits)
  }
  plot_ylim <- sort(plot_ylim)
  p <- p + coord_cartesian(ylim = plot_ylim)
  
  return(p)
}

##### App Metadata & Version Checking #####
# CUSTOMIZE: Update APP_VERSION when you release; set GITHUB_REPO to your repo.
# ------------------------
# 5. App version helpers
# ------------------------
APP_VERSION <- "0.5.2"
GITHUB_REPO <- "jiehua1995/HistoneMod"

#' Normalize a Version String
#'
#' Trims whitespace, removes a leading `v`, and returns a scalar character
#' version string.
#'
#' @param x A version-like object.
#'
#' @return A length-one character vector, or `NA_character_` when the input does
#'   not contain a usable version.
#' @export
normalize_version <- function(x) {
  if(is.null(x) || length(x) == 0) return(NA_character_)
  x <- as.character(x[1])
  x <- trimws(x)
  x <- sub("^v", "", x, ignore.case = TRUE)
  if(identical(x, "")) return(NA_character_)
  x
}

#' Query the Latest GitHub Release Version
#'
#' Fetches the latest release tag for a GitHub repository and normalizes it to a
#' plain version string.
#'
#' @param repo Repository slug in `"owner/repo"` format.
#'
#' @return A normalized version string, or `NA_character_` if the lookup fails.
#' @export
github_latest_release_version <- function(repo) {
  repo <- as.character(repo)[1]
  if(is.na(repo) || identical(trimws(repo), "")) return(NA_character_)
  api_url <- paste0("https://api.github.com/repos/", repo, "/releases/latest")

  # Avoid blocking the UI on slow networks.
  old_timeout <- getOption("timeout")
  if(is.null(old_timeout) || !is.finite(old_timeout)) old_timeout <- 60
  options(timeout = min(5, old_timeout))
  on.exit(options(timeout = old_timeout), add = TRUE)

  # Prefer httr if available (sets a User-Agent reliably), otherwise fall back to jsonlite.
  if(requireNamespace("httr", quietly = TRUE)) {
    resp <- tryCatch(
      httr::GET(api_url, httr::user_agent("HistoneMod-Shiny"), httr::timeout(5)),
      error = function(e) NULL
    )
    if(!is.null(resp) && httr::status_code(resp) == 200) {
      parsed <- tryCatch(httr::content(resp, as = "parsed", type = "application/json"), error = function(e) NULL)
      if(!is.null(parsed) && !is.null(parsed$tag_name)) {
        return(normalize_version(parsed$tag_name))
      }
    }
  }

  parsed <- tryCatch(jsonlite::fromJSON(api_url), error = function(e) NULL)
  if(!is.null(parsed) && !is.null(parsed$tag_name)) {
    return(normalize_version(parsed$tag_name))
  }
  NA_character_
}

get_latest_release_version_cached <- local({
  checked <- FALSE
  cached <- NA_character_

  function(repo) {
    if(isTRUE(checked)) return(cached)
    checked <<- TRUE
    cached <<- tryCatch(github_latest_release_version(repo), error = function(e) NA_character_)
    cached
  }
})

##### Download Module (Legacy Helper) #####
# NOTE: The main app now uses the export modals in server.R.
# This module remains as a small reusable download helper.
# CUSTOMIZE: Update the default width/height/dpi inside ggsave() if you use it.
# ------------------------
# 5. Downlod the picture
# ------------------------

# Download_UI
#' Build a Plot Download Module UI
#'
#' Creates a namespaced download button for the legacy reusable plot download
#' module.
#'
#' @param id Module id.
#' @param label Button label.
#'
#' @return A Shiny UI element.
#' @export
downloadPlotUI <- function(id, label = "Download Plot") {
  ns <- NS(id)
  downloadButton(ns("download"), label)
}

# Download_server
#' Register a Plot Download Module Server
#'
#' Registers server logic for a simple plot download module.
#'
#' @param id Module id.
#' @param plot_reactive A reactive expression returning a plot object.
#' @param filename_prefix Prefix used for the downloaded file name.
#'
#' @return The result of [shiny::moduleServer()].
#' @export
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
