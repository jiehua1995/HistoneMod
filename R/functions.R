##### HistoneMod: Shared Functions #####
# This file contains reusable helpers for: dependency setup, input validation,
# data processing, plotting (PCA/Heatmap/Barplot), and version/download helpers.
# It is sourced by both ui.R and server.R.

##### Dependencies & Environment Setup #####
# Package dependencies should be declared in DESCRIPTION so they are installed
# automatically with the package. `depends_check()` can optionally install any
# missing packages via pak, which handles both CRAN and Bioconductor sources.
# ------------------------
# 0. Prepare environment
# ------------------------

#' Check and optionally install HistoneMod dependencies
#'
#' Validates that the packages declared in `DESCRIPTION` are installed and
#' available to the current R session. If requested, missing packages are
#' installed with `pak`, which can manage both CRAN and Bioconductor packages.
#'
#' @param install_missing Logical; if `TRUE`, install any missing packages using
#'   `pak::pkg_install()`.
#' @param attach Logical; if `TRUE`, attach the required packages with
#'   `library()`. The default is `FALSE` because HistoneMod imports the required
#'   namespaces itself.
#'
#' @return Invisibly returns `TRUE` when all required packages are available
#'   after any optional installation step. Throws an error if packages are still
#'   missing.
#' @export
depends_check <- function(install_missing = FALSE, attach = FALSE) {
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
      "DT", "dplyr", "ggplot2", "ggrepel", "ggsignif", "pheatmap", "png",
      "shiny", "shinyalert", "shinycssloaders", "shinyFiles", "shinyjs",
      "shinyWidgets", "tidyr", "viridis"
    )
  }

  missing_packages <- required_packages[
    !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
  ]

  if (length(missing_packages) > 0 && isTRUE(install_missing)) {
    if (!requireNamespace("pak", quietly = TRUE)) {
      install.packages("pak")
    }

    pak::pkg_install(missing_packages)

    missing_packages <- required_packages[
      !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
    ]
  }

  if (length(missing_packages) > 0) {
    stop(
      "Missing required packages: ",
      paste(missing_packages, collapse = ", "),
      ". Reinstall HistoneMod or run depends_check(install_missing = TRUE).",
      call. = FALSE
    )
  }

  if (isTRUE(attach)) {
    for (pkg in required_packages) {
      library(pkg, character.only = TRUE)
    }
  }

  invisible(TRUE)
}

##### Input File Validation #####
# CUSTOMIZE: Required column names live in `required_cols` below.
# ------------------------
# File Validation Functions
# ------------------------
# Internal file validator for MS1 CSV inputs.
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

# Internal file validator for sample annotation CSV inputs.
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
#' Calculate relative peptide percentages
#'
#' Merges MS1 and sample tables, keeps light peptides, computes within-protein
#' relative abundances, and optionally filters unmodified peptides, peptide
#' selections, and sample selections.
#'
#' @param ms1 A data frame containing peptide-level MS1 measurements.
#' @param sample A data frame containing sample metadata.
#' @param exclude_un Logical; if `TRUE`, exclude peptide names ending in
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
  
  
  
  # Exclude unmodified peptides and apply peptide/sample selections
  df <- filter_processed_data(
    df,
    exclude_un = exclude_un,
    selected_peptides = selected_peptides,
    selected_samples = selected_samples
  )

  return(df)
}

##### Plotting: PCA #####
# CUSTOMIZE: Point/text sizes and theme settings are in `plot_pca()`.
# ------------------------
# 2. PCA plot
# ------------------------
# Internal helper: order group values according to a user-supplied vector.
group_levels <- function(x, order = NULL) {
  x <- as.character(x)
  if (is.null(order) || length(order) == 0) return(unique(x))
  c(intersect(order, x), setdiff(unique(x), order))
}

# Internal helper: parse a comma-separated group order string.
parse_group_order <- function(x) {
  if (is.null(x) || length(x) == 0) return(NULL)
  x <- trimws(unlist(strsplit(paste(x, collapse = ","), ",", fixed = TRUE)))
  x <- x[nzchar(x)]
  if (length(x) == 0) NULL else unique(x)
}

# Internal helper: parse "Group=#RRGGBB,Group2=#RRGGBB" colour settings.
parse_group_colors <- function(x) {
  entries <- parse_group_order(x)
  if (is.null(entries)) return(NULL)
  pairs <- strsplit(entries, "=", fixed = TRUE)
  ok <- vapply(pairs, length, integer(1)) == 2
  if (!all(ok)) return(NULL)
  values <- trimws(vapply(pairs, `[`, character(1), 2))
  names(values) <- trimws(vapply(pairs, `[`, character(1), 1))
  values[nzchar(values)]
}

# Internal helper: detect unmodified peptides using the "_un" naming rule.
is_unmodified_peptide <- function(x) {
  last_mod <- sub("^.*_", "", as.character(x))
  grepl("^un(un)*$", last_mod, ignore.case = TRUE)
}

# Internal helper: apply the shared peptide/sample/unmodified filters.
filter_processed_data <- function(df, exclude_un = TRUE,
                                  selected_peptides = NULL,
                                  selected_samples = NULL) {
  if (isTRUE(exclude_un) && "Peptide.Note" %in% names(df)) {
    df <- df[!is_unmodified_peptide(df$Peptide.Note), , drop = FALSE]
  }
  if (!is.null(selected_peptides) && "Peptide.Note" %in% names(df)) {
    df <- df[df$Peptide.Note %in% selected_peptides, , drop = FALSE]
  }
  if (!is.null(selected_samples) && "Replicate.Name" %in% names(df)) {
    df <- df[df$Replicate.Name %in% selected_samples, , drop = FALSE]
  }
  df
}

#' Plot PCA scores
#'
#' Builds a PCA score plot from a processed HistoneMod table. The value column,
#' transformation, missing-value policy, group order and group colours are
#' configurable so the same function can be used for percentages, ratios and
#' transformed (CLR/logit) data.
#'
#' @param data_merge A processed data frame containing at least
#'   `Replicate.Name`, `Peptide.Note` and the column named by `value_col`.
#' @param show_ellipse Logical; if `TRUE`, draws 95 percent confidence ellipses
#'   for groups with enough samples.
#' @param color_palette Character palette name. Supported values are
#'   `"viridis"`, `"magma"`, `"plasma"`, `"inferno"`, `"cividis"`,
#'   `"rocket"`, `"mako"`, and `"turbo"`.
#' @param value_col Character; name of the numeric value column.
#' @param transform Character; one of `"none"`, `"clr"`, `"logit"` or `"log2"`.
#'   `"clr"` is recommended for compositional percentage data.
#' @param missing Character; missing sample-by-peptide values are replaced by
#'   `"zero"` (default), `"min"` (half the smallest positive value) or dropped
#'   with `"drop"`.
#' @param group_order Optional character vector giving the display order of
#'   groups.
#' @param group_colors Optional named character vector of colours per group.
#'
#' @return A `ggplot` object.
#' @export
plot_pca <- function(data_merge, show_ellipse = TRUE, color_palette = "viridis",
                     value_col = "Percentage",
                     transform = c("none", "clr", "logit", "log2"),
                     missing = c("zero", "drop", "min"),
                     group_order = NULL, group_colors = NULL) {
  transform <- match.arg(transform)
  missing <- match.arg(missing)

  colnames(data_merge) <- gsub("\\s+", ".", colnames(data_merge))
  if (!value_col %in% colnames(data_merge)) {
    stop("Column '", value_col, "' not found in data_merge.", call. = FALSE)
  }

  for (col in c("Replicate.Name", "Peptide.Note", "Group")) {
    if (col %in% colnames(data_merge)) {
      data_merge[[col]] <- as.character(data_merge[[col]])
    }
  }

  duplicates <- data_merge %>%
    dplyr::count(Replicate.Name, Peptide.Note) %>%
    dplyr::filter(n > 1)
  if (nrow(duplicates) > 0) {
    stop("Duplicated Replicate.Name x Peptide.Note values; aggregate them before PCA.", call. = FALSE)
  }

  long <- data_merge[, c("Replicate.Name", "Peptide.Note", value_col), drop = FALSE]
  colnames(long)[3] <- "Value"

  fill_value <- switch(missing,
    "zero" = 0,
    "min" = {
      positive <- long$Value[!is.na(long$Value) & long$Value > 0]
      if (length(positive) == 0) 0 else min(positive) / 2
    },
    "drop" = NA_real_
  )

  wide <- tidyr::pivot_wider(
    long,
    names_from = "Peptide.Note",
    values_from = "Value",
    values_fill = fill_value
  )

  if (nrow(wide) == 0) return(ggplot() + ggtitle("No data for PCA"))

  numeric_mat <- as.matrix(wide[, setdiff(colnames(wide), "Replicate.Name"), drop = FALSE])
  storage.mode(numeric_mat) <- "numeric"
  rownames(numeric_mat) <- as.character(wide$Replicate.Name)

  if (missing == "drop") {
    keep_features <- colSums(is.na(numeric_mat)) == 0
    numeric_mat <- numeric_mat[, keep_features, drop = FALSE]
    complete_rows <- rowSums(is.na(numeric_mat)) == 0
    numeric_mat <- numeric_mat[complete_rows, , drop = FALSE]
    if (nrow(numeric_mat) == 0) {
      return(ggplot() + ggtitle("No complete samples for PCA"))
    }
  }

  if (transform != "none") {
    numeric_mat <- transform_composition(numeric_mat, method = transform)
  }

  sds <- apply(numeric_mat, 2, stats::sd, na.rm = TRUE)
  keep_cols <- names(sds)[is.finite(sds) & sds > 0]

  if (length(keep_cols) < 2) {
    return(ggplot() + geom_text(aes(0, 0, label = "Not enough variable peptides for PCA")) + theme_void())
  }

  pca_mat <- numeric_mat[, keep_cols, drop = FALSE]

  pca_res <- stats::prcomp(pca_mat, center = TRUE, scale. = transform %in% c("none", "log2"))
  scores <- as.data.frame(pca_res$x)
  scores$Replicate.Name <- rownames(pca_mat)

  if ("Group" %in% colnames(data_merge)) {
    scores <- dplyr::left_join(
      scores,
      data_merge %>% dplyr::select(Replicate.Name, Group) %>% dplyr::distinct(),
      by = "Replicate.Name"
    )
    scores$Group <- factor(scores$Group, levels = group_levels(scores$Group, group_order))
  } else {
    scores$Group <- factor("All")
  }
  
  variance_explained <- summary(pca_res)$importance[2, ]
  
  # Select color scale
  color_scale <- if (!is.null(group_colors)) {
    scale_color_manual(values = group_colors)
  } else {
    switch(color_palette,
           "viridis" = scale_color_viridis_d(),
           "magma" = scale_color_viridis_d(option = "magma"),
           "plasma" = scale_color_viridis_d(option = "plasma"),
           "inferno" = scale_color_viridis_d(option = "inferno"),
           "cividis" = scale_color_viridis_d(option = "cividis"),
           "rocket" = scale_color_viridis_d(option = "rocket"),
           "mako" = scale_color_viridis_d(option = "mako"),
           "turbo" = scale_color_viridis_d(option = "turbo"),
           scale_color_viridis_d())
  }
  
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
  
  # Add confidence ellipses for eligible groups only.
  if(show_ellipse) {
    group_counts <- table(scores$Group)
    eligible_groups <- names(group_counts[group_counts >= 4])
    if(length(eligible_groups) > 0) {
      build_group_ellipse <- function(df_group, level = 0.95, points = 120) {
        xy <- as.matrix(df_group[, c("PC1", "PC2"), drop = FALSE])
        center <- colMeans(xy, na.rm = TRUE)
        cov_mat <- stats::cov(xy, use = "complete.obs")

        if(!all(is.finite(cov_mat))) {
          cov_mat <- diag(c(1, 1))
        }

        diag(cov_mat) <- pmax(diag(cov_mat), 1e-8)
        cov_mat <- cov_mat + diag(1e-8, 2)

        eig <- eigen(cov_mat, symmetric = TRUE)
        eig$values <- pmax(eig$values, 1e-8)

        radius <- sqrt(stats::qchisq(level, df = 2))
        theta <- seq(0, 2 * pi, length.out = points)
        unit_circle <- cbind(cos(theta), sin(theta))
        transform <- eig$vectors %*% diag(sqrt(eig$values), nrow = 2) * radius
        coords <- unit_circle %*% t(transform)
        coords <- sweep(coords, 2, center, FUN = "+")

        data.frame(
          PC1 = coords[, 1],
          PC2 = coords[, 2],
          Group = df_group$Group[1]
        )
      }

      ellipse_df <- dplyr::bind_rows(lapply(eligible_groups, function(grp) {
        build_group_ellipse(scores %>% filter(Group == grp))
      }))

      p <- p + geom_path(
        data = ellipse_df,
        mapping = aes(x = PC1, y = PC2, group = Group, color = Group),
        inherit.aes = FALSE,
        linetype = 2,
        linewidth = 0.8
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
#' Plot a heatmap
#'
#' Converts a processed HistoneMod table into a peptide-by-sample matrix and
#' renders a heatmap with `pheatmap`.
#'
#' @param data_merge A processed data frame containing at least
#'   `Replicate.Name`, `Peptide.Note` and the column named by `value_col`.
#' @param cluster_rows Logical; whether peptides should be clustered.
#' @param cluster_cols Logical; whether samples should be clustered.
#' @param color_palette Character palette name. Supported values are
#'   `"viridis"`, `"magma"`, `"plasma"`, `"inferno"`, `"cividis"`,
#'   `"rocket"`, `"mako"`, and `"turbo"`.
#' @param fontsize Base font size passed to `pheatmap`.
#' @param fontsize_row Optional row label font size.
#' @param fontsize_col Optional column label font size.
#' @param value_col Character; name of the numeric value column.
#' @param missing Character; how to handle missing values (`"zero"`, `"min"`
#'   or `"drop"`).
#' @param scale Character; `"none"` (default) or `"row"` for row z-scores.
#' @param show_group_annotation Logical; add sample group annotation when a
#'   `Group` column is present.
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
  fontsize_col = NULL,
  value_col = "Percentage",
  missing = c("zero", "drop", "min"),
  scale = c("none", "row"),
  show_group_annotation = TRUE
) {
  missing <- match.arg(missing)
  scale <- match.arg(scale)

  colnames(data_merge) <- gsub("\\s+", ".", colnames(data_merge))
  if (!value_col %in% colnames(data_merge)) {
    stop("Column '", value_col, "' not found in data_merge.", call. = FALSE)
  }

  for (col in c("Replicate.Name", "Peptide.Note", "Group")) {
    if (col %in% colnames(data_merge)) {
      data_merge[[col]] <- as.character(data_merge[[col]])
    }
  }

  duplicates <- data_merge %>%
    dplyr::count(Replicate.Name, Peptide.Note) %>%
    dplyr::filter(n > 1)
  if (nrow(duplicates) > 0) {
    stop("Duplicated Replicate.Name x Peptide.Note values; aggregate them before the heatmap.", call. = FALSE)
  }

  long <- data_merge[, c("Replicate.Name", "Peptide.Note", value_col), drop = FALSE]
  colnames(long)[3] <- "Value"

  fill_value <- switch(missing,
    "zero" = 0,
    "min" = {
      positive <- long$Value[!is.na(long$Value) & long$Value > 0]
      if (length(positive) == 0) 0 else min(positive) / 2
    },
    "drop" = NA_real_
  )

  heat_data <- tidyr::pivot_wider(
    long,
    names_from = "Replicate.Name",
    values_from = "Value",
    values_fill = fill_value
  )

  # Sort by name if not clustering
  if (!cluster_rows) {
    heat_data <- heat_data %>% dplyr::arrange(Peptide.Note)
  }

  if (ncol(heat_data) <= 1) stop("Not enough data for heatmap")

  rownames_mat <- as.character(heat_data$Peptide.Note)
  heat_mat <- as.matrix(heat_data[, setdiff(colnames(heat_data), "Peptide.Note"), drop = FALSE])
  storage.mode(heat_mat) <- "numeric"
  rownames(heat_mat) <- rownames_mat
  colnames(heat_mat) <- as.character(colnames(heat_mat))

  if (missing == "drop") {
    heat_mat <- heat_mat[stats::complete.cases(heat_mat), , drop = FALSE]
    if (nrow(heat_mat) == 0) stop("No complete peptides left for the heatmap")
  }

  if (scale == "row") {
    row_means <- rowMeans(heat_mat, na.rm = TRUE)
    row_sds <- apply(heat_mat, 1, stats::sd, na.rm = TRUE)
    row_sds[!is.finite(row_sds) | row_sds == 0] <- 1
    heat_mat <- sweep(heat_mat - row_means, 1, row_sds, "/")
    heat_mat[!is.finite(heat_mat)] <- 0
  }

  # Sort columns by name if not clustering
  if (!cluster_cols) {
    heat_mat <- heat_mat[, order(colnames(heat_mat)), drop = FALSE]
  }

  annotation_col <- NULL
  if (isTRUE(show_group_annotation) && "Group" %in% colnames(data_merge)) {
    annotation <- as.data.frame(
      unique(data_merge[, c("Replicate.Name", "Group"), drop = FALSE]),
      stringsAsFactors = FALSE
    )
    rownames(annotation) <- annotation$Replicate.Name
    annotation$Replicate.Name <- NULL
    annotation_col <- annotation[colnames(heat_mat), , drop = FALSE]
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
    annotation_col = annotation_col,
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
#' Plot a single-peptide barplot
#'
#' Summarises peptide values by group and renders a single barplot with
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
#' @param value_col Character; name of the numeric value column.
#' @param group_order Optional character vector giving the display order of
#'   groups.
#' @param group_colors Optional named character vector of colours per group.
#' @param error_bar Character; `"se"` (default), `"sd"` or `"ci95"`.
#' @param show_n Logical; print the number of replicates above each bar.
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
  title_text = NULL,
  value_col = "Percentage",
  group_order = NULL,
  group_colors = NULL,
  error_bar = c("se", "sd", "ci95"),
  show_n = FALSE
) {
  error_bar <- match.arg(error_bar)
  colnames(data_merge) <- gsub("\\s+", ".", colnames(data_merge))
  if (!value_col %in% colnames(data_merge)) {
    stop("Column '", value_col, "' not found in data_merge.", call. = FALSE)
  }

  font_scale <- suppressWarnings(as.numeric(font_scale))
  if (!is.finite(font_scale) || is.na(font_scale) || font_scale <= 0) font_scale <- 1
  font_scale <- max(0.2, min(2.5, font_scale))

  # ggplot text `size` is in mm; theme text sizes are in pt.
  # Convert pt -> mm so annotation text can visually match axis tick text.
  pt_to_mm <- function(pt) pt / 2.845276
  axis_tick_pt <- 14
  axis_tick_mm <- pt_to_mm(axis_tick_pt) * font_scale

  df <- data_merge
  if (!is.null(protein_name)) df <- df %>% dplyr::filter(Protein.Name == protein_name)
  df_sub <- df %>% dplyr::filter(Peptide.Note == peptide_name)

  if (nrow(df_sub) == 0) {
    return(ggplot() +
           geom_text(aes(0, 0, label = "No data available"), size = 6 * font_scale) +
           theme_void())
  }

  df_sub$Group <- as.character(df_sub$Group)
  df_sub$Value <- as.numeric(df_sub[[value_col]])

  summary_data <- df_sub %>%
    dplyr::group_by(Group) %>%
    dplyr::summarise(
      mean_value = mean(Value, na.rm = TRUE),
      sd_value = stats::sd(Value, na.rm = TRUE),
      n = sum(!is.na(Value)),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      se = sd_value / sqrt(pmax(n, 1)),
      ci95 = ifelse(n > 1, se * stats::qt(0.975, df = pmax(n - 1, 1)), NA_real_),
      err = switch(error_bar,
                   "se" = se,
                   "sd" = sd_value,
                   "ci95" = ci95),
      Group = factor(Group, levels = group_levels(Group, group_order))
    ) %>%
    dplyr::arrange(Group)

  # Calculate y-axis range first to accommodate significance brackets
  y_base <- max(summary_data$mean_value + ifelse(is.na(summary_data$err), 0, summary_data$err), na.rm = TRUE)
  if (!is.finite(y_base)) y_base <- 0

  # Select fill scale
  fill_scale <- if (!is.null(group_colors)) {
    scale_fill_manual(values = group_colors)
  } else {
    switch(color_palette,
           "viridis" = scale_fill_viridis_d(),
           "magma" = scale_fill_viridis_d(option = "magma"),
           "plasma" = scale_fill_viridis_d(option = "plasma"),
           "inferno" = scale_fill_viridis_d(option = "inferno"),
           "cividis" = scale_fill_viridis_d(option = "cividis"),
           "rocket" = scale_fill_viridis_d(option = "rocket"),
           "mako" = scale_fill_viridis_d(option = "mako"),
           "turbo" = scale_fill_viridis_d(option = "turbo"),
           scale_fill_viridis_d())
  }

  p <- ggplot(summary_data, aes(x = Group, y = mean_value, fill = Group)) +
    geom_col(position = position_dodge(width = 0.9)) +
    geom_errorbar(aes(ymin = mean_value - err, ymax = mean_value + err),
                  width = 0.2, position = position_dodge(width = 0.9), na.rm = TRUE) +
    labs(title = title_text, y = value_col, x = "Group") +
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

  if (isTRUE(show_n)) {
    p <- p + geom_text(
      aes(y = mean_value + ifelse(is.na(err), 0, err), label = paste0("n=", n)),
      vjust = -0.5,
      size = axis_tick_mm * 0.9,
      show.legend = FALSE
    )
  }

  # Add global test p-value to the top-right corner if significance testing is enabled
  anova_present <- FALSE
  groups <- unique(df_sub$Group)
  if (add_signif && length(groups) >= 2) {
    anova_pval <- tryCatch({
      if (length(groups) == 2) {
        # Use Welch t-test for 2 groups
        stats::t.test(Value ~ Group, data = df_sub)$p.value
      } else {
        # Use ANOVA for 3+ groups
        aov_result <- stats::aov(Value ~ Group, data = df_sub)
        summary(aov_result)[[1]][["Pr(>F)"]][1]
      }
    }, error = function(e) NA_real_)

    if (!is.na(anova_pval)) {
      anova_present <- TRUE
      anova_text <- if (length(groups) == 2) {
        sprintf("Welch t-test p = %.6f", anova_pval)
      } else {
        sprintf("ANOVA p = %.6f", anova_pval)
      }
      p <- p + annotate("text", x = Inf, y = Inf, label = anova_text,
                        hjust = 1.1, vjust = 1.5, size = axis_tick_mm * 1, fontface = "plain")
    }
  }

  # Add significance stars (only p < 0.05)
  y_max <- y_base * 1.5  # Default extension
  if (add_signif && length(groups) >= 2) {
    # CUSTOMIZE: If you want a different significance threshold, change 0.05.
    comps <- utils::combn(groups, 2, simplify = FALSE)
    # Keep the first bracket closer to the bars
    step <- if (is.finite(y_base) && y_base > 0) y_base * 0.1 else 0.5
    ypos <- y_base + step
    annotations <- c()
    signif_comps <- list()
    y_positions <- c()

    for (cmp in comps) {
      subdf <- df_sub %>% dplyr::filter(Group %in% cmp)
      pval <- tryCatch(stats::t.test(Value ~ Group, data = subdf)$p.value, error = function(e) NA_real_)
      if (!is.na(pval) && pval < 0.05) {
        signif_comps[[length(signif_comps) + 1]] <- cmp
        if (pval < 0.001) ann <- "***"
        else if (pval < 0.01) ann <- "**"
        else ann <- "*"
        annotations <- c(annotations, ann)
        y_positions <- c(y_positions, ypos)
        ypos <- ypos + step
      }
    }

    # Add significance labels
    if (length(signif_comps) > 0) {
      p <- p + geom_signif(comparisons = signif_comps,
                           annotations = annotations,
                           y_position = y_positions,
                           tip_length = 0.02,
                           textsize = 6 * font_scale,
                           # Move stars down closer to the bracket line
                           vjust = 0.7)
      # Extend y-axis to show all significance brackets
      y_max <- max(y_positions) + step * 1.5

      # Leave extra room above the highest bracket for the test label
      if (isTRUE(anova_present)) {
        y_max <- y_max + step * 1.2
      }
    }
  }

  # Set y-axis limits after calculating significance positions
  auto_y_max <- if (is.finite(y_max) && y_max > 0) y_max else 1
  plot_ylim <- if (is.null(y_limits) || length(y_limits) != 2 || any(!is.finite(y_limits))) {
    c(0, auto_y_max)
  } else {
    as.numeric(y_limits)
  }
  plot_ylim <- sort(plot_ylim)
  p <- p + coord_cartesian(ylim = plot_ylim)

  return(p)
}

##### App Metadata & Version Checking #####
# CUSTOMIZE: Set GITHUB_REPO to your repo. The app version is read from
# DESCRIPTION (single source of truth), so it no longer needs manual updates.
# ------------------------
# 5. App version helpers
# ------------------------
HISTONEMOD_FALLBACK_VERSION <- "0.7.0"

# Internal helper that resolves the package version from DESCRIPTION.
# When the package is loaded with pkgload (development), system.file() is
# shimmed to the source tree, so this also reports the development version.
histonemod_package_version <- function() {
  desc_path <- tryCatch(
    system.file("DESCRIPTION", package = "HistoneMod"),
    error = function(e) ""
  )
  if (length(desc_path) == 1 && nzchar(desc_path) && file.exists(desc_path)) {
    version <- tryCatch(
      as.character(read.dcf(desc_path, fields = "Version")[1, 1]),
      error = function(e) NA_character_
    )
    if (length(version) == 1 && !is.na(version) && nzchar(version)) {
      return(version)
    }
  }

  desc <- tryCatch(utils::packageDescription("HistoneMod"), error = function(e) NULL)
  if (!is.null(desc) && !is.null(desc$Version) && nzchar(desc$Version)) {
    return(desc$Version)
  }

  HISTONEMOD_FALLBACK_VERSION
}

APP_VERSION <- histonemod_package_version()
GITHUB_REPO <- "jiehua1995/HistoneMod"

# Internal helper for normalizing version strings.
normalize_version <- function(x) {
  if(is.null(x) || length(x) == 0) return(NA_character_)
  x <- as.character(x[1])
  x <- trimws(x)
  x <- sub("^v", "", x, ignore.case = TRUE)
  if(identical(x, "")) return(NA_character_)
  x
}

# Internal helper for checking whether a release is newer than the running one.
version_is_newer <- function(latest, current) {
  latest <- normalize_version(latest)
  current <- normalize_version(current)
  if (is.na(latest) || is.na(current)) return(FALSE)
  isTRUE(utils::compareVersion(latest, current) > 0)
}

# Internal helper for querying the latest GitHub release version.
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

  if(requireNamespace("jsonlite", quietly = TRUE)) {
    parsed <- tryCatch(jsonlite::fromJSON(api_url), error = function(e) NULL)
    if(!is.null(parsed) && !is.null(parsed$tag_name)) {
      return(normalize_version(parsed$tag_name))
    }
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
# Internal legacy UI helper for download buttons.
downloadPlotUI <- function(id, label = "Download Plot") {
  ns <- NS(id)
  downloadButton(ns("download"), label)
}

# Download_server
# Internal legacy server helper for plot downloads.
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
