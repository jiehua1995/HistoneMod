##### HistoneMod: Composition Plots #####
# Stacked bar charts of peptide states (features) per protein, either per
# replicate or summarised as group means.

#' Plot peptide-state composition
#'
#' Draws stacked bars showing how the relative abundance of peptide states
#' (features) is distributed across samples or groups, optionally facetted by
#' protein. With `summarise = TRUE` the x-axis shows groups and each bar is the
#' mean composition of a group; otherwise each bar is one replicate, ordered by
#' group. The plot uses a viridis fill palette and the same clean theme as the
#' other HistoneMod plots.
#'
#' @param data A processed long-format data.frame as returned by
#'   [percentage_calculation()].
#' @param protein Optional character vector of protein names to keep. When
#'   `NULL`, all proteins are used.
#' @param summarise Logical; if `TRUE`, stacked bars show the mean value per
#'   group (`na.rm = TRUE`); otherwise each replicate gets its own stacked bar.
#' @param group_order Optional character vector giving the order of groups on
#'   the x-axis. Groups not listed are appended in order of appearance.
#' @param sample_col Name of the sample column (default `"Replicate.Name"`).
#' @param group_col Name of the group column (default `"Group"`).
#' @param feature_col Name of the feature column (default `"Peptide.Note"`).
#' @param protein_col Name of the protein column (default `"Protein.Name"`).
#' @param value_col Name of the numeric value column (default `"Percentage"`).
#' @param palette Viridis palette name. One of `"viridis"`, `"magma"`,
#'   `"plasma"`, `"inferno"`, `"cividis"`, `"rocket"`, `"mako"` or `"turbo"`.
#' @param legend_position Position of the fill legend (default `"right"`).
#'
#' @return A `ggplot` object.
#' @export
plot_composition <- function(data,
                             protein = NULL,
                             summarise = FALSE,
                             group_order = NULL,
                             sample_col = "Replicate.Name",
                             group_col = "Group",
                             feature_col = "Peptide.Note",
                             protein_col = "Protein.Name",
                             value_col = "Percentage",
                             palette = "viridis",
                             legend_position = "right") {
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
  summarise <- isTRUE(summarise)
  palette <- match.arg(
    palette,
    c("viridis", "magma", "plasma", "inferno", "cividis", "rocket", "mako", "turbo")
  )

  required <- c(sample_col, group_col, feature_col, protein_col, value_col)
  missing_cols <- setdiff(required, colnames(data))
  if (length(missing_cols) > 0L) {
    stop(
      "`data` is missing required column(s): ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  if (!is.numeric(data[[value_col]]) && !is.integer(data[[value_col]])) {
    stop("`", value_col, "` must be numeric.", call. = FALSE)
  }

  df <- data.frame(
    Sample = as.character(data[[sample_col]]),
    Group = as.character(data[[group_col]]),
    Feature = as.character(data[[feature_col]]),
    Protein = as.character(data[[protein_col]]),
    Value = as.numeric(data[[value_col]]),
    stringsAsFactors = FALSE
  )
  df <- df[!is.na(df$Value), , drop = FALSE]

  if (!is.null(protein)) {
    if (!is.character(protein)) {
      stop("`protein` must be a character vector of protein names.", call. = FALSE)
    }
    unknown <- setdiff(protein, unique(df$Protein))
    if (length(unknown) > 0L) {
      stop(
        "Unknown protein(s) in `protein`: ",
        paste(unknown, collapse = ", "),
        call. = FALSE
      )
    }
    df <- df[df$Protein %in% protein, , drop = FALSE]
  }

  if (nrow(df) == 0L) {
    return(
      ggplot2::ggplot() +
        ggplot2::geom_text(
          ggplot2::aes(x = 0, y = 0, label = "No data to plot"),
          size = 5
        ) +
        ggplot2::theme_void()
    )
  }

  groups <- unique(df$Group)
  if (is.null(group_order)) {
    group_levels <- groups
  } else {
    if (!is.character(group_order)) {
      stop("`group_order` must be a character vector of group names.", call. = FALSE)
    }
    group_levels <- c(intersect(group_order, groups), setdiff(groups, group_order))
  }
  group_levels <- group_levels[!is.na(group_levels)]

  feature_levels <- unique(df$Feature)
  df$Feature <- factor(df$Feature, levels = feature_levels)

  if (summarise) {
    df <- stats::aggregate(
      Value ~ Group + Protein + Feature,
      data = df,
      FUN = function(v) mean(v, na.rm = TRUE)
    )
    df$Group <- factor(df$Group, levels = group_levels)
    x_var <- "Group"
    x_label <- "Group"
  } else {
    sample_meta <- unique(df[, c("Sample", "Group")])
    sample_meta <- sample_meta[
      order(
        match(sample_meta$Group, group_levels),
        match(sample_meta$Sample, unique(df$Sample))
      ),
      ,
      drop = FALSE
    ]
    df$Sample <- factor(df$Sample, levels = sample_meta$Sample)
    x_var <- "Sample"
    x_label <- "Sample"
  }

  n_features <- length(feature_levels)
  y_label <- if (identical(value_col, "Percentage")) "Percentage" else value_col

  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = .data[[x_var]], y = Value, fill = Feature)
  ) +
    ggplot2::geom_col(width = 0.8) +
    ggplot2::labs(
      x = x_label,
      y = y_label,
      fill = feature_col
    ) +
    ggplot2::scale_fill_viridis_d(option = palette) +
    ggplot2::theme_minimal(base_family = "sans") +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.border = ggplot2::element_rect(
        color = "black", fill = NA, linewidth = 0.8, linejoin = "mitre"
      ),
      axis.title = ggplot2::element_text(size = 14),
      axis.text = ggplot2::element_text(size = 12),
      legend.title = ggplot2::element_text(size = 12),
      legend.text = ggplot2::element_text(size = 10),
      legend.position = legend_position
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(ncol = if (n_features > 20L) 2L else 1L)
    )

  if (nrow(df) > 0L && length(unique(df$Protein)) > 1L) {
    p <- p + ggplot2::facet_wrap(~Protein)
  }

  if (length(unique(df[[x_var]])) > 8L) {
    p <- p + ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
  }

  p
}
