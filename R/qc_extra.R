##### Additional QC visualisations #####

#' Plot sample-to-sample correlation
#'
#' Computes pairwise correlations between samples on transformed peptide
#' profiles and renders them as an annotated heatmap. Useful for spotting
#' outliers and batch effects.
#'
#' @param data A processed data frame.
#' @param value_col Numeric value column.
#' @param transform Transformation applied before correlation.
#' @param method Correlation method (`"pearson"` or `"spearman"`).
#' @param group_col Sample group column used for annotation.
#' @param cluster Logical; cluster samples.
#'
#' @return A `pheatmap` result object.
#' @export
plot_sample_correlation <- function(data, value_col = "Percentage",
                                    transform = c("clr", "none", "logit", "log2"),
                                    method = c("pearson", "spearman"),
                                    group_col = "Group", cluster = TRUE) {
  transform <- match.arg(transform)
  method <- match.arg(method)
  if (!is.data.frame(data)) stop("`data` must be a data.frame.", call. = FALSE)
  if (!value_col %in% names(data)) {
    stop("Column '", value_col, "' not found in data.", call. = FALSE)
  }

  mat <- composition_matrix(data, value_col = value_col, fill = 0)
  transformed <- transform_composition(mat, method = transform)
  correlation <- stats::cor(t(transformed), method = method, use = "pairwise.complete.obs")

  annotation_col <- NULL
  if (group_col %in% names(data)) {
    annotation <- as.data.frame(
      unique(data[, c("Replicate.Name", group_col), drop = FALSE]),
      stringsAsFactors = FALSE
    )
    rownames(annotation) <- annotation$Replicate.Name
    annotation$Replicate.Name <- NULL
    annotation_col <- annotation[colnames(correlation), , drop = FALSE]
  }

  pheatmap::pheatmap(
    correlation,
    cluster_rows = isTRUE(cluster),
    cluster_cols = isTRUE(cluster),
    color = viridis::viridis(100),
    annotation_col = annotation_col,
    border_color = "grey80",
    angle_col = 45,
    silent = TRUE
  )
}

#' Plot a missingness tile map
#'
#' Shows which sample-by-peptide values are missing (or zero) as a tile grid.
#'
#' @param data A processed data frame.
#' @param value_col Numeric value column.
#' @param count_zero Logical; treat zeros as missing as well.
#' @param max_features Maximum number of features to show (most missing first).
#'
#' @return A `ggplot` object.
#' @export
plot_missingness_tile <- function(data, value_col = "Percentage",
                                  count_zero = FALSE, max_features = 60) {
  if (!is.data.frame(data)) stop("`data` must be a data.frame.", call. = FALSE)
  if (!value_col %in% names(data)) {
    stop("Column '", value_col, "' not found in data.", call. = FALSE)
  }

  df <- data[, c("Replicate.Name", "Peptide.Note", value_col), drop = FALSE]
  if (isTRUE(count_zero)) {
    df$missing <- is.na(df[[value_col]]) | (!is.na(df[[value_col]]) & df[[value_col]] == 0)
  } else {
    df$missing <- is.na(df[[value_col]])
  }

  feature_missing <- stats::aggregate(
    df$missing,
    by = list(Peptide.Note = df$Peptide.Note),
    FUN = mean
  )
  names(feature_missing)[2] <- "missing_rate"
  feature_missing <- feature_missing[order(-feature_missing$missing_rate), , drop = FALSE]
  keep <- utils::head(feature_missing$Peptide.Note, max_features)
  df <- df[df$Peptide.Note %in% keep, , drop = FALSE]

  df$missing <- factor(ifelse(df$missing, "Missing", "Detected"),
                       levels = c("Detected", "Missing"))

  ggplot2::ggplot(df, ggplot2::aes(x = Replicate.Name, y = Peptide.Note, fill = missing)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.2) +
    ggplot2::scale_fill_manual(values = c(Detected = "#4C72B0", Missing = "#D55E00")) +
    ggplot2::labs(x = NULL, y = NULL, fill = NULL) +
    ggplot2::theme_minimal(base_family = "sans") +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 9),
      axis.text.y = ggplot2::element_text(size = 8)
    )
}

#' Plot retention-time consistency
#'
#' Shows the deviation of each peptide's retention time from its median across
#' samples, which helps spot peak-picking problems in Skyline exports.
#'
#' @param data A processed data frame.
#' @param rt_col Retention-time column.
#' @param max_features Maximum number of peptides (highest RT variability first).
#' @param ncol Number of facet columns.
#'
#' @return A `ggplot` object.
#' @export
plot_rt_consistency <- function(data, rt_col = "Peptide.Retention.Time",
                                max_features = 12, ncol = 3) {
  if (!is.data.frame(data)) stop("`data` must be a data.frame.", call. = FALSE)
  if (!rt_col %in% names(data)) {
    return(ggplot2::ggplot() +
      ggplot2::geom_text(ggplot2::aes(x = 0, y = 0,
                                      label = "Retention time column not available"),
                         size = 5) +
      ggplot2::theme_void())
  }

  df <- data[, c("Replicate.Name", "Peptide.Note", rt_col), drop = FALSE]
  colnames(df)[3] <- "RT"
  df$RT <- suppressWarnings(as.numeric(df$RT))
  df <- df[!is.na(df$RT), , drop = FALSE]
  if (nrow(df) == 0) {
    return(ggplot2::ggplot() +
      ggplot2::geom_text(ggplot2::aes(x = 0, y = 0, label = "No retention time data"),
                         size = 5) +
      ggplot2::theme_void())
  }

  variability <- stats::aggregate(
    df$RT,
    by = list(Peptide.Note = df$Peptide.Note),
    FUN = function(v) stats::sd(v, na.rm = TRUE)
  )
  names(variability)[2] <- "rt_sd"
  variability <- variability[order(-variability$rt_sd), , drop = FALSE]
  keep <- utils::head(variability$Peptide.Note, max_features)
  df <- df[df$Peptide.Note %in% keep, , drop = FALSE]

  df$deviation <- stats::ave(
    df$RT,
    df$Peptide.Note,
    FUN = function(v) v - stats::median(v, na.rm = TRUE)
  )

  ggplot2::ggplot(df, ggplot2::aes(x = Replicate.Name, y = deviation,
                                   group = Peptide.Note)) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2, colour = "grey70") +
    ggplot2::geom_line(colour = "#4C72B0", linewidth = 0.6) +
    ggplot2::geom_point(colour = "#4C72B0", size = 1.8) +
    ggplot2::facet_wrap(~Peptide.Note, scales = "free_y", ncol = ncol) +
    ggplot2::labs(x = NULL, y = "RT deviation from median (s)") +
    ggplot2::theme_minimal(base_family = "sans") +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 0.6),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 8),
      strip.text = ggplot2::element_text(size = 8)
    )
}
