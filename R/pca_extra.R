##### PCA diagnostics: scree and loadings #####

# Internal helper: prepare a PCA matrix from a processed HistoneMod table.
.hm_pca_prepare <- function(data, value_col = "Percentage",
                            transform = c("none", "clr", "logit", "log2"),
                            missing = c("zero", "drop", "min")) {
  transform <- match.arg(transform)
  missing <- match.arg(missing)

  data <- as.data.frame(data, stringsAsFactors = FALSE)
  colnames(data) <- gsub("\\s+", ".", colnames(data))
  if (!value_col %in% colnames(data)) {
    stop("Column '", value_col, "' not found in data.", call. = FALSE)
  }

  long <- data[, c("Replicate.Name", "Peptide.Note", value_col), drop = FALSE]
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
  numeric_mat <- as.matrix(wide[, setdiff(colnames(wide), "Replicate.Name"), drop = FALSE])
  storage.mode(numeric_mat) <- "numeric"
  rownames(numeric_mat) <- as.character(wide$Replicate.Name)

  if (identical(missing, "drop")) {
    numeric_mat <- numeric_mat[, colSums(is.na(numeric_mat)) == 0, drop = FALSE]
    numeric_mat <- numeric_mat[rowSums(is.na(numeric_mat)) == 0, , drop = FALSE]
  }
  if (nrow(numeric_mat) < 2 || ncol(numeric_mat) < 2) {
    stop("Not enough complete data for PCA.", call. = FALSE)
  }

  if (!identical(transform, "none")) {
    numeric_mat <- transform_composition(numeric_mat, method = transform)
  }

  sds <- apply(numeric_mat, 2, stats::sd, na.rm = TRUE)
  keep <- names(sds)[is.finite(sds) & sds > 0]
  if (length(keep) < 2) stop("Not enough variable peptides for PCA.", call. = FALSE)

  pca <- stats::prcomp(
    numeric_mat[, keep, drop = FALSE],
    center = TRUE,
    scale. = transform %in% c("none", "log2")
  )
  list(pca = pca, samples = rownames(numeric_mat))
}

#' Plot a PCA scree plot
#'
#' Shows the proportion of variance explained by each principal component.
#'
#' @param data A processed data frame.
#' @param value_col Numeric value column.
#' @param transform Transformation used before PCA.
#' @param missing Missing-value policy.
#' @param n_pcs Number of components to show.
#'
#' @return A `ggplot` object.
#' @export
plot_pca_scree <- function(data, value_col = "Percentage",
                           transform = c("none", "clr", "logit", "log2"),
                           missing = c("zero", "drop", "min"),
                           n_pcs = 10) {
  prepared <- .hm_pca_prepare(data, value_col, transform, missing)
  variance <- summary(prepared$pca)$importance[2, ]
  n_pcs <- suppressWarnings(as.integer(n_pcs))
  if (length(n_pcs) != 1L || is.na(n_pcs) || n_pcs < 1L) n_pcs <- 10
  n_pcs <- min(n_pcs, length(variance))

  df <- data.frame(
    component = factor(paste0("PC", seq_len(n_pcs)),
                       levels = paste0("PC", seq_len(n_pcs))),
    variance = as.numeric(variance[seq_len(n_pcs)]) * 100,
    stringsAsFactors = FALSE
  )

  ggplot2::ggplot(df, ggplot2::aes(x = component, y = variance)) +
    ggplot2::geom_col(fill = "#4C72B0", width = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f%%", variance)),
                       vjust = -0.4, size = 3) +
    ggplot2::labs(x = "Principal component", y = "Variance explained (%)") +
    ggplot2::theme_minimal(base_family = "sans") +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 0.6),
      axis.title = ggplot2::element_text(size = 13),
      axis.text = ggplot2::element_text(size = 11)
    )
}

#' Plot PCA loadings
#'
#' Shows the peptides with the largest contributions to one principal
#' component, which helps interpret what drives the separation of samples.
#'
#' @param data A processed data frame.
#' @param value_col Numeric value column.
#' @param transform Transformation used before PCA.
#' @param missing Missing-value policy.
#' @param pc Principal component to show (1 = PC1).
#' @param top Number of peptides with the largest absolute loading.
#'
#' @return A `ggplot` object.
#' @export
plot_pca_loadings <- function(data, value_col = "Percentage",
                              transform = c("none", "clr", "logit", "log2"),
                              missing = c("zero", "drop", "min"),
                              pc = 1, top = 15) {
  prepared <- .hm_pca_prepare(data, value_col, transform, missing)
  pc <- suppressWarnings(as.integer(pc))
  if (length(pc) != 1L || is.na(pc) || pc < 1L) pc <- 1
  pc <- min(pc, ncol(prepared$pca$rotation))
  top <- suppressWarnings(as.integer(top))
  if (length(top) != 1L || is.na(top) || top < 1L) top <- 15

  loadings <- prepared$pca$rotation[, pc]
  df <- data.frame(
    feature = names(loadings),
    loading = as.numeric(loadings),
    stringsAsFactors = FALSE
  )
  df <- df[order(-abs(df$loading)), , drop = FALSE]
  df <- utils::head(df, top)
  df$feature <- factor(df$feature, levels = rev(df$feature))
  df$direction <- ifelse(df$loading >= 0, "positive", "negative")

  ggplot2::ggplot(df, ggplot2::aes(x = feature, y = loading, fill = direction)) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = c(positive = "#D55E00", negative = "#0072B2"),
                               guide = "none") +
    ggplot2::labs(x = NULL, y = paste0("Loading on PC", pc)) +
    ggplot2::theme_minimal(base_family = "sans") +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 0.6),
      axis.title = ggplot2::element_text(size = 13),
      axis.text = ggplot2::element_text(size = 10)
    )
}
