##### HistoneMod: Quality Control #####
# Sample- and feature-level QC summaries for processed peptide-percentage
# tables, plus diagnostic plots for missingness and within-group CV.

# Internal: resolve a qc_summary() result or a compatible data.frame.
qc_sample_table <- function(x) {
  if (is.data.frame(x)) {
    if (all(c("sample", "missing_rate") %in% colnames(x))) {
      return(x)
    }
    return(qc_summary(x)$sample_qc)
  }
  if (is.list(x) && !is.null(x$sample_qc)) {
    return(x$sample_qc)
  }
  stop(
    "`x` must be a qc_summary() result, a sample QC table or a long data.frame.",
    call. = FALSE
  )
}

# Internal: resolve feature-level QC information.
qc_feature_table <- function(x) {
  if (is.data.frame(x)) {
    if (all(c("feature", "mean_cv_within_group") %in% colnames(x))) {
      return(x)
    }
    return(qc_summary(x)$feature_qc)
  }
  if (is.list(x) && !is.null(x$feature_qc)) {
    return(x$feature_qc)
  }
  stop(
    "`x` must be a qc_summary() result, a feature QC table or a long data.frame.",
    call. = FALSE
  )
}

#' Quality control summary
#'
#' Computes sample-level and feature-level quality control metrics from a
#' processed HistoneMod table. "Detected" means a non-missing, positive value;
#' sample/feature combinations that are absent from the long table are treated
#' as not detected (zero).
#'
#' @param data A processed long-format data.frame as returned by
#'   [percentage_calculation()].
#' @param value_col Name of the numeric value column (default `"Percentage"`).
#' @param sample_col Name of the sample column (default `"Replicate.Name"`).
#' @param feature_col Name of the feature column (default `"Peptide.Note"`).
#' @param group_col Name of the group column (default `"Group"`). When absent,
#'   group information is `NA`.
#' @param protein_col Name of the protein column (default `"Protein.Name"`).
#'   When absent, the `protein` column is `NA`.
#' @param rt_col Name of the retention-time column. When `NULL` (the default)
#'   `"Peptide.Retention.Time"` is used if present; otherwise RT metrics are
#'   `NA`.
#'
#' @return A list with elements:
#'   \describe{
#'     \item{sample_qc}{data.frame with `sample`, `group`, `n_features`,
#'       `total_value`, `missing_rate` and `n_groups` (number of groups in the
#'       experiment).}
#'     \item{feature_qc}{data.frame with `feature`, `protein`,
#'       `detection_rate`, `mean_cv_within_group` (mean of per-group
#'       `sd / mean`), `rt_sd` and `n_rt_outliers` (retention times more than
#'       3 MAD away from the feature median).}
#'     \item{rt_col}{the retention-time column used, or `NA`.}
#'   }
#' @export
qc_summary <- function(data,
                       value_col = "Percentage",
                       sample_col = "Replicate.Name",
                       feature_col = "Peptide.Note",
                       group_col = "Group",
                       protein_col = "Protein.Name",
                       rt_col = NULL) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }

  required <- c(sample_col, feature_col, value_col)
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

  has_group <- !is.null(group_col) && is.character(group_col) &&
    length(group_col) == 1L && group_col %in% colnames(data)
  has_protein <- !is.null(protein_col) && is.character(protein_col) &&
    length(protein_col) == 1L && protein_col %in% colnames(data)

  if (is.null(rt_col)) {
    rt_col <- if ("Peptide.Retention.Time" %in% colnames(data)) {
      "Peptide.Retention.Time"
    } else {
      NA_character_
    }
  }
  has_rt <- !is.na(rt_col) && is.character(rt_col) && length(rt_col) == 1L &&
    rt_col %in% colnames(data)

  mat <- composition_matrix(
    data,
    sample_col = sample_col,
    feature_col = feature_col,
    value_col = value_col,
    fill = 0
  )

  samples <- rownames(mat)
  features <- colnames(mat)
  n_samples <- length(samples)
  n_features <- length(features)

  if (n_samples == 0L || n_features == 0L) {
    return(list(
      sample_qc = data.frame(
        sample = character(0),
        group = character(0),
        n_features = integer(0),
        total_value = numeric(0),
        missing_rate = numeric(0),
        n_groups = integer(0),
        stringsAsFactors = FALSE
      ),
      feature_qc = data.frame(
        feature = character(0),
        protein = character(0),
        detection_rate = numeric(0),
        mean_cv_within_group = numeric(0),
        rt_sd = numeric(0),
        n_rt_outliers = integer(0),
        stringsAsFactors = FALSE
      ),
      rt_col = if (has_rt) rt_col else NA_character_
    ))
  }

  detected <- !is.na(mat) & mat > 0

  meta <- unique(data.frame(
    sample = as.character(data[[sample_col]]),
    group = if (has_group) as.character(data[[group_col]]) else NA_character_,
    stringsAsFactors = FALSE
  ))
  meta <- meta[!duplicated(meta$sample), , drop = FALSE]
  group_of_sample <- meta$group[match(samples, meta$sample)]

  all_groups <- unique(group_of_sample[!is.na(group_of_sample)])
  n_groups <- length(all_groups)

  sample_qc <- data.frame(
    sample = samples,
    group = group_of_sample,
    n_features = as.integer(rowSums(detected)),
    total_value = as.numeric(rowSums(mat, na.rm = TRUE)),
    missing_rate = if (n_features > 0L) {
      (n_features - as.numeric(rowSums(detected))) / n_features
    } else {
      rep(NA_real_, n_samples)
    },
    n_groups = as.integer(n_groups),
    stringsAsFactors = FALSE
  )

  # Feature-level: detection rate and within-group CV.
  detection_rate <- as.numeric(colMeans(detected))

  mean_cv <- rep(NA_real_, n_features)
  if (n_groups > 0L) {
    for (i in seq_len(n_features)) {
      cvs <- numeric(0)
      for (g in all_groups) {
        rows <- which(group_of_sample == g)
        values <- mat[rows, i]
        values <- values[!is.na(values)]
        if (length(values) >= 2L) {
          m <- mean(values)
          if (is.finite(m) && m != 0) {
            cvs <- c(cvs, stats::sd(values) / m)
          }
        }
      }
      cvs <- cvs[is.finite(cvs)]
      if (length(cvs) > 0L) {
        mean_cv[i] <- mean(cvs)
      }
    }
  }

  # Feature-level: retention time variability and robust outliers.
  rt_sd <- rep(NA_real_, n_features)
  n_rt_outliers <- rep(NA_integer_, n_features)
  if (has_rt) {
    rt_values <- suppressWarnings(as.numeric(data[[rt_col]]))
    feature_key <- as.character(data[[feature_col]])
    rt_split <- split(rt_values, factor(feature_key, levels = features))
    rt_sd <- vapply(rt_split, function(v) {
      v <- v[!is.na(v)]
      if (length(v) < 2L) NA_real_ else stats::sd(v)
    }, numeric(1))
    n_rt_outliers <- vapply(rt_split, function(v) {
      v <- v[!is.na(v)]
      if (length(v) < 2L) {
        return(NA_integer_)
      }
      center <- stats::median(v)
      spread <- stats::mad(v)
      if (!is.finite(spread) || spread == 0) {
        return(0L)
      }
      as.integer(sum(abs(v - center) > 3 * spread))
    }, integer(1))
  }

  feature_meta <- unique(data.frame(
    feature = as.character(data[[feature_col]]),
    protein = if (has_protein) as.character(data[[protein_col]]) else NA_character_,
    stringsAsFactors = FALSE
  ))
  feature_meta <- feature_meta[!duplicated(feature_meta$feature), , drop = FALSE]
  protein <- feature_meta$protein[match(features, feature_meta$feature)]

  feature_qc <- data.frame(
    feature = features,
    protein = protein,
    detection_rate = detection_rate,
    mean_cv_within_group = mean_cv,
    rt_sd = as.numeric(rt_sd),
    n_rt_outliers = as.integer(n_rt_outliers),
    stringsAsFactors = FALSE
  )

  list(
    sample_qc = sample_qc,
    feature_qc = feature_qc,
    rt_col = if (has_rt) rt_col else NA_character_
  )
}

#' Plot sample missingness
#'
#' Bar chart of the per-sample missing rate from [qc_summary()], optionally
#' coloured by group.
#'
#' @param x A [qc_summary()] result, a sample-level QC data.frame or a long
#'   processed data.frame.
#' @param palette Viridis palette used when groups are available.
#' @param fill_colour Colour used when no group information is available.
#'
#' @return A `ggplot` object.
#' @export
plot_qc_missingness <- function(x, palette = "viridis", fill_colour = "#4C72B0") {
  palette <- match.arg(
    palette,
    c("viridis", "magma", "plasma", "inferno", "cividis", "rocket", "mako", "turbo")
  )
  df <- qc_sample_table(x)

  required <- c("sample", "missing_rate")
  missing_cols <- setdiff(required, colnames(df))
  if (length(missing_cols) > 0L) {
    stop(
      "Sample QC table is missing column(s): ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  df <- df[!is.na(df$sample), , drop = FALSE]
  if (nrow(df) == 0L) {
    return(
      ggplot2::ggplot() +
        ggplot2::geom_text(
          ggplot2::aes(x = 0, y = 0, label = "No QC data to plot"),
          size = 5
        ) +
        ggplot2::theme_void()
    )
  }

  df$sample <- factor(
    as.character(df$sample),
    levels = as.character(df$sample[order(df$missing_rate, na.last = TRUE)])
  )

  has_group <- "group" %in% colnames(df) && any(!is.na(df$group))

  if (has_group) {
    df$group <- factor(as.character(df$group))
    p <- ggplot2::ggplot(
      df,
      ggplot2::aes(x = sample, y = missing_rate, fill = group)
    ) +
      ggplot2::geom_col(width = 0.8) +
      ggplot2::scale_fill_viridis_d(option = palette) +
      ggplot2::labs(x = "Sample", y = "Missing rate", fill = "Group")
  } else {
    p <- ggplot2::ggplot(
      df,
      ggplot2::aes(x = sample, y = missing_rate)
    ) +
      ggplot2::geom_col(width = 0.8, fill = fill_colour) +
      ggplot2::labs(x = "Sample", y = "Missing rate")
  }

  p +
    ggplot2::scale_y_continuous(
      labels = function(v) paste0(round(v * 100), "%"),
      limits = c(0, 1),
      expand = ggplot2::expansion(mult = c(0, 0.02))
    ) +
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
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.title = ggplot2::element_text(size = 12),
      legend.text = ggplot2::element_text(size = 10)
    )
}

#' Plot feature coefficients of variation
#'
#' Bar chart of the mean within-group coefficient of variation for each feature,
#' as computed by [qc_summary()].
#'
#' @param x A [qc_summary()] result, a feature-level QC data.frame or a long
#'   processed data.frame.
#' @param max_features Maximum number of features to show (highest CV first).
#' @param cv_threshold Optional numeric; when supplied, a dashed reference line
#'   is drawn at this CV value.
#' @param palette Viridis palette used for the bars.
#'
#' @return A `ggplot` object.
#' @export
plot_qc_cv <- function(x,
                       max_features = 30,
                       cv_threshold = NULL,
                       palette = "viridis") {
  palette <- match.arg(
    palette,
    c("viridis", "magma", "plasma", "inferno", "cividis", "rocket", "mako", "turbo")
  )
  df <- qc_feature_table(x)

  max_features <- suppressWarnings(as.integer(max_features))
  if (length(max_features) != 1L || is.na(max_features) || max_features < 1L) {
    stop("`max_features` must be a positive integer.", call. = FALSE)
  }

  if (!"mean_cv_within_group" %in% colnames(df) || !"feature" %in% colnames(df)) {
    stop(
      "Feature QC table must contain `feature` and `mean_cv_within_group` columns.",
      call. = FALSE
    )
  }

  df <- df[
    !is.na(df$feature) & is.finite(df$mean_cv_within_group),
    c("feature", "mean_cv_within_group"),
    drop = FALSE
  ]
  if (nrow(df) == 0L) {
    return(
      ggplot2::ggplot() +
        ggplot2::geom_text(
          ggplot2::aes(x = 0, y = 0, label = "No CV values to plot"),
          size = 5
        ) +
        ggplot2::theme_void()
    )
  }

  df <- df[order(df$mean_cv_within_group, decreasing = TRUE), , drop = FALSE]
  df <- utils::head(df, max_features)
  df$feature <- factor(
    as.character(df$feature),
    levels = rev(as.character(df$feature))
  )

  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = feature, y = mean_cv_within_group, fill = mean_cv_within_group)
  ) +
    ggplot2::geom_col(width = 0.8, show.legend = FALSE) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_viridis_c(option = palette) +
    ggplot2::labs(x = "Feature", y = "Mean within-group CV")

  if (!is.null(cv_threshold)) {
    cv_threshold <- suppressWarnings(as.numeric(cv_threshold))
    if (length(cv_threshold) == 1L && is.finite(cv_threshold)) {
      p <- p + ggplot2::geom_hline(
        yintercept = cv_threshold,
        linetype = "dashed",
        colour = "grey40",
        linewidth = 0.4
      )
    }
  }

  p +
    ggplot2::theme_minimal(base_family = "sans") +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.border = ggplot2::element_rect(
        color = "black", fill = NA, linewidth = 0.8, linejoin = "mitre"
      ),
      axis.title = ggplot2::element_text(size = 14),
      axis.text = ggplot2::element_text(size = 11)
    )
}
