##### HistoneMod: Effect-size heatmap across comparisons #####
# Feature-by-comparison tile heatmap of the effect sizes returned by
# differential_analysis(), with optional significance stars and row clustering.

# Internal: normalise a comparison label for case/whitespace-insensitive
# matching, e.g. "  sampleA  VS  sampleB " -> "samplea vs sampleb".
.hm_effect_normalise_label <- function(x) {
  x <- gsub("\\s+", " ", trimws(as.character(x)))
  x <- gsub(" ?vs ?", " vs ", x, ignore.case = TRUE)
  tolower(x)
}

# Internal: turn the `comparisons` argument into labels in requested order.
# Returns NULL when no filtering/ordering was requested.
.hm_effect_requested_comparisons <- function(comparisons) {
  if (is.null(comparisons)) {
    return(NULL)
  }

  if (is.list(comparisons) && !is.data.frame(comparisons)) {
    return(vapply(
      comparisons,
      function(cmp) {
        if (!is.character(cmp) || length(cmp) != 2L) {
          stop(
            "Each `comparisons` element must be a character vector of length 2.",
            call. = FALSE
          )
        }
        cmp <- trimws(cmp)
        if (anyNA(cmp) || any(!nzchar(cmp))) {
          stop(
            "Comparison groups must be non-missing, non-empty names.",
            call. = FALSE
          )
        }
        paste(cmp, collapse = " vs ")
      },
      character(1)
    ))
  }

  if (is.character(comparisons)) {
    labels <- trimws(as.character(comparisons))
    return(labels[nzchar(labels)])
  }

  stop(
    "`comparisons` must be NULL, a character vector of labels, or a list of ",
    "length-2 character vectors such as `list(c(\"A\", \"B\"))`.",
    call. = FALSE
  )
}

# Internal: keep only the comparison columns that match `requested`, in the
# requested order. Case and whitespace differences are ignored.
.hm_effect_resolve_comparisons <- function(available, requested) {
  if (is.null(requested)) {
    return(available)
  }
  idx <- match(
    .hm_effect_normalise_label(requested),
    .hm_effect_normalise_label(available)
  )
  idx <- idx[!is.na(idx)]
  idx <- idx[!duplicated(idx)]
  available[idx]
}

# Internal: significance stars for adjusted p-values. Stars are only shown for
# p < alpha; the number of stars uses the conventional 0.001/0.01/0.05 cutoffs.
.hm_effect_stars <- function(p, alpha = 0.05) {
  stars <- rep("", length(p))
  significant <- !is.na(p) & p < alpha
  stars[significant & p < 0.001] <- "***"
  stars[significant & p >= 0.001 & p < 0.01] <- "**"
  stars[significant & p >= 0.01] <- "*"
  stars
}

# Internal: coerce a column to numeric, going through character for factors so
# that factor levels such as "0.5" are not interpreted as integer codes.
.hm_effect_numeric <- function(x) {
  if (is.factor(x)) {
    x <- as.character(x)
  }
  suppressWarnings(as.numeric(x))
}

# Internal: symmetric colour limits around zero.
.hm_effect_value_limits <- function(value_limits, effect) {
  if (is.null(value_limits)) {
    max_abs <- suppressWarnings(max(abs(effect), na.rm = TRUE))
    if (!is.finite(max_abs) || max_abs <= 0) {
      max_abs <- 1
    }
    return(c(-max_abs, max_abs))
  }

  values <- suppressWarnings(as.numeric(value_limits))
  values <- values[is.finite(values)]
  if (length(values) == 0L) {
    stop(
      "`value_limits` must be a finite numeric vector (for example a single ",
      "positive limit such as `2`).",
      call. = FALSE
    )
  }

  limit <- max(abs(values))
  if (!is.finite(limit) || limit <= 0) {
    limit <- 1
  }
  c(-limit, limit)
}

# Internal: placeholder plot for empty or all-missing input.
.hm_effect_heatmap_placeholder <- function(text = "No differential effects to plot") {
  ggplot2::ggplot() +
    ggplot2::geom_text(ggplot2::aes(x = 0, y = 0, label = text), size = 5) +
    ggplot2::theme_void()
}

#' Heatmap of effect sizes across pairwise comparisons
#'
#' Renders a feature-by-comparison tile heatmap of the effect sizes returned by
#' [differential_analysis()]. Row names are features (optionally transformed by
#' `label_fun`) and column names are comparison labels built from `group1` and
#' `group2`, for example `"SampleA vs SampleB"`. Fill colour is the effect size
#' on a diverging scale centred at zero, and optional significance stars are
#' derived from the adjusted p-values.
#'
#' @param differential A data.frame returned by [differential_analysis()] with
#'   at least the columns named by `effect_col`, `p_col` (when
#'   `show_stars = TRUE`), `feature`, `group1` and `group2`. A `protein` column
#'   is used for the optional facets. Rows with a missing or non-finite effect
#'   are dropped. An empty or all-missing table returns a placeholder plot.
#' @param effect_col Name of the numeric effect-size column.
#' @param p_col Name of the adjusted p-value column used for the stars.
#' @param alpha Significance threshold. Stars are only drawn when
#'   `p_col < alpha`.
#' @param comparisons Optional selection of comparisons as well as their column
#'   order. Either a character vector of comparison labels (for example
#'   `c("SampleA vs SampleB", "SampleA vs SampleC")`) or a list of length-2
#'   character vectors (for example `list(c("SampleA", "SampleB"))`). Labels are
#'   matched case- and whitespace-insensitively. `NULL` keeps all comparisons in
#'   the order in which they appear in `differential`.
#' @param protein Optional character vector of protein names used to subset the
#'   heatmap (matched against the `protein` column).
#' @param show_stars Logical; if `TRUE`, add significance stars to the tiles.
#' @param star_size Size of the significance-star text.
#' @param cluster_rows Logical; if `TRUE`, order features by hierarchical
#'   clustering (`stats::hclust()`) of the effect matrix. Falls back to the
#'   feature order for a single row or when clustering fails. The same order is
#'   used in every facet.
#' @param facet Logical; if `TRUE` and the data contain more than one protein,
#'   facet by protein with `scales = "free_y"`.
#' @param palette Character vector of three colours used for low, zero and high
#'   effects.
#' @param value_limits Optional numeric limit(s) for the fill scale. The largest
#'   absolute value defines a symmetric range around zero. When `NULL`, the
#'   range is derived from the largest absolute effect and falls back to `1`
#'   when the effects are all zero or missing.
#' @param label_fun Optional function applied to the unique feature labels before
#'   plotting, for example `function(x) sub("_", " ", x)`. It must return one
#'   label per feature; duplicated results are made unique.
#'
#' @return A [ggplot2::ggplot()] object. When there is nothing to plot, a
#'   placeholder `ggplot` with a short message is returned instead.
#'
#' @seealso [differential_analysis()], [plot_volcano()]
#' @export
plot_effect_heatmap <- function(differential,
                                effect_col = "effect",
                                p_col = "p_adj",
                                alpha = 0.05,
                                comparisons = NULL,
                                protein = NULL,
                                show_stars = TRUE,
                                star_size = 3,
                                cluster_rows = TRUE,
                                facet = TRUE,
                                palette = c("#0072B2", "white", "#D55E00"),
                                value_limits = NULL,
                                label_fun = NULL) {
  if (!is.data.frame(differential)) {
    stop(
      "`differential` must be a data.frame returned by `differential_analysis()`.",
      call. = FALSE
    )
  }

  show_stars <- isTRUE(show_stars)
  cluster_rows <- isTRUE(cluster_rows)
  facet <- isTRUE(facet)

  if (!is.character(effect_col) || length(effect_col) != 1L ||
      is.na(effect_col) || !nzchar(effect_col)) {
    stop("`effect_col` must be a single column name.", call. = FALSE)
  }
  if (!is.character(p_col) || length(p_col) != 1L ||
      is.na(p_col) || !nzchar(p_col)) {
    stop("`p_col` must be a single column name.", call. = FALSE)
  }

  alpha <- suppressWarnings(as.numeric(alpha))
  if (length(alpha) != 1L || is.na(alpha) || !is.finite(alpha) ||
      alpha <= 0 || alpha >= 1) {
    stop("`alpha` must be a single number between 0 and 1.", call. = FALSE)
  }

  star_size <- suppressWarnings(as.numeric(star_size))
  if (length(star_size) != 1L || is.na(star_size) || !is.finite(star_size) ||
      star_size <= 0) {
    stop("`star_size` must be a single positive number.", call. = FALSE)
  }

  if (!is.character(palette) || length(palette) < 3L || anyNA(palette)) {
    stop(
      "`palette` must be a character vector with at least three colours ",
      "(low, mid, high).",
      call. = FALSE
    )
  }

  if (!is.null(protein) &&
      (!is.character(protein) || length(protein) == 0L || anyNA(protein))) {
    stop(
      "`protein` must be NULL or a character vector of protein names.",
      call. = FALSE
    )
  }

  if (!is.null(label_fun) && !is.function(label_fun)) {
    stop(
      "`label_fun` must be NULL or a function applied to feature labels.",
      call. = FALSE
    )
  }

  requested <- .hm_effect_requested_comparisons(comparisons)

  dat <- as.data.frame(differential, stringsAsFactors = FALSE)
  if (nrow(dat) == 0L) {
    return(.hm_effect_heatmap_placeholder())
  }

  required <- c(effect_col, "feature", "group1", "group2")
  if (show_stars) {
    required <- c(required, p_col)
  }
  missing_cols <- setdiff(unique(required), names(dat))
  if (length(missing_cols) > 0L) {
    stop(
      "`differential` is missing required column(s): ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  has_protein <- "protein" %in% names(dat)
  if (!is.null(protein) && !has_protein) {
    stop(
      "`protein` was supplied but `differential` has no `protein` column.",
      call. = FALSE
    )
  }
  if (has_protein) {
    dat$protein <- as.character(dat$protein)
    dat$protein[is.na(dat$protein)] <- "Unassigned"
  } else {
    dat$protein <- "All"
  }

  dat$feature <- as.character(dat$feature)
  dat$group1 <- trimws(as.character(dat$group1))
  dat$group2 <- trimws(as.character(dat$group2))
  dat[[effect_col]] <- .hm_effect_numeric(dat[[effect_col]])
  if (show_stars) {
    dat[[p_col]] <- .hm_effect_numeric(dat[[p_col]])
  }

  keep <- !is.na(dat$feature) & nzchar(dat$feature) &
    !is.na(dat$group1) & nzchar(dat$group1) &
    !is.na(dat$group2) & nzchar(dat$group2) &
    is.finite(dat[[effect_col]])
  dat <- dat[keep, , drop = FALSE]

  if (!is.null(protein)) {
    dat <- dat[dat$protein %in% protein, , drop = FALSE]
  }
  if (nrow(dat) == 0L) {
    return(.hm_effect_heatmap_placeholder())
  }

  dat$.comparison <- paste(dat$group1, "vs", dat$group2)
  available_labels <- unique(dat$.comparison)
  comparison_labels <- .hm_effect_resolve_comparisons(available_labels, requested)
  if (length(comparison_labels) == 0L) {
    return(.hm_effect_heatmap_placeholder())
  }
  dat <- dat[dat$.comparison %in% comparison_labels, , drop = FALSE]

  feature_keys <- unique(dat$feature)
  n_features <- length(feature_keys)
  n_comparisons <- length(comparison_labels)

  protein_per_feature <- dat$protein[match(feature_keys, dat$feature)]

  feature_labels <- feature_keys
  if (!is.null(label_fun)) {
    labelled <- tryCatch(
      label_fun(feature_keys),
      error = function(e) {
        stop("`label_fun` failed: ", conditionMessage(e), call. = FALSE)
      }
    )
    labelled <- as.character(labelled)
    if (length(labelled) != n_features) {
      stop(
        "`label_fun` must return exactly one label per feature (",
        n_features, " labels expected).",
        call. = FALSE
      )
    }
    labelled[is.na(labelled)] <- feature_keys[is.na(labelled)]
    feature_labels <- make.unique(labelled)
  }

  # Wide feature x comparison matrices for the effect and the star labels.
  effect_matrix <- matrix(
    NA_real_,
    nrow = n_features,
    ncol = n_comparisons,
    dimnames = list(feature_keys, comparison_labels)
  )
  star_matrix <- matrix(
    "",
    nrow = n_features,
    ncol = n_comparisons,
    dimnames = list(feature_keys, comparison_labels)
  )

  row_index <- match(dat$feature, feature_keys)
  col_index <- match(dat$.comparison, comparison_labels)
  effect_matrix[cbind(row_index, col_index)] <- dat[[effect_col]]
  if (show_stars) {
    star_matrix[cbind(row_index, col_index)] <-
      .hm_effect_stars(dat[[p_col]], alpha)
  }

  row_order <- seq_len(n_features)
  if (cluster_rows && n_features >= 2L) {
    cluster_matrix <- effect_matrix
    cluster_matrix[!is.finite(cluster_matrix)] <- 0
    fit <- tryCatch(
      stats::hclust(stats::dist(cluster_matrix)),
      error = function(e) NULL
    )
    if (!is.null(fit)) {
      row_order <- fit$order
    }
  }

  effect_matrix <- effect_matrix[row_order, , drop = FALSE]
  star_matrix <- star_matrix[row_order, , drop = FALSE]
  feature_labels <- feature_labels[row_order]
  protein_per_feature <- protein_per_feature[row_order]

  limits <- .hm_effect_value_limits(value_limits, effect_matrix)

  long <- data.frame(
    feature = factor(
      rep(feature_labels, times = n_comparisons),
      levels = feature_labels
    ),
    comparison = factor(
      rep(comparison_labels, each = n_features),
      levels = comparison_labels
    ),
    effect = as.vector(effect_matrix),
    stars = as.vector(star_matrix),
    protein = rep(protein_per_feature, times = n_comparisons),
    stringsAsFactors = FALSE
  )

  p <- ggplot2::ggplot(
    long,
    ggplot2::aes(x = .data$comparison, y = .data$feature, fill = .data$effect)
  ) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.3) +
    ggplot2::scale_fill_gradient2(
      low = palette[1],
      mid = palette[2],
      high = palette[3],
      midpoint = 0,
      limits = limits,
      na.value = "grey85"
    ) +
    ggplot2::labs(x = NULL, y = NULL, fill = effect_col) +
    ggplot2::theme_minimal(base_family = "sans") +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "white", colour = NA),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1),
      axis.text.y = ggplot2::element_text(size = 9),
      axis.title = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(fill = "grey95", colour = NA),
      strip.text = ggplot2::element_text(face = "bold", size = 9),
      legend.title = ggplot2::element_text(size = 10),
      legend.text = ggplot2::element_text(size = 9)
    )

  if (show_stars) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(
        x = .data$comparison,
        y = .data$feature,
        label = .data$stars
      ),
      inherit.aes = FALSE,
      size = star_size,
      colour = "grey10",
      show.legend = FALSE
    )
  }

  if (facet && length(unique(long$protein)) > 1L) {
    p <- p + ggplot2::facet_wrap(~protein, scales = "free_y")
  }

  p
}
