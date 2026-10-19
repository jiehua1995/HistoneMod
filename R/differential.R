##### HistoneMod: Differential Analysis #####
# Pairwise differential testing of peptide states across experimental groups.
# Uses base R statistics only (Welch t-tests by default, optional one-way
# ANOVA), with BH-adjusted p-values and a ggplot2 volcano plot.

# Internal: empty differential result with the documented column layout.
empty_differential_result <- function(within_protein = FALSE) {
  out <- data.frame(
    protein = character(0),
    feature = character(0),
    group1 = character(0),
    group2 = character(0),
    n1 = integer(0),
    n2 = integer(0),
    mean1 = numeric(0),
    mean2 = numeric(0),
    effect = numeric(0),
    statistic = numeric(0),
    p_value = numeric(0),
    p_adj = numeric(0),
    stringsAsFactors = FALSE
  )
  if (isTRUE(within_protein)) {
    out$p_adj_protein <- numeric(0)
  }
  out$direction <- factor(character(0), levels = c("up", "down", "ns"))
  out
}

# Internal: per-feature one-way ANOVA across all groups.
global_anova_summary <- function(feat_mat, group, protein, features) {
  groups <- unique(group)
  nfeat <- nrow(feat_mat)
  statistic <- rep(NA_real_, nfeat)
  p_value <- rep(NA_real_, nfeat)

  if (length(groups) >= 2L) {
    for (i in seq_len(nfeat)) {
      dat <- data.frame(
        value = feat_mat[i, ],
        grp = factor(group, levels = groups)
      )
      fit <- tryCatch(stats::aov(value ~ grp, data = dat), error = function(e) NULL)
      if (!is.null(fit)) {
        tab <- tryCatch(suppressWarnings(summary(fit)[[1]]), error = function(e) NULL)
        if (!is.null(tab) && nrow(tab) >= 1L) {
          statistic[i] <- tab[["F value"]][1]
          p_value[i] <- tab[["Pr(>F)"]][1]
        }
      }
    }
  }

  data.frame(
    protein = protein,
    feature = features,
    n_groups = length(groups),
    statistic = statistic,
    p_value = p_value,
    p_adj = stats::p.adjust(p_value, method = "BH"),
    stringsAsFactors = FALSE
  )
}

#' Differential analysis of peptide states
#'
#' Tests each peptide state (feature) for differences between experimental
#' groups. The long HistoneMod table is converted to a sample-by-feature matrix
#' with [composition_matrix()], transformed with [transform_composition()], and
#' tested on the transformed scale. Original-scale group means (`mean1`,
#' `mean2`) are always reported next to the transformed-scale effect.
#'
#' @param data A processed long-format data.frame as returned by
#'   [percentage_calculation()]. Must contain `Replicate.Name`, `Group`,
#'   `Protein.Name`, `Peptide.Note` and the value column. Rows with a missing
#'   value are ignored.
#' @param comparisons Optional list of length-2 character vectors defining the
#'   comparisons, e.g. `list(c("A", "B"), c("A", "C"))`. When `NULL`, all
#'   pairwise comparisons among groups are used (or comparisons against
#'   `reference_group` when supplied).
#' @param reference_group Optional reference group. When `comparisons` is
#'   `NULL`, every other group is compared with the reference group.
#' @param method Statistical test: `"auto"` (same as `"welch"`), `"welch"` for
#'   per-feature Welch t-tests, or `"anova"` for per-comparison one-way ANOVA
#'   F-tests. With `method = "anova"` a per-feature global one-way ANOVA across
#'   all groups is attached to the result as the `"global_anova"` attribute.
#' @param transform Transformation applied before testing, passed to
#'   [transform_composition()]. Defaults to `"clr"`.
#' @param zeros Zero handling passed to [transform_composition()].
#' @param pseudocount Optional pseudocount passed to
#'   [transform_composition()].
#' @param min_n Minimum number of samples required in each group of a
#'   comparison. Comparisons that do not meet this requirement are skipped.
#' @param paired Logical; if `TRUE`, a paired t-test (or a paired ANOVA with
#'   pair as a blocking factor) is used.
#' @param pair_col Column identifying pairs when `paired = TRUE`. Defaults to
#'   `"Replicate.No"` when `NULL`.
#' @param within_protein Logical; if `TRUE`, an extra `p_adj_protein` column with
#'   BH-adjusted p-values computed within each protein is added. `p_adj` always
#'   remains the global BH adjustment.
#' @param alpha Significance threshold used to label `direction` as `"up"` or
#'   `"down"` (`p_adj < alpha`).
#' @param value_col Name of the numeric value column (default `"Percentage"`).
#'
#' @return A data.frame with columns `protein`, `feature`, `group1`, `group2`,
#'   `n1`, `n2`, `mean1`, `mean2`, `effect`, `statistic`, `p_value`, `p_adj`
#'   and `direction` (a factor with levels `"up"`, `"down"`, `"ns"`). `effect`
#'   is `mean1 - mean2` on the transformed scale and `mean1`/`mean2` are
#'   original-scale means, so `"up"` means higher in `group1` and `"down"` means
#'   higher in `group2`. When `within_protein = TRUE`, `p_adj_protein` is
#'   inserted before `direction`. When `method = "anova"`, the attribute
#'   `"global_anova"` holds a per-feature one-way ANOVA summary.
#' @export
differential_analysis <- function(data,
                                  comparisons = NULL,
                                  reference_group = NULL,
                                  method = c("auto", "welch", "anova"),
                                  transform = c("clr", "none", "log2", "alr", "logit"),
                                  zeros = c("pseudocount", "multiplicative"),
                                  pseudocount = NULL,
                                  min_n = 2L,
                                  paired = FALSE,
                                  pair_col = NULL,
                                  within_protein = FALSE,
                                  alpha = 0.05,
                                  value_col = "Percentage") {
  method <- match.arg(method)
  transform <- match.arg(tolower(transform[1]), c("clr", "none", "log2", "alr", "logit"))
  zeros <- match.arg(zeros)
  paired <- isTRUE(paired)
  within_protein <- isTRUE(within_protein)

  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
  required <- c("Replicate.Name", "Group", "Protein.Name", "Peptide.Note", value_col)
  missing_cols <- setdiff(required, colnames(data))
  if (length(missing_cols) > 0L) {
    stop(
      "`data` is missing required column(s): ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  min_n <- suppressWarnings(as.integer(min_n))
  if (length(min_n) != 1L || is.na(min_n) || min_n < 1L) {
    stop("`min_n` must be a positive integer.", call. = FALSE)
  }

  alpha <- suppressWarnings(as.numeric(alpha))
  if (length(alpha) != 1L || is.na(alpha) || !is.finite(alpha) || alpha <= 0 || alpha >= 1) {
    stop("`alpha` must be a single number between 0 and 1.", call. = FALSE)
  }

  df <- data
  df$Replicate.Name <- as.character(df$Replicate.Name)
  df$Group <- as.character(df$Group)
  df$Protein.Name <- as.character(df$Protein.Name)
  df$Peptide.Note <- as.character(df$Peptide.Note)
  if (!is.numeric(df[[value_col]]) && !is.integer(df[[value_col]])) {
    stop("`", value_col, "` must be numeric.", call. = FALSE)
  }

  df <- df[!is.na(df[[value_col]]), , drop = FALSE]
  if (nrow(df) == 0L) {
    stop("`data` does not contain any non-missing values to test.", call. = FALSE)
  }

  groups <- unique(df$Group)
  if (length(groups) < 2L) {
    stop(
      "`differential_analysis()` requires at least two groups in `data`.",
      call. = FALSE
    )
  }

  if (!is.null(reference_group)) {
    if (!is.character(reference_group) || length(reference_group) != 1L ||
        !reference_group %in% groups) {
      stop("`reference_group` must be one of the groups in `data`.", call. = FALSE)
    }
  }

  check_comparison <- function(cmp) {
    if (!is.character(cmp) || length(cmp) != 2L) {
      stop("Each comparison must be a character vector of length 2.", call. = FALSE)
    }
    if (anyNA(cmp) || any(!nzchar(cmp))) {
      stop("Comparison groups must be non-missing, non-empty names.", call. = FALSE)
    }
    if (identical(cmp[1], cmp[2])) {
      stop("A comparison must contain two distinct groups.", call. = FALSE)
    }
    unknown <- setdiff(cmp, groups)
    if (length(unknown) > 0L) {
      stop(
        "Unknown group(s) in comparison: ",
        paste(unknown, collapse = ", "),
        call. = FALSE
      )
    }
    as.character(cmp)
  }

  if (is.null(comparisons)) {
    if (!is.null(reference_group)) {
      comparisons <- lapply(
        setdiff(groups, reference_group),
        function(g) c(reference_group, g)
      )
    } else {
      comparisons <- utils::combn(groups, 2, simplify = FALSE)
    }
  } else {
    if (!is.list(comparisons)) {
      comparisons <- list(comparisons)
    }
    comparisons <- lapply(comparisons, check_comparison)
  }

  if (length(comparisons) == 0L) {
    stop("No comparisons could be constructed from `data`.", call. = FALSE)
  }

  meta <- unique(df[, c("Replicate.Name", "Group")])
  if (anyDuplicated(meta$Replicate.Name)) {
    stop("Each sample must belong to exactly one group.", call. = FALSE)
  }
  group_n <- table(meta$Group)
  comparison_ok <- vapply(
    comparisons,
    function(cmp) all(group_n[cmp] >= min_n),
    logical(1)
  )
  if (!any(comparison_ok)) {
    message(
      "No comparison has at least ", min_n,
      " samples per group; returning an empty result."
    )
    return(empty_differential_result(within_protein))
  }
  if (!all(comparison_ok)) {
    skipped <- vapply(
      comparisons[!comparison_ok],
      function(cmp) paste(cmp, collapse = " vs "),
      character(1)
    )
    message(
      "Skipping comparison(s) with fewer than ", min_n,
      " samples per group: ", paste(skipped, collapse = ", ")
    )
  }
  comparisons <- comparisons[comparison_ok]

  feature_meta <- unique(df[, c("Peptide.Note", "Protein.Name")])
  if (anyDuplicated(feature_meta$Peptide.Note)) {
    stop(
      "Some peptide names map to more than one protein; ",
      "`Peptide.Note` must be unique per feature.",
      call. = FALSE
    )
  }

  orig_mat <- composition_matrix(df, value_col = value_col)
  samples <- rownames(orig_mat)
  features <- colnames(orig_mat)

  group_of_sample <- meta$Group[match(samples, meta$Replicate.Name)]
  protein_of_feature <- feature_meta$Protein.Name[match(features, feature_meta$Peptide.Note)]

  pair_ids <- NULL
  if (paired) {
    if (is.null(pair_col)) {
      pair_col <- "Replicate.No"
    }
    if (!is.character(pair_col) || length(pair_col) != 1L ||
        !pair_col %in% colnames(df)) {
      stop(
        "`paired = TRUE` requires a `pair_col` that exists in `data` ",
        "(default \"Replicate.No\").",
        call. = FALSE
      )
    }
    pair_map <- unique(df[, c("Replicate.Name", pair_col)])
    colnames(pair_map) <- c("sample", "pair")
    if (anyDuplicated(pair_map$sample)) {
      stop(
        "Each sample must map to exactly one pair identifier.",
        call. = FALSE
      )
    }
    pair_ids <- as.character(pair_map$pair[match(samples, pair_map$sample)])
    if (anyNA(pair_ids)) {
      stop("Missing pair identifiers found in `pair_col`.", call. = FALSE)
    }
  }

  tmat <- transform_composition(
    orig_mat,
    method = transform,
    zeros = zeros,
    pseudocount = pseudocount
  )
  feat_mat <- t(tmat)
  orig_feat <- t(orig_mat)

  nfeat <- nrow(feat_mat)
  use_anova <- identical(method, "anova")

  run_comparison <- function(cmp) {
    g1 <- cmp[1]
    g2 <- cmp[2]

    idx1 <- which(group_of_sample == g1)
    idx2 <- which(group_of_sample == g2)

    pair_common <- NULL
    if (paired) {
      p1 <- pair_ids[idx1]
      p2 <- pair_ids[idx2]
      pair_common <- intersect(p1, p2)
      if (length(pair_common) < min_n) {
        return(NULL)
      }
      idx1 <- idx1[match(pair_common, p1)]
      idx2 <- idx2[match(pair_common, p2)]
    }

    xsub <- feat_mat[, idx1, drop = FALSE]
    ysub <- feat_mat[, idx2, drop = FALSE]

    n1 <- as.integer(rowSums(is.finite(xsub)))
    n2 <- as.integer(rowSums(is.finite(ysub)))
    effect <- rowMeans(xsub) - rowMeans(ysub)
    mean1 <- rowMeans(orig_feat[, idx1, drop = FALSE], na.rm = TRUE)
    mean2 <- rowMeans(orig_feat[, idx2, drop = FALSE], na.rm = TRUE)

    statistic <- rep(NA_real_, nfeat)
    p_value <- rep(NA_real_, nfeat)

    for (i in seq_len(nfeat)) {
      x <- xsub[i, ]
      y <- ysub[i, ]

      if (!use_anova) {
        tt <- tryCatch(
          stats::t.test(x, y, paired = paired, var.equal = FALSE),
          error = function(e) NULL
        )
        if (!is.null(tt)) {
          statistic[i] <- unname(tt$statistic)
          p_value[i] <- unname(tt$p.value)
        }
      } else {
        dat <- data.frame(
          value = c(x, y),
          grp = factor(rep(c(g1, g2), c(length(x), length(y))), levels = cmp)
        )
        if (paired) {
          dat$pair <- factor(c(pair_common, pair_common))
          fit <- tryCatch(
            stats::aov(value ~ grp + pair, data = dat),
            error = function(e) NULL
          )
        } else {
          fit <- tryCatch(
            stats::aov(value ~ grp, data = dat),
            error = function(e) NULL
          )
        }
        if (!is.null(fit)) {
          tab <- tryCatch(suppressWarnings(summary(fit)[[1]]), error = function(e) NULL)
          if (!is.null(tab) && nrow(tab) >= 1L) {
            statistic[i] <- tab[["F value"]][1]
            p_value[i] <- tab[["Pr(>F)"]][1]
          }
        }
      }
    }

    data.frame(
      protein = protein_of_feature,
      feature = features,
      group1 = g1,
      group2 = g2,
      n1 = n1,
      n2 = n2,
      mean1 = as.numeric(mean1),
      mean2 = as.numeric(mean2),
      effect = as.numeric(effect),
      statistic = as.numeric(statistic),
      p_value = as.numeric(p_value),
      stringsAsFactors = FALSE
    )
  }

  results <- lapply(comparisons, run_comparison)
  results <- results[!vapply(results, is.null, logical(1))]
  if (length(results) == 0L) {
    return(empty_differential_result(within_protein))
  }

  res <- do.call(rbind, results)
  rownames(res) <- NULL

  res$p_adj <- stats::p.adjust(res$p_value, method = "BH")

  if (within_protein) {
    res$p_adj_protein <- NA_real_
    for (protein in unique(res$protein)) {
      idx <- if (is.na(protein)) {
        which(is.na(res$protein))
      } else {
        which(res$protein == protein)
      }
      res$p_adj_protein[idx] <- stats::p.adjust(res$p_value[idx], method = "BH")
    }
  }

  significant <- !is.na(res$p_adj) & res$p_adj < alpha
  res$direction <- factor(
    ifelse(
      significant & res$effect > 0,
      "up",
      ifelse(significant & res$effect < 0, "down", "ns")
    ),
    levels = c("up", "down", "ns")
  )

  cols <- c(
    "protein", "feature", "group1", "group2", "n1", "n2",
    "mean1", "mean2", "effect", "statistic", "p_value", "p_adj"
  )
  if (within_protein) {
    cols <- c(cols, "p_adj_protein")
  }
  res <- res[, c(cols, "direction"), drop = FALSE]

  if (use_anova) {
    attr(res, "global_anova") <- global_anova_summary(
      feat_mat,
      group_of_sample,
      protein_of_feature,
      features
    )
  }

  res
}

#' Volcano plot of differential results
#'
#' Plots the effect size against `-log10(p_adj)` for the output of
#' [differential_analysis()], colouring features by their `direction` and
#' labelling the most significant features with `ggrepel`.
#'
#' @param data A data.frame with at least the columns `effect`, `p_adj` and
#'   (optionally) `direction`. Rows with missing `effect` or `p_adj` are
#'   dropped from the plot.
#' @param alpha Significance threshold. A dashed horizontal line is drawn at
#'   `-log10(alpha)` and used to derive directions when `direction` is absent.
#' @param effect_threshold Numeric; when greater than zero, dashed vertical
#'   lines are drawn at `-effect_threshold` and `effect_threshold`.
#' @param label_top Number of top features (smallest `p_adj`) to label.
#' @param label_col Optional column with label text. Defaults to the `feature`
#'   column, or to row names when there is no `feature` column.
#' @param point_size Size of the points.
#' @param label_size Text size of the repelled labels.
#'
#' @return A `ggplot` object.
#' @export
plot_volcano <- function(data,
                         alpha = 0.05,
                         effect_threshold = 0,
                         label_top = 10,
                         label_col = NULL,
                         point_size = 1.8,
                         label_size = 3) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
  required <- c("effect", "p_adj")
  missing_cols <- setdiff(required, colnames(data))
  if (length(missing_cols) > 0L) {
    stop(
      "`data` is missing required column(s): ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  alpha <- suppressWarnings(as.numeric(alpha))
  if (length(alpha) != 1L || is.na(alpha) || !is.finite(alpha) || alpha <= 0 || alpha >= 1) {
    stop("`alpha` must be a single number between 0 and 1.", call. = FALSE)
  }
  effect_threshold <- suppressWarnings(as.numeric(effect_threshold))
  if (length(effect_threshold) != 1L || !is.finite(effect_threshold)) {
    stop("`effect_threshold` must be a single finite number.", call. = FALSE)
  }
  label_top <- suppressWarnings(as.integer(label_top))
  if (length(label_top) != 1L || is.na(label_top) || label_top < 0L) {
    stop("`label_top` must be a non-negative integer.", call. = FALSE)
  }

  dat <- data
  dat <- dat[!is.na(dat$effect) & is.finite(dat$effect), , drop = FALSE]
  dat <- dat[!is.na(dat$p_adj), , drop = FALSE]

  if (nrow(dat) == 0L) {
    return(
      ggplot2::ggplot() +
        ggplot2::geom_text(
          ggplot2::aes(x = 0, y = 0, label = "No features to plot"),
          size = 5
        ) +
        ggplot2::theme_void()
    )
  }

  dat$p_adj <- as.numeric(dat$p_adj)
  non_positive <- is.finite(dat$p_adj) & dat$p_adj <= 0
  if (any(non_positive)) {
    positive <- dat$p_adj[is.finite(dat$p_adj) & dat$p_adj > 0]
    floor_value <- if (length(positive) > 0L) min(positive) / 10 else 1e-300
    dat$p_adj[non_positive] <- floor_value
  }
  dat$neg_log10_p <- -log10(dat$p_adj)

  if ("direction" %in% colnames(dat)) {
    direction <- as.character(dat$direction)
  } else {
    significant <- dat$p_adj < alpha
    direction <- ifelse(
      significant & dat$effect > 0, "up",
      ifelse(significant & dat$effect < 0, "down", "ns")
    )
  }
  direction[is.na(direction)] <- "ns"
  dat$direction <- factor(direction, levels = c("up", "down", "ns"))

  if (is.null(label_col)) {
    if ("feature" %in% colnames(dat)) {
      dat$.label <- as.character(dat$feature)
    } else {
      dat$.label <- as.character(rownames(dat))
    }
  } else {
    if (!is.character(label_col) || length(label_col) != 1L ||
        !label_col %in% colnames(dat)) {
      stop("`label_col` must be a column name of `data`.", call. = FALSE)
    }
    dat$.label <- as.character(dat[[label_col]])
  }

  label_dat <- dat[order(dat$p_adj, -abs(dat$effect)), , drop = FALSE]
  if (label_top > 0L) {
    label_dat <- utils::head(label_dat, label_top)
  } else {
    label_dat <- label_dat[0, , drop = FALSE]
  }

  p <- ggplot2::ggplot(
    dat,
    ggplot2::aes(x = effect, y = neg_log10_p, colour = direction)
  ) +
    ggplot2::geom_point(size = point_size, alpha = 0.85) +
    ggplot2::geom_hline(
      yintercept = -log10(alpha),
      linetype = "dashed",
      colour = "grey40",
      linewidth = 0.4
    ) +
    ggplot2::labs(
      x = "Effect (transformed scale)",
      y = expression(-log[10](p[adj])),
      colour = NULL
    ) +
    ggplot2::scale_colour_manual(
      values = c(up = "#D55E00", down = "#0072B2", ns = "grey70"),
      drop = FALSE
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
      legend.text = ggplot2::element_text(size = 12),
      legend.title = ggplot2::element_text(size = 12)
    )

  if (effect_threshold > 0) {
    p <- p +
      ggplot2::geom_vline(
        xintercept = c(-effect_threshold, effect_threshold),
        linetype = "dashed",
        colour = "grey40",
        linewidth = 0.4
      )
  }

  p <- p +
    ggrepel::geom_text_repel(
      data = label_dat,
      mapping = ggplot2::aes(x = effect, y = neg_log10_p, label = .label),
      inherit.aes = FALSE,
      size = label_size,
      max.overlaps = Inf,
      show.legend = FALSE,
      colour = "black"
    )

  p
}
