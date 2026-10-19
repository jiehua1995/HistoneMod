##### Simulation and power analysis #####
# Helpers for planning histone PTM experiments.

#' Simulate a histone modification table
#'
#' Generates a synthetic long-format dataset with a known subset of features
#' affected by the group variable. Useful for testing the analysis workflow and
#' for planning experiments.
#'
#' @param n_groups Number of groups (default 2).
#' @param n_replicates Number of replicates per group.
#' @param n_features Number of peptide states.
#' @param effect_fraction Fraction of features with a true group effect.
#' @param effect_size Effect size on the CLR scale.
#' @param baseline_sd Between-feature variability on the CLR scale.
#' @param residual_sd Within-feature technical variability on the CLR scale.
#' @param seed Optional random seed.
#'
#' @return A long data frame with `Replicate.Name`, `Group`, `Protein.Name`,
#'   `Peptide.Note`, `Percentage` and `TrueEffect`.
#' @export
simulate_histone_data <- function(n_groups = 2, n_replicates = 3,
                                  n_features = 40, effect_fraction = 0.3,
                                  effect_size = 1, baseline_sd = 0.4,
                                  residual_sd = 0.2, seed = NULL) {
  n_groups <- as.integer(n_groups)
  n_replicates <- as.integer(n_replicates)
  n_features <- as.integer(n_features)
  if (any(is.na(c(n_groups, n_replicates, n_features))) ||
      n_groups < 2L || n_replicates < 2L || n_features < 3L) {
    stop("`n_groups`, `n_replicates` and `n_features` must be valid integers.", call. = FALSE)
  }
  if (!is.null(seed)) set.seed(seed)

  groups <- paste0("G", seq_len(n_groups))
  features <- sprintf("H3K%02dme3", seq_len(n_features))
  n_effect <- max(1L, round(n_features * effect_fraction))
  effect_index <- sample(seq_len(n_features), n_effect)
  effects <- rep(0, n_features)
  effects[effect_index] <- effect_size * sample(c(-1, 1), n_effect, replace = TRUE)
  baseline <- stats::rnorm(n_features, 0, baseline_sd)

  rows <- expand.grid(
    feature = features,
    group = groups,
    replicate = seq_len(n_replicates),
    stringsAsFactors = FALSE
  )
  rows$Replicate.Name <- paste0(rows$group, "_rep", rows$replicate)
  rows$Group <- rows$group
  rows$Protein.Name <- "H3"
  rows$Peptide.Note <- rows$feature
  rows$TrueEffect <- effects[match(rows$feature, features)]

  group_shift <- ifelse(rows$Group == groups[1], 0, rows$TrueEffect)
  rows$LogIntensity <- baseline[match(rows$feature, features)] +
    group_shift + stats::rnorm(nrow(rows), 0, residual_sd)

  # Convert to percentages within each replicate (single synthetic protein).
  rows$Percentage <- stats::ave(
    exp(rows$LogIntensity),
    rows$Replicate.Name,
    FUN = function(v) v / sum(v) * 100
  )

  rows[, c("Replicate.Name", "Group", "Protein.Name", "Peptide.Note",
           "Percentage", "TrueEffect"), drop = FALSE]
}

#' Estimate power for a histone PTM experiment
#'
#' Repeatedly simulates datasets with [simulate_histone_data()], runs
#' CLR-transformed Welch t-tests with BH correction, and estimates power and the
#' false discovery rate for several replicate numbers.
#'
#' @param n_replicates Integer vector of replicate numbers to evaluate.
#' @param n_features Number of simulated peptide states.
#' @param effect_fraction Fraction of features with a true effect.
#' @param effect_size Effect size on the CLR scale.
#' @param n_sim Number of simulations per replicate number.
#' @param alpha Significance threshold.
#' @param adjust P-value adjustment method passed to [stats::p.adjust()].
#' @param seed Optional random seed.
#'
#' @return A data frame with `n_replicates`, `power`, `fdr` and `n_sim`.
#' @export
estimate_power <- function(n_replicates = c(3, 4, 5, 6),
                           n_features = 40, effect_fraction = 0.3,
                           effect_size = 1, n_sim = 100,
                           alpha = 0.05, adjust = "BH", seed = NULL) {
  n_replicates <- as.integer(n_replicates)
  n_sim <- as.integer(n_sim)
  if (length(n_replicates) == 0 || any(is.na(n_replicates)) || any(n_replicates < 2L)) {
    stop("`n_replicates` must contain integers >= 2.", call. = FALSE)
  }
  if (is.na(n_sim) || n_sim < 1L) stop("`n_sim` must be a positive integer.", call. = FALSE)
  if (!is.null(seed)) set.seed(seed)

  results <- lapply(n_replicates, function(n_rep) {
    detected <- logical(0)
    true_effect <- logical(0)

    for (sim in seq_len(n_sim)) {
      data <- simulate_histone_data(
        n_groups = 2,
        n_replicates = n_rep,
        n_features = n_features,
        effect_fraction = effect_fraction,
        effect_size = effect_size
      )
      wide <- tidyr::pivot_wider(
        data[, c("Replicate.Name", "Peptide.Note", "Percentage")],
        names_from = "Peptide.Note",
        values_from = "Percentage",
        values_fill = 0
      )
      mat <- as.matrix(wide[, setdiff(colnames(wide), "Replicate.Name"), drop = FALSE])
      storage.mode(mat) <- "numeric"
      transformed <- transform_composition(mat, method = "clr")

      groups <- data$Group[match(wide$Replicate.Name, data$Replicate.Name)]
      p_values <- apply(transformed, 2, function(values) {
        tryCatch(
          stats::t.test(values ~ groups)$p.value,
          error = function(e) NA_real_
        )
      })
      p_values <- stats::p.adjust(p_values, method = adjust)
      called <- !is.na(p_values) & p_values < alpha
      truth <- tapply(data$TrueEffect, data$Peptide.Note, function(v) any(v != 0))
      truth <- truth[colnames(transformed)]

      detected <- c(detected, called & truth)
      true_effect <- c(true_effect, truth)
    }

    true_total <- sum(true_effect)
    called_total <- sum(detected)
    false_positive <- sum(detected & !true_effect)
    data.frame(
      n_replicates = n_rep,
      power = if (true_total > 0) sum(detected) / true_total else NA_real_,
      fdr = if (called_total > 0) false_positive / called_total else NA_real_,
      n_sim = n_sim,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, results)
}

#' Plot a power curve
#'
#' @param power_table Output of [estimate_power()].
#' @param target Target power line (default 0.8).
#'
#' @return A `ggplot` object.
#' @export
plot_power_curve <- function(power_table, target = 0.8) {
  required <- c("n_replicates", "power")
  missing_cols <- setdiff(required, colnames(power_table))
  if (length(missing_cols) > 0) {
    stop("`power_table` is missing column(s): ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  ggplot2::ggplot(
    power_table,
    ggplot2::aes(x = n_replicates, y = power)
  ) +
    ggplot2::geom_line(colour = "#4C72B0", linewidth = 1) +
    ggplot2::geom_point(colour = "#4C72B0", size = 3) +
    ggplot2::geom_hline(yintercept = target, linetype = 2, colour = "#D55E00") +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::labs(x = "Replicates per group", y = "Estimated power") +
    ggplot2::theme_minimal(base_family = "sans") +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 0.6)
    )
}
