##### Report text helpers #####
# Small helpers that turn analysis settings and results into manuscript-ready text.

#' Generate a methods paragraph
#'
#' Builds a short methods paragraph from the settings used in an analysis. The
#' text is intentionally generic; review and adapt it before submitting.
#'
#' @param settings Named list of analysis settings. Recognised entries include
#'   `quant_mode`, `normalize`, `transform`, `exclude_un`, `test`, `p_adjust`
#'   and `n_replicates`.
#'
#' @return A single character string.
#' @export
methods_text <- function(settings = list()) {
  if (is.null(settings)) settings <- list()
  if (!is.list(settings)) stop("`settings` must be a list.", call. = FALSE)

  version <- tryCatch(histonemod_package_version(), error = function(e) "0.8.0")

  quant_mode <- settings$quant_mode
  if (is.null(quant_mode) || !nzchar(quant_mode)) quant_mode <- "percentage"

  quant_sentence <- switch(
    quant_mode,
    "percentage" = paste0(
      "Peptide-level MS1 intensities were exported from Skyline and relative ",
      "peptide abundances were calculated as the percentage of the summed MS1 ",
      "area within each protein and replicate."
    ),
    "light_heavy_ratio" = paste0(
      "Peptide-level light/heavy MS1 area ratios were calculated from the ",
      "Skyline export and normalised across samples."
    ),
    "spike_in" = paste0(
      "Peptide-level light/heavy ratios were calculated and normalised to the ",
      "spike-in signal recorded in the sample annotation table."
    ),
    paste0("Peptide abundances were quantified using mode '", quant_mode, "'.")
  )

  if (isTRUE(settings$exclude_un)) {
    quant_sentence <- paste0(
      quant_sentence,
      " Unmodified peptides (identified by the '_un'/'_unun' naming convention) ",
      "were excluded from the visualisation."
    )
  }

  transform <- settings$transform
  if (is.null(transform) || !nzchar(transform)) transform <- "none"
  transform_sentence <- switch(
    transform,
    "clr" = "Compositional data were centred log-ratio (CLR) transformed before multivariate analysis.",
    "alr" = "Compositional data were additive log-ratio (ALR) transformed before multivariate analysis.",
    "logit" = "Percentage data were logit transformed before multivariate analysis.",
    "log2" = "Values were log2 transformed before multivariate analysis.",
    ""
  )

  test <- settings$test
  if (is.null(test) || !nzchar(test)) test <- "Welch t-test"
  adjust <- settings$p_adjust
  if (is.null(adjust) || !nzchar(adjust)) adjust <- "BH"
  adjust_label <- if (identical(toupper(adjust), "BH")) "Benjamini-Hochberg" else adjust

  stats_sentence <- paste0(
    "Group differences were assessed with a ", test,
    " and p-values were adjusted using the ", adjust_label, " procedure."
  )

  paste(
    quant_sentence,
    transform_sentence,
    stats_sentence,
    paste0("All analyses were performed with HistoneMod v", version, ".")
  )
}

#' Tabulate differential results
#'
#' Adds a significance label to the output of [differential_analysis()] and
#' sorts the table by adjusted p-value.
#'
#' @param results Output of [differential_analysis()].
#' @param alpha Adjusted p-value threshold.
#'
#' @return A data frame with an added `significance` column.
#' @export
statistics_table <- function(results, alpha = 0.05) {
  if (!is.data.frame(results)) stop("`results` must be a data.frame.", call. = FALSE)
  required <- intersect(c("p_adj", "p_value", "direction"), colnames(results))
  if (!"p_adj" %in% required) {
    stop("`results` must contain a 'p_adj' column.", call. = FALSE)
  }
  alpha <- suppressWarnings(as.numeric(alpha))
  if (length(alpha) != 1L || is.na(alpha) || alpha <= 0 || alpha >= 1) {
    stop("`alpha` must be a single number between 0 and 1.", call. = FALSE)
  }

  table <- results
  table$significance <- ifelse(
    is.na(table$p_adj),
    "",
    ifelse(table$p_adj < 0.001, "***",
           ifelse(table$p_adj < 0.01, "**",
                  ifelse(table$p_adj < 0.05, "*",
                         ifelse(table$p_adj < alpha, ".", "ns"))))
  )

  if ("p_adj" %in% colnames(table)) {
    table <- table[order(table$p_adj, na.last = TRUE), , drop = FALSE]
  }
  rownames(table) <- NULL
  table
}
