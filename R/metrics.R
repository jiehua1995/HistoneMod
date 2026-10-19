##### HistoneMod: Summary Metrics #####
# User-defined metrics built from regex specifications over peptide names,
# e.g. "K4me3 / all" or "K9ac / K9me3".

#' Compute per-sample summary metrics
#'
#' Builds custom per-sample metrics from a processed HistoneMod table. Each
#' metric specification matches features by regular expression, sums the
#' matching values per sample and divides the numerator sum by the denominator
#' sum. The denominator may be `"all"` to use the total of all features.
#'
#' @param data A processed long-format data.frame as returned by
#'   [percentage_calculation()].
#' @param specs A data.frame with the columns `metric`, `numerator` and
#'   `denominator` (regular expressions matched against the feature column).
#'   The numerator is the sum of all matching features; the denominator is
#'   either a regular expression or the literal `"all"` for the total of all
#'   features. An optional `scale` column multiplies the ratio (default 100).
#' @param sample_col Name of the sample column (default `"Replicate.Name"`).
#' @param group_col Name of the group column (default `"Group"`). When the
#'   column is absent the `group` output column is `NA`.
#' @param feature_col Name of the feature column (default `"Peptide.Note"`).
#' @param value_col Name of the numeric value column (default `"Percentage"`).
#'
#' @return A data.frame with columns `sample`, `group`, `metric` and `value`,
#'   with one row per metric and sample. Samples are returned in order of first
#'   appearance and metrics in the order given by `specs`. A denominator sum of
#'   zero gives `NA`.
#' @export
compute_metrics <- function(data,
                            specs,
                            sample_col = "Replicate.Name",
                            group_col = "Group",
                            feature_col = "Peptide.Note",
                            value_col = "Percentage") {
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
  if (!is.data.frame(specs)) {
    stop("`specs` must be a data.frame.", call. = FALSE)
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

  required_specs <- c("metric", "numerator", "denominator")
  missing_specs <- setdiff(required_specs, colnames(specs))
  if (length(missing_specs) > 0L) {
    stop(
      "`specs` is missing required column(s): ",
      paste(missing_specs, collapse = ", "),
      call. = FALSE
    )
  }

  if (!is.numeric(data[[value_col]]) && !is.integer(data[[value_col]])) {
    stop("`", value_col, "` must be numeric.", call. = FALSE)
  }

  value <- as.numeric(data[[value_col]])
  sample <- as.character(data[[sample_col]])
  feature <- as.character(data[[feature_col]])

  has_group <- !is.null(group_col) && is.character(group_col) &&
    length(group_col) == 1L && group_col %in% colnames(data)

  sample_levels <- unique(sample)
  sample_levels <- sample_levels[!is.na(sample_levels)]

  empty_result <- function() {
    data.frame(
      sample = character(0),
      group = character(0),
      metric = character(0),
      value = numeric(0),
      stringsAsFactors = FALSE
    )
  }

  if (length(sample_levels) == 0L) {
    return(empty_result())
  }

  if (has_group) {
    group_map <- unique(data.frame(
      sample = as.character(data[[sample_col]]),
      group = as.character(data[[group_col]]),
      stringsAsFactors = FALSE
    ))
    group_map <- group_map[!duplicated(group_map$sample), , drop = FALSE]
    group <- group_map$group[match(sample_levels, group_map$sample)]
  } else {
    group <- rep(NA_character_, length(sample_levels))
  }

  if (nrow(specs) == 0L) {
    return(empty_result())
  }

  specs$metric <- as.character(specs$metric)
  specs$numerator <- as.character(specs$numerator)
  specs$denominator <- as.character(specs$denominator)

  invalid <- is.na(specs$metric) | !nzchar(specs$metric) |
    is.na(specs$numerator) | !nzchar(specs$numerator) |
    is.na(specs$denominator) | !nzchar(specs$denominator)
  if (any(invalid)) {
    stop(
      "`specs` contains missing or empty metric/numerator/denominator values.",
      call. = FALSE
    )
  }

  if ("scale" %in% colnames(specs)) {
    scale <- suppressWarnings(as.numeric(specs$scale))
    scale[is.na(scale)] <- 100
  } else {
    scale <- rep(100, nrow(specs))
  }

  sample_factor <- factor(sample, levels = sample_levels)

  compile_regex <- function(pattern, index) {
    tryCatch(
      suppressWarnings(grepl(pattern, "", perl = FALSE)),
      error = function(e) {
        stop(
          "Invalid regular expression in specs row ", index, ": ", pattern,
          " (", conditionMessage(e), ")",
          call. = FALSE
        )
      }
    )
    pattern
  }

  metric_values <- vector("list", nrow(specs))
  metric_names <- character(nrow(specs))

  for (i in seq_len(nrow(specs))) {
    numerator <- compile_regex(specs$numerator[i], i)
    denominator <- compile_regex(specs$denominator[i], i)

    num_mask <- grepl(numerator, feature)
    num_sum <- tapply(
      value * num_mask,
      sample_factor,
      sum,
      na.rm = TRUE
    )

    if (identical(denominator, "all")) {
      den_sum <- tapply(value, sample_factor, sum, na.rm = TRUE)
    } else {
      den_mask <- grepl(denominator, feature)
      den_sum <- tapply(
        value * den_mask,
        sample_factor,
        sum,
        na.rm = TRUE
      )
    }

    num_sum <- as.numeric(num_sum)
    den_sum <- as.numeric(den_sum)

    out_value <- ifelse(
      is.na(den_sum) | den_sum == 0,
      NA_real_,
      scale[i] * num_sum / den_sum
    )

    metric_values[[i]] <- data.frame(
      sample = sample_levels,
      group = group,
      metric = specs$metric[i],
      value = out_value,
      stringsAsFactors = FALSE
    )
    metric_names[i] <- specs$metric[i]
  }

  do.call(rbind, metric_values)
}
