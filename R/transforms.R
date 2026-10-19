##### HistoneMod: Compositional Transforms #####
# Helpers for turning long peptide-percentage tables into wide sample-by-feature
# matrices and for applying common compositional transformations (CLR, ALR,
# logit, log2) while handling zeros and missing values explicitly.

# Bindings used inside non-standard evaluation (tidyselect/ggplot2).
utils::globalVariables(c(
  "feature", "value", "Value", "Feature", "Protein",
  "effect", "neg_log10_p", ".label",
  "missing_rate", "group", "mean_cv_within_group"
))

# Internal: coerce input to a plain numeric matrix.
as_numeric_matrix <- function(x, name = "x") {
  if (is.data.frame(x)) {
    if (ncol(x) > 0L && !all(vapply(x, is.numeric, logical(1)))) {
      stop("`", name, "` must contain only numeric columns.", call. = FALSE)
    }
    x <- as.matrix(x)
  }
  if (!is.matrix(x)) {
    stop("`", name, "` must be a numeric matrix or data.frame.", call. = FALSE)
  }
  if (!is.numeric(x)) {
    stop("`", name, "` must be numeric.", call. = FALSE)
  }
  storage.mode(x) <- "double"
  x
}

# Internal: default pseudocount = half the smallest positive value.
default_pseudocount <- function(x) {
  positive <- x[x > 0]
  if (length(positive) == 0L) {
    # Every value is zero: any positive offset gives an all-zero CLR/ALR table.
    return(1)
  }
  min(positive) / 2
}

# Internal: validate a user supplied pseudocount.
validate_pseudocount <- function(pseudocount, allow_zero = FALSE) {
  ok <- is.numeric(pseudocount) &&
    length(pseudocount) == 1L &&
    !is.na(pseudocount) &&
    is.finite(pseudocount) &&
    if (allow_zero) pseudocount >= 0 else pseudocount > 0
  if (!ok) {
    stop(
      "`pseudocount` must be a single finite ",
      if (allow_zero) "non-negative" else "positive",
      " number.",
      call. = FALSE
    )
  }
  as.numeric(pseudocount)
}

# Internal: resolve the reference column used by ALR.
resolve_reference <- function(mat, reference) {
  if (is.null(reference)) {
    reference <- ncol(mat)
  }
  if (is.character(reference)) {
    if (length(reference) != 1L) {
      stop("`reference` must be a single column name or index.", call. = FALSE)
    }
    reference <- match(reference, colnames(mat))
    if (is.na(reference)) {
      stop("`reference` must match a column name of `x`.", call. = FALSE)
    }
  }
  if (!is.numeric(reference) || length(reference) != 1L ||
      is.na(reference) || !is.finite(reference) ||
      reference < 1 || reference > ncol(mat)) {
    stop(
      "`reference` must be a column name or a column index between 1 and ncol(x).",
      call. = FALSE
    )
  }
  as.integer(reference)
}

# Internal: replace zeros before a log transform.
# Returns a list with the (possibly modified) matrix and the pseudocount used.
impute_zeros <- function(x, zeros, pseudocount) {
  if (!any(x == 0)) {
    return(list(x = x, pseudocount = NULL, imputed = FALSE))
  }

  if (identical(zeros, "multiplicative")) {
    if (requireNamespace("zCompositions", quietly = TRUE)) {
      replaced <- tryCatch({
        out <- zCompositions::cmultRepl(
          as.data.frame(x),
          label = 0,
          method = "CZM",
          output = "prop"
        )
        out <- as.matrix(out)
        if (!identical(dim(out), dim(x)) || !all(is.finite(out))) NULL else out
      }, error = function(e) NULL)

      if (!is.null(replaced)) {
        return(list(
          x = replaced,
          pseudocount = NA_real_,
          imputed = TRUE,
          method = "zCompositions::cmultRepl"
        ))
      }
      warning(
        "zCompositions::cmultRepl() failed; falling back to a pseudocount.",
        call. = FALSE
      )
    } else {
      warning(
        "Package 'zCompositions' is not installed; falling back to a pseudocount.",
        call. = FALSE
      )
    }
  }

  pc <- if (is.null(pseudocount)) default_pseudocount(x) else validate_pseudocount(pseudocount)
  x[x == 0] <- pc
  list(x = x, pseudocount = pc, imputed = TRUE, method = "pseudocount")
}

#' Transform compositional data
#'
#' Applies a compositional transformation to a numeric matrix of relative
#' abundances (typically percentage data). Rows are samples and columns are
#' features, matching the output of [composition_matrix()].
#'
#' @param x A numeric matrix or data.frame (samples in rows, features in
#'   columns). Values must be finite and non-negative.
#' @param method Transformation to apply. One of `"none"`, `"clr"` (centred log
#'   ratio: `log(x) - rowMeans(log(x))`), `"alr"` (additive log ratio:
#'   `log(x / reference)`), `"logit"` (log-odds, treating input as proportions,
#'   or as percentages when the maximum value exceeds 1) or `"log2"`
#'   (`log2(x + pseudocount)`).
#' @param reference Reference feature for `method = "alr"`, given as a column
#'   name or column index. The default (`NULL`) uses the last column.
#' @param zeros Zero handling for `"clr"` and `"alr"`. `"pseudocount"` replaces
#'   zeros with `pseudocount`; `"multiplicative"` calls
#'   `zCompositions::cmultRepl()` when the package is installed and otherwise
#'   falls back to the pseudocount with a warning.
#' @param pseudocount Positive number used to replace zeros. When `NULL` (the
#'   default) half of the smallest positive value of `x` is used. For
#'   `method = "log2"` the pseudocount is always added (also when there are no
#'   zeros).
#'
#' @return A numeric matrix with the same dimensions and dimnames as `x`.
#'   Transformations never return `NaN` or infinite values: zeros are imputed
#'   and the logit is clamped away from 0 and 1.
#'
#' @seealso [composition_matrix()]
#' @export
transform_composition <- function(x,
                                  method = c("none", "clr", "alr", "logit", "log2"),
                                  reference = NULL,
                                  zeros = c("pseudocount", "multiplicative"),
                                  pseudocount = NULL) {
  method <- match.arg(tolower(method[1]), c("none", "clr", "alr", "logit", "log2"))
  zeros <- match.arg(zeros)

  mat <- as_numeric_matrix(x)

  if (length(mat) == 0L) {
    return(mat)
  }
  if (anyNA(mat)) {
    stop(
      "`x` contains missing values; impute or remove them before transforming.",
      call. = FALSE
    )
  }
  if (any(!is.finite(mat))) {
    stop("`x` must contain only finite values.", call. = FALSE)
  }
  if (any(mat < 0)) {
    stop("`x` must contain only non-negative values.", call. = FALSE)
  }

  if (method == "none") {
    return(mat)
  }

  if (method == "logit") {
    p <- mat
    if (max(p) > 1) {
      # Percentages instead of proportions.
      p <- p / 100
    }
    eps <- sqrt(.Machine$double.eps)
    p <- pmin(pmax(p, eps), 1 - eps)
    return(log(p / (1 - p)))
  }

  if (method == "log2") {
    pc <- if (is.null(pseudocount)) default_pseudocount(mat) else validate_pseudocount(pseudocount, allow_zero = TRUE)
    if (pc == 0 && any(mat == 0)) {
      stop(
        "A positive `pseudocount` is required for log2 when `x` contains zeros.",
        call. = FALSE
      )
    }
    return(log2(mat + pc))
  }

  # CLR and ALR need positive values.
  imputed <- impute_zeros(mat, zeros, pseudocount)
  mat <- imputed$x

  if (method == "clr") {
    logged <- log(mat)
    return(logged - rowMeans(logged))
  }

  # ALR
  ref <- resolve_reference(mat, reference)
  logged <- log(mat)
  return(sweep(logged, 1L, logged[, ref], FUN = "-"))
}

#' Build a sample-by-feature composition matrix
#'
#' Converts a long HistoneMod table (one row per sample/peptide) into a wide
#' numeric matrix with samples in rows and features in columns. Missing
#' sample/feature combinations are filled with `fill`, which is `0` by default
#' (below detection).
#'
#' @param data A data.frame in long format, e.g. the output of
#'   [percentage_calculation()].
#' @param sample_col Name of the sample column (default `"Replicate.Name"`).
#' @param feature_col Name of the feature column (default `"Peptide.Note"`).
#' @param value_col Name of the numeric value column (default `"Percentage"`).
#' @param fill Value used for missing sample/feature combinations.
#'
#' @return A numeric matrix with samples in rows and features in columns.
#'   Row names are sample labels and column names are feature labels, both in
#'   order of first appearance in `data`.
#'
#' @seealso [transform_composition()]
#' @export
composition_matrix <- function(data,
                               sample_col = "Replicate.Name",
                               feature_col = "Peptide.Note",
                               value_col = "Percentage",
                               fill = 0) {
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

  if (!is.numeric(fill) || length(fill) != 1L || (!is.na(fill) && !is.finite(fill))) {
    stop("`fill` must be a single numeric value (or NA).", call. = FALSE)
  }
  if (!is.numeric(data[[value_col]]) && !is.integer(data[[value_col]])) {
    stop("`", value_col, "` must be numeric.", call. = FALSE)
  }

  if (nrow(data) == 0L) {
    return(matrix(
      numeric(0),
      nrow = 0L,
      ncol = 0L,
      dimnames = list(character(0), character(0))
    ))
  }

  df <- data.frame(
    sample = as.character(data[[sample_col]]),
    feature = as.character(data[[feature_col]]),
    value = as.numeric(data[[value_col]]),
    stringsAsFactors = FALSE
  )

  if (anyNA(df$sample) || anyNA(df$feature)) {
    stop("Sample and feature columns must not contain missing values.", call. = FALSE)
  }

  if (anyDuplicated(paste(df$sample, df$feature, sep = "\r"))) {
    stop(
      "Duplicate sample/feature combinations found; ",
      "aggregate or de-duplicate `data` before building a composition matrix.",
      call. = FALSE
    )
  }

  wide <- tidyr::pivot_wider(
    df,
    names_from = feature,
    values_from = value,
    values_fill = fill
  )

  mat <- as.matrix(wide[, -1L, drop = FALSE])
  storage.mode(mat) <- "double"
  rownames(mat) <- wide$sample
  mat
}
