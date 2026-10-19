##### HistoneMod: Quantification Modes #####
# Programmatic quantification of peptide-level MS1 data. Three modes are
# supported:
#   * "percentage"        within-protein relative abundance (light peptides)
#   * "light_heavy_ratio" light / heavy isotope ratio per peptide
#   * "spike_in"          light / heavy ratio normalised by a sample-level
#                         spike-in factor
#
# Unlike percentage_calculation(), these functions do not apply the Shiny
# application's exclude_un/peptide/sample filters; those remain the caller's
# responsibility.

# Internal: sum areas by the requested grouping columns. All-missing groups
# stay NA rather than silently becoming zero.
.hm_aggregate_areas <- function(df, by_cols, area) {
  by <- lapply(by_cols, function(col) df[[col]])
  names(by) <- by_cols
  out <- stats::aggregate(
    list(area = area),
    by = by,
    FUN = function(v) {
      if (length(v) == 0L || all(is.na(v))) NA_real_ else sum(v, na.rm = TRUE)
    }
  )
  for (col in by_cols) {
    out[[col]] <- as.character(out[[col]])
  }
  out
}

# Internal: attach the metadata attributes shared by all quantify_peptides()
# results.
.hm_set_quantity_attributes <- function(out, mode, normalize, spike_in_col,
                                         area_col) {
  attr(out, "value_type") <- if (identical(mode, "percentage")) "percentage" else "ratio"
  attr(out, "method") <- mode
  # Normalisation is only applied to ratio modes.
  attr(out, "normalize") <- if (identical(mode, "percentage")) "none" else normalize
  attr(out, "spike_in_col") <- spike_in_col
  attr(out, "area_col") <- area_col
  out
}

# Internal: empty result with the right columns and attributes.
.hm_empty_quantification <- function(mode, normalize, spike_in_col, area_col) {
  base_cols <- list(
    Replicate.Name = character(0),
    Protein.Name = character(0),
    Peptide.Note = character(0),
    Group = character(0),
    Value = numeric(0)
  )
  extra <- if (identical(mode, "percentage")) {
    list(Percentage = numeric(0))
  } else {
    list(Ratio = numeric(0))
  }
  out <- as.data.frame(c(base_cols, extra), stringsAsFactors = FALSE)
  .hm_set_quantity_attributes(out, mode, normalize, spike_in_col, area_col)
}

# Internal: merge the sample-level metadata used by quantify_peptides().
.hm_sample_metadata <- function(sample, extra_cols = NULL) {
  cols <- unique(c("Replicate.Name", "Group", extra_cols))
  cols <- intersect(cols, names(sample))
  meta <- sample[, cols, drop = FALSE]
  meta$Replicate.Name <- as.character(meta$Replicate.Name)
  meta[!duplicated(meta$Replicate.Name), , drop = FALSE]
}

#' Normalise numeric values
#'
#' Applies a simple normalisation to a numeric vector. The `median` and `sum`
#' methods divide each value by the median or sum of the (optionally grouped)
#' values; `quantile` performs a simple quantile normalisation by ranking
#' values within `group` and replacing them with corresponding quantiles of the
#' pooled distribution. Missing values are preserved and are ignored when
#' computing the normalisation factors.
#'
#' @param values Numeric vector to normalise.
#' @param method Normalisation method. One of `"none"`, `"median"`, `"sum"` or
#'   `"quantile"`.
#' @param group Optional grouping vector (for example one label per peptide) of
#'   the same length as `values`. When supplied, `median`/`sum` normalisation is
#'   performed within each group; for `quantile` the rank transformation is
#'   applied within each group. `NULL` treats all values as a single group.
#'
#' @return A numeric vector of the same length as `values`. Values are returned
#'   unchanged (apart from numeric coercion) when the normalisation factor is
#'   zero, missing or non-finite.
#'
#' @export
normalize_values <- function(values,
                             method = c("none", "median", "sum", "quantile"),
                             group = NULL) {
  method <- match.arg(method)
  if (is.null(values)) return(numeric(0))

  x <- values
  if (!is.numeric(x)) {
    converted <- suppressWarnings(as.numeric(as.character(x)))
    bad <- !is.na(x) & is.na(converted)
    if (any(bad)) {
      stop("`values` must be numeric; ",
           sum(bad), " value(s) could not be converted.", call. = FALSE)
    }
    x <- converted
  }
  x <- as.numeric(x)
  n <- length(x)
  if (n == 0L || method == "none") return(x)

  na_pos <- is.na(x)
  if (!is.null(group)) {
    group <- as.character(group)
    if (length(group) == 1L && n > 1L) group <- rep(group, n)
    if (length(group) != n) {
      stop("`group` must have the same length as `values`.", call. = FALSE)
    }
  }

  if (method %in% c("median", "sum")) {
    if (is.null(group)) {
      denominator <- rep(
        if (method == "median") stats::median(x, na.rm = TRUE) else
          sum(x, na.rm = TRUE),
        n
      )
    } else {
      denominator <- stats::ave(
        x, group,
        FUN = function(v) {
          if (all(is.na(v))) return(NA_real_)
          if (method == "median") stats::median(v, na.rm = TRUE) else
            sum(v, na.rm = TRUE)
        }
      )
    }

    out <- x / denominator
    bad <- !is.finite(denominator) | denominator == 0
    out[bad] <- x[bad]
    out[na_pos] <- NA_real_
    if (any(bad & !na_pos)) {
      warning("Normalisation factor is zero or non-finite for ",
              sum(bad & !na_pos), " value(s); those values were left unchanged.",
              call. = FALSE)
    }
    return(out)
  }

  # method == "quantile"
  if (is.null(group)) group <- rep(".all", n)
  reference <- sort(x[!na_pos])
  if (length(reference) < 2L) return(x)

  out <- x
  for (g in unique(group[!is.na(group)])) {
    idx <- which(!is.na(group) & group == g & !na_pos)
    if (length(idx) < 2L) next
    ranks <- rank(x[idx], ties.method = "average")
    probs <- (ranks - 0.5) / length(idx)
    out[idx] <- stats::quantile(
      reference,
      probs = probs,
      names = FALSE,
      type = 7
    )
  }
  out[na_pos] <- NA_real_
  out
}

#' Quantify histone PTM peptides
#'
#' Merges an MS1 table with sample metadata and computes either within-protein
#' peptide percentages or light/heavy isotope ratios. This is the programmatic
#' counterpart of the Shiny application's quantification step: it does *not*
#' apply the `exclude_un`, peptide or sample filters that
#' [percentage_calculation()] applies, so those filters should be applied by
#' the caller if desired.
#'
#' In `"percentage"` mode only light-labelled rows are used. Areas are summed
#' per `Replicate.Name` x `Protein.Name` x `Peptide.Note`, then
#' `Percentage = area / sum(area within Replicate.Name x Protein.Name) * 100`
#' and `Value = Percentage`.
#'
#' In `"light_heavy_ratio"` and `"spike_in"` modes, light and heavy areas are
#' summed per `Replicate.Name` x `Protein.Name` x `Peptide.Note` and
#' `Ratio = light / heavy`. The ratio is `NA` when the heavy value is missing
#' or zero. If `normalize` is not `"none"`, ratios are normalised across
#' samples within each peptide with [normalize_values()]. In `"spike_in"` mode
#' the normalised ratio is additionally divided by the per-sample spike-in
#' factor taken from `spike_in_col` in the sample table. `Value` holds the final
#' ratio and `Ratio` keeps the un-normalised light/heavy ratio.
#'
#' @param ms1 A data frame with peptide-level MS1 measurements.
#' @param sample A data frame with sample metadata. Must contain
#'   `Replicate.Name` and `Group`; may contain a spike-in factor column.
#' @param mode Quantification mode: `"percentage"`, `"light_heavy_ratio"` or
#'   `"spike_in"`.
#' @param area_col Name of the column holding the area values.
#' @param normalize Normalisation applied to ratios (ratio modes only): one of
#'   `"none"`, `"median"` or `"sum"`.
#' @param spike_in_col Name of the sample-table column holding the spike-in
#'   factor. Required for `mode = "spike_in"` and ignored otherwise.
#'
#' @return A data frame with columns `Replicate.Name`, `Protein.Name`,
#'   `Peptide.Note`, `Group`, `Value` and either `Percentage` (percentage mode)
#'   or `Ratio` (ratio modes). The attributes `value_type` (`"percentage"` or
#'   `"ratio"`), `method`, `normalize`, `spike_in_col` and `area_col` describe
#'   how the values were produced.
#'
#' @export
quantify_peptides <- function(ms1, sample,
                              mode = c("percentage", "light_heavy_ratio", "spike_in"),
                              area_col = "Total.Area.MS1",
                              normalize = c("none", "median", "sum"),
                              spike_in_col = NULL) {
  mode <- match.arg(mode)
  normalize <- match.arg(normalize)

  ms1 <- .hm_normalise_table(ms1)
  sample <- .hm_normalise_table(sample)

  if (!"Replicate.Name" %in% names(ms1)) {
    stop("`ms1` must contain a 'Replicate.Name' column.", call. = FALSE)
  }
  if (!"Replicate.Name" %in% names(sample)) {
    stop("`sample` must contain a 'Replicate.Name' column.", call. = FALSE)
  }
  if (!"Group" %in% names(sample)) {
    stop("`sample` must contain a 'Group' column.", call. = FALSE)
  }
  if (!area_col %in% names(ms1)) {
    stop("Area column '", area_col, "' was not found in `ms1`. ",
         "Available columns: ", paste(names(ms1), collapse = ", "),
         call. = FALSE)
  }
  if (!"Isotope.Label.Type" %in% names(ms1)) {
    stop("`ms1` must contain an 'Isotope.Label.Type' column.", call. = FALSE)
  }

  if (identical(mode, "spike_in")) {
    if (is.null(spike_in_col) || length(spike_in_col) != 1L ||
        is.na(spike_in_col) || !nzchar(trimws(spike_in_col))) {
      stop("mode = 'spike_in' requires `spike_in_col`.", call. = FALSE)
    }
    spike_in_col <- trimws(spike_in_col)
    if (!spike_in_col %in% names(sample)) {
      stop("Spike-in column '", spike_in_col, "' was not found in `sample`.",
           call. = FALSE)
    }
    spike_raw <- suppressWarnings(as.numeric(as.character(sample[[spike_in_col]])))
    if (all(is.na(spike_raw))) {
      stop("Spike-in column '", spike_in_col, "' contains no numeric values.",
           call. = FALSE)
    }
  } else if (!is.null(spike_in_col)) {
    warning("`spike_in_col` is ignored unless mode = 'spike_in'.",
            call. = FALSE)
    spike_in_col <- NULL
  }

  if (identical(mode, "percentage") && !identical(normalize, "none")) {
    warning("`normalize` is only applied to ratio modes; ignored for percentage mode.",
            call. = FALSE)
  }

  ms1$Replicate.Name <- as.character(ms1$Replicate.Name)
  ms1$Protein.Name <- as.character(ms1$Protein.Name)
  ms1$Peptide.Note <- as.character(ms1$Peptide.Note)
  ms1$Isotope.Label.Type <- tolower(trimws(as.character(ms1$Isotope.Label.Type)))
  area <- suppressWarnings(as.numeric(as.character(ms1[[area_col]])))

  valid_rows <- !is.na(ms1$Replicate.Name) & nzchar(ms1$Replicate.Name) &
    !is.na(ms1$Protein.Name) & !is.na(ms1$Peptide.Note)
  ms1 <- ms1[valid_rows, , drop = FALSE]
  area <- area[valid_rows]

  key_cols <- c("Replicate.Name", "Protein.Name", "Peptide.Note")

  if (identical(mode, "percentage")) {
    light <- ms1$Isotope.Label.Type == "light"
    light[is.na(light)] <- FALSE
    if (!any(light)) {
      return(.hm_empty_quantification(mode, normalize, spike_in_col, area_col))
    }

    aggregated <- .hm_aggregate_areas(
      ms1[light, , drop = FALSE],
      by_cols = key_cols,
      area = area[light]
    )

    totals <- stats::aggregate(
      aggregated$area,
      by = list(
        Replicate.Name = aggregated$Replicate.Name,
        Protein.Name = aggregated$Protein.Name
      ),
      FUN = function(v) sum(v, na.rm = TRUE)
    )
    names(totals)[3L] <- "Protein.Area.Sum"

    out <- merge(
      aggregated, totals,
      by = c("Replicate.Name", "Protein.Name"),
      sort = FALSE
    )
    valid_total <- is.finite(out$Protein.Area.Sum) & out$Protein.Area.Sum > 0
    out$Percentage <- NA_real_
    out$Percentage[valid_total] <- out$area[valid_total] /
      out$Protein.Area.Sum[valid_total] * 100
    out$Value <- out$Percentage

    out <- merge(
      out,
      .hm_sample_metadata(sample),
      by = "Replicate.Name",
      all.x = TRUE,
      sort = FALSE
    )
    out <- out[, c("Replicate.Name", "Protein.Name", "Peptide.Note",
                   "Group", "Value", "Percentage"), drop = FALSE]
  } else {
    isotope <- ms1$Isotope.Label.Type
    usable <- !is.na(isotope) & isotope %in% c("light", "heavy")
    if (!any(usable)) {
      return(.hm_empty_quantification(mode, normalize, spike_in_col, area_col))
    }

    aggregated <- .hm_aggregate_areas(
      ms1[usable, , drop = FALSE],
      by_cols = c(key_cols, "Isotope.Label.Type"),
      area = area[usable]
    )

    light <- aggregated[aggregated$Isotope.Label.Type == "light",
                        c(key_cols, "area"), drop = FALSE]
    names(light)[names(light) == "area"] <- "light_area"
    heavy <- aggregated[aggregated$Isotope.Label.Type == "heavy",
                        c(key_cols, "area"), drop = FALSE]
    names(heavy)[names(heavy) == "area"] <- "heavy_area"

    out <- merge(light, heavy, by = key_cols, all = TRUE, sort = FALSE)
    ratio_valid <- !is.na(out$light_area) & !is.na(out$heavy_area) &
      out$heavy_area != 0
    out$Ratio <- NA_real_
    out$Ratio[ratio_valid] <- out$light_area[ratio_valid] /
      out$heavy_area[ratio_valid]

    normalized_ratio <- out$Ratio
    if (!identical(normalize, "none") && nrow(out) > 0L) {
      peptide_group <- paste(out$Protein.Name, out$Peptide.Note, sep = "\r")
      normalized_ratio <- normalize_values(
        out$Ratio,
        method = normalize,
        group = peptide_group
      )
    }
    # Attach Value before merging metadata so that row alignment is guaranteed
    # even if merge() reorders rows.
    out$Value <- normalized_ratio

    extra_cols <- if (identical(mode, "spike_in")) spike_in_col else NULL
    out <- merge(
      out,
      .hm_sample_metadata(sample, extra_cols = extra_cols),
      by = "Replicate.Name",
      all.x = TRUE,
      sort = FALSE
    )

    if (identical(mode, "spike_in")) {
      spike_factor <- suppressWarnings(as.numeric(as.character(out[[spike_in_col]])))
      invalid_factor <- is.na(spike_factor) | !is.finite(spike_factor) |
        spike_factor <= 0
      out$Value <- out$Value / spike_factor
      out$Value[invalid_factor] <- NA_real_
      if (any(invalid_factor & !is.na(out$Ratio))) {
        warning("Spike-in factor is missing, zero or non-finite for ",
                sum(invalid_factor & !is.na(out$Ratio)),
                " ratio(s); those values were set to NA.", call. = FALSE)
      }
      out <- out[, c("Replicate.Name", "Protein.Name", "Peptide.Note",
                     "Group", "Value", "Ratio"), drop = FALSE]
    } else {
      out <- out[, c("Replicate.Name", "Protein.Name", "Peptide.Note",
                     "Group", "Value", "Ratio"), drop = FALSE]
    }
  }

  out <- out[order(out$Replicate.Name, out$Protein.Name, out$Peptide.Note), ,
             drop = FALSE]
  rownames(out) <- NULL

  .hm_set_quantity_attributes(out, mode, normalize, spike_in_col, area_col)
}
