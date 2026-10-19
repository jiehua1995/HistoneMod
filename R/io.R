##### HistoneMod: Input/Output Helpers #####
# Reusable helpers for reading Skyline-like CSV exports, inferring sample
# annotation tables, mapping non-canonical column names and validating the
# MS1/sample table pair before quantification.
#
# These helpers are independent of the Shiny app and use only base R; the
# optional `data.table` package is used for faster CSV reading when installed.

# Internal: normalise column names the same way throughout the package.
# Leading/trailing whitespace is removed and any run of whitespace becomes a
# dot, matching the conventions used in `R/functions.R`.
.hm_normalise_table <- function(data) {
  if (!is.data.frame(data)) {
    data <- tryCatch(
      as.data.frame(data, stringsAsFactors = FALSE),
      error = function(e) {
        stop("Input must be coercible to a data frame: ", conditionMessage(e),
             call. = FALSE)
      }
    )
  }
  names(data) <- trimws(names(data))
  names(data) <- gsub("\\s+", ".", names(data))
  data
}

# Internal: read the first line of a text file as raw bytes and detect a
# UTF-8 byte-order mark (BOM). Returns a list with the decoded first line and a
# logical `bom` flag.
.hm_first_line <- function(path) {
  con <- file(path, open = "rb")
  on.exit(close(con), add = TRUE)

  bytes <- raw(0)
  repeat {
    chunk <- readBin(con, what = "raw", n = 8192L)
    if (length(chunk) == 0L) break
    bytes <- c(bytes, chunk)
    nl <- which(bytes == as.raw(0x0aL))
    if (length(nl) > 0L) {
      bytes <- bytes[seq_len(nl[1L] - 1L)]
      break
    }
    # Guard against pathological files without any newline.
    if (length(bytes) > 5e6) break
  }

  bom <- length(bytes) >= 3L &&
    identical(bytes[1:3], as.raw(c(0xEF, 0xBB, 0xBF)))
  if (bom) bytes <- bytes[-(1:3)]

  list(line = rawToChar(bytes), bom = bom)
}

# Internal: pick the delimiter that produces the most fields in the header.
.hm_detect_delimiter <- function(header_line, candidates = c(",", ";", "\t")) {
  counts <- vapply(
    candidates,
    function(d) length(strsplit(header_line, d, fixed = TRUE)[[1L]]),
    integer(1)
  )
  candidates[which.max(counts)]
}

# Internal: does a character vector contain decimal-comma numbers, e.g.
# "12,34" as produced in locales such as de_DE?
.hm_has_decimal_comma <- function(x) {
  if (!is.character(x)) return(FALSE)
  x <- x[!is.na(x)]
  any(grepl("^\\s*[-+]?[0-9]+,[0-9]+\\s*$", x))
}

# Internal: canonical column names required by the MS1 and sample tables.
.hm_required_ms1_cols <- c(
  "Protein.Name", "Peptide.Note", "Replicate.Name",
  "Total.Area.MS1", "Isotope.Label.Type"
)
.hm_required_sample_cols <- c("Replicate.Name", "Group", "Replicate.No")

#' Read a Skyline-like CSV file
#'
#' Reads a peptide-level CSV export such as a Skyline MS1 report. The
#' delimiter is detected automatically from the header (comma, semicolon or
#' tab) unless `delim` is supplied, a UTF-8 byte-order mark is stripped, and
#' column names are normalised by replacing whitespace with dots. Reading uses
#' `data.table::fread()` when the optional `data.table` package is available
#' and falls back to [utils::read.csv()] otherwise.
#'
#' @param path Path to the CSV file.
#' @param delim Optional single-character delimiter. If `NULL` (the default)
#'   it is detected from the header. Must be one of `","`, `";"` or `"\t"`.
#' @param na.strings Character vector of strings that should be interpreted as
#'   missing values.
#' @param check.names Logical; passed to the CSV reader. When `TRUE`, names are
#'   made syntactically valid before the whitespace-to-dot normalisation.
#'
#' @return A data frame with normalised column names. The detected delimiter
#'   and whether a UTF-8 BOM was present are available as the `delimiter` and
#'   `bom` attributes.
#'
#' @export
read_skyline_csv <- function(path, delim = NULL,
                             na.strings = c("", "NA", "#N/A"),
                             check.names = TRUE) {
  if (!is.character(path) || length(path) != 1L || is.na(path) ||
      !nzchar(trimws(path))) {
    stop("`path` must be a single non-empty file path.", call. = FALSE)
  }
  if (!file.exists(path)) {
    stop("File does not exist: ", path, call. = FALSE)
  }
  if (!is.character(na.strings)) {
    stop("`na.strings` must be a character vector.", call. = FALSE)
  }

  first <- .hm_first_line(path)
  if (!nzchar(trimws(first$line))) {
    stop("File appears to be empty: ", path, call. = FALSE)
  }

  if (is.null(delim)) {
    delim <- .hm_detect_delimiter(first$line)
  } else {
    delim <- as.character(delim)[1L]
    if (!delim %in% c(",", ";", "\t")) {
      stop("`delim` must be one of \",\", \";\" or \"\\t\".", call. = FALSE)
    }
  }

  header_fields <- strsplit(first$line, delim, fixed = TRUE)[[1L]]
  comma_in_header <- .hm_has_decimal_comma(header_fields)

  use_data_table <- requireNamespace("data.table", quietly = TRUE)

  read_one <- function(encoding) {
    if (use_data_table) {
      data.table::fread(
        file = path,
        sep = delim,
        na.strings = na.strings,
        data.table = FALSE,
        check.names = check.names,
        integer64 = "double",
        encoding = "UTF-8",
        showProgress = FALSE
      )
    } else {
      utils::read.csv(
        file = path,
        sep = delim,
        na.strings = na.strings,
        check.names = check.names,
        stringsAsFactors = FALSE,
        fileEncoding = encoding
      )
    }
  }

  encodings <- unique(c(if (first$bom) "UTF-8-BOM" else "UTF-8", ""))
  df <- NULL
  last_error <- NULL
  for (encoding in encodings) {
    df <- tryCatch(
      read_one(encoding),
      error = function(e) {
        last_error <<- e
        NULL
      }
    )
    if (!is.null(df)) break
  }
  if (is.null(df)) {
    stop(
      "Could not read CSV file '", path, "': ",
      if (is.null(last_error)) "unknown error" else conditionMessage(last_error),
      call. = FALSE
    )
  }

  df <- as.data.frame(df, stringsAsFactors = FALSE)
  names(df) <- trimws(names(df))
  names(df) <- gsub("\\s+", ".", names(df))
  # A BOM may survive as a literal character when the encoding fallback was
  # used; strip it from the first column name.
  if (length(names(df)) > 0L) {
    names(df)[1L] <- sub("^\ufeff", "", names(df)[1L])
    names(df)[1L] <- sub("^\u00ef\u00bb\u00bf", "", names(df)[1L])
  }

  decimal_comma_data <- any(vapply(df, .hm_has_decimal_comma, logical(1)))
  if (identical(delim, ";") || comma_in_header || decimal_comma_data) {
    warning(
      "Possible decimal-comma locale detected: ",
      if (identical(delim, ";")) "semicolon delimiter" else
        "comma used as decimal separator",
      ". Check that numeric columns were parsed with the intended decimal separator.",
      call. = FALSE
    )
  }

  attr(df, "delimiter") <- delim
  attr(df, "bom") <- first$bom
  df
}

#' Infer a sample sheet from replicate names
#'
#' Builds a sample annotation table from the replicate names found in an MS1
#' table. By default replicate names are expected to look like
#' `"SampleA_rep1"`, where the suffix `_rep<number>` encodes the replicate
#' number. The group is the replicate name with that suffix removed.
#'
#' When a replicate name does not match `pattern`, a warning is emitted and the
#' affected name is used as its own group with `Replicate.No` numbered within
#' that group (which yields `1` for a name-group). This fallback keeps the
#' function usable for data sets that use a different naming scheme, but the
#' resulting sample sheet should be checked manually.
#'
#' @param ms1 A data frame (or character vector) containing the MS1
#'   measurements or replicate names.
#' @param replicate_col Name of the column holding replicate names when `ms1`
#'   is a data frame.
#' @param pattern Regular expression matching the replicate suffix. The first
#'   capture group (if any) should contain the replicate number.
#'
#' @return A data frame with columns `Replicate.Name`, `Group` and
#'   `Replicate.No` (integer), ready to be edited and used as the sample table.
#'   One row is returned per unique replicate, in order of first appearance.
#'
#' @export
infer_sample_sheet <- function(ms1, replicate_col = "Replicate.Name",
                               pattern = "_rep(\\d+)$") {
  if (is.character(ms1)) {
    replicate_names <- ms1
  } else {
    ms1 <- .hm_normalise_table(ms1)
    if (!replicate_col %in% names(ms1)) {
      stop(
        "Column '", replicate_col, "' not found in `ms1`. Available columns: ",
        paste(names(ms1), collapse = ", "),
        call. = FALSE
      )
    }
    replicate_names <- ms1[[replicate_col]]
  }

  replicate_names <- unique(as.character(replicate_names))
  replicate_names <- replicate_names[
    !is.na(replicate_names) & nzchar(trimws(replicate_names))
  ]
  if (length(replicate_names) == 0L) {
    stop("No replicate names found in `ms1`.", call. = FALSE)
  }
  replicate_names <- trimws(replicate_names)

  matched <- tryCatch(
    grepl(pattern, replicate_names),
    error = function(e) {
      stop("`pattern` is not a valid regular expression: ",
           conditionMessage(e), call. = FALSE)
    }
  )

  group <- ifelse(matched, sub(pattern, "", replicate_names), replicate_names)
  group <- trimws(group)

  replicate_no <- rep(NA_integer_, length(replicate_names))
  # Use the trailing digit run of the replicate name, as in
  # as.integer(sub(".*?(\\d+)$", "\\1", name)).
  replicate_no[matched] <- suppressWarnings(as.integer(
    sub(".*?(\\d+)$", "\\1", replicate_names[matched])
  ))
  # Final fallback: number rows within each group.
  needs_fallback <- is.na(replicate_no)
  if (any(needs_fallback)) {
    idx <- which(needs_fallback)
    replicate_no[idx] <- as.integer(
      stats::ave(seq_along(idx), group[idx], FUN = seq_along)
    )
  }

  if (any(!matched)) {
    warning(
      sum(!matched), " replicate name(s) did not match pattern '", pattern,
      "' and were treated as their own group: ",
      paste(utils::head(replicate_names[!matched], 5L), collapse = ", "),
      if (sum(!matched) > 5L) ", ..." else "",
      call. = FALSE
    )
  }

  data.frame(
    Replicate.Name = replicate_names,
    Group = group,
    Replicate.No = as.integer(replicate_no),
    stringsAsFactors = FALSE
  )
}

#' Map non-canonical columns to canonical names
#'
#' Renames source columns to the canonical column names used by HistoneMod.
#' `mapping` is a named character vector (or named list) in which the names are
#' the canonical names and the values are the column names as they appear in
#' `data`. Column names in `data` are first normalised (whitespace to dots), so
#' both `"Replicate Name"` and `"Replicate.Name"` can be used as sources.
#'
#' Columns that are not listed in `mapping` are kept unchanged, so arbitrary
#' extra Skyline columns do not break the mapping.
#'
#' @param data A data frame to rename.
#' @param mapping Named character vector or named list with canonical names as
#'   the names and source column names as the values.
#'
#' @return A data frame with the mapped columns renamed. Original row order and
#'   extra columns are preserved.
#'
#' @export
map_columns <- function(data, mapping) {
  data <- .hm_normalise_table(data)

  if (is.list(mapping) && !is.character(mapping)) {
    mapping <- unlist(mapping, use.names = TRUE)
  }
  if (!is.character(mapping) || length(mapping) == 0L) {
    stop("`mapping` must be a non-empty named character vector or named list.",
         call. = FALSE)
  }
  if (is.null(names(mapping)) || any(!nzchar(names(mapping)))) {
    stop("`mapping` must be named: names are canonical columns, values are source columns.",
         call. = FALSE)
  }
  if (anyDuplicated(names(mapping))) {
    stop("`mapping` contains duplicated canonical column names.",
         call. = FALSE)
  }

  canonical <- gsub("\\s+", ".", trimws(names(mapping)))
  source <- gsub("\\s+", ".", trimws(as.character(unname(mapping))))

  if (anyNA(source) || any(!nzchar(source))) {
    stop("`mapping` contains empty source column names.", call. = FALSE)
  }
  if (anyDuplicated(source)) {
    stop("`mapping` maps more than one canonical name to the same source column.",
         call. = FALSE)
  }

  missing_sources <- source[!source %in% names(data)]
  if (length(missing_sources) > 0L) {
    stop(
      "Source column(s) not found in `data`: ",
      paste(missing_sources, collapse = ", "),
      ". Available columns: ",
      paste(names(data), collapse = ", "),
      call. = FALSE
    )
  }

  collisions <- canonical[canonical %in% names(data) & canonical != source]
  if (length(collisions) > 0L) {
    stop(
      "Cannot map column(s) because the canonical name already exists in `data`: ",
      paste(collisions, collapse = ", "),
      call. = FALSE
    )
  }

  for (i in seq_along(canonical)) {
    if (!identical(canonical[i], source[i])) {
      names(data)[names(data) == source[i]] <- canonical[i]
    }
  }

  data
}

#' Validate MS1 and sample input tables
#'
#' Checks that the MS1 and sample tables contain the columns required by
#' HistoneMod, that the tables are non-empty, that areas are numeric, that
#' replicate names are unique in the sample sheet and that the two tables refer
#' to a consistent set of replicates. Column names are normalised
#' (whitespace to dots) as a side effect and the normalised tables are returned
#' alongside the validation messages.
#'
#' Replicate-name mismatches between the tables are reported as warnings
#' because partially annotated data sets can still be analysed (the offending
#' samples are simply absent after merging). Duplicated replicate names in the
#' sample sheet, missing required columns, empty tables and duplicated
#' `Replicate.Name` x `Peptide.Note` x `Isotope.Label.Type` combinations are
#' errors. A complete lack of overlap between the two tables is also an error.
#'
#' @param ms1 A data frame containing peptide-level MS1 measurements.
#' @param sample A data frame containing sample metadata.
#'
#' @return A list with elements:
#'   \describe{
#'     \item{valid}{Logical; `TRUE` when no errors were found.}
#'     \item{errors}{Character vector of fatal validation errors.}
#'     \item{warnings}{Character vector of non-fatal validation warnings.}
#'     \item{ms1}{The normalised MS1 table (best effort, even when invalid).}
#'     \item{sample}{The normalised sample table (best effort, even when invalid).}
#'   }
#'
#' @export
validate_input_tables <- function(ms1, sample) {
  errors <- character(0)
  warnings <- character(0)
  add_error <- function(...) errors <<- c(errors, paste0(...))
  add_warning <- function(...) warnings <<- c(warnings, paste0(...))

  ms1_ok <- TRUE
  sample_ok <- TRUE

  if (is.null(ms1)) {
    add_error("`ms1` is NULL; an MS1 data frame is required.")
    ms1_ok <- FALSE
  } else {
    ms1 <- tryCatch(
      .hm_normalise_table(ms1),
      error = function(e) {
        add_error("`ms1` is not a valid data frame: ", conditionMessage(e))
        NULL
      }
    )
    if (is.null(ms1)) ms1_ok <- FALSE
  }

  if (is.null(sample)) {
    add_error("`sample` is NULL; a sample data frame is required.")
    sample_ok <- FALSE
  } else {
    sample <- tryCatch(
      .hm_normalise_table(sample),
      error = function(e) {
        add_error("`sample` is not a valid data frame: ", conditionMessage(e))
        NULL
      }
    )
    if (is.null(sample)) sample_ok <- FALSE
  }

  # ---- MS1 table checks ---------------------------------------------------
  if (ms1_ok) {
    if (nrow(ms1) == 0L) add_error("MS1 table is empty.")

    missing_ms1 <- setdiff(.hm_required_ms1_cols, names(ms1))
    if (length(missing_ms1) > 0L) {
      add_error("Missing required column(s) in MS1: ",
                paste(missing_ms1, collapse = ", "), ".")
    }

    if ("Replicate.Name" %in% names(ms1)) {
      ms1$Replicate.Name <- trimws(as.character(ms1$Replicate.Name))
      bad_rep <- is.na(ms1$Replicate.Name) | !nzchar(ms1$Replicate.Name)
      if (any(bad_rep)) {
        add_error("MS1 contains ", sum(bad_rep),
                  " row(s) with a missing or empty Replicate.Name.")
      }
    }

    for (col in c("Protein.Name", "Peptide.Note")) {
      if (col %in% names(ms1)) {
        values <- trimws(as.character(ms1[[col]]))
        ms1[[col]] <- values
        n_missing <- sum(is.na(values) | !nzchar(values))
        if (n_missing > 0L) {
          add_warning("MS1 contains ", n_missing, " row(s) with a missing or empty ",
                      col, "; these cannot be grouped reliably.")
        }
      }
    }

    if ("Total.Area.MS1" %in% names(ms1)) {
      area_raw <- ms1$Total.Area.MS1
      area_num <- suppressWarnings(as.numeric(as.character(area_raw)))
      bad_area <- !is.na(area_raw) & is.na(area_num)
      if (any(bad_area)) {
        examples <- unique(as.character(area_raw[bad_area]))
        add_error(
          "Total.Area.MS1 contains ", sum(bad_area),
          " non-numeric value(s), e.g. ",
          paste(utils::head(examples, 3L), collapse = ", "), "."
        )
      }
      n_na_area <- sum(is.na(area_num))
      if (n_na_area > 0L && !any(bad_area)) {
        add_warning("Total.Area.MS1 contains ", n_na_area,
                    " missing value(s); missing areas are ignored when summing.")
      }
      if (nrow(ms1) > 0L && all(is.na(area_num))) {
        add_error("Total.Area.MS1 contains no numeric values.")
      }
      ms1$Total.Area.MS1 <- area_num
    }

    if ("Isotope.Label.Type" %in% names(ms1)) {
      isotope <- tolower(trimws(as.character(ms1$Isotope.Label.Type)))
      ms1$Isotope.Label.Type <- isotope
      unexpected <- unique(isotope[!is.na(isotope) & !isotope %in% c("light", "heavy")])
      if (length(unexpected) > 0L) {
        add_warning("Unexpected Isotope.Label.Type value(s): ",
                    paste(unexpected, collapse = ", "),
                    ". Only 'light' and 'heavy' are used for quantification.")
      }
    }

    key_cols <- c("Replicate.Name", "Peptide.Note", "Isotope.Label.Type")
    if (all(key_cols %in% names(ms1)) && nrow(ms1) > 0L) {
      dup <- duplicated(ms1[, key_cols, drop = FALSE])
      if (any(dup)) {
        add_error("MS1 contains ", sum(dup),
                  " duplicated Replicate.Name x Peptide.Note x Isotope.Label.Type combination(s).")
      }
    }
  }

  # ---- sample table checks ------------------------------------------------
  if (sample_ok) {
    if (nrow(sample) == 0L) add_error("Sample table is empty.")

    missing_sample <- setdiff(.hm_required_sample_cols, names(sample))
    if (length(missing_sample) > 0L) {
      add_error("Missing required column(s) in sample: ",
                paste(missing_sample, collapse = ", "), ".")
    }

    if ("Replicate.Name" %in% names(sample)) {
      sample$Replicate.Name <- trimws(as.character(sample$Replicate.Name))
      bad_rep <- is.na(sample$Replicate.Name) | !nzchar(sample$Replicate.Name)
      if (any(bad_rep)) {
        add_error("Sample table contains ", sum(bad_rep),
                  " row(s) with a missing or empty Replicate.Name.")
      }
      dup_sample <- duplicated(sample$Replicate.Name[!bad_rep])
      if (any(dup_sample)) {
        dup_names <- unique(sample$Replicate.Name[!bad_rep][dup_sample])
        add_error("Sample table contains duplicated Replicate.Name value(s): ",
                  paste(utils::head(dup_names, 5L), collapse = ", "),
                  if (length(dup_names) > 5L) ", ..." else "", ".")
      }
    }

    if ("Group" %in% names(sample)) {
      sample$Group <- trimws(as.character(sample$Group))
      n_missing_group <- sum(is.na(sample$Group) | !nzchar(sample$Group))
      if (n_missing_group > 0L) {
        add_warning("Sample table contains ", n_missing_group,
                    " row(s) with a missing or empty Group.")
      }
    }

    if ("Replicate.No" %in% names(sample)) {
      rep_raw <- sample$Replicate.No
      rep_num <- suppressWarnings(as.numeric(as.character(rep_raw)))
      bad_rep_no <- !is.na(rep_raw) & is.na(rep_num)
      if (any(bad_rep_no)) {
        add_warning("Sample table contains ", sum(bad_rep_no),
                    " non-numeric Replicate.No value(s).")
      }
      if (!any(bad_rep_no)) {
        sample$Replicate.No <- as.integer(rep_num)
      }
    }
  }

  # ---- cross-table checks -------------------------------------------------
  if (ms1_ok && sample_ok &&
      all(c("Replicate.Name") %in% names(ms1)) &&
      all(c("Replicate.Name") %in% names(sample))) {
    ms1_reps <- unique(ms1$Replicate.Name)
    sample_reps <- unique(sample$Replicate.Name)
    ms1_reps <- ms1_reps[!is.na(ms1_reps) & nzchar(ms1_reps)]
    sample_reps <- sample_reps[!is.na(sample_reps) & nzchar(sample_reps)]

    only_ms1 <- setdiff(ms1_reps, sample_reps)
    only_sample <- setdiff(sample_reps, ms1_reps)

    if (length(only_ms1) > 0L) {
      add_warning("Replicate name(s) present in MS1 but missing from the sample table: ",
                  paste(utils::head(only_ms1, 5L), collapse = ", "),
                  if (length(only_ms1) > 5L) ", ..." else "", ".")
    }
    if (length(only_sample) > 0L) {
      add_warning("Replicate name(s) present in the sample table but missing from MS1: ",
                  paste(utils::head(only_sample, 5L), collapse = ", "),
                  if (length(only_sample) > 5L) ", ..." else "", ".")
    }
    if (length(intersect(ms1_reps, sample_reps)) == 0L &&
        length(ms1_reps) > 0L && length(sample_reps) > 0L) {
      add_error("No overlapping Replicate.Name values between MS1 and sample tables.")
    }
  }

  list(
    valid = length(errors) == 0L,
    errors = errors,
    warnings = warnings,
    ms1 = ms1,
    sample = sample
  )
}
