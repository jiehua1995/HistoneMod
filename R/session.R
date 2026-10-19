##### HistoneMod: Session Persistence #####
# Save and restore a complete analysis input (MS1 table + sample annotation
# table + user settings) as a single compressed RDS file. The format tag and
# version allow future releases to detect and migrate older session files.

# Internal: version stamp written into session files.
.hm_session_version <- function() {
  version <- tryCatch(
    utils::packageDescription("HistoneMod", fields = "Version"),
    error = function(e) NA_character_,
    warning = function(w) NA_character_
  )
  version <- as.character(version)[1L]
  if (is.na(version) || !nzchar(trimws(version))) {
    version <- "0.8.0"
  }
  trimws(version)
}

# Internal: validate that a loaded object is a HistoneMod session.
.hm_validate_session <- function(x, file = NULL) {
  label <- if (is.null(file)) "session object" else paste0("file '", file, "'")
  if (!is.list(x)) {
    stop("The ", label, " is not a HistoneMod session: expected a list.",
         call. = FALSE)
  }
  if (!identical(x$format, "HistoneMod-session")) {
    stop("The ", label,
         " is not a HistoneMod session: missing or invalid 'format' tag.",
         call. = FALSE)
  }
  if (is.null(x$ms1) || !is.data.frame(x$ms1)) {
    stop("The ", label, " is missing a valid 'ms1' data frame.", call. = FALSE)
  }
  if (is.null(x$sample) || !is.data.frame(x$sample)) {
    stop("The ", label, " is missing a valid 'sample' data frame.", call. = FALSE)
  }
  if (!is.list(x$settings)) {
    stop("The ", label, " has an invalid 'settings' element.", call. = FALSE)
  }
  invisible(x)
}

#' Save a HistoneMod analysis session
#'
#' Writes the MS1 table, the sample annotation table and any user settings to a
#' single compressed RDS file, together with a format tag, the package version
#' and a creation timestamp. The file can be restored with [load_session()].
#'
#' @param file Path to the output file. The conventional extension is `.rds`.
#' @param ms1 A data frame with peptide-level MS1 measurements.
#' @param sample A data frame with sample metadata.
#' @param settings A list of user settings to store with the data. Defaults to
#'   an empty list.
#'
#' @return Invisibly returns the normalised path to the saved file.
#'
#' @export
save_session <- function(file, ms1, sample, settings = list()) {
  if (!is.character(file) || length(file) != 1L || is.na(file) ||
      !nzchar(trimws(file))) {
    stop("`file` must be a single non-empty file path.", call. = FALSE)
  }
  if (!is.data.frame(ms1)) {
    stop("`ms1` must be a data frame.", call. = FALSE)
  }
  if (!is.data.frame(sample)) {
    stop("`sample` must be a data frame.", call. = FALSE)
  }
  if (is.null(settings)) settings <- list()
  if (!is.list(settings)) {
    stop("`settings` must be a list.", call. = FALSE)
  }

  session <- list(
    format = "HistoneMod-session",
    version = .hm_session_version(),
    created = Sys.time(),
    ms1 = ms1,
    sample = sample,
    settings = settings
  )

  tryCatch(
    saveRDS(session, file, compress = "xz"),
    error = function(e) {
      stop("Could not save session to '", file, "': ",
           conditionMessage(e), call. = FALSE)
    }
  )

  invisible(normalizePath(file, mustWork = FALSE))
}

#' Load a HistoneMod analysis session
#'
#' Reads a session file written by [save_session()] and validates its structure
#' before returning it. Informative errors are raised when the file does not
#' exist, cannot be read, or is not a HistoneMod session.
#'
#' @param file Path to a session file created by [save_session()].
#'
#' @return Invisibly returns a list with elements `format`, `version`,
#'   `created`, `ms1`, `sample` and `settings`.
#'
#' @export
load_session <- function(file) {
  if (!is.character(file) || length(file) != 1L || is.na(file) ||
      !nzchar(trimws(file))) {
    stop("`file` must be a single non-empty file path.", call. = FALSE)
  }
  if (!file.exists(file)) {
    stop("Session file does not exist: ", file, call. = FALSE)
  }

  session <- tryCatch(
    readRDS(file),
    error = function(e) {
      stop("Could not read session file '", file, "': ",
           conditionMessage(e), call. = FALSE)
    }
  )

  .hm_validate_session(session, file = file)
  invisible(session)
}
