##### Project files and reproducible analysis bundles #####

# Internal helper: package version with a safe fallback.
.hm_project_version <- function() {
  tryCatch(
    histonemod_package_version(),
    error = function(e) "0.8.0"
  )
}

#' Save a HistoneMod project
#'
#' Writes the input tables, analysis settings and optional analysis results to a
#' single compressed RDS file. Unlike [save_session()], a project can also carry
#' derived results (differential tables, occupancy tables, QC summaries) so a
#' complete analysis can be restored later.
#'
#' @param file Output file path (conventionally `.rds`).
#' @param ms1 MS1 data frame.
#' @param sample Sample annotation data frame.
#' @param settings Named list of analysis settings.
#' @param results Named list of derived analysis results.
#'
#' @return Invisibly returns the normalised file path.
#' @export
save_project <- function(file, ms1, sample, settings = list(), results = list()) {
  if (!is.character(file) || length(file) != 1L || is.na(file) || !nzchar(trimws(file))) {
    stop("`file` must be a single non-empty file path.", call. = FALSE)
  }
  if (!is.data.frame(ms1) || !is.data.frame(sample)) {
    stop("`ms1` and `sample` must be data frames.", call. = FALSE)
  }
  if (is.null(settings)) settings <- list()
  if (is.null(results)) results <- list()
  if (!is.list(settings) || !is.list(results)) {
    stop("`settings` and `results` must be lists.", call. = FALSE)
  }

  project <- list(
    format = "HistoneMod-project",
    version = .hm_project_version(),
    created = Sys.time(),
    ms1 = ms1,
    sample = sample,
    settings = settings,
    results = results
  )

  tryCatch(
    saveRDS(project, file, compress = "xz"),
    error = function(e) {
      stop("Could not save project to '", file, "': ", conditionMessage(e), call. = FALSE)
    }
  )

  invisible(normalizePath(file, mustWork = FALSE))
}

#' Load a HistoneMod project
#'
#' @param file A project file created by [save_project()].
#'
#' @return The project list with elements `format`, `version`, `created`,
#'   `ms1`, `sample`, `settings` and `results`.
#' @export
load_project <- function(file) {
  if (!is.character(file) || length(file) != 1L || is.na(file) || !nzchar(trimws(file))) {
    stop("`file` must be a single non-empty file path.", call. = FALSE)
  }
  if (!file.exists(file)) stop("Project file does not exist: ", file, call. = FALSE)

  project <- tryCatch(
    readRDS(file),
    error = function(e) {
      stop("Could not read project file '", file, "': ", conditionMessage(e), call. = FALSE)
    }
  )

  if (!is.list(project) || !identical(project$format, "HistoneMod-project")) {
    stop("File is not a HistoneMod project: ", file, call. = FALSE)
  }
  if (!is.data.frame(project$ms1) || !is.data.frame(project$sample)) {
    stop("Project file is missing the input tables.", call. = FALSE)
  }
  if (is.null(project$settings)) project$settings <- list()
  if (is.null(project$results)) project$results <- list()

  project
}

#' Export a reproducible analysis bundle
#'
#' Writes the input tables, project file, settings, a short README and an R
#' script that reproduces the standard analysis. This is intended for sharing an
#' analysis with collaborators or archiving it alongside a manuscript.
#'
#' @param dir Output directory (created if needed).
#' @param ms1 MS1 data frame.
#' @param sample Sample annotation data frame.
#' @param settings Named list of analysis settings.
#' @param results Named list of derived results (optional).
#'
#' @return Invisibly returns the output directory.
#' @export
export_analysis_bundle <- function(dir, ms1, sample, settings = list(), results = list()) {
  if (!is.character(dir) || length(dir) != 1L || is.na(dir) || !nzchar(trimws(dir))) {
    stop("`dir` must be a single non-empty directory path.", call. = FALSE)
  }
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(dir)) stop("Could not create output directory: ", dir, call. = FALSE)

  utils::write.csv(ms1, file.path(dir, "MS1.csv"), row.names = FALSE)
  utils::write.csv(sample, file.path(dir, "samples.csv"), row.names = FALSE)
  save_project(file.path(dir, "project.rds"), ms1, sample, settings, results)

  if (requireNamespace("jsonlite", quietly = TRUE)) {
    jsonlite::write_json(settings, file.path(dir, "settings.json"),
                         auto_unbox = TRUE, pretty = TRUE)
  } else {
    utils::capture.output(utils::str(settings), file = file.path(dir, "settings.txt"))
  }

  script <- c(
    "# Reproducible HistoneMod analysis bundle",
    "library(HistoneMod)",
    "",
    "ms1 <- utils::read.csv('MS1.csv', check.names = TRUE)",
    "sample <- utils::read.csv('samples.csv', check.names = TRUE)",
    "",
    "data <- HistoneMod::percentage_calculation(ms1, sample)",
    "HistoneMod::plot_pca(data)",
    "HistoneMod::plot_heatmap(data)",
    "",
    "# Restore the exact inputs and settings",
    "project <- HistoneMod::load_project('project.rds')",
    "str(project$settings)",
    "str(project$results)"
  )
  writeLines(script, file.path(dir, "analysis.R"))

  readme <- c(
    "# HistoneMod analysis bundle",
    "",
    paste0("Created with HistoneMod ", .hm_project_version(), " on ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    "",
    "Files:",
    "- `MS1.csv`: MS1 input table",
    "- `samples.csv`: sample annotation table",
    "- `project.rds`: inputs, settings and derived results",
    "- `analysis.R`: script that reproduces the standard analysis",
    "- `settings.json` (or `settings.txt`): analysis settings",
    "",
    "Run `analysis.R` in R from this directory to reproduce the analysis."
  )
  writeLines(readme, file.path(dir, "README.md"))

  invisible(normalizePath(dir, mustWork = FALSE))
}
