##### HistoneMod Package Entrypoints #####
# This file exposes the package-level helpers used to launch the Shiny app.

HISTONEMOD_RESOURCE_PREFIX <- "HistoneMod-assets"
HISTONEMOD_PACKAGE_FALLBACK_VERSION <- "0.5.1"

histonemod_package_version <- function() {
  desc <- tryCatch(utils::packageDescription("HistoneMod"), error = function(e) NULL)
  if (!is.null(desc) && !is.null(desc$Version) && nzchar(desc$Version)) {
    return(desc$Version)
  }
  HISTONEMOD_PACKAGE_FALLBACK_VERSION
}

histonemod_logo_path <- function() {
  system.file("app", "www", "logo.png", package = "HistoneMod")
}

.register_histonemod_resources <- function() {
  app_www <- system.file("app", "www", package = "HistoneMod")
  if (!nzchar(app_www) || !dir.exists(app_www)) {
    return(invisible(FALSE))
  }

  resource_paths <- shiny::resourcePaths()
  if (!(HISTONEMOD_RESOURCE_PREFIX %in% names(resource_paths))) {
    shiny::addResourcePath(HISTONEMOD_RESOURCE_PREFIX, app_www)
  }

  invisible(TRUE)
}

.onLoad <- function(libname, pkgname) {
  .register_histonemod_resources()
}

#' Build the HistoneMod Shiny application object.
#'
#' @return A `shiny.appobj` that can be launched with `shiny::runApp()`.
#' @export
histonemod_app <- function() {
  .register_histonemod_resources()
  shiny::shinyApp(ui = histonemod_ui(), server = histonemod_server)
}

#' Launch the HistoneMod Shiny application.
#'
#' @param ... Additional arguments passed to [shiny::runApp()].
#'
#' @return Invisibly returns the value from [shiny::runApp()].
#' @export
runHistoneMod <- function(...) {
  shiny::runApp(histonemod_app(), ...)
}
