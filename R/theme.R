##### Plotting theme and accessible palettes #####

#' HistoneMod ggplot theme
#'
#' A clean theme used across the package: white background, black panel border,
#' no grid lines and readable axis text.
#'
#' @param base_size Base font size.
#' @param base_family Base font family.
#'
#' @return A `ggplot2` theme object.
#' @export
theme_histonemod <- function(base_size = 12, base_family = "sans") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "white", colour = NA),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.border = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 0.7),
      axis.title = ggplot2::element_text(size = base_size + 1),
      axis.text = ggplot2::element_text(size = base_size - 1),
      legend.title = ggplot2::element_text(size = base_size),
      legend.text = ggplot2::element_text(size = base_size - 1),
      strip.text = ggplot2::element_text(size = base_size - 1, face = "bold")
    )
}

#' Accessible colour palettes
#'
#' Returns a colour-blind-friendly palette for groups or categories.
#'
#' @param n Number of colours.
#' @param type Palette type: `"okabe_ito"`, `"tol"` or `"viridis"`.
#'
#' @return A character vector of colours.
#' @export
histonemod_palette <- function(n = 8, type = c("okabe_ito", "tol", "viridis")) {
  type <- match.arg(type)
  n <- suppressWarnings(as.integer(n))
  if (length(n) != 1L || is.na(n) || n < 1L) n <- 8

  okabe_ito <- c(
    "#000000", "#E69F00", "#56B4E9", "#009E73",
    "#F0E442", "#0072B2", "#D55E00", "#CC79A7"
  )
  tol <- c(
    "#332288", "#117733", "#44AA99", "#88CCEE",
    "#DDCC77", "#CC6677", "#AA4499", "#882255",
    "#661100", "#6699CC"
  )

  colours <- switch(type,
    "okabe_ito" = okabe_ito,
    "tol" = tol,
    "viridis" = viridis::viridis(max(n, 3))
  )

  if (identical(type, "viridis")) {
    return(colours[seq_len(n)])
  }
  rep_len(colours, n)
}

#' Accessible colour and fill scales
#'
#' ggplot2 scales that use [histonemod_palette()].
#'
#' @param ... Additional arguments passed to the ggplot2 scale.
#' @param type Palette type.
#'
#' @return A `ggplot2` scale object.
#' @export
scale_colour_histonemod <- function(..., type = c("okabe_ito", "tol", "viridis")) {
  type <- match.arg(type)
  ggplot2::scale_colour_manual(values = histonemod_palette(12, type = type), ...)
}

#' @rdname scale_colour_histonemod
#' @export
scale_fill_histonemod <- function(..., type = c("okabe_ito", "tol", "viridis")) {
  type <- match.arg(type)
  ggplot2::scale_fill_manual(values = histonemod_palette(12, type = type), ...)
}
