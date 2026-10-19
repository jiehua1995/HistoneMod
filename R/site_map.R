##### HistoneMod: Histone Site Map / Lollipop Plot #####
# Protein-site map ("lollipop") built from the long output of
# compute_site_occupancy() or from any per-site table that has a protein, a
# protein position, a site label and a numeric value column.
#
# Each row category is drawn as a light backbone (protein start to protein end)
# with a short vertical stem from the backbone up to a point. The point colour
# encodes the value and, in occupancy/value mode, the point size is scaled to
# the value. Only ggplot2 and ggrepel are used; both are already imported by
# HistoneMod, so no new hard dependency is introduced.

# Column names used inside ggplot2 non-standard evaluation in this file.
# Declaring them here keeps R CMD check free of "no visible binding" notes.
utils::globalVariables(c(
  "site_position", "site_value", "size_value", "stem_base", "stem_top",
  "site_label", "protein_length"
))

# ------------------------- Internal helpers -------------------------------

# Built-in histone protein lengths (in amino acids) used to draw the backbone
# when the user does not supply `protein_lengths`. The map is keyed by the
# canonical protein vocabulary of normalise_histone_protein(); unknown proteins
# fall back to their observed maximum position.
.hm_site_map_protein_lengths <- function() {
  c(
    "H3" = 135,
    "H3.3" = 135,
    "H4" = 102,
    "H2A" = 129,
    "H2A.Z" = 128,
    "H2B" = 125,
    "macroH2A" = 372
  )
}

# Resolve one backbone length per distinct protein name. Priority:
#   1. exact name in the user-supplied `protein_lengths`;
#   2. normalised histone name in the user-supplied `protein_lengths`;
#   3. built-in length for the normalised histone name;
#   4. observed maximum position.
# A returned length is never shorter than the observed maximum position so that
# every point stays on its backbone.
.hm_site_map_resolve_lengths <- function(protein, position,
                                         protein_lengths = NULL) {
  protein <- as.character(protein)
  position <- suppressWarnings(as.numeric(position))

  valid_protein <- !is.na(protein) & nzchar(trimws(protein))
  proteins <- unique(protein[valid_protein])
  if (length(proteins) == 0L) {
    return(stats::setNames(numeric(0), character(0)))
  }

  observed <- vapply(proteins, function(p) {
    values <- position[valid_protein & protein == p]
    values <- values[is.finite(values)]
    if (length(values) == 0L) NA_real_ else max(values)
  }, numeric(1))

  user <- NULL
  if (!is.null(protein_lengths)) {
    if (!is.numeric(protein_lengths) || is.null(names(protein_lengths))) {
      stop(
        "`protein_lengths` must be a named numeric vector of protein lengths.",
        call. = FALSE
      )
    }
    keep <- is.finite(protein_lengths) & protein_lengths > 0
    user <- protein_lengths[keep]
  }

  builtin <- .hm_site_map_protein_lengths()
  normalised <- normalise_histone_protein(proteins)
  out <- stats::setNames(rep(NA_real_, length(proteins)), proteins)

  for (i in seq_along(proteins)) {
    p <- proteins[[i]]
    length_value <- NA_real_

    if (!is.null(user) && p %in% names(user)) {
      length_value <- unname(user[[p]])
    }

    norm <- normalised[[i]]
    if (is.na(length_value) && !is.na(norm)) {
      if (!is.null(user) && norm %in% names(user)) {
        length_value <- unname(user[[norm]])
      }
      if (is.na(length_value) && norm %in% names(builtin)) {
        length_value <- unname(builtin[[norm]])
      }
    }

    if (is.na(length_value) || !is.finite(length_value) ||
        length_value <= 0) {
      length_value <- observed[[i]]
    }
    if (is.na(length_value) || !is.finite(length_value) ||
        length_value <= 0) {
      length_value <- 1
    }

    observed_length <- observed[[i]]
    if (is.finite(observed_length) && observed_length > length_value) {
      length_value <- observed_length
    }
    out[[i]] <- length_value
  }

  out
}

# Scale values to [0, 1] on their absolute magnitude. Used for the stem height
# so that negative effects still grow away from the backbone. Constant input
# maps to the middle of the range.
.hm_site_map_scale01 <- function(value) {
  value <- abs(as.numeric(value))
  limits <- range(value, na.rm = TRUE)
  if (!all(is.finite(limits)) || diff(limits) == 0) {
    return(rep(0.5, length(value)))
  }
  (value - limits[[1L]]) / diff(limits)
}

# Order levels: user `group_order` first, then remaining values in order of
# appearance.
.hm_site_map_levels <- function(values, group_order = NULL) {
  observed <- unique(as.character(values))
  if (is.null(group_order)) {
    return(observed)
  }
  if (!is.character(group_order)) {
    stop(
      "`group_order` must be a character vector of group names.",
      call. = FALSE
    )
  }
  c(intersect(group_order, observed), setdiff(observed, group_order))
}

# Build one display label per protein x site: the canonical mark name when the
# site matches the curated marks table (for example H3K4me3), otherwise the
# site name as supplied. Unmodified baseline marks ("H3K4un") are not used as
# labels because the site-map table usually labels the site itself ("H3K4").
.hm_site_map_labels <- function(site, protein, residue = NULL,
                                position = NULL, mod = NULL,
                                marks = histone_marks()) {
  site <- as.character(site)
  if (length(site) == 0L) {
    return(character(0))
  }

  frame <- data.frame(
    site = site,
    protein = as.character(protein),
    stringsAsFactors = FALSE
  )
  if (!is.null(residue)) frame$residue <- as.character(residue)
  if (!is.null(position)) {
    frame$position <- suppressWarnings(as.numeric(position))
  }
  if (!is.null(mod)) frame$mod <- as.character(mod)

  labels <- tryCatch({
    annotated <- suppressWarnings(annotate_sites(frame, marks = marks))
    canonical <- as.character(annotated$canonical)
    use_canonical <- !is.na(canonical) & nzchar(canonical) &
      !grepl("un$", canonical, ignore.case = TRUE)
    ifelse(use_canonical, canonical, site)
  }, error = function(e) {
    # Labels are cosmetic: an unusable marks table falls back to site names.
    site
  })

  blank <- is.na(labels) | !nzchar(labels)
  labels[blank] <- site[blank]
  labels
}

# Placeholder ggplot used whenever there is nothing to draw.
.hm_site_map_placeholder <- function(text) {
  ggplot2::ggplot() +
    ggplot2::annotate(
      "text", x = 0, y = 0, label = text, size = 5, colour = "grey30"
    ) +
    ggplot2::theme_void()
}

# Clean package theme shared with the other site-level plots.
.hm_site_map_theme <- function(base_size = 12) {
  ggplot2::theme_minimal(base_family = "sans", base_size = base_size) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.border = ggplot2::element_rect(
        color = "black", fill = NA, linewidth = 0.8, linejoin = "mitre"
      ),
      strip.background = ggplot2::element_rect(fill = "grey95", color = NA),
      strip.text = ggplot2::element_text(size = base_size, face = "bold"),
      axis.title = ggplot2::element_text(size = base_size + 2),
      axis.text = ggplot2::element_text(size = base_size - 1),
      legend.text = ggplot2::element_text(size = base_size - 1),
      legend.title = ggplot2::element_text(size = base_size)
    )
}

# Case/separator-insensitive column pick with a helpful error.
.hm_site_map_pick <- function(data, col, label) {
  name <- .hm_find_column(names(data), col)
  if (is.null(name)) {
    stop(
      "Column '", col, "' (", label, ") not found in `data`. ",
      "Available columns: ", paste(names(data), collapse = ", "),
      ". Use the output of compute_site_occupancy().",
      call. = FALSE
    )
  }
  name
}

# ------------------------- Exported functions -----------------------------

#' Plot a histone modification site map (lollipop plot)
#'
#' Draws a protein-site map from a long per-site table, typically the output of
#' [compute_site_occupancy()]. The protein position is on the x axis; each group
#' (default) or each protein is a row drawn as a light backbone from position 0
#' to the protein length. Every site is drawn as a point tethered to its
#' backbone by a short stem, so the plot reads as a lollipop/site map. The value
#' is mapped to the point colour and, in occupancy/value mode, to the point size.
#'
#' Two facet layouts are available:
#' \describe{
#'   \item{`"protein"`}{(default) groups are the rows and every protein gets its
#'     own facet, so the sites of one protein are comparable across groups.}
#'   \item{`"group"`}{proteins are the rows and every group gets its own facet.}
#' }
#' Both layouts use `facet_wrap(scales = "free_x")` so that proteins of
#' different lengths are not stretched onto a common x range.
#'
#' Rows with a missing value, position, site or group are dropped. Unrecognised
#' protein names are kept: their backbone uses the observed maximum position
#' unless a length is supplied in `protein_lengths`. A zero-row result returns a
#' placeholder `ggplot`.
#'
#' When `value_col = "effect"` and the data contains an `effect` column, the
#' plot switches to effect mode: a diverging blue-white-red colour scale centred
#' at zero with symmetric limits is used and the point size is fixed. A negative
#' value in the ordinary value mode is handled by scaling the point size and the
#' stem height to the absolute value, while the colour still shows the sign.
#'
#' @param data A `data.frame` of per-site values, for example the long output of
#'   [compute_site_occupancy()] (columns `Protein.Name`, `Group`, `site`,
#'   `position` and `occupancy`). See the `*_col` arguments for custom names.
#' @param value_col Name of the numeric value column plotted as point colour.
#'   Defaults to `"occupancy"`.
#' @param group_col Name of the group column, `"Group"` by default. When the
#'   column is absent all rows are plotted as a single group called `"All"`.
#' @param protein_col Name of the protein column (`"Protein.Name"` by default).
#' @param position_col Name of the numeric protein-position column
#'   (`"position"` by default).
#' @param site_col Name of the site label column (`"site"` by default).
#' @param mod_col Optional name of a modification-token column (`"me3"`, `"ac"`,
#'   ...) used to look up canonical mark names. Defaults to `NULL`, in which case
#'   an existing `"mod"` column is used when present.
#' @param marks Curated mark table used to resolve canonical labels, by default
#'   [histone_marks()]. Labels fall back to the site name when a site is not in
#'   the table.
#' @param group_order Optional character vector giving the display order of
#'   groups. Groups not listed are appended in order of appearance. The order
#'   also applies to the facets of `facet = "group"`.
#' @param facet Layout, `"protein"` (groups on y, one facet per protein) or
#'   `"group"` (proteins on y, one facet per group).
#' @param show_labels Logical; add repel labels (canonical mark name or site
#'   name) to the points. At most one label is drawn per site inside a panel.
#'   Defaults to `TRUE`.
#' @param label_size Size of the repel labels in mm. Defaults to `2.4`.
#' @param palette viridis palette option for the value colour scale: one of
#'   `"viridis"`, `"magma"`, `"plasma"`, `"inferno"` or `"cividis"`.
#' @param point_range Numeric vector of length 2 giving the minimum and maximum
#'   point size. Defaults to `c(1.5, 6)`. In effect mode the midpoint of the
#'   range is used as the fixed point size.
#' @param protein_lengths Optional named numeric vector of protein lengths,
#'   e.g. `c("H3_3-8" = 135, "H4_4-17" = 102)`. Names are matched against the
#'   protein column values and, failing that, against the normalised histone
#'   names of [normalise_histone_protein()]. When `NULL`, a built-in map is used
#'   for H3/H3.3 (135), H4 (102), H2A (129), H2A.Z (128), H2B (125) and
#'   macroH2A (372); other proteins use their observed maximum position.
#'
#' @return A `ggplot` object.
#'
#' @examples
#' \dontrun{
#' occ <- compute_site_occupancy(ms1, sample)
#' plot_site_map(occ)
#' plot_site_map(occ, group_order = c("SampleC", "SampleA"))
#' plot_site_map(occ, facet = "group")
#' }
#' @export
plot_site_map <- function(data,
                          value_col = "occupancy",
                          group_col = "Group",
                          protein_col = "Protein.Name",
                          position_col = "position",
                          site_col = "site",
                          mod_col = NULL,
                          marks = histone_marks(),
                          group_order = NULL,
                          facet = c("protein", "group"),
                          show_labels = TRUE,
                          label_size = 2.4,
                          palette = "viridis",
                          point_range = c(1.5, 6),
                          protein_lengths = NULL) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
  facet <- match.arg(facet)
  palette <- match.arg(
    palette, c("viridis", "magma", "plasma", "inferno", "cividis")
  )
  show_labels <- isTRUE(show_labels)

  label_size <- suppressWarnings(as.numeric(label_size))
  if (length(label_size) != 1L || !is.finite(label_size) || label_size <= 0) {
    label_size <- 2.4
  }

  point_range <- suppressWarnings(as.numeric(point_range))
  if (length(point_range) != 2L || any(!is.finite(point_range)) ||
      any(point_range <= 0)) {
    stop(
      "`point_range` must be a numeric vector of length 2 with positive values.",
      call. = FALSE
    )
  }
  point_range <- sort(point_range)

  value_name <- .hm_site_map_pick(data, value_col, "value_col")
  protein_name <- .hm_site_map_pick(data, protein_col, "protein_col")
  position_name <- .hm_site_map_pick(data, position_col, "position_col")
  site_name <- .hm_site_map_pick(data, site_col, "site_col")

  group_name <- .hm_find_column(names(data), group_col)
  residue_name <- .hm_find_column(names(data), "residue")

  if (!is.null(mod_col) && length(mod_col) == 1L && !is.na(mod_col) &&
      nzchar(mod_col)) {
    mod_name <- .hm_find_column(names(data), mod_col)
    if (is.null(mod_name)) {
      stop(
        "Column '", mod_col, "' (`mod_col`) not found in `data`.",
        call. = FALSE
      )
    }
  } else {
    mod_name <- .hm_find_column(names(data), "mod")
  }

  dat <- as.data.frame(data, stringsAsFactors = FALSE)

  if (is.null(group_name)) {
    group_name <- if (!"Group" %in% names(dat)) "Group" else ".hm_group"
    dat[[group_name]] <- "All"
  }

  value_raw <- dat[[value_name]]
  site_value <- suppressWarnings(as.numeric(as.character(value_raw)))
  coerced <- !is.na(value_raw) & is.na(site_value) &
    nzchar(trimws(as.character(value_raw)))
  if (any(coerced)) {
    stop("Column '", value_name, "' must be numeric.", call. = FALSE)
  }

  site_position <- suppressWarnings(as.numeric(as.character(dat[[position_name]])))
  coerced_position <- !is.na(dat[[position_name]]) & is.na(site_position) &
    nzchar(trimws(as.character(dat[[position_name]])))
  if (any(coerced_position)) {
    stop("Column '", position_name, "' must be numeric.", call. = FALSE)
  }

  group_values <- trimws(as.character(dat[[group_name]]))
  protein_values <- trimws(as.character(dat[[protein_name]]))
  site_values <- trimws(as.character(dat[[site_name]]))

  # Unknown proteins are kept but labelled "unknown"; missing groups are
  # dropped because every row needs a y category.
  protein_values[is.na(protein_values) | !nzchar(protein_values)] <- "unknown"

  keep <- !is.na(site_value) & !is.na(site_position) &
    !is.na(site_values) & nzchar(site_values) &
    !is.na(group_values) & nzchar(group_values)
  dat <- dat[keep, , drop = FALSE]
  site_value <- site_value[keep]
  site_position <- site_position[keep]
  group_values <- group_values[keep]
  protein_values <- protein_values[keep]
  site_values <- site_values[keep]

  dat$site_value <- site_value
  dat$site_position <- site_position

  if (nrow(dat) == 0L) {
    return(.hm_site_map_placeholder("No site data to plot"))
  }

  # Preserve the user order of the row categories and of the facet categories.
  group_levels <- .hm_site_map_levels(group_values, group_order)
  protein_levels <- unique(protein_values)
  dat[[group_name]] <- factor(group_values, levels = group_levels)
  dat[[protein_name]] <- factor(protein_values, levels = protein_levels)

  if (facet == "protein") {
    y_name <- group_name
    facet_name <- protein_name
    y_label <- as.character(group_col)[1L]
  } else {
    y_name <- protein_name
    facet_name <- group_name
    y_label <- "Protein"
  }

  # Collapse replicate/backbone rows to one mean value per site and category so
  # that every site is one lollipop per row of the map.
  row_key <- paste(
    as.character(dat[[facet_name]]),
    as.character(dat[[y_name]]),
    as.character(dat[[protein_name]]),
    as.character(dat[[site_name]]),
    dat$site_position,
    sep = "\r"
  )
  dat$site_value <- stats::ave(
    dat$site_value, row_key, FUN = function(values) mean(values, na.rm = TRUE)
  )
  dat <- dat[!duplicated(row_key), , drop = FALSE]

  # Backbone length per protein, then the category position of every row.
  lengths_map <- .hm_site_map_resolve_lengths(
    as.character(dat[[protein_name]]),
    dat$site_position,
    protein_lengths = protein_lengths
  )
  dat$protein_length <- unname(
    lengths_map[as.character(dat[[protein_name]])]
  )
  dat$protein_length[!is.finite(dat$protein_length)] <- max(
    dat$site_position, na.rm = TRUE
  )

  dat$stem_base <- as.numeric(dat[[y_name]])
  dat$size_value <- abs(dat$site_value)
  dat$stem_top <- dat$stem_base + 0.10 + 0.30 *
    .hm_site_map_scale01(dat$site_value)

  # One display label per protein x site. `first non-NA` means the first row of
  # a site (in factor order) carries the label inside each panel.
  site_key <- paste(
    as.character(dat[[protein_name]]),
    as.character(dat[[site_name]]),
    sep = "\r"
  )
  label_sites <- dat[!duplicated(site_key), , drop = FALSE]
  label_values <- .hm_site_map_labels(
    site = as.character(label_sites[[site_name]]),
    protein = as.character(label_sites[[protein_name]]),
    residue = if (!is.null(residue_name)) {
      as.character(label_sites[[residue_name]])
    } else {
      NULL
    },
    position = label_sites$site_position,
    mod = if (!is.null(mod_name)) as.character(label_sites[[mod_name]]) else NULL,
    marks = marks
  )
  dat$site_label <- label_values[match(site_key, site_key[
    !duplicated(site_key)
  ])]

  effect_mode <- identical(tolower(as.character(value_col)[1L]), "effect")

  # One light backbone (protein start to protein end) per map row.
  backbone <- unique(dat[, c(
    facet_name, y_name, "stem_base", "protein_length"
  ), drop = FALSE])

  p <- ggplot2::ggplot(dat, ggplot2::aes(x = site_position)) +
    ggplot2::geom_segment(
      data = backbone,
      ggplot2::aes(
        x = 0, xend = protein_length, y = stem_base, yend = stem_base
      ),
      inherit.aes = FALSE,
      colour = "grey85",
      linewidth = 0.35
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(xend = site_position, y = stem_base, yend = stem_top),
      colour = "grey70",
      linewidth = 0.3
    )

  if (effect_mode) {
    p <- p + ggplot2::geom_point(
      ggplot2::aes(y = stem_top, colour = site_value),
      size = mean(point_range),
      na.rm = TRUE
    )
  } else {
    p <- p + ggplot2::geom_point(
      ggplot2::aes(y = stem_top, colour = site_value, size = size_value),
      na.rm = TRUE
    )
  }

  if (show_labels) {
    label_key <- paste(
      as.character(dat[[facet_name]]),
      as.character(dat[[protein_name]]),
      as.character(dat[[site_name]]),
      sep = "\r"
    )
    label_rows <- dat[!duplicated(label_key), , drop = FALSE]
    label_rows <- label_rows[
      !is.na(label_rows$site_label) & nzchar(label_rows$site_label), ,
      drop = FALSE
    ]
    if (nrow(label_rows) > 0L) {
      p <- p + ggrepel::geom_text_repel(
        data = label_rows,
        ggplot2::aes(x = site_position, y = stem_top, label = site_label),
        inherit.aes = FALSE,
        size = label_size,
        colour = "grey15",
        segment.colour = "grey60",
        segment.alpha = 0.6,
        box.padding = 0.15,
        point.padding = 0.1,
        min.segment.length = 0,
        max.overlaps = Inf,
        seed = 42,
        show.legend = FALSE
      )
    }
  }

  p <- p +
    ggplot2::scale_y_continuous(
      breaks = seq_along(levels(dat[[y_name]])),
      labels = levels(dat[[y_name]]),
      expand = ggplot2::expansion(mult = c(0.03, 0.10))
    ) +
    ggplot2::labs(
      x = "Protein position",
      y = y_label,
      colour = as.character(value_col)[1L]
    ) +
    .hm_site_map_theme() +
    ggplot2::theme(legend.position = "right") +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::facet_wrap(
      stats::as.formula(paste0("~ `", facet_name, "`")),
      scales = "free_x"
    )

  if (effect_mode) {
    effect_limit <- max(abs(dat$site_value), na.rm = TRUE)
    if (!is.finite(effect_limit) || effect_limit <= 0) effect_limit <- 1
    p <- p + ggplot2::scale_colour_gradient2(
      low = "#2166AC",
      mid = "white",
      high = "#B2182B",
      midpoint = 0,
      limits = c(-effect_limit, effect_limit),
      name = as.character(value_col)[1L]
    )
  } else {
    p <- p +
      ggplot2::scale_colour_viridis_c(
        option = palette,
        name = as.character(value_col)[1L]
      ) +
      ggplot2::scale_size_continuous(
        range = point_range,
        name = as.character(value_col)[1L],
        guide = "none"
      )
  }

  p
}
