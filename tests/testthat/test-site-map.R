# Tests for R/site_map.R: plot_site_map() histone site map / lollipop plot.
#
# Covers the return class, the y-category ordering, both facet layouts, the
# diverging effect mode, point-size scaling, repel labels and canonical mark
# names, backbone protein lengths, empty/NA input and input validation.

# ---- helpers -------------------------------------------------------------

# Resolve a bundled extdata file both under pkgload and under R CMD check.
.hm_site_map_extdata <- function(file) {
  candidates <- c(
    system.file("extdata", file, package = "HistoneMod"),
    file.path("inst", "extdata", file),
    file.path("..", "..", "inst", "extdata", file),
    file.path("..", "..", "..", "inst", "extdata", file)
  )
  candidates <- candidates[nzchar(candidates)]
  found <- candidates[file.exists(candidates)]
  if (length(found) == 0L) {
    testthat::skip(paste("Bundled extdata file not found:", file))
  }
  found[[1L]]
}

.hm_site_map_demo_occupancy <- function() {
  tables <- list(
    ms1 = read_skyline_csv(.hm_site_map_extdata("MS1_demo.csv")),
    sample = read_skyline_csv(.hm_site_map_extdata("samples_demo.csv"))
  )
  compute_site_occupancy(tables$ms1, tables$sample)
}

# Two proteins (H3, H4), two groups and five sites.
.hm_site_map_synthetic <- function() {
  data.frame(
    Group = c(rep("Ctrl", 5L), rep("Treated", 5L)),
    Protein.Name = c(
      "H3_3-8", "H3_3-8", "H3_3-8", "H4_4-17", "H4_4-17",
      "H3_3-8", "H3_3-8", "H3_3-8", "H4_4-17", "H4_4-17"
    ),
    site = c(
      "H3K4", "H3K9", "H3K27", "H4K5", "H4K16",
      "H3K4", "H3K9", "H3K27", "H4K5", "H4K16"
    ),
    residue = "K",
    position = c(4, 9, 27, 5, 16, 4, 9, 27, 5, 16),
    occupancy = c(
      0.80, 0.20, 0.45, 0.60, 0.90,
      0.35, 0.70, 0.25, 0.85, 0.40
    ),
    stringsAsFactors = FALSE
  )
}

# Same layout with an `effect` column that contains negative and positive values.
.hm_site_map_effect <- function() {
  data <- .hm_site_map_synthetic()
  data$effect <- c(-0.8, -0.3, 0.1, 0.5, 0.9, -0.6, 0.2, 0.4, 0.7, -0.1)
  data$occupancy <- NULL
  data
}

# The point layer of a built plot (backbone + stems + points [+ labels]).
.hm_site_map_point_data <- function(plot) {
  built <- ggplot2::ggplot_build(plot)
  built$data[[3L]]
}

# The label layer of a built plot.
.hm_site_map_label_data <- function(plot) {
  built <- ggplot2::ggplot_build(plot)
  built$data[[4L]]
}

# ---- basic structure -----------------------------------------------------

test_that("plot_site_map returns a ggplot lollipop for occupancy data", {
  data <- .hm_site_map_synthetic()

  p <- plot_site_map(data)
  expect_s3_class(p, "ggplot")
  expect_equal(levels(p$data$Group), c("Ctrl", "Treated"))
  expect_equal(levels(p$data$Protein.Name), c("H3_3-8", "H4_4-17"))

  built <- ggplot2::ggplot_build(p)
  expect_gte(length(built$data), 4L)

  # The light backbone spans the built-in protein lengths.
  expect_setequal(unique(built$data[[1L]]$xend), c(135, 102))

  # Stems connect the backbone (y) to the point (yend).
  stems <- built$data[[2L]]
  expect_true(all(stems$yend > stems$y))

  points <- built$data[[3L]]
  # Point size is scaled to the occupancy value.
  expect_gt(length(unique(round(points$size, 4L))), 1L)
  # Every point sits above its backbone.
  expect_true(all(points$y > stems$y))
  # Occupancy is on the x axis as the protein position.
  expect_true(all(points$x %in% data$position))
})

test_that("plot_site_map preserves group_order and appends unlisted groups", {
  data <- .hm_site_map_synthetic()

  p <- plot_site_map(data, group_order = c("Treated", "Ctrl"))
  expect_equal(levels(p$data$Group), c("Treated", "Ctrl"))

  # Unlisted groups are appended in order of appearance.
  p2 <- plot_site_map(data, group_order = "Treated")
  expect_equal(levels(p2$data$Group), c("Treated", "Ctrl"))

  expect_error(
    plot_site_map(data, group_order = 1:2),
    "character vector"
  )
})

test_that("plot_site_map supports both facet layouts with free x scales", {
  data <- .hm_site_map_synthetic()

  # Default: one facet per protein, groups on y.
  p_protein <- plot_site_map(data)
  expect_true("Protein.Name" %in% names(
    ggplot2::ggplot_build(p_protein)$layout$layout
  ))
  expect_equal(names(p_protein$facet$params$facets), "Protein.Name")
  expect_true(isTRUE(p_protein$facet$params$free$x))
  expect_false(isTRUE(p_protein$facet$params$free$y))

  # facet = "group": one facet per group, proteins on y.
  p_group <- plot_site_map(data, facet = "group")
  expect_true("Group" %in% names(
    ggplot2::ggplot_build(p_group)$layout$layout
  ))
  expect_equal(names(p_group$facet$params$facets), "Group")
  expect_true(isTRUE(p_group$facet$params$free$x))

  panel_labels <- ggplot2::ggplot_build(p_group)$layout$panel_params[[1L]]$
    y$get_labels()
  expect_equal(panel_labels, levels(p_group$data$Protein.Name))

  # group_order also drives the facet order in group mode.
  p_ordered <- plot_site_map(
    data, facet = "group", group_order = c("Treated", "Ctrl")
  )
  expect_equal(levels(p_ordered$data$Group), c("Treated", "Ctrl"))
})

# ---- value encoding ------------------------------------------------------

test_that("plot_site_map effect mode uses a diverging scale and fixed size", {
  data <- .hm_site_map_effect()
  p <- plot_site_map(data, value_col = "effect", show_labels = FALSE)
  expect_s3_class(p, "ggplot")

  points <- .hm_site_map_point_data(p)
  # Fixed point size (midpoint of point_range).
  expect_equal(length(unique(round(points$size, 6L))), 1L)
  expect_equal(
    unique(round(points$size, 6L)),
    round(mean(c(1.5, 6)), 6L)
  )

  # Diverging blue-white-red scale centred at zero: a negative effect is blue,
  # the largest positive effect is red.
  negative <- which(data$effect < 0)[[1L]]
  positive <- which.max(data$effect)
  rgb_negative <- grDevices::col2rgb(points$colour[[negative]])
  rgb_positive <- grDevices::col2rgb(points$colour[[positive]])
  expect_gt(rgb_negative["blue", 1L], rgb_negative["red", 1L])
  expect_gt(rgb_positive["red", 1L], rgb_positive["blue", 1L])

  colour_scale <- p$scales$get_scales("colour")
  expect_s3_class(colour_scale, "ScaleContinuous")
  # The gradient is centred at zero: on a symmetric range zero is the midpoint.
  effect_limit <- max(abs(data$effect))
  expect_equal(
    colour_scale$rescaler(c(-effect_limit, 0, effect_limit)),
    c(0, 0.5, 1)
  )
  # Fixed size means no size scale in effect mode.
  expect_null(p$scales$get_scales("size"))
})

test_that("plot_site_map value mode scales point size to the absolute value", {
  data <- .hm_site_map_synthetic()[1:2, , drop = FALSE]
  data$occupancy <- c(-0.4, 0.4)

  p <- plot_site_map(data, show_labels = FALSE)
  points <- .hm_site_map_point_data(p)
  expect_equal(round(points$size[[1L]], 6L), round(points$size[[2L]], 6L))

  # A size scale is present in value mode, hidden behind a single colour legend.
  expect_s3_class(p$scales$get_scales("size"), "ScaleContinuous")
  expect_s3_class(p$scales$get_scales("colour"), "ScaleContinuous")

  # The observed values span the requested point_range.
  p_full <- plot_site_map(.hm_site_map_synthetic(), show_labels = FALSE)
  expect_equal(range(.hm_site_map_point_data(p_full)$size), c(1.5, 6))
})

# ---- labels --------------------------------------------------------------

test_that("plot_site_map labels sites once and prefers canonical mark names", {
  data <- .hm_site_map_synthetic()

  # Occupancy mode has no modification token: labels are the site names and
  # never the unmodified canonical baseline ("H3K4un").
  p <- plot_site_map(data)
  labels <- .hm_site_map_label_data(p)$label
  expect_setequal(labels, data$site)
  expect_false(any(grepl("un$", labels)))

  # With modification tokens, canonical mark names replace the bare site names.
  data$mod <- c(
    "me3", "me3", "me3", "ac", "ac",
    "me3", "me3", "me3", "ac", "ac"
  )
  p_canonical <- plot_site_map(data, mod_col = "mod")
  expect_setequal(
    .hm_site_map_label_data(p_canonical)$label,
    c("H3K4me3", "H3K9me3", "H3K27me3", "H4K5ac", "H4K16ac")
  )

  # An existing `mod` column is used automatically when mod_col is NULL.
  p_auto <- plot_site_map(data)
  expect_setequal(
    .hm_site_map_label_data(p_auto)$label,
    c("H3K4me3", "H3K9me3", "H3K27me3", "H4K5ac", "H4K16ac")
  )

  # Sites that do not match the curated table keep their own name.
  unmatched <- data
  unmatched$site[[1L]] <- "H3K999me3"
  unmatched$position[[1L]] <- 999
  p_unmatched <- plot_site_map(unmatched, mod_col = "mod")
  labels_unmatched <- .hm_site_map_label_data(p_unmatched)$label
  expect_true("H3K999me3" %in% labels_unmatched)

  # An empty marks table falls back to the site names without an error.
  p_empty_marks <- plot_site_map(data, marks = histone_marks()[0, , drop = FALSE])
  expect_setequal(.hm_site_map_label_data(p_empty_marks)$label, data$site)
})

test_that("plot_site_map draws at most one label per site inside a panel", {
  data <- .hm_site_map_synthetic()

  # facet = "protein": five sites, one label each (five panels' points).
  expect_equal(nrow(.hm_site_map_label_data(plot_site_map(data))), 5L)

  # facet = "group": two group panels x five sites.
  expect_equal(
    nrow(.hm_site_map_label_data(plot_site_map(data, facet = "group"))),
    10L
  )
})

test_that("plot_site_map can hide labels and rescale them", {
  data <- .hm_site_map_synthetic()

  p_hidden <- plot_site_map(data, show_labels = FALSE)
  has_repel <- vapply(
    p_hidden$layers,
    function(layer) inherits(layer$geom, "GeomTextRepel"),
    logical(1)
  )
  expect_false(any(has_repel))
  expect_length(p_hidden$layers, 3L)

  p_sized <- plot_site_map(data, label_size = 3.1)
  expect_equal(p_sized$layers[[4L]]$aes_params$size, 3.1)
})

# ---- backbone lengths ----------------------------------------------------

test_that("plot_site_map resolves protein lengths and falls back to observed", {
  resolve <- HistoneMod:::.hm_site_map_resolve_lengths

  # Built-in map for known histones, observed maximum for unknown proteins.
  lengths <- resolve(
    c("H3_3-8", "H4_4-17", "X_prot"), c(4, 16, 50)
  )
  expect_equal(unname(lengths), c(135, 102, 50))

  # A built-in length is never shorter than the observed maximum position.
  expect_equal(unname(resolve("H3_3-8", 200)), 200)

  # User-supplied lengths win, by exact name or by normalised histone name.
  overridden <- resolve(
    c("H3_3-8", "H4_4-17"), c(4, 16),
    protein_lengths = c(H3 = 100, "H4_4-17" = 80)
  )
  expect_equal(unname(overridden), c(100, 80))

  expect_error(
    resolve("H3_3-8", 4, protein_lengths = 135),
    "named numeric vector"
  )

  # The plot uses the resolved lengths for the backbone segments.
  data <- .hm_site_map_synthetic()
  p <- plot_site_map(data, show_labels = FALSE)
  built <- ggplot2::ggplot_build(p)
  expect_setequal(unique(built$data[[1L]]$xend), c(135, 102))

  p_override <- plot_site_map(
    data, show_labels = FALSE, protein_lengths = c(H3 = 50, H4 = 60)
  )
  expect_setequal(
    unique(ggplot2::ggplot_build(p_override)$data[[1L]]$xend), c(50, 60)
  )

  # Unknown proteins keep the observed maximum position as backbone.
  unknown <- data.frame(
    Group = "Ctrl",
    Protein.Name = "X_prot",
    site = c("X1", "X2"),
    position = c(10, 40),
    occupancy = c(0.2, 0.7),
    stringsAsFactors = FALSE
  )
  p_unknown <- plot_site_map(unknown, show_labels = FALSE)
  expect_equal(
    unique(ggplot2::ggplot_build(p_unknown)$data[[1L]]$xend), 40
  )
})

# ---- data handling and validation ---------------------------------------

test_that("plot_site_map averages replicate rows per site and category", {
  data <- .hm_site_map_synthetic()
  duplicated_rows <- rbind(data[1L, , drop = FALSE], data[1L, , drop = FALSE])
  duplicated_rows$occupancy <- c(0.2, 0.8)

  p <- plot_site_map(duplicated_rows)
  expect_equal(p$data$site_value, 0.5)
})

test_that("plot_site_map handles empty, all-NA and missing rows", {
  data <- .hm_site_map_synthetic()

  # Zero-row input returns a placeholder ggplot.
  p_empty <- plot_site_map(data[0L, , drop = FALSE])
  expect_s3_class(p_empty, "ggplot")
  expect_length(p_empty$layers, 1L)

  # All values missing is treated like an empty table.
  all_na <- data
  all_na$occupancy <- NA_real_
  p_na <- plot_site_map(all_na)
  expect_s3_class(p_na, "ggplot")
  expect_length(p_na$layers, 1L)

  # A single missing value only drops that row.
  one_na <- data
  one_na$occupancy[[1L]] <- NA_real_
  p_one_na <- plot_site_map(one_na, show_labels = FALSE)
  expect_equal(nrow(p_one_na$data), 9L)
})

test_that("plot_site_map supports custom column names and a missing group", {
  data <- .hm_site_map_synthetic()
  custom <- data.frame(
    condition = data$Group,
    protein = data$Protein.Name,
    pos = data$position,
    site = data$site,
    value = data$occupancy,
    stringsAsFactors = FALSE
  )

  p <- plot_site_map(
    custom,
    value_col = "value",
    group_col = "condition",
    protein_col = "protein",
    position_col = "pos"
  )
  expect_s3_class(p, "ggplot")
  expect_equal(levels(p$data$condition), c("Ctrl", "Treated"))

  # A missing group column means a single "All" group.
  no_group <- data
  no_group$Group <- NULL
  p_no_group <- plot_site_map(no_group)
  expect_equal(levels(p_no_group$data$Group), "All")
})

test_that("plot_site_map validates its input", {
  data <- .hm_site_map_synthetic()

  expect_error(plot_site_map(1:3), "must be a data.frame")
  expect_error(plot_site_map(data.frame(a = 1)), "not found")
  expect_error(plot_site_map(data, value_col = "not_there"), "not found")
  expect_error(plot_site_map(data, position_col = "not_there"), "not found")
  expect_error(plot_site_map(data, site_col = "not_there"), "not found")
  expect_error(plot_site_map(data, mod_col = "not_there"), "not found")
  expect_error(plot_site_map(data, point_range = c(1, -2)), "point_range")
  expect_error(plot_site_map(data, palette = "not_a_palette"), "one of")

  non_numeric <- data
  non_numeric$occupancy <- as.character(non_numeric$occupancy)
  non_numeric$occupancy[[1L]] <- "not a number"
  expect_error(plot_site_map(non_numeric), "must be numeric")

  # `effect` requested but absent.
  expect_error(plot_site_map(data, value_col = "effect"), "not found")
})

# ---- demo data -----------------------------------------------------------

test_that("plot_site_map works on compute_site_occupancy output", {
  occupancy <- .hm_site_map_demo_occupancy()
  expect_true(all(
    c("Protein.Name", "Group", "site", "position", "occupancy") %in%
      names(occupancy)
  ))

  p <- plot_site_map(occupancy)
  expect_s3_class(p, "ggplot")
  points <- .hm_site_map_point_data(p)
  expect_gt(nrow(points), 0L)
  expect_true(all(points$x %in% occupancy$position))

  # Labels are site names (the occupancy table carries no modification token).
  labels <- .hm_site_map_label_data(p)$label
  expect_true(all(labels %in% occupancy$site))

  # Backbone lengths come from the built-in histone lengths.
  built <- ggplot2::ggplot_build(p)
  expect_setequal(unique(built$data[[1L]]$xend), c(135, 102))

  # Group ordering is respected.
  p_ordered <- plot_site_map(occupancy, group_order = c("SampleC", "SampleA"))
  expect_equal(levels(p_ordered$data$Group)[1:2], c("SampleC", "SampleA"))

  # Group facet layout on demo data.
  p_group <- plot_site_map(
    occupancy, facet = "group", group_order = c("SampleC", "SampleA")
  )
  expect_equal(levels(p_group$data$Group)[1:2], c("SampleC", "SampleA"))
  expect_true("Group" %in% names(
    ggplot2::ggplot_build(p_group)$layout$layout
  ))
  expect_equal(
    levels(p_group$data$Protein.Name),
    unique(occupancy$Protein.Name)
  )
})
