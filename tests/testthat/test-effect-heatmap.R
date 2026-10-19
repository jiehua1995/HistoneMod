##### Tests: effect-size heatmap across comparisons #####

demo_csv <- function(name) {
  path <- system.file("extdata", name, package = "HistoneMod")
  if (!nzchar(path)) {
    path <- file.path("inst", "extdata", name)
  }
  path
}

demo_table <- function() {
  ms1 <- utils::read.csv(demo_csv("MS1_demo.csv"), stringsAsFactors = FALSE)
  samples <- utils::read.csv(demo_csv("samples_demo.csv"), stringsAsFactors = FALSE)
  colnames(ms1) <- gsub("\\s+", ".", colnames(ms1))
  colnames(samples) <- gsub("\\s+", ".", colnames(samples))
  percentage_calculation(ms1, samples, exclude_un = FALSE)
}

# Two comparisons and two proteins: the minimal table with the columns returned
# by differential_analysis().
effect_table <- function() {
  data.frame(
    protein = c("H3", "H3", "H4", "H4", "H3", "H3", "H4", "H4"),
    feature = c("f1", "f2", "f3", "f4", "f1", "f2", "f3", "f4"),
    group1 = c("A", "A", "A", "A", "B", "B", "B", "B"),
    group2 = c("B", "B", "B", "B", "C", "C", "C", "C"),
    effect = c(1, -1, 0.5, -0.3, 0.2, 0.4, -0.6, 0.1),
    p_adj = c(0.0001, 0.02, 0.2, 0.005, 0.5, 0.001, 0.03, 0.9),
    stringsAsFactors = FALSE
  )
}

# Internal helper mirroring the documented star convention, used to check the
# plotted text layer.
expected_stars <- function(p, alpha = 0.05) {
  stars <- rep("", length(p))
  significant <- !is.na(p) & p < alpha
  stars[significant & p < 0.001] <- "***"
  stars[significant & p >= 0.001 & p < 0.01] <- "**"
  stars[significant & p >= 0.01] <- "*"
  stars
}

test_that("plot_effect_heatmap builds a ggplot from differential results", {
  dat <- effect_table()
  p <- plot_effect_heatmap(dat)

  expect_s3_class(p, "ggplot")
  expect_equal(levels(p$data$comparison), c("A vs B", "B vs C"))
  expect_setequal(as.character(p$data$feature), c("f1", "f2", "f3", "f4"))
  expect_equal(nrow(p$data), 8L)
  expect_silent(ggplot2::ggplot_build(p))

  geoms <- vapply(
    p$layers,
    function(layer) class(layer$geom)[1],
    character(1),
    USE.NAMES = FALSE
  )
  expect_equal(geoms, c("GeomTile", "GeomText"))

  # The label is built as group1 "vs" group2, so swapping the groups changes it.
  swapped <- dat[dat$group1 == "A", ]
  swapped$group1 <- "B"
  swapped$group2 <- "A"
  p_swapped <- plot_effect_heatmap(swapped)
  expect_equal(levels(p_swapped$data$comparison), "B vs A")
})

test_that("significance stars follow adjusted p-values", {
  dat <- effect_table()
  p <- plot_effect_heatmap(dat, cluster_rows = FALSE, facet = FALSE)
  built <- ggplot2::ggplot_build(p)
  expect_equal(
    built$data[[2]]$label,
    expected_stars(dat$p_adj)
  )

  boundary <- data.frame(
    protein = "H3",
    feature = c("a", "b", "c", "d"),
    group1 = "A",
    group2 = "B",
    effect = c(1, 1, 1, 1),
    p_adj = c(0.0009, 0.001, 0.009, 0.01),
    stringsAsFactors = FALSE
  )
  p_boundary <- plot_effect_heatmap(boundary, cluster_rows = FALSE, facet = FALSE)
  expect_equal(p_boundary$data$stars, c("***", "**", "**", "*"))

  # alpha gates the stars and NA p-values never produce a star.
  na_p <- boundary
  na_p$p_adj[1] <- NA_real_
  p_na <- plot_effect_heatmap(na_p, cluster_rows = FALSE, facet = FALSE)
  expect_equal(p_na$data$stars, c("", "**", "**", "*"))

  p_alpha <- plot_effect_heatmap(
    boundary,
    alpha = 0.001,
    cluster_rows = FALSE,
    facet = FALSE
  )
  expect_equal(p_alpha$data$stars, c("***", "", "", ""))
})

test_that("comparisons selection filters and orders the columns", {
  dat <- effect_table()

  p <- plot_effect_heatmap(
    dat,
    comparisons = c(" b   VS   c ", "A vs B"),
    cluster_rows = FALSE
  )
  expect_equal(levels(p$data$comparison), c("B vs C", "A vs B"))
  expect_equal(nrow(p$data), 8L)

  p_list <- plot_effect_heatmap(
    dat,
    comparisons = list(c("B", "C"), c("A", "B")),
    cluster_rows = FALSE
  )
  expect_equal(levels(p_list$data$comparison), c("B vs C", "A vs B"))

  p_one <- plot_effect_heatmap(
    dat,
    comparisons = list(c("B", "C")),
    cluster_rows = FALSE
  )
  expect_equal(levels(p_one$data$comparison), "B vs C")
  expect_equal(nrow(p_one$data), 4L)

  # A comparison that is not present yields the empty placeholder.
  p_none <- plot_effect_heatmap(dat, comparisons = "Z vs Y")
  expect_s3_class(p_none, "ggplot")
  expect_equal(length(p_none$layers), 1L)
  expect_match(
    ggplot2::ggplot_build(p_none)$data[[1]]$label,
    "No differential effects"
  )

  expect_error(plot_effect_heatmap(dat, comparisons = list("A")), "length 2")
  expect_error(plot_effect_heatmap(dat, comparisons = 1), "comparisons")
})

test_that("three comparisons render as three ordered columns", {
  dat <- effect_table()
  extra <- dat[dat$group2 == "C", ]
  extra$group1 <- "C"
  extra$group2 <- "D"
  extra$effect <- c(0.4, -0.2, 0.6, -0.1)
  three <- rbind(dat, extra)

  p <- plot_effect_heatmap(three, cluster_rows = FALSE, facet = FALSE)
  expect_equal(levels(p$data$comparison), c("A vs B", "B vs C", "C vs D"))
  expect_equal(nrow(p$data), 12L)
})

test_that("row clustering is stable and preserves the matrix layout", {
  dat <- effect_table()

  p1 <- plot_effect_heatmap(dat, facet = FALSE)
  p2 <- plot_effect_heatmap(dat, facet = FALSE)
  expect_equal(levels(p1$data$feature), levels(p2$data$feature))
  expect_setequal(as.character(levels(p1$data$feature)), c("f1", "f2", "f3", "f4"))

  wide <- matrix(
    NA_real_,
    nrow = 4L,
    ncol = 2L,
    dimnames = list(c("f1", "f2", "f3", "f4"), c("A vs B", "B vs C"))
  )
  wide[cbind(
    match(dat$feature, rownames(wide)),
    match(paste(dat$group1, "vs", dat$group2), colnames(wide))
  )] <- dat$effect
  expected <- rownames(wide)[stats::hclust(stats::dist(wide))$order]
  expect_equal(levels(p1$data$feature), expected)

  p_plain <- plot_effect_heatmap(dat, cluster_rows = FALSE, facet = FALSE)
  expect_equal(levels(p_plain$data$feature), c("f1", "f2", "f3", "f4"))

  # A single row and non-finite effect values must not break clustering.
  p_single <- plot_effect_heatmap(dat[dat$feature == "f1", ], facet = FALSE)
  expect_equal(levels(p_single$data$feature), "f1")

  with_na <- dat
  with_na$effect[with_na$feature == "f2" & with_na$group2 == "B"] <- NA_real_
  p_na <- plot_effect_heatmap(with_na, facet = FALSE)
  expect_s3_class(p_na, "ggplot")
  expect_setequal(as.character(levels(p_na$data$feature)), c("f1", "f2", "f3", "f4"))
})

test_that("facets by protein keep one global row order", {
  dat <- effect_table()
  p <- plot_effect_heatmap(dat, facet = TRUE)

  expect_true(inherits(p$facet, "FacetWrap"))
  expect_true("protein" %in% names(p$data))
  expect_silent(ggplot2::ggplot_build(p))

  p_no_facet <- plot_effect_heatmap(dat, facet = FALSE)
  expect_false(inherits(p_no_facet$facet, "FacetWrap"))
  expect_equal(levels(p$data$feature), levels(p_no_facet$data$feature))

  single_protein <- dat[dat$protein == "H3", ]
  p_single <- plot_effect_heatmap(single_protein, facet = TRUE)
  expect_false(inherits(p_single$facet, "FacetWrap"))

  no_protein <- dat[, setdiff(names(dat), "protein")]
  p_no_protein <- plot_effect_heatmap(no_protein)
  expect_false(inherits(p_no_protein$facet, "FacetWrap"))
  expect_equal(unique(as.character(p_no_protein$data$protein)), "All")
})

test_that("fill limits are symmetric around zero", {
  dat <- effect_table()

  p <- plot_effect_heatmap(dat, cluster_rows = FALSE, facet = FALSE)
  expect_equal(p$scales$get_scales("fill")$limits, c(-1, 1))

  p_one <- plot_effect_heatmap(
    dat,
    value_limits = 2.5,
    cluster_rows = FALSE,
    facet = FALSE
  )
  expect_equal(p_one$scales$get_scales("fill")$limits, c(-2.5, 2.5))

  p_two <- plot_effect_heatmap(
    dat,
    value_limits = c(-3, 1.5),
    cluster_rows = FALSE,
    facet = FALSE
  )
  expect_equal(p_two$scales$get_scales("fill")$limits, c(-3, 3))

  # All-zero effects fall back to a unit range.
  zero <- dat
  zero$effect <- 0
  p_zero <- plot_effect_heatmap(zero, cluster_rows = FALSE, facet = FALSE)
  expect_equal(p_zero$scales$get_scales("fill")$limits, c(-1, 1))

  expect_error(plot_effect_heatmap(dat, value_limits = "wide"), "value_limits")
})

test_that("label_fun transforms feature labels", {
  dat <- effect_table()
  p <- plot_effect_heatmap(
    dat,
    label_fun = function(x) paste0("feature ", toupper(x)),
    cluster_rows = FALSE,
    facet = FALSE
  )
  expect_equal(
    levels(p$data$feature),
    c("feature F1", "feature F2", "feature F3", "feature F4")
  )

  p_dup <- plot_effect_heatmap(
    dat,
    label_fun = function(x) rep("mark", length(x)),
    cluster_rows = FALSE,
    facet = FALSE
  )
  expect_equal(levels(p_dup$data$feature), c("mark", "mark.1", "mark.2", "mark.3"))

  expect_error(
    plot_effect_heatmap(dat, label_fun = function(x) "one"),
    "one label per feature"
  )
  expect_error(plot_effect_heatmap(dat, label_fun = 1), "label_fun")
})

test_that("empty and all-missing input return a placeholder", {
  empty <- data.frame(
    protein = character(0),
    feature = character(0),
    group1 = character(0),
    group2 = character(0),
    effect = numeric(0),
    p_adj = numeric(0),
    stringsAsFactors = FALSE
  )
  p_empty <- plot_effect_heatmap(empty)
  expect_s3_class(p_empty, "ggplot")
  expect_equal(length(p_empty$layers), 1L)
  expect_silent(ggplot2::ggplot_build(p_empty))
  expect_match(
    ggplot2::ggplot_build(p_empty)$data[[1]]$label,
    "No differential effects"
  )

  # A zero-row data.frame without columns also yields the placeholder.
  expect_s3_class(plot_effect_heatmap(data.frame()), "ggplot")

  all_na <- effect_table()
  all_na$effect <- NA_real_
  p_na <- plot_effect_heatmap(all_na)
  expect_equal(length(p_na$layers), 1L)
  expect_match(
    ggplot2::ggplot_build(p_na)$data[[1]]$label,
    "No differential effects"
  )

  # Filtering to an unknown protein leaves nothing to plot.
  p_protein <- plot_effect_heatmap(effect_table(), protein = "missing")
  expect_equal(length(p_protein$layers), 1L)
})

test_that("rows with missing effects are dropped", {
  dat <- effect_table()
  dat$effect[dat$feature == "f1"] <- NA_real_
  p <- plot_effect_heatmap(dat, cluster_rows = FALSE, facet = FALSE)
  expect_equal(levels(p$data$feature), c("f2", "f3", "f4"))
  expect_equal(nrow(p$data), 6L)

  dat2 <- effect_table()
  dat2$p_adj[1] <- NA_real_
  p2 <- plot_effect_heatmap(dat2, cluster_rows = FALSE, facet = FALSE)
  expect_equal(p2$data$stars[1], "")
})

test_that("protein filters the heatmap", {
  dat <- effect_table()
  p <- plot_effect_heatmap(dat, protein = "H4")
  expect_equal(unique(as.character(p$data$protein)), "H4")
  expect_setequal(as.character(p$data$feature), c("f3", "f4"))
  expect_false(inherits(p$facet, "FacetWrap"))

  expect_error(
    plot_effect_heatmap(dat[, setdiff(names(dat), "protein")], protein = "H3"),
    "no `protein` column"
  )
  expect_error(plot_effect_heatmap(dat, protein = 1), "`protein`")
})

test_that("show_stars = FALSE omits the text layer", {
  dat <- effect_table()
  no_p <- dat[, c("protein", "feature", "group1", "group2", "effect")]
  p <- plot_effect_heatmap(no_p, show_stars = FALSE, facet = FALSE)
  geoms <- vapply(
    p$layers,
    function(layer) class(layer$geom)[1],
    character(1),
    USE.NAMES = FALSE
  )
  expect_false(any(grepl("Text", geoms)))
  expect_s3_class(p, "ggplot")

  expect_error(plot_effect_heatmap(no_p), "missing required column")
})

test_that("plot_effect_heatmap validates its input", {
  dat <- effect_table()

  expect_error(plot_effect_heatmap(42), "data.frame")
  expect_error(
    plot_effect_heatmap(dat, effect_col = "nope"),
    "missing required column"
  )
  expect_error(
    plot_effect_heatmap(dat, effect_col = c("effect", "nope")),
    "effect_col"
  )
  expect_error(plot_effect_heatmap(dat, p_col = NA_character_), "p_col")
  expect_error(plot_effect_heatmap(dat, alpha = 2), "alpha")
  expect_error(plot_effect_heatmap(dat, alpha = NA_real_), "alpha")
  expect_error(plot_effect_heatmap(dat, star_size = -1), "star_size")
  expect_error(plot_effect_heatmap(dat, palette = "red"), "palette")
})

test_that("plot_effect_heatmap works on differential_analysis output", {
  table <- demo_table()
  res <- differential_analysis(
    table,
    comparisons = list(c("SampleA", "SampleB"), c("SampleC", "SampleD"))
  )
  p <- plot_effect_heatmap(res)

  expect_s3_class(p, "ggplot")
  expect_equal(
    levels(p$data$comparison),
    c("SampleA vs SampleB", "SampleC vs SampleD")
  )
  expect_setequal(as.character(p$data$feature), unique(res$feature))
  expect_equal(nrow(p$data), nrow(res))
  expect_true(inherits(p$facet, "FacetWrap"))
  expect_silent(ggplot2::ggplot_build(p))

  protein <- unique(res$protein)[1]
  p_protein <- plot_effect_heatmap(res, protein = protein)
  expect_equal(unique(as.character(p_protein$data$protein)), protein)
  expect_false(inherits(p_protein$facet, "FacetWrap"))
})
