##### Tests: composition plots #####

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

test_that("plot_composition returns a facetted ggplot for multiple proteins", {
  df <- demo_table()
  p <- plot_composition(df)

  expect_s3_class(p, "ggplot")
  expect_s3_class(p$facet, "FacetWrap")
  expect_equal(p$labels$x, "Sample")
  expect_equal(p$labels$y, "Percentage")
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("summarise = TRUE draws one stacked bar per group and protein", {
  df <- demo_table()
  p <- plot_composition(df, summarise = TRUE)
  expect_equal(p$labels$x, "Group")
  expect_equal(levels(p$data$Group), c("SampleA", "SampleB", "SampleC", "SampleD", "SampleE"))

  built <- ggplot2::ggplot_build(p)$data[[1]]
  built$bar_total <- built$ymax - built$ymin
  totals <- stats::aggregate(bar_total ~ PANEL + x, data = built, FUN = sum)

  stacked <- stats::aggregate(
    Percentage ~ Group + Protein.Name + Replicate.Name,
    data = df,
    FUN = sum
  )
  expected <- stats::aggregate(
    Percentage ~ Group + Protein.Name,
    data = stacked,
    FUN = mean
  )

  expect_equal(nrow(totals), nrow(expected))
  expect_equal(sort(totals$bar_total), sort(expected$Percentage), tolerance = 1e-12)
})

test_that("per-replicate bars equal the individual sample totals", {
  df <- demo_table()
  p <- plot_composition(df)
  built <- ggplot2::ggplot_build(p)$data[[1]]
  built$bar_total <- built$ymax - built$ymin

  totals <- stats::aggregate(bar_total ~ PANEL + x, data = built, FUN = sum)
  stacked <- stats::aggregate(
    Percentage ~ Replicate.Name + Protein.Name,
    data = df,
    FUN = sum
  )

  expect_equal(nrow(totals), nrow(stacked))
  expect_equal(sort(totals$bar_total), sort(stacked$Percentage), tolerance = 1e-12)
})

test_that("group_order controls the x-axis order", {
  df <- demo_table()
  order <- c("SampleC", "SampleA", "SampleB", "SampleD", "SampleE")
  p <- plot_composition(df, summarise = TRUE, group_order = order)
  expect_equal(levels(p$data$Group), order)
})

test_that("protein filtering keeps a single panel", {
  df <- demo_table()
  p <- plot_composition(df, protein = "H3_3-8")
  expect_equal(unique(p$data$Protein), "H3_3-8")
  expect_s3_class(p$facet, "FacetNull")
})

test_that("plot_composition uses a viridis fill scale", {
  p <- plot_composition(demo_table(), summarise = TRUE)
  built <- ggplot2::ggplot_build(p)$data[[1]]
  expect_equal(
    sort(unique(built$fill)),
    sort(viridis::viridis(4))
  )
})

test_that("plot_composition validates input and handles empty data", {
  df <- demo_table()
  expect_error(plot_composition(df, protein = "not_a_protein"), "Unknown protein")
  expect_error(
    plot_composition(df[, c("Replicate.Name", "Percentage")]),
    "missing required column"
  )

  empty <- df[0, , drop = FALSE]
  p <- plot_composition(empty)
  expect_s3_class(p, "ggplot")
  expect_silent(ggplot2::ggplot_build(p))
})
