demo_processed <- function() {
  ms1 <- utils::read.csv(
    system.file("extdata", "MS1_demo.csv", package = "HistoneMod"),
    check.names = TRUE
  )
  sample <- utils::read.csv(
    system.file("extdata", "samples_demo.csv", package = "HistoneMod"),
    check.names = TRUE
  )
  percentage_calculation(ms1, sample)
}

test_that("plot_pca_scree returns a scree plot", {
  data <- demo_processed()
  plot <- plot_pca_scree(data, transform = "clr", n_pcs = 5)
  expect_s3_class(plot, "ggplot")
  expect_gt(nrow(plot$data), 0L)
  expect_lte(nrow(plot$data), 5L)
  expect_true(all(plot$data$variance >= 0))
})

test_that("plot_pca_loadings shows the requested number of features", {
  data <- demo_processed()
  plot <- plot_pca_loadings(data, transform = "clr", pc = 1, top = 2)
  expect_s3_class(plot, "ggplot")
  expect_equal(nrow(plot$data), 2L)
  expect_true(all(c("feature", "loading") %in% names(plot$data)))
})

test_that("PCA diagnostics validate input", {
  data <- demo_processed()
  expect_error(plot_pca_scree(data, value_col = "NotThere"), "not found")
})
