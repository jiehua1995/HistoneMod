demo_processed_qc <- function() {
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

test_that("plot_sample_correlation returns an annotated heatmap", {
  data <- demo_processed_qc()
  hm <- plot_sample_correlation(data, transform = "clr", method = "pearson")
  expect_true(inherits(hm, "pheatmap"))
  expect_equal(dim(hm$gtable$layout)[1] > 0, TRUE)
})

test_that("plot_missingness_tile returns a tile plot", {
  data <- demo_processed_qc()
  plot <- plot_missingness_tile(data)
  expect_s3_class(plot, "ggplot")
  expect_true(all(c("Replicate.Name", "Peptide.Note", "missing") %in% names(plot$data)))

  plot_zero <- plot_missingness_tile(data, count_zero = TRUE)
  expect_s3_class(plot_zero, "ggplot")
})

test_that("plot_rt_consistency handles available and missing RT columns", {
  data <- demo_processed_qc()
  plot <- plot_rt_consistency(data)
  expect_s3_class(plot, "ggplot")

  no_rt <- data[, setdiff(names(data), "Peptide.Retention.Time"), drop = FALSE]
  plot_empty <- plot_rt_consistency(no_rt)
  expect_s3_class(plot_empty, "ggplot")
})
