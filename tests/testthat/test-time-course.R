test_that("fit_time_course recovers a decay half-life", {
  times <- c(0, 1, 2, 4, 8, 12)
  values <- 20 + 80 * exp(-0.25 * times)
  data <- data.frame(
    Protein.Name = "H3",
    Peptide.Note = "H3K9me3",
    Time = times,
    Percentage = values
  )

  fits <- fit_time_course(data, value_col = "Percentage")
  expect_equal(nrow(fits), 1L)
  expect_true(fits$converged)
  expect_true(fits$model %in% c("decay", "linear"))
  if (identical(fits$model, "decay")) {
    expect_equal(fits$half_life, log(2) / 0.25, tolerance = 0.15)
    expect_equal(fits$direction, "decrease")
  }

  plot <- plot_time_course(data, fits, value_col = "Percentage")
  expect_s3_class(plot, "ggplot")
})

test_that("fit_time_course handles too few points", {
  data <- data.frame(
    Protein.Name = "H3",
    Peptide.Note = "H3K4me3",
    Time = c(0, 1),
    Percentage = c(40, 45)
  )
  fits <- fit_time_course(data, value_col = "Percentage")
  expect_equal(nrow(fits), 1L)
  expect_false(fits$converged)
  expect_true(is.na(fits$half_life))
})
