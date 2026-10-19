test_that("save_plot_object writes ggplot and pheatmap output", {
  data <- data.frame(x = 1:5, y = 1:5)
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = x, y = y)) + ggplot2::geom_point()

  png <- tempfile(fileext = ".png")
  pdf <- tempfile(fileext = ".pdf")
  on.exit(unlink(c(png, pdf)), add = TRUE)

  save_plot_object(plot, png, format = "png", width = 4, height = 3, dpi = 100)
  save_plot_object(plot, pdf, format = "pdf", width = 4, height = 3)
  expect_true(file.exists(png))
  expect_true(file.exists(pdf))
  expect_gt(file.size(png), 0)
  expect_gt(file.size(pdf), 0)
})

test_that("save_plot_object supports pheatmap results", {
  mat <- matrix(1:12, nrow = 3, dimnames = list(paste0("r", 1:3), paste0("c", 1:4)))
  hm <- pheatmap::pheatmap(mat, silent = TRUE)

  png <- tempfile(fileext = ".png")
  pdf <- tempfile(fileext = ".pdf")
  on.exit(unlink(c(png, pdf)), add = TRUE)

  save_plot_object(hm, png, format = "png", width = 4, height = 3, dpi = 100)
  save_plot_object(hm, pdf, format = "pdf", width = 4, height = 3)
  expect_true(file.exists(png))
  expect_true(file.exists(pdf))
  expect_gt(file.size(png), 0)
})

test_that("save_plot_object rejects unsupported objects", {
  expect_error(save_plot_object(1:3, tempfile()), "Unsupported plot object")
})
