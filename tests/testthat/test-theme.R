test_that("theme_histonemod returns a ggplot theme", {
  theme <- theme_histonemod()
  expect_s3_class(theme, "theme")
  expect_s3_class(theme_histonemod(base_size = 14), "theme")
})

test_that("histonemod_palette returns the requested number of colours", {
  expect_length(histonemod_palette(4), 4L)
  expect_true(all(grepl("^#", histonemod_palette(8))))
  expect_equal(histonemod_palette(2, type = "okabe_ito"),
               c("#000000", "#E69F00"))
  expect_length(histonemod_palette(5, type = "tol"), 5L)
  expect_length(histonemod_palette(5, type = "viridis"), 5L)
  expect_length(histonemod_palette(20, type = "okabe_ito"), 20L)
})

test_that("accessible scales build", {
  expect_s3_class(scale_colour_histonemod(), "ScaleDiscrete")
  expect_s3_class(scale_fill_histonemod(type = "tol"), "ScaleDiscrete")
})
