demo_paths <- function() {
  list(
    ms1 = system.file("extdata", "MS1_demo.csv", package = "HistoneMod"),
    sample = system.file("extdata", "samples_demo.csv", package = "HistoneMod")
  )
}

test_that("run_histonemod_batch processes a file pair", {
  paths <- demo_paths()
  dir <- tempfile("histonemod-batch-")
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  result <- run_histonemod_batch(
    paths$ms1, paths$sample, dir,
    settings = list(quant_mode = "percentage", exclude_un = TRUE, transform = "clr"),
    labels = "demo"
  )

  expect_equal(nrow(result), 1L)
  expect_true(result$valid)
  expect_true(result$n_rows > 0)
  expect_true(file.exists(result$project_file))
  expect_true(file.exists(result$processed_file))

  project <- load_project(result$project_file)
  expect_equal(project$format, "HistoneMod-project")
  expect_true(is.data.frame(project$results$processed))
})

test_that("run_histonemod_batch handles invalid input", {
  dir <- tempfile("histonemod-batch-bad-")
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  bad <- tempfile(fileext = ".csv")
  on.exit(unlink(bad), add = TRUE)
  writeLines("a,b\n1,2", bad)

  result <- run_histonemod_batch(bad, bad, dir, labels = "bad")
  expect_false(result$valid)
  expect_match(result$message, "missing required|Missing required|column", ignore.case = TRUE)
})

make_experiment <- function(shift) {
  samples <- paste0(rep(c("ctrl", "trt"), each = 4), "_rep", rep(1:4, 2))
  groups <- rep(c("ctrl", "trt"), each = 4)
  features <- c("H3_3_8_K4_me3", "H4_4_17_K5_ac")
  rows <- expand.grid(sample = samples, feature = features, stringsAsFactors = FALSE)
  rows$Group <- ifelse(grepl("^ctrl", rows$sample), "ctrl", "trt")
  base <- c(40, 60)[match(rows$feature, features)]
  effect <- ifelse(rows$Group == "trt", shift, 0)
  rows$Percentage <- pmax(1, base + effect + rep(c(-1, 0, 1, 0), length.out = nrow(rows)))
  data.frame(
    Replicate.Name = rows$sample,
    Group = rows$Group,
    Protein.Name = ifelse(grepl("^H3", rows$feature), "H3_3-8", "H4_4-17"),
    Peptide.Note = rows$feature,
    Percentage = rows$Percentage,
    stringsAsFactors = FALSE
  )
}

test_that("compare_experiments correlates effects across experiments", {
  data_a <- make_experiment(shift = c(10, 0))
  data_b <- make_experiment(shift = c(10, 0))

  comparison <- compare_experiments(
    data_a, data_b,
    groups_a = c("ctrl", "trt"),
    groups_b = c("ctrl", "trt"),
    value_col = "Percentage",
    label_a = "A", label_b = "B"
  )

  expect_equal(comparison$summary$n_features, 2L)
  expect_true(is.finite(comparison$summary$pearson))
  expect_equal(comparison$summary$concordance, 1)

  plot <- plot_experiment_comparison(comparison)
  expect_s3_class(plot, "ggplot")
})

test_that("compare_experiments detects discordant effects", {
  data_a <- make_experiment(shift = c(10, 0))
  data_b <- make_experiment(shift = c(-10, 0))

  comparison <- compare_experiments(
    data_a, data_b,
    groups_a = c("ctrl", "trt"),
    groups_b = c("ctrl", "trt")
  )
  expect_lt(comparison$summary$concordance, 1)
})
