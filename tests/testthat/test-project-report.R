demo_tables <- function() {
  ms1 <- utils::read.csv(
    system.file("extdata", "MS1_demo.csv", package = "HistoneMod"),
    check.names = TRUE
  )
  sample <- utils::read.csv(
    system.file("extdata", "samples_demo.csv", package = "HistoneMod"),
    check.names = TRUE
  )
  list(ms1 = ms1, sample = sample)
}

test_that("project files round-trip inputs, settings and results", {
  tables <- demo_tables()
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)

  settings <- list(transform = "clr", exclude_un = TRUE)
  results <- list(differential = data.frame(feature = "H3_3_8_K4_me3", p_adj = 0.01))
  save_project(path, tables$ms1, tables$sample, settings, results)

  project <- load_project(path)
  expect_equal(project$format, "HistoneMod-project")
  expect_equal(project$settings, settings)
  expect_equal(project$results, results)
  expect_equal(nrow(project$ms1), nrow(tables$ms1))
})

test_that("load_project rejects unrelated files", {
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  saveRDS(list(a = 1), path)
  expect_error(load_project(path), "not a HistoneMod project")
  expect_error(load_project(tempfile()), "does not exist")
})

test_that("export_analysis_bundle writes the expected files", {
  tables <- demo_tables()
  dir <- tempfile("histonemod-bundle-")
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  export_analysis_bundle(
    dir, tables$ms1, tables$sample,
    settings = list(transform = "clr"),
    results = list()
  )

  expect_true(file.exists(file.path(dir, "MS1.csv")))
  expect_true(file.exists(file.path(dir, "samples.csv")))
  expect_true(file.exists(file.path(dir, "project.rds")))
  expect_true(file.exists(file.path(dir, "analysis.R")))
  expect_true(file.exists(file.path(dir, "README.md")))
})

test_that("methods_text reflects the settings", {
  text <- methods_text(list(
    quant_mode = "percentage",
    transform = "clr",
    exclude_un = TRUE,
    test = "Welch t-test",
    p_adjust = "BH"
  ))
  expect_type(text, "character")
  expect_match(text, "CLR")
  expect_match(text, "Benjamini")
  expect_match(text, "HistoneMod v")
})

test_that("figure_legend returns readable text for plot types", {
  expect_match(figure_legend("pca", list(transform = "clr")), "CLR")
  expect_match(figure_legend("volcano"), "Volcano")
  expect_match(figure_legend("occupancy"), "occupancy", ignore.case = TRUE)
  expect_error(figure_legend("not_a_plot"), "should be one of")
})

test_that("statistics_table adds stars and sorts by adjusted p-value", {
  results <- data.frame(
    protein = c("H3", "H3", "H4"),
    feature = c("a", "b", "c"),
    p_value = c(0.2, 0.0001, 0.01),
    p_adj = c(0.2, 0.0005, 0.02),
    direction = c("ns", "up", "down"),
    stringsAsFactors = FALSE
  )
  table <- statistics_table(results)
  expect_true("significance" %in% names(table))
  expect_equal(table$feature[1], "b")
  expect_equal(table$significance[1], "***")
  expect_true(all(diff(table$p_adj) >= 0))
})
