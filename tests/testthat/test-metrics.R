##### Tests: per-sample metrics #####

metric_table <- function() {
  data.frame(
    Replicate.Name = c("S1", "S1", "S1", "S2", "S2", "S2"),
    Group = c("G1", "G1", "G1", "G2", "G2", "G2"),
    Protein.Name = "P",
    Peptide.Note = c("K4me3", "K4ac", "K4un", "K4me3", "K4ac", "K4un"),
    Percentage = c(10, 30, 60, 20, 20, 60),
    stringsAsFactors = FALSE
  )
}

test_that("compute_metrics returns the documented layout", {
  specs <- data.frame(
    metric = c("me3_share", "modified_ratio"),
    numerator = c("^K4me3$", "^K4(me3|ac)$"),
    denominator = c("all", "^K4un$"),
    stringsAsFactors = FALSE
  )
  res <- compute_metrics(metric_table(), specs)

  expect_s3_class(res, "data.frame")
  expect_equal(names(res), c("sample", "group", "metric", "value"))
  expect_equal(nrow(res), 4L)
  expect_equal(unique(res$sample), c("S1", "S2"))
  expect_equal(unique(res$metric), c("me3_share", "modified_ratio"))
})

test_that("compute_metrics performs the documented arithmetic", {
  specs <- data.frame(
    metric = c("me3_share", "modified_ratio", "modified_ratio_scaled"),
    numerator = c("^K4me3$", "^K4(me3|ac)$", "^K4(me3|ac)$"),
    denominator = c("all", "^K4un$", "^K4un$"),
    scale = c(100, 100, 1),
    stringsAsFactors = FALSE
  )
  res <- compute_metrics(metric_table(), specs)

  get_value <- function(metric, sample) {
    res$value[res$metric == metric & res$sample == sample]
  }

  expect_equal(get_value("me3_share", "S1"), 10)
  expect_equal(get_value("me3_share", "S2"), 20)
  expect_equal(get_value("modified_ratio", "S1"), 100 * 40 / 60)
  expect_equal(get_value("modified_ratio", "S2"), 100 * 40 / 60)
  expect_equal(get_value("modified_ratio_scaled", "S1"), 40 / 60)
})

test_that("denominator 'all' uses the total of all features", {
  data <- data.frame(
    Replicate.Name = c("S1", "S1", "S2", "S2"),
    Group = "G1",
    Peptide.Note = c("a", "b", "a", "b"),
    Percentage = c(25, 75, 80, 20),
    stringsAsFactors = FALSE
  )
  specs <- data.frame(
    metric = "a_share",
    numerator = "^a$",
    denominator = "all",
    stringsAsFactors = FALSE
  )
  res <- compute_metrics(data, specs)
  expect_equal(res$value, c(25, 80))
})

test_that("zero denominators produce NA", {
  data <- data.frame(
    Replicate.Name = c("S1", "S1", "S2", "S2"),
    Group = "G1",
    Peptide.Note = c("a", "zero", "a", "zero"),
    Percentage = c(10, 90, 20, 0),
    stringsAsFactors = FALSE
  )
  specs <- data.frame(
    metric = "a_over_zero",
    numerator = "^a$",
    denominator = "^zero$",
    stringsAsFactors = FALSE
  )
  res <- compute_metrics(data, specs)
  expect_equal(res$value[res$sample == "S1"], 10 / 90 * 100)
  expect_true(is.na(res$value[res$sample == "S2"]))
})

test_that("metrics with no numerator matches are zero", {
  specs <- data.frame(
    metric = "nothing",
    numerator = "^absent$",
    denominator = "all",
    stringsAsFactors = FALSE
  )
  res <- compute_metrics(metric_table(), specs)
  expect_equal(res$value, c(0, 0))
})

test_that("compute_metrics handles a missing group column", {
  data <- metric_table()
  data$Group <- NULL
  specs <- data.frame(
    metric = "me3_share",
    numerator = "^K4me3$",
    denominator = "all",
    stringsAsFactors = FALSE
  )
  res <- compute_metrics(data, specs)
  expect_true(all(is.na(res$group)))
  expect_equal(res$value, c(10, 20))
})

test_that("compute_metrics validates regex and spec columns", {
  data <- metric_table()
  bad_specs <- data.frame(
    metric = "bad",
    numerator = "(",
    denominator = "all",
    stringsAsFactors = FALSE
  )
  expect_error(compute_metrics(data, bad_specs), "Invalid regular expression")

  expect_error(
    compute_metrics(data, data.frame(metric = "x", numerator = "a")),
    "missing required column"
  )

  empty_specs <- data.frame(
    metric = character(0),
    numerator = character(0),
    denominator = character(0),
    stringsAsFactors = FALSE
  )
  res <- compute_metrics(data, empty_specs)
  expect_equal(nrow(res), 0L)
  expect_equal(names(res), c("sample", "group", "metric", "value"))

  no_data <- compute_metrics(
    data[0, , drop = FALSE],
    data.frame(metric = "a_share", numerator = "^a$", denominator = "all",
               stringsAsFactors = FALSE)
  )
  expect_equal(nrow(no_data), 0L)
  expect_equal(names(no_data), c("sample", "group", "metric", "value"))
})
