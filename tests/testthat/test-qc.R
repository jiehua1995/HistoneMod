##### Tests: quality control #####

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

long_from_matrix <- function(mat, groups, protein = "P", rt = NULL) {
  samples <- rownames(mat)
  features <- colnames(mat)
  n <- length(samples)
  out <- data.frame(
    Replicate.Name = rep(samples, times = length(features)),
    Group = rep(as.character(groups), times = length(features)),
    Protein.Name = protein,
    Peptide.Note = rep(features, each = n),
    Percentage = as.vector(mat),
    stringsAsFactors = FALSE
  )
  if (!is.null(rt)) {
    out$Peptide.Retention.Time <- as.vector(rt)
  }
  out
}

test_that("qc_summary summarises the demo dataset", {
  qc <- qc_summary(demo_table())

  expect_equal(sort(names(qc)), sort(c("sample_qc", "feature_qc", "rt_col")))
  expect_equal(qc$rt_col, "Peptide.Retention.Time")

  sample_qc <- qc$sample_qc
  expect_equal(
    names(sample_qc),
    c("sample", "group", "n_features", "total_value", "missing_rate", "n_groups")
  )
  expect_equal(nrow(sample_qc), 20L)
  expect_equal(unique(sample_qc$n_features), 4L)
  expect_equal(unique(sample_qc$total_value), 200)
  expect_equal(unique(sample_qc$missing_rate), 0)
  expect_equal(unique(sample_qc$n_groups), 5L)

  feature_qc <- qc$feature_qc
  expect_equal(
    names(feature_qc),
    c("feature", "protein", "detection_rate", "mean_cv_within_group",
      "rt_sd", "n_rt_outliers")
  )
  expect_equal(nrow(feature_qc), 4L)
  expect_equal(unique(feature_qc$detection_rate), 1)
  expect_true(all(is.finite(feature_qc$mean_cv_within_group)))
  expect_true(all(feature_qc$mean_cv_within_group >= 0))
  expect_true(all(is.finite(feature_qc$rt_sd)))
  expect_equal(unique(feature_qc$n_rt_outliers), 0L)
})

test_that("missing sample/feature combinations are detected as missing", {
  mat <- rbind(
    c(10, 20, 70),
    c(10, 20, 0)
  )
  dimnames(mat) <- list(c("s1", "s2"), c("a", "b", "c"))
  df <- long_from_matrix(mat, c("G1", "G1"))
  df <- df[!(df$Replicate.Name == "s2" & df$Peptide.Note == "c"), ]

  qc <- qc_summary(df)
  sample_qc <- qc$sample_qc
  expect_equal(sample_qc$n_features, c(3L, 2L))
  expect_equal(sample_qc$total_value, c(100, 30))
  expect_equal(sample_qc$missing_rate, c(0, 1 / 3))

  feature_qc <- qc$feature_qc
  expect_equal(
    feature_qc$detection_rate[feature_qc$feature == "c"],
    0.5
  )
  expect_equal(
    feature_qc$detection_rate[feature_qc$feature == "a"],
    1
  )
})

test_that("within-group CV is sd / mean averaged over groups", {
  mat <- rbind(
    c(10, 10),
    c(10, 20),
    c(10, 10),
    c(10, 20)
  )
  dimnames(mat) <- list(c("g1a", "g1b", "g2a", "g2b"), c("f_const", "f_var"))
  df <- long_from_matrix(mat, c("G1", "G1", "G2", "G2"))

  qc <- qc_summary(df)
  feature_qc <- qc$feature_qc

  expect_equal(feature_qc$mean_cv_within_group[feature_qc$feature == "f_const"], 0)
  expected_cv <- stats::sd(c(10, 20)) / mean(c(10, 20))
  expect_equal(
    feature_qc$mean_cv_within_group[feature_qc$feature == "f_var"],
    expected_cv
  )
})

test_that("retention time outliers use a 3 MAD threshold", {
  mat <- matrix(
    c(10, 90, 20, 80, 30, 70, 40, 60),
    nrow = 4, byrow = TRUE,
    dimnames = list(c("s1", "s2", "s3", "s4"), c("a", "b"))
  )
  rt <- rbind(
    c(10, 5),
    c(10.1, 5.1),
    c(9.9, 5.2),
    c(25, 5.05)
  )
  df <- long_from_matrix(mat, rep("G1", 4), rt = rt)

  qc <- qc_summary(df)
  feature_qc <- qc$feature_qc
  expect_equal(qc$rt_col, "Peptide.Retention.Time")
  expect_equal(feature_qc$n_rt_outliers[feature_qc$feature == "a"], 1L)
  expect_equal(feature_qc$n_rt_outliers[feature_qc$feature == "b"], 0L)
  expect_true(feature_qc$rt_sd[feature_qc$feature == "a"] > 0)
})

test_that("absent retention time columns are handled gracefully", {
  mat <- rbind(
    c(10, 20, 70),
    c(20, 30, 50),
    c(15, 25, 60),
    c(25, 35, 40)
  )
  dimnames(mat) <- list(c("s1", "s2", "s3", "s4"), c("a", "b", "c"))
  df <- long_from_matrix(mat, c("G1", "G1", "G2", "G2"))

  qc <- qc_summary(df)
  expect_true(is.na(qc$rt_col))
  expect_true(all(is.na(qc$feature_qc$rt_sd)))
  expect_true(all(is.na(qc$feature_qc$n_rt_outliers)))
})

test_that("qc_summary validates its input", {
  expect_error(
    qc_summary(data.frame(Replicate.Name = "s1", Peptide.Note = "a")),
    "missing required column"
  )
  expect_error(
    qc_summary(data.frame(
      Replicate.Name = "s1",
      Peptide.Note = "a",
      Percentage = "10",
      stringsAsFactors = FALSE
    )),
    "must be numeric"
  )

  empty <- qc_summary(data.frame(
    Replicate.Name = character(0),
    Group = character(0),
    Protein.Name = character(0),
    Peptide.Note = character(0),
    Percentage = numeric(0),
    stringsAsFactors = FALSE
  ))
  expect_equal(nrow(empty$sample_qc), 0L)
  expect_equal(nrow(empty$feature_qc), 0L)
})

test_that("plot_qc_missingness accepts all supported inputs", {
  qc <- qc_summary(demo_table())

  p1 <- plot_qc_missingness(qc)
  expect_s3_class(p1, "ggplot")
  expect_silent(ggplot2::ggplot_build(p1))

  p2 <- plot_qc_missingness(qc$sample_qc)
  expect_s3_class(p2, "ggplot")

  p3 <- plot_qc_missingness(demo_table())
  expect_s3_class(p3, "ggplot")
})

test_that("plot_qc_cv plots the highest CV features", {
  qc <- qc_summary(demo_table())

  p <- plot_qc_cv(qc)
  expect_s3_class(p, "ggplot")
  expect_silent(ggplot2::ggplot_build(p))

  p_top <- plot_qc_cv(qc$feature_qc, max_features = 2, cv_threshold = 0.1)
  built <- ggplot2::ggplot_build(p_top)
  expect_equal(nrow(built$data[[1]]), 2L)

  p_plain <- plot_qc_cv(qc, max_features = 1)
  expect_s3_class(p_plain, "ggplot")
})

test_that("plot_qc_cv handles features without CV values", {
  df <- data.frame(
    Replicate.Name = c("s1", "s1", "s2", "s2"),
    Group = c("G1", "G1", "G2", "G2"),
    Protein.Name = "P",
    Peptide.Note = c("a", "b", "a", "b"),
    Percentage = c(10, 20, 30, 40),
    stringsAsFactors = FALSE
  )
  # One sample per group -> no within-group CV can be computed.
  qc <- qc_summary(df)
  expect_true(all(is.na(qc$feature_qc$mean_cv_within_group)))

  p <- plot_qc_cv(qc)
  expect_s3_class(p, "ggplot")
  expect_silent(ggplot2::ggplot_build(p))
})
