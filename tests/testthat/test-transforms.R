##### Tests: compositional transforms #####

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

test_that("composition_matrix builds a sample-by-feature matrix", {
  df <- demo_table()
  mat <- composition_matrix(df)

  expect_true(is.matrix(mat))
  expect_true(is.numeric(mat))
  expect_equal(nrow(mat), 20L)
  expect_equal(ncol(mat), 4L)
  expect_equal(rownames(mat)[1], "SampleA_rep1")
  expect_equal(
    colnames(mat),
    c("H3_3_8_K4_un", "H3_3_8_K4_me3", "H4_4_17_K5_ac", "H4_4_17_K12_ac")
  )
  expect_true(all(is.finite(mat)))
  expect_true(all(mat >= 0))
})

test_that("composition_matrix fills missing combinations", {
  df <- data.frame(
    Replicate.Name = c("s1", "s1", "s2"),
    Peptide.Note = c("a", "b", "a"),
    Percentage = c(10, 20, 30),
    stringsAsFactors = FALSE
  )
  mat <- composition_matrix(df)
  expect_equal(nrow(mat), 2L)
  expect_equal(ncol(mat), 2L)
  expect_equal(mat["s1", "a"], 10)
  expect_equal(mat["s1", "b"], 20)
  expect_equal(mat["s2", "a"], 30)
  expect_equal(mat["s2", "b"], 0)

  mat_na <- composition_matrix(df, fill = NA_real_)
  expect_true(is.na(mat_na["s2", "b"]))
})

test_that("composition_matrix validates its input", {
  df <- data.frame(
    Replicate.Name = c("s1", "s1"),
    Peptide.Note = c("a", "a"),
    Percentage = c(10, 20),
    stringsAsFactors = FALSE
  )
  expect_error(composition_matrix(df), "Duplicate")

  expect_error(
    composition_matrix(df[, c("Replicate.Name", "Percentage")]),
    "missing required column"
  )

  expect_error(
    composition_matrix(data.frame(Replicate.Name = "s1", Peptide.Note = "a",
                                  Percentage = "10", stringsAsFactors = FALSE)),
    "must be numeric"
  )

  empty <- composition_matrix(df[0, , drop = FALSE])
  expect_equal(dim(empty), c(0L, 0L))
})

test_that("transform_composition none is the identity", {
  mat <- composition_matrix(demo_table())
  expect_identical(transform_composition(mat, "none"), mat)
})

test_that("CLR centres the log values per sample", {
  mat <- composition_matrix(demo_table())
  clr <- transform_composition(mat, "clr")

  expected <- log(mat) - rowMeans(log(mat))
  expect_equal(clr, expected, tolerance = 1e-12)
  expect_lt(max(abs(rowMeans(clr))), 1e-12)
  expect_identical(dimnames(clr), dimnames(mat))
})

test_that("CLR replaces zeros with half the smallest positive value", {
  x <- matrix(
    c(10, 0, 5,
      20, 10, 0,
      5, 20, 10),
    nrow = 3, byrow = TRUE,
    dimnames = list(c("s1", "s2", "s3"), c("a", "b", "c"))
  )
  pc <- 5 / 2
  x_imputed <- x
  x_imputed[x_imputed == 0] <- pc
  expected <- log(x_imputed) - rowMeans(log(x_imputed))

  clr <- transform_composition(x, "clr")
  expect_equal(clr, expected, tolerance = 1e-12)
  expect_true(all(is.finite(clr)))
  expect_identical(dimnames(clr), dimnames(x))
})

test_that("CLR of an all-zero row is zero", {
  x <- matrix(c(1, 2, 3, 0, 0, 0), nrow = 2, byrow = TRUE)
  clr <- transform_composition(x, "clr")
  expect_true(all(is.finite(clr)))
  expect_equal(clr[2, ], c(0, 0, 0), tolerance = 1e-12)
})

test_that("ALR uses the requested reference column", {
  mat <- composition_matrix(demo_table())

  alr_default <- transform_composition(mat, "alr")
  expected_default <- log(mat) - log(mat[, ncol(mat)])
  expect_equal(alr_default, expected_default, tolerance = 1e-12)

  alr_named <- transform_composition(mat, "alr", reference = "H3_3_8_K4_un")
  expected_named <- log(mat) - log(mat[, "H3_3_8_K4_un"])
  expect_equal(alr_named, expected_named, tolerance = 1e-12)

  alr_index <- transform_composition(mat, "alr", reference = 1)
  expect_equal(alr_index, expected_named, tolerance = 1e-12)

  expect_error(
    transform_composition(mat, "alr", reference = "not_a_feature"),
    "reference"
  )
  expect_error(
    transform_composition(mat, "alr", reference = 99),
    "reference"
  )
})

test_that("ALR handles zeros and preserves dimnames", {
  x <- matrix(c(10, 0, 5, 20, 10, 0), nrow = 2, byrow = TRUE,
              dimnames = list(c("s1", "s2"), c("a", "b", "c")))
  alr <- transform_composition(x, "alr")
  expect_true(all(is.finite(alr)))
  expect_identical(dimnames(alr), dimnames(x))
  expect_equal(unname(alr[, 3]), rep(0, 2), tolerance = 1e-12)
})

test_that("logit assumes proportions and rescales percentages", {
  p <- matrix(c(0.25, 0.75, 0.1, 0.9), nrow = 2, byrow = TRUE,
              dimnames = list(c("s1", "s2"), c("a", "b")))
  expected <- log(p / (1 - p))

  expect_equal(transform_composition(p, "logit"), expected, tolerance = 1e-12)
  expect_equal(
    transform_composition(p * 100, "logit"),
    expected,
    tolerance = 1e-12
  )
})

test_that("logit is finite at the boundaries", {
  p <- matrix(c(0, 1, 0, 0, 1, 1), nrow = 2, byrow = TRUE)
  logit <- transform_composition(p, "logit")
  expect_true(all(is.finite(logit)))
  expect_true(all(logit >= -40 & logit <= 40))
})

test_that("log2 adds the default pseudocount", {
  x <- matrix(c(0, 1, 2, 3), nrow = 2, byrow = TRUE)
  # Smallest positive value is 1, so the default pseudocount is 0.5.
  expect_equal(
    transform_composition(x, "log2"),
    log2(x + 0.5),
    tolerance = 1e-12
  )
})

test_that("log2 uses an explicit pseudocount", {
  x <- matrix(c(0, 1, 2, 3), nrow = 2, byrow = TRUE)
  expect_equal(
    transform_composition(x, "log2", pseudocount = 1),
    log2(x + 1),
    tolerance = 1e-12
  )
  expect_error(
    transform_composition(x, "log2", pseudocount = 0),
    "positive"
  )
})

test_that("multiplicative zero handling falls back with a warning", {
  skip_if(
    requireNamespace("zCompositions", quietly = TRUE),
    "zCompositions is installed"
  )
  x <- matrix(c(10, 0, 5, 20, 10, 0), nrow = 2, byrow = TRUE)
  expect_warning(
    out <- transform_composition(x, "clr", zeros = "multiplicative"),
    "zCompositions"
  )
  expect_true(all(is.finite(out)))
  expect_equal(dim(out), dim(x))
})

test_that("transform_composition validates its input", {
  x <- matrix(c(1, 2, 3, 4), nrow = 2)
  expect_error(transform_composition(x, "not_a_method"), "arg")
  expect_error(transform_composition(x, "clr", zeros = "bad"), "arg")
  expect_error(
    transform_composition(matrix(c(1, NA), nrow = 1), "clr"),
    "missing values"
  )
  expect_error(
    transform_composition(matrix(c(-1, 2), nrow = 1), "clr"),
    "non-negative"
  )
  expect_error(
    transform_composition(matrix(c(Inf, 2), nrow = 1), "clr"),
    "finite"
  )
  expect_error(
    transform_composition(matrix(c(0, 1, 0, 1), nrow = 2), "clr",
                          pseudocount = -1),
    "pseudocount"
  )
})

test_that("all-zero matrices transform without producing NaN", {
  x <- matrix(0, nrow = 2, ncol = 3,
              dimnames = list(c("s1", "s2"), c("a", "b", "c")))
  for (method in c("clr", "alr", "logit", "log2")) {
    out <- transform_composition(x, method)
    expect_true(all(is.finite(out)))
    expect_identical(dimnames(out), dimnames(x))
  }
})

test_that("matrix input from a data.frame keeps dimnames", {
  df <- data.frame(a = c(1, 2), b = c(3, 4), row.names = c("s1", "s2"))
  out <- transform_composition(df, "clr")
  expect_true(is.matrix(out))
  expect_equal(rownames(out), c("s1", "s2"))
  expect_equal(colnames(out), c("a", "b"))
})
