# Tests for R/session.R: saving and loading HistoneMod session files.

.hm_session_extdata <- function(file) {
  candidates <- c(
    system.file("extdata", file, package = "HistoneMod"),
    file.path("inst", "extdata", file),
    file.path("..", "..", "inst", "extdata", file),
    file.path("..", "..", "..", "inst", "extdata", file)
  )
  candidates <- candidates[nzchar(candidates)]
  found <- candidates[file.exists(candidates)]
  if (length(found) == 0L) {
    testthat::skip(paste("Bundled extdata file not found:", file))
  }
  found[[1L]]
}

.hm_session_demo_tables <- function() {
  list(
    ms1 = read_skyline_csv(.hm_session_extdata("MS1_demo.csv")),
    sample = read_skyline_csv(.hm_session_extdata("samples_demo.csv"))
  )
}

test_that("save_session and load_session round-trip the data and settings", {
  tables <- .hm_session_demo_tables()
  path <- withr::local_tempfile(fileext = ".rds")
  settings <- list(exclude_un = TRUE, color_palette = "viridis", n = 3L)

  result <- save_session(path, tables$ms1, tables$sample, settings = settings)
  expect_true(file.exists(path))
  expect_false(is.null(result))

  session <- load_session(path)
  expect_type(session, "list")
  expect_identical(session$format, "HistoneMod-session")
  expect_true(is.character(session$version))
  expect_true(nzchar(session$version))
  expect_s3_class(session$created, "POSIXct")
  expect_identical(session$ms1, tables$ms1)
  expect_identical(session$sample, tables$sample)
  expect_identical(session$settings, settings)
  expect_identical(names(session),
                   c("format", "version", "created", "ms1", "sample", "settings"))
})

test_that("save_session defaults settings to an empty list", {
  tables <- .hm_session_demo_tables()
  path <- withr::local_tempfile(fileext = ".rds")
  save_session(path, tables$ms1, tables$sample)
  expect_identical(load_session(path)$settings, list())
})

test_that("save_session validates its inputs", {
  data <- data.frame(a = 1)
  path <- withr::local_tempfile(fileext = ".rds")

  expect_error(save_session(NA_character_, data, data), "single non-empty file path")
  expect_error(save_session(path, "not a data frame", data), "`ms1` must be a data frame")
  expect_error(save_session(path, data, "not a data frame"), "`sample` must be a data frame")
  expect_error(save_session(path, data, data, settings = "nope"),
               "`settings` must be a list")
})

test_that("load_session gives informative errors for unusable files", {
  expect_error(load_session(file.path(tempdir(), "missing-session.rds")),
               "does not exist")
  expect_error(load_session(NA_character_), "single non-empty file path")

  not_rds <- withr::local_tempfile(fileext = ".rds")
  writeLines("not an RDS file", not_rds)
  expect_error(load_session(not_rds), "Could not read session file")

  wrong_object <- withr::local_tempfile(fileext = ".rds")
  saveRDS(list(ms1 = data.frame(a = 1), sample = data.frame(b = 1)), wrong_object)
  expect_error(load_session(wrong_object), "format")

  missing_tables <- withr::local_tempfile(fileext = ".rds")
  saveRDS(list(format = "HistoneMod-session"), missing_tables)
  expect_error(load_session(missing_tables), "ms1")

  bad_sample <- withr::local_tempfile(fileext = ".rds")
  saveRDS(list(format = "HistoneMod-session",
               ms1 = data.frame(a = 1),
               sample = NULL), bad_sample)
  expect_error(load_session(bad_sample), "sample")

  bad_settings <- withr::local_tempfile(fileext = ".rds")
  saveRDS(list(format = "HistoneMod-session",
               ms1 = data.frame(a = 1),
               sample = data.frame(b = 1),
               settings = "nope"), bad_settings)
  expect_error(load_session(bad_settings), "settings")
})
