# Tests for R/io.R: CSV reading, sample-sheet inference, column mapping and
# input-table validation.

# Locate bundled demo data both when tests are run from the package root and
# from tests/testthat (as testthat::test_dir() does).
.hm_test_extdata <- function(file) {
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

.hm_demo_ms1 <- function() {
  read_skyline_csv(.hm_test_extdata("MS1_demo.csv"))
}

.hm_demo_sample <- function() {
  read_skyline_csv(.hm_test_extdata("samples_demo.csv"))
}

test_that("read_skyline_csv reads the bundled demo data and normalises names", {
  ms1 <- .hm_demo_ms1()

  expect_s3_class(ms1, "data.frame")
  expect_equal(nrow(ms1), 80L)
  expect_true(all(c(
    "Protein.Name", "Peptide.Note", "Replicate.Name",
    "Total.Area.MS1", "Isotope.Label.Type"
  ) %in% names(ms1)))
  expect_false(any(grepl("\\s", names(ms1))))
  expect_true(is.numeric(ms1$Total.Area.MS1))
  expect_true(is.character(ms1$Peptide.Note))
  expect_identical(attr(ms1, "delimiter"), ",")
  expect_false(isTRUE(attr(ms1, "bom")))

  sample <- .hm_demo_sample()
  expect_equal(nrow(sample), 20L)
  expect_identical(names(sample), c("Replicate.Name", "Group", "Replicate.No"))
})

test_that("read_skyline_csv detects semicolon delimiters and warns", {
  path <- withr::local_tempfile(fileext = ".csv")
  writeLines(
    c(
      "Protein Name;Peptide Note;Total Area MS1;Isotope Label Type",
      "H3;H3_K4_me3;123.5;light"
    ),
    path
  )

  expect_warning(ms1 <- read_skyline_csv(path), "decimal-comma")
  expect_identical(attr(ms1, "delimiter"), ";")
  expect_identical(
    names(ms1),
    c("Protein.Name", "Peptide.Note", "Total.Area.MS1", "Isotope.Label.Type")
  )
  expect_equal(ms1$Total.Area.MS1, 123.5)
})

test_that("read_skyline_csv detects tab delimiters without a locale warning", {
  path <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("Protein Name\tTotal Area MS1", "H3\t42"), path)

  expect_no_warning(ms1 <- read_skyline_csv(path))
  expect_identical(attr(ms1, "delimiter"), "\t")
  expect_identical(names(ms1), c("Protein.Name", "Total.Area.MS1"))
  expect_equal(ms1$Total.Area.MS1, 42)
})

test_that("read_skyline_csv strips a UTF-8 BOM", {
  path <- withr::local_tempfile(fileext = ".csv")
  con <- file(path, open = "wb")
  writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), con)
  writeBin(charToRaw("Protein Name,Total Area MS1\r\nH3,12.5\r\n"), con)
  close(con)

  ms1 <- read_skyline_csv(path)
  expect_true(attr(ms1, "bom"))
  expect_identical(names(ms1), c("Protein.Name", "Total.Area.MS1"))
  expect_equal(ms1$Protein.Name, "H3")
})

test_that("read_skyline_csv handles NA strings, header-only files and errors", {
  path <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("Protein Name,Total Area MS1", "H3,#N/A", "H4,"), path)
  ms1 <- read_skyline_csv(path)
  expect_equal(nrow(ms1), 2L)
  expect_true(all(is.na(ms1$Total.Area.MS1)))

  header_only <- withr::local_tempfile(fileext = ".csv")
  writeLines("Protein Name,Total Area MS1", header_only)
  expect_equal(nrow(read_skyline_csv(header_only)), 0L)

  empty <- withr::local_tempfile(fileext = ".csv")
  writeLines(character(0), empty)
  expect_error(read_skyline_csv(empty), "empty")

  expect_error(read_skyline_csv(file.path(tempdir(), "does-not-exist.csv")),
               "does not exist")
  expect_error(read_skyline_csv(NA_character_), "single non-empty file path")
  expect_error(read_skyline_csv(path, delim = "|"), "must be one of")
})

test_that("infer_sample_sheet derives Group and Replicate.No from demo names", {
  sample <- expect_no_warning(infer_sample_sheet(.hm_demo_ms1()))

  expect_s3_class(sample, "data.frame")
  expect_identical(names(sample), c("Replicate.Name", "Group", "Replicate.No"))
  expect_equal(nrow(sample), 20L)
  expect_identical(unique(sample$Group), paste0("Sample", LETTERS[1:5]))
  expect_identical(sample$Replicate.No, rep(1:4, 5))
  expect_true(is.integer(sample$Replicate.No))
})

test_that("infer_sample_sheet falls back when the pattern does not match", {
  ms1 <- data.frame(
    Replicate.Name = c("alpha_rep1", "alpha_rep2", "weird", "beta_rep1"),
    stringsAsFactors = FALSE
  )

  expect_warning(sample <- infer_sample_sheet(ms1), "did not match")
  expect_identical(
    sample$Group,
    c("alpha", "alpha", "weird", "beta")
  )
  expect_identical(sample$Replicate.No, c(1L, 2L, 1L, 1L))
})

test_that("infer_sample_sheet supports custom patterns and character input", {
  sample <- expect_no_warning(
    infer_sample_sheet(c("ctrl-1", "ctrl-2", "trt-1"), pattern = "-\\d+$")
  )
  expect_identical(sample$Group, c("ctrl", "ctrl", "trt"))
  expect_identical(sample$Replicate.No, c(1L, 2L, 1L))

  expect_error(
    infer_sample_sheet(data.frame(other = 1)),
    "Column 'Replicate.Name' not found"
  )
  expect_error(infer_sample_sheet(character(0)), "No replicate names")
})

test_that("map_columns renames canonical columns and keeps extras", {
  data <- data.frame(
    "Protein Name" = "H3",
    "Peptide Note" = "H3_K4_me3",
    "Total Area MS1" = 100,
    extra = "keep",
    check.names = FALSE
  )

  mapped <- map_columns(
    data,
    c(
      Protein.Name = "Protein Name",
      Peptide.Note = "Peptide Note",
      Total.Area.MS1 = "Total Area MS1"
    )
  )
  expect_identical(
    names(mapped),
    c("Protein.Name", "Peptide.Note", "Total.Area.MS1", "extra")
  )
  expect_equal(mapped$Total.Area.MS1, 100)

  # Named lists work as well.
  mapped_list <- map_columns(data, list(Protein.Name = "Protein Name"))
  expect_true("Protein.Name" %in% names(mapped_list))
  expect_true("extra" %in% names(mapped_list))
})

test_that("map_columns reports mapping problems", {
  data <- data.frame("Protein Name" = "H3", Total.Area.MS1 = 1,
                     check.names = FALSE)

  expect_error(
    map_columns(data, c(Replicate.Name = "Replicate Name")),
    "Source column\\(s\\) not found"
  )
  expect_error(map_columns(data, c("Protein Name")), "must be named")
  expect_error(map_columns(data, c(Protein.Name = "Total.Area.MS1")),
               "already exists")
  expect_error(map_columns(data, c(Protein.Name = "Protein Name",
                                   Peptide.Note = "Protein Name")),
               "more than one canonical name")
})

test_that("validate_input_tables accepts the bundled demo data", {
  result <- validate_input_tables(.hm_demo_ms1(), .hm_demo_sample())

  expect_true(result$valid)
  expect_length(result$errors, 0L)
  expect_length(result$warnings, 0L)
  expect_true(is.numeric(result$ms1$Total.Area.MS1))
  expect_true(is.integer(result$sample$Replicate.No))
  expect_false(any(grepl("\\s", names(result$ms1))))
})

test_that("validate_input_tables reports structural errors", {
  ms1 <- data.frame(
    Protein.Name = "H3",
    Peptide.Note = "H3_K4_me3",
    Replicate.Name = "A_rep1",
    Total.Area.MS1 = 100,
    Isotope.Label.Type = "light",
    stringsAsFactors = FALSE
  )
  sample <- data.frame(
    Replicate.Name = "A_rep1",
    Group = "A",
    Replicate.No = 1L,
    stringsAsFactors = FALSE
  )

  expect_true(validate_input_tables(ms1, sample)$valid)

  empty <- validate_input_tables(ms1[0, , drop = FALSE],
                                 sample[0, , drop = FALSE])
  expect_false(empty$valid)
  expect_true(any(grepl("MS1 table is empty", empty$errors)))
  expect_true(any(grepl("Sample table is empty", empty$errors)))

  missing_ms1 <- validate_input_tables(
    ms1[, setdiff(names(ms1), "Isotope.Label.Type"), drop = FALSE], sample
  )
  expect_false(missing_ms1$valid)
  expect_true(any(grepl("Missing required column\\(s\\) in MS1",
                        missing_ms1$errors)))

  missing_sample <- validate_input_tables(
    ms1, sample[, setdiff(names(sample), "Group"), drop = FALSE]
  )
  expect_false(missing_sample$valid)
  expect_true(any(grepl("Missing required column\\(s\\) in sample",
                        missing_sample$errors)))

  null_ms1 <- validate_input_tables(NULL, sample)
  expect_false(null_ms1$valid)
  expect_true(any(grepl("is NULL", null_ms1$errors)))
})

test_that("validate_input_tables detects area, duplicate and overlap problems", {
  ms1 <- data.frame(
    Protein.Name = "H3",
    Peptide.Note = c("H3_K4_me3", "H3_K4_me3"),
    Replicate.Name = "A_rep1",
    Total.Area.MS1 = c(100, 50),
    Isotope.Label.Type = c("light", "heavy"),
    stringsAsFactors = FALSE
  )
  sample <- data.frame(
    Replicate.Name = c("A_rep1", "B_rep1"),
    Group = c("A", "B"),
    Replicate.No = c(1L, 1L),
    stringsAsFactors = FALSE
  )

  # Unexpected isotope labels are a warning, not an error.
  isotope <- validate_input_tables(
    transform(ms1, Isotope.Label.Type = c("light", "medium")), sample
  )
  expect_true(isotope$valid)
  expect_true(any(grepl("Unexpected Isotope.Label.Type", isotope$warnings)))

  non_numeric <- ms1
  non_numeric$Total.Area.MS1 <- as.character(non_numeric$Total.Area.MS1)
  non_numeric$Total.Area.MS1[1] <- "not-a-number"
  area_result <- validate_input_tables(non_numeric, sample)
  expect_false(area_result$valid)
  expect_true(any(grepl("non-numeric", area_result$errors)))

  duplicate_sample <- rbind(sample, sample[1, , drop = FALSE])
  dup_result <- validate_input_tables(ms1, duplicate_sample)
  expect_false(dup_result$valid)
  expect_true(any(grepl("duplicated Replicate.Name", dup_result$errors)))

  duplicate_keys <- rbind(ms1, ms1[1, , drop = FALSE])
  dup_key_result <- validate_input_tables(duplicate_keys, sample)
  expect_false(dup_key_result$valid)
  expect_true(any(grepl("duplicated Replicate.Name x Peptide.Note",
                        dup_key_result$errors)))

  # Replicate mismatches are warnings but the tables remain usable.
  ms1_mismatch <- rbind(
    ms1,
    transform(ms1[1, , drop = FALSE], Replicate.Name = "C_rep1",
              Isotope.Label.Type = "light")
  )
  mismatch <- validate_input_tables(ms1_mismatch, sample)
  expect_true(mismatch$valid)
  expect_true(any(grepl("missing from the sample table", mismatch$warnings)))
  expect_true(any(grepl("missing from MS1", mismatch$warnings)))

  no_overlap_sample <- sample
  no_overlap_sample$Replicate.Name <- "Z_rep1"
  no_overlap <- validate_input_tables(ms1, no_overlap_sample)
  expect_false(no_overlap$valid)
  expect_true(any(grepl("No overlapping Replicate.Name", no_overlap$errors)))

  bad_rep_no <- sample
  bad_rep_no$Replicate.No <- c("one", "two")
  rep_no_result <- validate_input_tables(ms1, bad_rep_no)
  expect_true(any(grepl("non-numeric Replicate.No", rep_no_result$warnings)))
})
