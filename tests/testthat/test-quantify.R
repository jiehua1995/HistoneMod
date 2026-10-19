# Tests for R/quantify.R: quantification modes and value normalisation.

.hm_quant_extdata <- function(file) {
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

.hm_demo_quant_tables <- function() {
  list(
    ms1 = read_skyline_csv(.hm_quant_extdata("MS1_demo.csv")),
    sample = read_skyline_csv(.hm_quant_extdata("samples_demo.csv"))
  )
}

# Synthetic light/heavy data: one protein, one peptide, four replicates with
# light/heavy ratios 10, 10, 30 and 20.
.hm_ratio_tables <- function() {
  replicate <- c("A_rep1", "A_rep2", "B_rep1", "B_rep2")
  ms1 <- data.frame(
    Replicate.Name = rep(replicate, each = 2L),
    Protein.Name = "H3",
    Peptide.Note = "H3_K4_me3",
    Isotope.Label.Type = rep(c("light", "heavy"), times = 4L),
    Total.Area.MS1 = c(100, 10, 200, 20, 300, 10, 400, 20),
    stringsAsFactors = FALSE
  )
  sample <- data.frame(
    Replicate.Name = replicate,
    Group = c("A", "A", "B", "B"),
    Replicate.No = c(1L, 2L, 1L, 2L),
    stringsAsFactors = FALSE
  )
  list(ms1 = ms1, sample = sample)
}

test_that("quantify_peptides percentage mode reproduces percentage_calculation", {
  tables <- .hm_demo_quant_tables()

  reference <- percentage_calculation(
    tables$ms1, tables$sample, exclude_un = FALSE, max_percentage = 100
  )
  quantified <- quantify_peptides(tables$ms1, tables$sample, mode = "percentage")

  expect_s3_class(quantified, "data.frame")
  expect_equal(nrow(quantified), nrow(reference))
  expect_identical(
    names(quantified),
    c("Replicate.Name", "Protein.Name", "Peptide.Note", "Group",
      "Value", "Percentage")
  )

  keys <- c("Replicate.Name", "Protein.Name", "Peptide.Note")
  merged <- merge(
    reference[, c(keys, "Percentage")],
    quantified[, c(keys, "Percentage")],
    by = keys
  )
  expect_equal(nrow(merged), nrow(reference))
  expect_equal(merged$Percentage.x, merged$Percentage.y, tolerance = 1e-10)
  expect_equal(quantified$Value, quantified$Percentage)

  expect_identical(attr(quantified, "value_type"), "percentage")
  expect_identical(attr(quantified, "method"), "percentage")
  expect_identical(attr(quantified, "normalize"), "none")

  # percentage_calculation() filters unmodified peptides by default, while
  # quantify_peptides() intentionally does not.
  expect_lt(nrow(percentage_calculation(tables$ms1, tables$sample)),
            nrow(quantified))
})

test_that("quantify_peptides percentage mode sums duplicate rows and ignores heavy", {
  ms1 <- data.frame(
    Replicate.Name = c("A_rep1", "A_rep1", "A_rep1", "A_rep1"),
    Protein.Name = "H3",
    Peptide.Note = c("H3_K4_me3", "H3_K4_me3", "H3_K9_ac", "H3_K9_ac"),
    Isotope.Label.Type = c("light", "light", "light", "heavy"),
    Total.Area.MS1 = c(100, 50, 50, 999),
    stringsAsFactors = FALSE
  )
  sample <- data.frame(
    Replicate.Name = "A_rep1",
    Group = "A",
    Replicate.No = 1L,
    stringsAsFactors = FALSE
  )

  result <- quantify_peptides(ms1, sample, mode = "percentage")
  expect_equal(nrow(result), 2L)
  me3 <- result[result$Peptide.Note == "H3_K4_me3", ]
  ac <- result[result$Peptide.Note == "H3_K9_ac", ]
  expect_equal(me3$Percentage, 75)
  expect_equal(ac$Percentage, 25)
  expect_equal(sum(result$Percentage), 100)
})

test_that("quantify_peptides ratio mode computes light/heavy ratios", {
  tables <- .hm_ratio_tables()
  result <- quantify_peptides(tables$ms1, tables$sample,
                              mode = "light_heavy_ratio")

  expect_identical(
    names(result),
    c("Replicate.Name", "Protein.Name", "Peptide.Note", "Group",
      "Value", "Ratio")
  )
  expect_equal(result$Ratio, c(10, 10, 30, 20))
  expect_equal(result$Value, result$Ratio)
  expect_identical(attr(result, "value_type"), "ratio")
  expect_identical(attr(result, "method"), "light_heavy_ratio")
})

test_that("quantify_peptides ratio mode is NA-safe for missing or zero heavy", {
  tables <- .hm_ratio_tables()

  missing_heavy <- tables$ms1[
    !(tables$ms1$Replicate.Name == "A_rep1" &
        tables$ms1$Isotope.Label.Type == "heavy"), ,
    drop = FALSE
  ]
  missing_result <- quantify_peptides(missing_heavy, tables$sample,
                                      mode = "light_heavy_ratio")
  expect_true(is.na(missing_result$Ratio[missing_result$Replicate.Name == "A_rep1"]))
  expect_equal(missing_result$Ratio[missing_result$Replicate.Name == "B_rep1"], 30)

  zero_heavy <- tables$ms1
  zero_heavy$Total.Area.MS1[
    zero_heavy$Replicate.Name == "B_rep2" &
      zero_heavy$Isotope.Label.Type == "heavy"
  ] <- 0
  zero_result <- quantify_peptides(zero_heavy, tables$sample,
                                   mode = "light_heavy_ratio")
  expect_true(is.na(zero_result$Ratio[zero_result$Replicate.Name == "B_rep2"]))
})

test_that("quantify_peptides sums multiple rows per isotope before the ratio", {
  tables <- .hm_ratio_tables()
  extra_light <- tables$ms1[1, , drop = FALSE]
  extra_light$Total.Area.MS1 <- 50

  result <- quantify_peptides(rbind(tables$ms1, extra_light), tables$sample,
                              mode = "light_heavy_ratio")
  expect_equal(result$Ratio[result$Replicate.Name == "A_rep1"], 15)
})

test_that("quantify_peptides normalises ratios across samples per peptide", {
  tables <- .hm_ratio_tables()

  median_result <- quantify_peptides(
    tables$ms1, tables$sample,
    mode = "light_heavy_ratio", normalize = "median"
  )
  # median of 10, 10, 30, 20 is 15
  expect_equal(median_result$Value, c(10, 10, 30, 20) / 15)
  expect_equal(median_result$Ratio, c(10, 10, 30, 20))

  sum_result <- quantify_peptides(
    tables$ms1, tables$sample,
    mode = "light_heavy_ratio", normalize = "sum"
  )
  expect_equal(sum_result$Value, c(10, 10, 30, 20) / 70)
})

test_that("quantify_peptides spike_in mode divides by the sample spike-in factor", {
  tables <- .hm_ratio_tables()
  tables$sample$spike_in <- c(2, 2, 4, 4)

  result <- quantify_peptides(
    tables$ms1, tables$sample,
    mode = "spike_in", spike_in_col = "spike_in"
  )
  expect_equal(result$Value, c(5, 5, 7.5, 5))
  expect_identical(attr(result, "spike_in_col"), "spike_in")

  # Normalisation happens before applying the spike-in factor.
  normalised <- quantify_peptides(
    tables$ms1, tables$sample,
    mode = "spike_in", normalize = "median", spike_in_col = "spike_in"
  )
  expect_equal(normalised$Value, c(10, 10, 30, 20) / 15 / c(2, 2, 4, 4))

  tables$sample$spike_in[1] <- NA
  expect_warning(
    missing_factor <- quantify_peptides(
      tables$ms1, tables$sample,
      mode = "spike_in", spike_in_col = "spike_in"
    ),
    "Spike-in factor"
  )
  expect_true(is.na(missing_factor$Value[1]))
})

test_that("quantify_peptides validates its inputs", {
  tables <- .hm_ratio_tables()

  expect_error(
    quantify_peptides(tables$ms1, tables$sample, mode = "spike_in"),
    "requires `spike_in_col`"
  )
  tables$sample$spike_in <- c(2, 2, 4, 4)
  expect_error(
    quantify_peptides(tables$ms1, tables$sample, mode = "spike_in",
                      spike_in_col = "not_there"),
    "not found in `sample`"
  )
  expect_error(
    quantify_peptides(tables$ms1[, setdiff(names(tables$ms1), "Total.Area.MS1")],
                      tables$sample),
    "Area column"
  )
  expect_error(
    quantify_peptides(tables$ms1,
                      tables$sample[, setdiff(names(tables$sample), "Group")]),
    "must contain a 'Group' column"
  )
  expect_error(
    quantify_peptides(
      tables$ms1[, setdiff(names(tables$ms1), "Isotope.Label.Type")],
      tables$sample
    ),
    "Isotope.Label.Type"
  )
  expect_error(
    quantify_peptides(tables$ms1[, setdiff(names(tables$ms1), "Replicate.Name")],
                      tables$sample),
    "Replicate.Name"
  )
})

test_that("quantify_peptides handles empty and light-only inputs", {
  tables <- .hm_ratio_tables()
  sample <- tables$sample

  light_only <- tables$ms1[tables$ms1$Isotope.Label.Type == "light", , drop = FALSE]
  percentages <- quantify_peptides(light_only, sample, mode = "percentage")
  expect_gt(nrow(percentages), 0L)

  heavy_only <- light_only
  heavy_only$Isotope.Label.Type <- "heavy"
  empty <- quantify_peptides(heavy_only, sample, mode = "percentage")
  expect_equal(nrow(empty), 0L)
  expect_true("Percentage" %in% names(empty))
  expect_identical(attr(empty, "value_type"), "percentage")

  ratios <- quantify_peptides(light_only, sample, mode = "light_heavy_ratio")
  expect_gt(nrow(ratios), 0L)
  expect_true(all(is.na(ratios$Ratio)))
})

test_that("quantify_peptides warns when normalisation is requested for percentages", {
  tables <- .hm_demo_quant_tables()
  expect_warning(
    quantify_peptides(tables$ms1, tables$sample, mode = "percentage",
                      normalize = "median"),
    "only applied to ratio modes"
  )
})

test_that("quantify_peptides is robust to sample-table row order", {
  tables <- .hm_ratio_tables()
  shuffled <- tables$sample[c(4, 2, 1, 3), , drop = FALSE]
  rownames(shuffled) <- NULL

  expected <- quantify_peptides(tables$ms1, tables$sample,
                                mode = "light_heavy_ratio",
                                normalize = "median")
  actual <- quantify_peptides(tables$ms1, shuffled,
                              mode = "light_heavy_ratio",
                              normalize = "median")
  expect_equal(actual$Value, expected$Value)
  expect_equal(actual$Group, expected$Group)

  expected_pct <- quantify_peptides(tables$ms1, tables$sample,
                                    mode = "percentage")
  actual_pct <- quantify_peptides(tables$ms1, shuffled, mode = "percentage")
  expect_equal(actual_pct$Value, expected_pct$Value)
})

test_that("normalize_values implements none, median and sum methods", {
  expect_equal(normalize_values(c(1, 2, 3, NA), method = "none"),
               c(1, 2, 3, NA))
  expect_equal(normalize_values(c(1, 2, 3, NA), method = "median"),
               c(0.5, 1, 1.5, NA))
  expect_equal(normalize_values(c(1, 2, 3, NA), method = "sum"),
               c(1, 2, 3, NA) / 6)

  # Group-wise normalisation.
  grouped <- normalize_values(
    c(1, 2, 10, 20), method = "median", group = c("a", "a", "b", "b")
  )
  expect_equal(grouped, c(2 / 3, 4 / 3, 2 / 3, 4 / 3))

  grouped_sum <- normalize_values(
    c(1, 2, 10, 20), method = "sum", group = c("a", "a", "b", "b")
  )
  expect_equal(grouped_sum, c(1, 2, 10, 20) / c(3, 3, 30, 30))
})

test_that("normalize_values is NA- and zero-safe and validates arguments", {
  expect_equal(normalize_values(numeric(0), "median"), numeric(0))
  expect_warning(
    unchanged <- normalize_values(c(0, 0), method = "median"),
    "left unchanged"
  )
  expect_equal(unchanged, c(0, 0))

  # All-missing input does not fail.
  expect_equal(normalize_values(c(NA_real_, NA_real_), method = "median"),
               c(NA_real_, NA_real_))

  expect_error(normalize_values(c("a", "b")), "must be numeric")
  expect_error(normalize_values(1:3, method = "median", group = c("a", "b")),
               "same length")
  expect_error(normalize_values(1:3, method = "not-a-method"), "should be one of")
})

test_that("normalize_values performs a simple quantile normalisation", {
  values <- c(1, 2, 3, 10)
  result <- normalize_values(values, method = "quantile")
  expect_length(result, length(values))
  expect_false(anyNA(result))
  expect_true(all(diff(result) >= 0))

  with_na <- normalize_values(c(1, NA, 3, 10), method = "quantile")
  expect_true(is.na(with_na[2]))
  expect_false(anyNA(with_na[-2]))

  # Within-group rank mapping makes both groups share a distribution.
  grouped <- normalize_values(
    c(1, 2, 100, 200), method = "quantile", group = c("a", "a", "b", "b")
  )
  expect_equal(grouped[1:2], grouped[3:4])
})
