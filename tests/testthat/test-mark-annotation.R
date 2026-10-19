##### Tests: curated histone mark annotation #####

mark_demo_path <- function(name) {
  path <- system.file("extdata", name, package = "HistoneMod")
  if (!nzchar(path)) {
    path <- file.path("inst", "extdata", name)
  }
  path
}

# Demo data with the peptide notes exposed as site strings so that the
# demo-style naming ("H3_3_8_K4_me3") is exercised by the annotation code.
mark_demo_annotated <- function() {
  ms1 <- utils::read.csv(mark_demo_path("MS1_demo.csv"), stringsAsFactors = FALSE)
  samples <- utils::read.csv(
    mark_demo_path("samples_demo.csv"),
    stringsAsFactors = FALSE
  )
  colnames(ms1) <- gsub("\\s+", ".", colnames(ms1))
  colnames(samples) <- gsub("\\s+", ".", colnames(samples))
  df <- percentage_calculation(ms1, samples, exclude_un = FALSE)
  df$site <- df$Peptide.Note
  df
}

test_that("histone_marks() loads the packaged curated table", {
  marks <- histone_marks()

  expect_s3_class(marks, "data.frame")
  expect_equal(
    names(marks),
    c(
      "protein", "residue", "position", "mod", "canonical", "class",
      "writer", "eraser", "reader", "notes"
    )
  )
  expect_type(marks$position, "integer")
  expect_false(any(is.na(marks$canonical)))
  expect_false(any(is.na(marks$class)))
  expect_false(anyDuplicated(marks$canonical) > 0L)
  expect_true(all(marks$class %in% c("active", "repressive", "poised", "other")))
  expect_true(
    all(unique(marks$mod[!is.na(marks$mod)]) %in% c("un", "me1", "me2", "me3", "ac", "ph", "ub"))
  )

  required <- c(
    "H3K4me1", "H3K4me2", "H3K4me3",
    "H3K9me1", "H3K9me2", "H3K9me3",
    "H3K14ac", "H3K18ac", "H3K23ac",
    "H3K27me1", "H3K27me2", "H3K27me3",
    "H3K36me1", "H3K36me2", "H3K36me3",
    "H3K56ac",
    "H3K79me1", "H3K79me2", "H3K79me3",
    "H3S10ph", "H3S28ph",
    "H4K5ac", "H4K8ac", "H4K12ac", "H4K16ac",
    "H4K20me1", "H4K20me2", "H4K20me3", "H4S1ph",
    "H2AK5ac", "H2AK119ub", "H2AS1ph",
    "H2A.ZK4ac", "H2A.ZK7ac", "H2A.ZK11ac",
    "H2BK5ac", "H2BK12ac", "H2BK15ac", "H2BK20ac",
    "H2BS14ph", "H2BK120ub"
  )
  expect_true(all(required %in% marks$canonical))
  expect_true(
    all(c("H3", "H3.3", "H4", "H2A", "H2A.Z", "H2B", "macroH2A") %in% marks$protein)
  )

  # The packaged CSV must be plain comma-separated UTF-8 with the documented
  # header, no quoting and the same number of fields on every line.
  csv_path <- mark_demo_path("histone_marks.csv")
  expect_true(file.exists(csv_path))
  lines <- readLines(csv_path)
  header <- strsplit(lines[1L], ",", fixed = TRUE)[[1L]]
  expect_equal(
    header,
    c(
      "protein", "residue", "position", "mod", "canonical", "class",
      "writer", "eraser", "reader", "notes"
    )
  )
  expect_false(any(grepl('"', lines, fixed = TRUE)))
  field_counts <- vapply(strsplit(lines, ",", fixed = TRUE), length, integer(1))
  expect_equal(unique(field_counts), length(header))
})

test_that("curated marks carry the expected classes and positions", {
  marks <- histone_marks()
  class_of <- function(canonical) marks$class[match(canonical, marks$canonical)]

  expect_equal(class_of("H3K4me3"), "active")
  expect_equal(class_of("H4K16ac"), "active")
  expect_equal(class_of("H2BK120ub"), "active")
  expect_equal(class_of("H3K9me3"), "repressive")
  expect_equal(class_of("H3K27me3"), "repressive")
  expect_equal(class_of("H2AK119ub"), "repressive")
  expect_equal(class_of("H3S10ph"), "other")

  index <- match("H3K27me3", marks$canonical)
  expect_equal(marks$position[index], 27L)
  expect_equal(marks$residue[index], "K")
  expect_equal(marks$mod[index], "me3")
  expect_true(!is.na(marks$writer[index]))
  expect_true(!is.na(marks$reader[index]))
})

test_that("normalise_histone_protein() maps families and preserves variants", {
  expect_equal(
    normalise_histone_protein(c(
      "H3", "H3_3-8", "H3_3_8", "H3.1", "H3.2", "H3.3",
      "H4_4-17", "H2A", "H2A.Z", "H2B", "macroH2A", "macroH2A1.2", "H2A.X"
    )),
    c(
      "H3", "H3", "H3", "H3", "H3", "H3.3",
      "H4", "H2A", "H2A.Z", "H2B", "macroH2A", "macroH2A", "H2A"
    )
  )
  expect_equal(
    normalise_histone_protein(c(" h4 ", "h2a.z", "H3.3_3-8")),
    c("H4", "H2A.Z", "H3.3")
  )
  expect_true(is.na(normalise_histone_protein("H1")))
  expect_true(is.na(normalise_histone_protein(NA_character_)))
  expect_equal(normalise_histone_protein(character(0)), character(0))
  expect_equal(normalise_histone_protein(factor("H3.3")), "H3.3")
})

test_that("annotate_sites() matches demo-style site strings", {
  sites <- c(
    "H3K4me3", "H4K5ac", "H3K9me3", "H4K16ac", "H2BK120ub", "H3K27me3",
    "H3_3_8_K4_me3", "H3_3-8_K4_me3", "H4_4_17_K12_ac", "H3_3_8_K4_un"
  )
  annotations <- annotate_sites(sites)

  expect_equal(nrow(annotations), length(sites))
  expect_true(
    all(c("canonical", "class", "writer", "eraser", "reader", "matched") %in%
          names(annotations))
  )
  expect_equal(
    annotations$canonical[1:6],
    c("H3K4me3", "H4K5ac", "H3K9me3", "H4K16ac", "H2BK120ub", "H3K27me3")
  )
  expect_equal(
    annotations$class[1:6],
    c("active", "active", "repressive", "active", "active", "repressive")
  )
  expect_equal(
    annotations$canonical[7:10],
    c("H3K4me3", "H3K4me3", "H4K12ac", "H3K4un")
  )
  expect_true(all(annotations$matched))
  expect_silent(annotate_sites(sites))
})

test_that("annotate_sites() matches explicit protein/position/mod columns", {
  sites <- data.frame(
    protein = c("H3_3-8", "H4_4-17", "h3", "H2A.Z", "H3.3", "H3.1"),
    position = c(4, 5, 9, 4, 4, 27),
    mod = c("me3", "ac", "ME3", "Ac", "trimethyl", "me3"),
    stringsAsFactors = FALSE
  )
  annotations <- annotate_sites(sites)

  expect_equal(
    annotations$canonical,
    c("H3K4me3", "H4K5ac", "H3K9me3", "H2A.ZK4ac", "H3K4me3", "H3K27me3")
  )
  expect_true(all(annotations$matched))

  # A one-letter residue column is used when present.
  with_residue <- data.frame(
    protein = c("H3", "H3"),
    residue = c("k", "S"),
    position = c(4, 10),
    mod = c("me3", "ph"),
    stringsAsFactors = FALSE
  )
  expect_equal(
    annotate_sites(with_residue)$canonical,
    c("H3K4me3", "H3S10ph")
  )

  # Modification aliases and missing modifications are normalised.
  aliases <- data.frame(
    protein = "H3",
    position = 4,
    mod = c("trimethyl", "ME3", "tri", "acetylation", NA),
    stringsAsFactors = FALSE
  )
  expect_equal(
    suppressWarnings(annotate_sites(aliases))$canonical,
    c("H3K4me3", "H3K4me3", "H3K4me3", NA, "H3K4un")
  )

  # H3.3-specific rows win; shared H3 marks are inherited otherwise.
  h33 <- annotate_sites(data.frame(
    protein = c("H3.3", "H3.3", "H3.3"),
    position = c(31, 4, 27),
    mod = c("ph", "me3", "me3"),
    stringsAsFactors = FALSE
  ))
  expect_equal(h33$canonical, c("H3.3S31ph", "H3K4me3", "H3K27me3"))
  expect_true(all(h33$matched))
})

test_that("annotate_sites() accepts user-supplied mark tables", {
  custom <- data.frame(
    protein = "H3",
    position = 4L,
    mod = "me3",
    canonical = "H3K4me3",
    class = "active",
    stringsAsFactors = FALSE
  )
  expect_warning(
    annotations <- annotate_sites(c("H3K4me3", "H4K5ac"), marks = custom),
    "could not be matched"
  )
  expect_equal(annotations$canonical, c("H3K4me3", NA))
  expect_equal(annotations$class, c("active", NA))
  expect_equal(annotations$matched, c(TRUE, FALSE))
})

test_that("annotate_sites() returns NA for unmatched sites with one summary warning", {
  sites <- c("H3K4me3", "H3K4me9", "H1K2ac", "H4K16ac", "H3K999ac")

  warnings_seen <- character(0)
  annotations <- withCallingHandlers(
    annotate_sites(sites),
    warning = function(w) {
      warnings_seen <<- c(warnings_seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  expect_length(warnings_seen, 1L)
  expect_match(warnings_seen[1L], "3 of 5 site")
  expect_match(warnings_seen[1L], "could not be matched")
  expect_equal(annotations$matched, c(TRUE, FALSE, FALSE, TRUE, FALSE))
  expect_true(all(is.na(annotations$canonical[!annotations$matched])))
  expect_true(all(is.na(annotations$class[!annotations$matched])))
  expect_true(all(is.na(annotations$writer[!annotations$matched])))
  expect_true(all(is.na(annotations$reader[!annotations$matched])))

  expect_warning(annotate_sites("H1K2ac"), "could not be matched")
})

test_that("annotate_sites() is idempotent and validates its input", {
  sites <- data.frame(site = c("H3K4me3", "H4K5ac"), stringsAsFactors = FALSE)
  twice <- annotate_sites(annotate_sites(sites))
  expect_equal(sum(duplicated(names(twice))), 0L)
  expect_equal(twice$canonical, c("H3K4me3", "H4K5ac"))

  expect_error(annotate_sites(42), "data.frame")
  expect_error(annotate_sites(data.frame(x = 1)), "site")

  empty <- annotate_sites(data.frame(site = character(0), stringsAsFactors = FALSE))
  expect_equal(nrow(empty), 0L)
  expect_true(all(c("canonical", "class", "matched") %in% names(empty)))
})

test_that("summarise_by_class() aggregates group x class means", {
  sites <- data.frame(
    protein = c("H3", "H3", "H4", "H3", "H4", "H2B"),
    position = c(4, 27, 5, 9, 16, 120),
    mod = c("me3", "me3", "ac", "me3", "ac", "ub"),
    Group = c("A", "A", "A", "B", "B", "B"),
    occupancy = c(10, 20, 30, 40, 50, 60),
    stringsAsFactors = FALSE
  )

  summary <- summarise_by_class(sites, value_col = "occupancy")
  expect_s3_class(summary, "data.frame")
  expect_equal(
    names(summary),
    c("Group", "class", "mean", "sd", "n", "n_sites", "se")
  )
  expect_equal(nrow(summary), 4L)

  active_a <- summary[summary$Group == "A" & summary$class == "active", ]
  expect_equal(active_a$mean, 20)
  expect_equal(active_a$sd, stats::sd(c(10, 30)))
  expect_equal(active_a$n, 2L)
  expect_equal(active_a$n_sites, 2L)
  expect_equal(active_a$se, stats::sd(c(10, 30)) / sqrt(2))

  repressive_b <- summary[summary$Group == "B" & summary$class == "repressive", ]
  expect_equal(repressive_b$mean, 40)
  expect_true(is.na(repressive_b$sd))

  # Replicate measurements of one site: n counts rows, n_sites counts marks.
  replicates <- data.frame(
    protein = "H3",
    position = 4,
    mod = "me3",
    Group = "A",
    value = c(10, 14),
    stringsAsFactors = FALSE
  )
  replicate_summary <- summarise_by_class(replicates, value_col = "value")
  expect_equal(replicate_summary$n, 2L)
  expect_equal(replicate_summary$n_sites, 1L)
  expect_equal(replicate_summary$mean, 12)
})

test_that("summarise_by_class() drops unmatched sites and NA values", {
  sites <- data.frame(
    protein = c("H3", "H1"),
    position = c(4, 2),
    mod = c("me3", "ac"),
    Group = "A",
    value = c(10, 99),
    stringsAsFactors = FALSE
  )
  expect_warning(summary <- summarise_by_class(sites, "value"), "could not be matched")
  expect_equal(nrow(summary), 1L)
  expect_equal(summary$mean, 10)

  with_na <- data.frame(
    protein = "H3",
    position = c(4, 4),
    mod = "me3",
    Group = "A",
    value = c(10, NA),
    stringsAsFactors = FALSE
  )
  na_summary <- summarise_by_class(with_na, "value")
  expect_equal(na_summary$n, 1L)
  expect_equal(na_summary$mean, 10)
})

test_that("summarise_by_class() annotates the demo data", {
  df <- mark_demo_annotated()
  summary <- summarise_by_class(df, value_col = "Percentage", group_col = "Group")

  expect_equal(
    sort(unique(summary$Group)),
    c("SampleA", "SampleB", "SampleC", "SampleD", "SampleE")
  )
  expect_true(all(c("active", "other") %in% summary$class))

  active_a <- summary[summary$Group == "SampleA" & summary$class == "active", ]
  expect_equal(active_a$n, 12L)      # 3 active marks x 4 replicates
  expect_equal(active_a$n_sites, 3L)

  annotated <- annotate_sites(df[, c("site", "Group", "Percentage")])
  expect_true(all(annotated$matched))
  expected <- stats::aggregate(
    Percentage ~ Group + class,
    data = annotated,
    FUN = mean
  )
  merged <- merge(
    expected,
    summary[, c("Group", "class", "mean")],
    by = c("Group", "class")
  )
  expect_equal(nrow(merged), nrow(expected))
  expect_equal(sort(merged$Percentage), sort(merged$mean), tolerance = 1e-12)
})

test_that("plot_mark_classes() returns a grouped viridis bar chart", {
  df <- mark_demo_annotated()
  group_order <- c("SampleC", "SampleA", "SampleB", "SampleD", "SampleE")

  p <- plot_mark_classes(df, value_col = "Percentage", group_order = group_order)
  expect_s3_class(p, "ggplot")
  expect_equal(levels(p$data$Group), group_order)
  expect_setequal(as.character(unique(p$data$class)), c("active", "other"))
  expect_equal(p$labels$y, "Percentage")
  expect_equal(p$labels$fill, "Mark class")

  expect_silent(built <- ggplot2::ggplot_build(p))
  bars <- built$data[[1L]]
  summary <- summarise_by_class(df, value_col = "Percentage")
  expect_equal(nrow(bars), nrow(summary))
  expect_equal(sort(unique(bars$fill)), sort(viridis::viridis(2)))
  expect_equal(sort(bars$y), sort(summary$mean), tolerance = 1e-12)

  # A summary table can be passed directly, with control over class order.
  p2 <- plot_mark_classes(
    summary,
    group_order = group_order,
    class_order = c("other", "active", "repressive", "poised")
  )
  expect_s3_class(p2, "ggplot")
  expect_equal(levels(p2$data$Group), group_order)
  expect_equal(levels(p2$data$class), c("other", "active"))
  expect_equal(p2$labels$y, "Mean value")
  expect_silent(ggplot2::ggplot_build(p2))
})

test_that("plot_mark_classes() handles data without matchable sites", {
  sites <- data.frame(
    protein = "H1",
    position = 2,
    mod = "ac",
    Group = "A",
    value = 1,
    stringsAsFactors = FALSE
  )
  expect_warning(p <- plot_mark_classes(sites, value_col = "value"), "could not be matched")
  expect_s3_class(p, "ggplot")
  expect_silent(ggplot2::ggplot_build(p))

  expect_error(plot_mark_classes(42), "data.frame")
})
