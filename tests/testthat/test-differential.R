##### Tests: differential analysis and volcano plots #####

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

make_long <- function(mat, groups, protein = "P") {
  samples <- rownames(mat)
  features <- colnames(mat)
  data.frame(
    Replicate.Name = rep(samples, times = length(features)),
    Group = rep(as.character(groups), times = length(features)),
    Protein.Name = protein,
    Peptide.Note = rep(features, each = length(samples)),
    Percentage = as.vector(mat),
    stringsAsFactors = FALSE
  )
}

two_group_matrix <- function() {
  mat <- rbind(
    c(70, 30),
    c(72, 28),
    c(68, 32),
    c(20, 80),
    c(22, 78),
    c(18, 82)
  )
  dimnames(mat) <- list(
    c("A1", "A2", "A3", "B1", "B2", "B3"),
    c("f_up", "f_down")
  )
  mat
}

test_that("differential_analysis returns the documented columns", {
  res <- differential_analysis(demo_table())

  expect_s3_class(res, "data.frame")
  expect_equal(
    names(res),
    c("protein", "feature", "group1", "group2", "n1", "n2",
      "mean1", "mean2", "effect", "statistic", "p_value", "p_adj",
      "direction")
  )
  # Five groups -> ten pairwise comparisons of four features.
  expect_equal(nrow(res), 40L)
  expect_equal(unique(res$n1), 4L)
  expect_equal(unique(res$n2), 4L)
  expect_s3_class(res$direction, "factor")
  expect_equal(levels(res$direction), c("up", "down", "ns"))
})

test_that("original-scale means are reported next to transformed effects", {
  df <- demo_table()
  res <- differential_analysis(df, comparisons = list(c("SampleA", "SampleB")))

  feature <- "H3_3_8_K4_un"
  mean1 <- mean(df$Percentage[df$Group == "SampleA" & df$Peptide.Note == feature])
  mean2 <- mean(df$Percentage[df$Group == "SampleB" & df$Peptide.Note == feature])

  row <- res[res$feature == feature, ]
  expect_equal(row$mean1, mean1)
  expect_equal(row$mean2, mean2)
  expect_equal(row$group1, "SampleA")
  expect_equal(row$group2, "SampleB")
})

test_that("Welch tests detect a strong synthetic difference", {
  df <- make_long(two_group_matrix(), rep(c("A", "B"), each = 3))
  res <- differential_analysis(df, comparisons = list(c("A", "B")))
  res <- res[order(res$feature), ]

  up <- res[res$feature == "f_up", ]
  down <- res[res$feature == "f_down", ]

  expect_lt(up$p_value, 0.01)
  expect_gt(up$effect, 0)
  expect_equal(up$direction, factor("up", levels = c("up", "down", "ns")))
  expect_lt(down$p_value, 0.01)
  expect_lt(down$effect, 0)
  expect_equal(down$direction, factor("down", levels = c("up", "down", "ns")))
})

test_that("p_adj is a BH adjustment of p_value", {
  res <- differential_analysis(
    demo_table(),
    comparisons = list(c("SampleA", "SampleB"), c("SampleC", "SampleD"))
  )
  expect_equal(res$p_adj, stats::p.adjust(res$p_value, method = "BH"))
  expect_true(all(res$p_adj >= res$p_value | is.na(res$p_value)))
  expect_true(all(res$direction == "ns" | res$p_adj < 0.05))
})

test_that("within_protein adds per-protein adjusted p-values", {
  df <- demo_table()
  res <- differential_analysis(df, within_protein = TRUE)

  expect_equal(
    names(res)[2:13],
    c("feature", "group1", "group2", "n1", "n2", "mean1", "mean2",
      "effect", "statistic", "p_value", "p_adj", "p_adj_protein")
  )
  expect_equal(res$p_adj, stats::p.adjust(res$p_value, method = "BH"))

  for (protein in unique(res$protein)) {
    idx <- res$protein == protein
    expect_equal(
      res$p_adj_protein[idx],
      stats::p.adjust(res$p_value[idx], method = "BH")
    )
  }
})

test_that("reference_group defines comparisons against the reference", {
  res <- differential_analysis(demo_table(), reference_group = "SampleA")

  expect_equal(nrow(res), 16L)
  expect_equal(unique(res$group1), "SampleA")
  expect_equal(sort(unique(res$group2)), c("SampleB", "SampleC", "SampleD", "SampleE"))

  expect_error(
    differential_analysis(demo_table(), reference_group = "Nope"),
    "reference_group"
  )
})

test_that("all pairwise comparisons are used by default", {
  df <- demo_table()
  df <- df[df$Group %in% c("SampleA", "SampleB", "SampleC"), ]
  res <- differential_analysis(df)
  expect_equal(nrow(res), 12L)
  expect_equal(length(unique(paste(res$group1, res$group2))), 3L)
})

test_that("min_n skips comparisons with too few samples", {
  mat <- rbind(
    c(70, 30),
    c(72, 28),
    c(68, 32),
    c(20, 80)
  )
  dimnames(mat) <- list(c("A1", "A2", "A3", "B1"), c("f_up", "f_down"))
  df <- make_long(mat, c("A", "A", "A", "B"))

  expect_message(
    res <- differential_analysis(df, min_n = 2),
    "No comparison"
  )
  expect_equal(nrow(res), 0L)
  expect_equal(names(res)[12], "p_adj")

  # With a third, sufficiently large group the A vs C comparison survives.
  mat3 <- rbind(
    c(70, 30),
    c(72, 28),
    c(68, 32),
    c(20, 80),
    c(70, 30),
    c(72, 28),
    c(68, 32)
  )
  dimnames(mat3) <- list(
    c("A1", "A2", "A3", "B1", "C1", "C2", "C3"),
    c("f_up", "f_down")
  )
  df3 <- make_long(mat3, c("A", "A", "A", "B", "C", "C", "C"))

  expect_message(
    res3 <- differential_analysis(df3, min_n = 2),
    "Skipping comparison"
  )
  expect_equal(nrow(res3), 2L)
  expect_equal(unique(paste(res3$group1, res3$group2)), "A C")
})

test_that("a single group is rejected", {
  df <- make_long(
    matrix(c(50, 50), nrow = 1, dimnames = list("A1", c("f1", "f2"))),
    "A"
  )
  expect_error(differential_analysis(df), "at least two groups")
})

test_that("constant features are reported with NA p-values", {
  mat <- rbind(
    c(50, 70),
    c(50, 72),
    c(50, 68),
    c(50, 20),
    c(50, 22),
    c(50, 18)
  )
  dimnames(mat) <- list(
    c("A1", "A2", "A3", "B1", "B2", "B3"),
    c("f_const", "f_var")
  )
  df <- make_long(mat, rep(c("A", "B"), each = 3))
  res <- differential_analysis(
    df,
    comparisons = list(c("A", "B")),
    transform = "none"
  )

  constant <- res[res$feature == "f_const", ]
  expect_true(is.na(constant$p_value))
  expect_true(is.na(constant$statistic))
  expect_equal(constant$effect, 0)
  expect_equal(constant$direction, factor("ns", levels = c("up", "down", "ns")))

  variable <- res[res$feature == "f_var", ]
  expect_true(is.finite(variable$p_value))
})

test_that("paired analysis matches a paired t-test", {
  mat <- rbind(
    c(60, 40),
    c(65, 35),
    c(70, 30),
    c(40, 60),
    c(45, 55),
    c(50, 50)
  )
  dimnames(mat) <- list(
    c("A1", "A2", "A3", "B1", "B2", "B3"),
    c("f_up", "f_down")
  )
  df <- make_long(mat, rep(c("A", "B"), each = 3))
  df$Replicate.No <- rep(1:3, 2)

  res <- differential_analysis(
    df,
    comparisons = list(c("A", "B")),
    paired = TRUE
  )

  clr <- transform_composition(composition_matrix(df), "clr")
  expected <- stats::t.test(
    clr[c("A1", "A2", "A3"), "f_up"],
    clr[c("B1", "B2", "B3"), "f_up"],
    paired = TRUE
  )
  row <- res[res$feature == "f_up", ]
  expect_equal(row$p_value, expected$p.value)
  expect_equal(row$statistic, unname(expected$statistic))
  expect_equal(row$n1, 3L)
  expect_equal(row$n2, 3L)

  df_no_pair <- df
  df_no_pair$Replicate.No <- NULL
  expect_error(
    differential_analysis(df_no_pair, comparisons = list(c("A", "B")), paired = TRUE),
    "pair_col"
  )
})

test_that("method anova returns F statistics and a global summary", {
  df <- demo_table()
  res <- differential_analysis(df, method = "anova")

  expect_equal(nrow(res), 40L)
  expect_equal(names(res), names(differential_analysis(df, method = "welch")))
  expect_true(all(is.na(res$statistic) | res$statistic >= 0))

  global <- attr(res, "global_anova")
  expect_s3_class(global, "data.frame")
  expect_equal(nrow(global), 4L)
  expect_true(all(c("protein", "feature", "n_groups", "statistic", "p_value", "p_adj") %in%
                    names(global)))
  expect_equal(unique(global$n_groups), 5L)
  expect_equal(global$p_adj, stats::p.adjust(global$p_value, method = "BH"))

  auto <- differential_analysis(df, method = "auto")
  welch <- differential_analysis(df, method = "welch")
  expect_equal(auto, welch)
})

test_that("unknown comparison groups are rejected", {
  expect_error(
    differential_analysis(demo_table(), comparisons = list(c("SampleA", "Nope"))),
    "Unknown group"
  )
  expect_error(
    differential_analysis(demo_table(), comparisons = list(c("SampleA", "SampleA"))),
    "distinct"
  )
})

test_that("plot_volcano builds from differential results", {
  res <- differential_analysis(
    demo_table(),
    comparisons = list(c("SampleA", "SampleB"))
  )
  p <- plot_volcano(res, alpha = 0.05, label_top = 2)

  expect_s3_class(p, "ggplot")
  built <- ggplot2::ggplot_build(p)
  points <- built$data[[1]]
  expect_equal(sort(points$x), sort(res$effect))
  expect_equal(sort(points$y), sort(-log10(res$p_adj)), tolerance = 1e-12)
  # Two labels requested from four features.
  expect_equal(nrow(built$data[[length(built$data)]]), 2L)
})

test_that("plot_volcano thresholds and labels are configurable", {
  res <- differential_analysis(
    demo_table(),
    comparisons = list(c("SampleA", "SampleB"))
  )

  p <- plot_volcano(res, effect_threshold = 0.2, label_col = "protein")
  expect_s3_class(p, "ggplot")
  built <- ggplot2::ggplot_build(p)
  layer_classes <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true(any(grepl("Hline", layer_classes)))
  expect_true(any(grepl("Vline", layer_classes)))
  expect_s3_class(p, "ggplot")
  # The label layer uses the requested column.
  label_data <- built$data[[length(built$data)]]
  expect_equal(sort(unique(label_data$label)), sort(unique(res$protein)))
})

test_that("plot_volcano handles p_adj = 0 and empty input", {
  df <- data.frame(
    feature = c("a", "b"),
    effect = c(1, -1),
    p_adj = c(0, 0.01),
    direction = c("up", "ns"),
    stringsAsFactors = FALSE
  )
  p <- plot_volcano(df, label_top = 1)
  built <- ggplot2::ggplot_build(p)
  expect_true(all(is.finite(built$data[[1]]$y)))
  expect_lt(built$data[[1]]$y[1], 100)

  empty <- plot_volcano(data.frame(effect = numeric(0), p_adj = numeric(0)))
  expect_s3_class(empty, "ggplot")
  expect_silent(ggplot2::ggplot_build(empty))
})

test_that("plot_volcano validates its input", {
  expect_error(plot_volcano(data.frame(effect = 1)), "missing required column")
  expect_error(
    plot_volcano(data.frame(effect = 1, p_adj = 0.1), label_col = "nope"),
    "label_col"
  )
})
