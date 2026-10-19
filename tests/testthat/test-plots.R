demo_data <- function() {
  ms1_path <- system.file("extdata", "MS1_demo.csv", package = "HistoneMod")
  sample_path <- system.file("extdata", "samples_demo.csv", package = "HistoneMod")
  ms1 <- utils::read.csv(ms1_path, check.names = TRUE)
  sample <- utils::read.csv(sample_path, check.names = TRUE)
  percentage_calculation(ms1, sample)
}

test_that("plot_pca supports value columns, transforms and ordering", {
  data <- demo_data()

  p_default <- plot_pca(data)
  expect_s3_class(p_default, "ggplot")

  p_clr <- plot_pca(data, transform = "clr")
  expect_s3_class(p_clr, "ggplot")

  p_logit <- plot_pca(data, transform = "logit")
  expect_s3_class(p_logit, "ggplot")

  p_drop <- plot_pca(data, missing = "drop")
  expect_s3_class(p_drop, "ggplot")

  p_order <- plot_pca(
    data,
    group_order = c("SampleC", "SampleA"),
    group_colors = c(SampleA = "#4C72B0", SampleC = "#DD8452")
  )
  expect_s3_class(p_order, "ggplot")
})

test_that("plot_pca validates inputs", {
  data <- demo_data()
  expect_error(plot_pca(data, value_col = "NotThere"), "not found")
})

test_that("plot_heatmap supports scaling, missing policy and annotation", {
  data <- demo_data()

  hm <- plot_heatmap(data)
  expect_true(inherits(hm, "pheatmap"))

  hm_scaled <- plot_heatmap(data, scale = "row")
  expect_true(inherits(hm_scaled, "pheatmap"))

  hm_plain <- plot_heatmap(data, show_group_annotation = FALSE)
  expect_true(inherits(hm_plain, "pheatmap"))
})

test_that("plot_barplot_single supports ordering, colours and error bars", {
  data <- demo_data()

  p <- plot_barplot_single(
    data, "H3_3-8", "H3_3_8_K4_me3",
    group_order = c("SampleC", "SampleA"),
    group_colors = c(SampleA = "#4C72B0", SampleC = "#DD8452"),
    error_bar = "sd",
    show_n = TRUE
  )
  expect_s3_class(p, "ggplot")

  p_ci <- plot_barplot_single(data, "H3_3-8", "H3_3_8_K4_me3", error_bar = "ci95")
  expect_s3_class(p_ci, "ggplot")

  expect_error(
    plot_barplot_single(data, "H3_3-8", "H3_3_8_K4_me3", value_col = "NotThere"),
    "not found"
  )
})

test_that("unmodified peptide filtering uses the documented naming rule", {
  expect_true(HistoneMod:::is_unmodified_peptide("H3_3_8_K4_un"))
  expect_true(HistoneMod:::is_unmodified_peptide("H3_9_17_K8K14_unun"))
  expect_false(HistoneMod:::is_unmodified_peptide("H3_3_8_K4_me3"))

  data <- demo_data()
  filtered <- HistoneMod:::filter_processed_data(data, exclude_un = TRUE)
  expect_false(any(HistoneMod:::is_unmodified_peptide(filtered$Peptide.Note)))

  kept <- HistoneMod:::filter_processed_data(
    data,
    exclude_un = FALSE,
    selected_peptides = "H3_3_8_K4_me3",
    selected_samples = "SampleA_rep1"
  )
  expect_equal(unique(kept$Peptide.Note), "H3_3_8_K4_me3")
  expect_equal(unique(kept$Replicate.Name), "SampleA_rep1")
})

test_that("group order and colour parsing is robust", {
  expect_null(HistoneMod:::parse_group_order(NULL))
  expect_equal(HistoneMod:::parse_group_order("A, B ,,B"), c("A", "B"))
  expect_equal(HistoneMod:::parse_group_order(" A , C "), c("A", "C"))

  colours <- HistoneMod:::parse_group_colors("A=#FF0000, B=#00FF00")
  expect_equal(unname(colours), c("#FF0000", "#00FF00"))
  expect_equal(names(colours), c("A", "B"))
  expect_null(HistoneMod:::parse_group_colors("not-a-colour"))
})
