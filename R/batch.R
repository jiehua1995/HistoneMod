##### Batch processing and multi-experiment comparison #####
# Helpers for running several Skyline exports and comparing two experiments.

#' Run a batch of Skyline exports
#'
#' Reads, validates and analyses several MS1/sample table pairs. For each pair
#' the processed table, the differential results and a project file are written
#' to `output_dir`.
#'
#' @param ms1_files Character vector of MS1 CSV paths.
#' @param sample_files Character vector of sample CSV paths (same length).
#' @param output_dir Directory for project and processed-table output.
#' @param settings Named list of analysis settings. Recognised entries include
#'   `quant_mode`, `normalize`, `spike_in_col`, `exclude_un`, `transform` and
#'   `p_adjust`.
#' @param labels Optional labels for each pair (defaults to file names).
#' @param overwrite Logical; overwrite existing output files.
#'
#' @return A data frame with one row per input pair and columns `label`,
#'   `valid`, `n_rows`, `n_proteins`, `n_peptides`, `n_groups`,
#'   `project_file`, `processed_file` and `message`.
#' @export
run_histonemod_batch <- function(ms1_files, sample_files, output_dir,
                                 settings = list(), labels = NULL,
                                 overwrite = FALSE) {
  if (!is.character(ms1_files) || !is.character(sample_files)) {
    stop("`ms1_files` and `sample_files` must be character vectors.", call. = FALSE)
  }
  if (length(ms1_files) != length(sample_files)) {
    stop("`ms1_files` and `sample_files` must have the same length.", call. = FALSE)
  }
  if (length(ms1_files) == 0) stop("No input files supplied.", call. = FALSE)
  if (!is.character(output_dir) || length(output_dir) != 1L || is.na(output_dir)) {
    stop("`output_dir` must be a single directory path.", call. = FALSE)
  }
  if (is.null(settings)) settings <- list()
  if (!is.list(settings)) stop("`settings` must be a list.", call. = FALSE)
  if (is.null(labels)) labels <- tools::file_path_sans_ext(basename(ms1_files))
  if (length(labels) != length(ms1_files)) {
    stop("`labels` must have the same length as the input files.", call. = FALSE)
  }

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(output_dir)) stop("Could not create output_dir: ", output_dir, call. = FALSE)

  quant_mode <- settings$quant_mode
  if (is.null(quant_mode) || !nzchar(quant_mode)) quant_mode <- "percentage"
  normalize <- settings$normalize
  if (is.null(normalize) || !nzchar(normalize)) normalize <- "median"
  if (identical(quant_mode, "percentage")) normalize <- "none"
  spike_in_col <- settings$spike_in_col
  if (!is.null(spike_in_col) && !nzchar(spike_in_col)) spike_in_col <- NULL
  value_col <- if (identical(quant_mode, "percentage")) "Percentage" else "Value"

  rows <- lapply(seq_along(ms1_files), function(i) {
    label <- labels[i]
    result <- list(
      label = label, valid = FALSE, n_rows = NA_integer_,
      n_proteins = NA_integer_, n_peptides = NA_integer_,
      n_groups = NA_integer_, project_file = NA_character_,
      processed_file = NA_character_, message = NA_character_
    )

    ms1 <- tryCatch(read_skyline_csv(ms1_files[i]), error = function(e) NULL)
    sample <- tryCatch(read_skyline_csv(sample_files[i]), error = function(e) NULL)
    if (is.null(ms1) || is.null(sample)) {
      result$message <- "Could not read input files"
      return(data.frame(result, stringsAsFactors = FALSE))
    }

    check <- validate_input_tables(ms1, sample)
    if (!isTRUE(check$valid)) {
      result$message <- paste(check$errors, collapse = "; ")
      return(data.frame(result, stringsAsFactors = FALSE))
    }

    processed <- tryCatch(
      quantify_peptides(
        check$ms1, check$sample,
        mode = quant_mode,
        normalize = normalize,
        spike_in_col = if (identical(quant_mode, "spike_in")) spike_in_col else NULL
      ),
      error = function(e) e
    )
    if (inherits(processed, "error")) {
      result$message <- conditionMessage(processed)
      return(data.frame(result, stringsAsFactors = FALSE))
    }

    processed <- filter_processed_data(
      processed,
      exclude_un = isTRUE(settings$exclude_un),
      selected_peptides = NULL,
      selected_samples = NULL
    )

    differential <- tryCatch(
      differential_analysis(
        processed,
        value_col = value_col,
        transform = if (is.null(settings$transform)) "clr" else settings$transform
      ),
      error = function(e) NULL
    )

    project_file <- file.path(output_dir, paste0(label, ".rds"))
    processed_file <- file.path(output_dir, paste0(label, "_processed.csv"))
    if (file.exists(project_file) && !isTRUE(overwrite)) {
      result$message <- "Output exists; not overwritten"
      return(data.frame(result, stringsAsFactors = FALSE))
    }

    save_project(
      project_file,
      check$ms1, check$sample,
      settings = settings,
      results = list(processed = processed, differential = differential)
    )
    utils::write.csv(processed, processed_file, row.names = FALSE)

    result$valid <- TRUE
    result$n_rows <- nrow(processed)
    result$n_proteins <- length(unique(processed$Protein.Name))
    result$n_peptides <- length(unique(processed$Peptide.Note))
    result$n_groups <- length(unique(processed$Group))
    result$project_file <- project_file
    result$processed_file <- processed_file
    result$message <- "OK"
    data.frame(result, stringsAsFactors = FALSE)
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

# Internal helper: per-feature effects for one experiment.
.hm_experiment_effects <- function(data, groups, value_col, transform) {
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  required <- c("Replicate.Name", "Group", "Protein.Name", "Peptide.Note", value_col)
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    stop("`data` is missing column(s): ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  data$Group <- as.character(data$Group)
  data <- data[data$Group %in% groups, , drop = FALSE]
  if (length(unique(data$Group)) < 2) {
    stop("Both comparison groups must be present.", call. = FALSE)
  }

  wide <- tidyr::pivot_wider(
    data[, c("Replicate.Name", "Peptide.Note", value_col)],
    names_from = "Peptide.Note",
    values_from = value_col,
    values_fill = 0,
    values_fn = mean
  )
  mat <- as.matrix(wide[, setdiff(colnames(wide), "Replicate.Name"), drop = FALSE])
  storage.mode(mat) <- "numeric"
  transformed <- transform_composition(mat, method = transform)

  group_of_sample <- data$Group[match(wide$Replicate.Name, data$Replicate.Name)]
  ref <- groups[1]
  treat <- groups[2]
  ref_rows <- group_of_sample == ref
  treat_rows <- group_of_sample == treat

  protein_of_feature <- tapply(
    data$Protein.Name,
    data$Peptide.Note,
    function(v) v[1]
  )

  ref_mean <- colMeans(transformed[ref_rows, , drop = FALSE], na.rm = TRUE)
  treat_mean <- colMeans(transformed[treat_rows, , drop = FALSE], na.rm = TRUE)

  data.frame(
    Protein.Name = as.character(protein_of_feature[colnames(transformed)]),
    Peptide.Note = colnames(transformed),
    mean_ref = as.numeric(ref_mean),
    mean_treat = as.numeric(treat_mean),
    effect = as.numeric(treat_mean - ref_mean),
    n_ref = sum(ref_rows),
    n_treat = sum(treat_rows),
    stringsAsFactors = FALSE
  )
}

#' Compare two experiments
#'
#' Compares per-feature treatment effects between two experiments (for example
#' wild-type versus knockout, or two inhibitor treatments) using CLR-transformed
#' group means.
#'
#' @param data_a Processed data from the first experiment.
#' @param data_b Processed data from the second experiment.
#' @param groups_a Length-2 character vector: reference and treatment group in
#'   experiment A.
#' @param groups_b Length-2 character vector: reference and treatment group in
#'   experiment B.
#' @param value_col Numeric value column.
#' @param transform Transformation used before computing means.
#' @param label_a Label for experiment A.
#' @param label_b Label for experiment B.
#'
#' @return A list with `effects` (merged per-feature effects), `summary`
#'   (correlation and concordance) and `settings`.
#' @export
compare_experiments <- function(data_a, data_b, groups_a, groups_b,
                                value_col = "Percentage", transform = "clr",
                                label_a = "Experiment A", label_b = "Experiment B") {
  if (length(groups_a) != 2 || length(groups_b) != 2) {
    stop("`groups_a` and `groups_b` must each contain two group names.", call. = FALSE)
  }

  effects_a <- .hm_experiment_effects(data_a, groups_a, value_col, transform)
  effects_b <- .hm_experiment_effects(data_b, groups_b, value_col, transform)

  merged <- merge(
    effects_a,
    effects_b,
    by = c("Protein.Name", "Peptide.Note"),
    suffixes = c("_a", "_b")
  )
  if (nrow(merged) < 2) {
    stop("Fewer than two shared features between the experiments.", call. = FALSE)
  }

  pearson <- stats::cor(merged$effect_a, merged$effect_b, use = "complete.obs", method = "pearson")
  spearman <- stats::cor(merged$effect_a, merged$effect_b, use = "complete.obs", method = "spearman")
  slope <- stats::coef(stats::lm(effect_b ~ effect_a, data = merged))[2]
  concordant <- sign(merged$effect_a) == sign(merged$effect_b)
  merged$concordant <- concordant

  list(
    effects = merged,
    summary = data.frame(
      label_a = label_a,
      label_b = label_b,
      n_features = nrow(merged),
      pearson = pearson,
      spearman = spearman,
      slope = unname(slope),
      concordance = mean(concordant, na.rm = TRUE),
      stringsAsFactors = FALSE
    ),
    settings = list(groups_a = groups_a, groups_b = groups_b,
                    transform = transform, value_col = value_col)
  )
}

#' Plot a two-experiment comparison
#'
#' @param comparison Output of [compare_experiments()].
#' @param label_top Number of most discordant features to label.
#'
#' @return A `ggplot` object.
#' @export
plot_experiment_comparison <- function(comparison, label_top = 8) {
  effects <- comparison$effects
  if (!is.data.frame(effects) || !all(c("effect_a", "effect_b") %in% names(effects))) {
    stop("`comparison` must be the output of compare_experiments().", call. = FALSE)
  }

  summary <- comparison$summary
  label_a <- summary$label_a[1]
  label_b <- summary$label_b[1]

  effects$feature <- paste(effects$Protein.Name, effects$Peptide.Note, sep = " | ")
  effects$discordance <- abs(effects$effect_a - effects$effect_b)

  label_data <- utils::head(effects[order(-effects$discordance), , drop = FALSE], label_top)

  ggplot2::ggplot(effects, ggplot2::aes(x = effect_a, y = effect_b)) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2, colour = "grey70") +
    ggplot2::geom_vline(xintercept = 0, linetype = 2, colour = "grey70") +
    ggplot2::geom_abline(slope = 1, intercept = 0, colour = "grey50") +
    ggplot2::geom_point(alpha = 0.7, size = 2, colour = "#4C72B0") +
    ggrepel::geom_text_repel(
      data = label_data,
      ggplot2::aes(label = feature),
      size = 2.6,
      max.overlaps = Inf
    ) +
    ggplot2::labs(
      x = paste0(label_a, " effect (CLR)"),
      y = paste0(label_b, " effect (CLR)"),
      subtitle = sprintf("r = %.2f, concordance = %.0f%%",
                         summary$pearson[1], summary$concordance[1] * 100)
    ) +
    ggplot2::theme_minimal(base_family = "sans") +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 0.6)
    )
}
