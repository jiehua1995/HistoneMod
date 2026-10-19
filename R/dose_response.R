##### Dose-response and time-course analysis #####
# Helpers for inhibitor / dose / time-course histone PTM experiments.

# Internal helper: goodness-of-fit R-squared.
.hm_r_squared <- function(observed, fitted) {
  if (length(observed) == 0) return(NA_real_)
  sst <- sum((observed - mean(observed))^2)
  if (!is.finite(sst) || sst == 0) return(NA_real_)
  sse <- sum((observed - fitted)^2)
  1 - sse / sst
}

# Internal helper: label for a feature group.
.hm_feature_label <- function(data, group_cols) {
  if (length(group_cols) == 0) return(rep("feature", nrow(data)))
  do.call(paste, c(data[, group_cols, drop = FALSE], sep = " | "))
}

#' Fit dose-response curves
#'
#' Fits a four-parameter logistic curve (or a log-dose linear fallback) to each
#' feature across a dose series. Useful for inhibitor and dose-response
#' experiments where histone mark levels are measured at several concentrations.
#'
#' @param data A long data frame with a numeric dose column, a numeric response
#'   column and one or more grouping columns.
#' @param dose_col Name of the dose column.
#' @param value_col Name of the response column (for example `"Percentage"` or
#'   `"Value"`).
#' @param group_cols Character vector of columns that identify a feature
#'   (default protein and peptide).
#' @param model One of `"auto"`, `"4pl"` or `"linear"`. `"auto"` tries the
#'   four-parameter logistic fit first and falls back to a linear fit on
#'   log10(dose).
#' @param min_points Minimum number of non-zero dose points required for a fit.
#'
#' @return A data frame with one row per feature and columns for `ec50`,
#'   `top`, `bottom`, `hill`, `slope`, `intercept`, `r_squared`, `n`, `model`
#'   and `converged`.
#' @export
fit_dose_response <- function(data, dose_col = "Dose", value_col = "Value",
                              group_cols = c("Protein.Name", "Peptide.Note"),
                              model = c("auto", "4pl", "linear"),
                              min_points = 4L) {
  model <- match.arg(model)
  if (!is.data.frame(data)) stop("`data` must be a data.frame.", call. = FALSE)
  required <- c(dose_col, value_col, group_cols)
  missing_cols <- setdiff(required, colnames(data))
  if (length(missing_cols) > 0) {
    stop("`data` is missing column(s): ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  min_points <- suppressWarnings(as.integer(min_points))
  if (length(min_points) != 1L || is.na(min_points) || min_points < 3L) {
    stop("`min_points` must be an integer >= 3.", call. = FALSE)
  }

  data <- as.data.frame(data, stringsAsFactors = FALSE)
  data[[dose_col]] <- suppressWarnings(as.numeric(data[[dose_col]]))
  data[[value_col]] <- suppressWarnings(as.numeric(data[[value_col]]))
  data <- data[is.finite(data[[dose_col]]) & is.finite(data[[value_col]]), , drop = FALSE]

  if (nrow(data) == 0) stop("No finite dose/response values to fit.", call. = FALSE)

  labels <- .hm_feature_label(data, group_cols)
  split_data <- split(data, labels)

  fits <- lapply(split_data, function(df) {
    x <- df[[dose_col]]
    y <- df[[value_col]]
    positive <- x > 0
    xp <- x[positive]
    yp <- y[positive]

    fit <- NULL
    use_4pl <- model %in% c("auto", "4pl") &&
      sum(positive) >= max(4L, min_points) &&
      length(unique(xp)) >= 4L

    if (use_4pl) {
      fit <- suppressWarnings(tryCatch(
        stats::nls(
          y ~ SSfpl(x, A, B, xmid, scal),
          data = data.frame(x = xp, y = yp),
          control = stats::nls.control(maxiter = 200, warnOnly = TRUE)
        ),
        error = function(e) NULL
      ))
      if (!is.null(fit) && any(!is.finite(stats::coef(fit)))) fit <- NULL
    }

    if (!is.null(fit)) {
      coefs <- stats::coef(fit)
      pred <- tryCatch(stats::fitted(fit), error = function(e) rep(NA_real_, length(yp)))
      r2 <- .hm_r_squared(yp, pred)
      ec50 <- unname(coefs[["xmid"]])
      top <- unname(coefs[["B"]])
      bottom <- unname(coefs[["A"]])
      hill <- unname(coefs[["scal"]])

      # Reject implausible fits (e.g. singular-gradient solutions) so the
      # linear fallback is used instead of returning nonsense.
      valid <- all(is.finite(c(ec50, top, bottom, hill))) &&
        is.finite(r2) && r2 >= 0 &&
        ec50 >= min(xp) / 1000 && ec50 <= max(xp) * 1000
      if (!valid) fit <- NULL
    }

    if (!is.null(fit)) {
      data.frame(
        ec50 = ec50,
        top = top,
        bottom = bottom,
        hill = hill,
        slope = NA_real_,
        intercept = NA_real_,
        r_squared = r2,
        n = sum(positive),
        model = "4pl",
        converged = TRUE,
        stringsAsFactors = FALSE
      )
    } else {
      linear_fit <- if (sum(positive) >= 2L && length(unique(xp)) >= 2L) {
        tryCatch(stats::lm(yp ~ log10(xp)), error = function(e) NULL)
      } else {
        NULL
      }
      if (!is.null(linear_fit)) {
        coefs <- stats::coef(linear_fit)
        data.frame(
          ec50 = NA_real_,
          top = NA_real_,
          bottom = NA_real_,
          hill = NA_real_,
          slope = unname(coefs[2]),
          intercept = unname(coefs[1]),
          r_squared = summary(linear_fit)$r.squared,
          n = sum(positive),
          model = "linear",
          converged = TRUE,
          stringsAsFactors = FALSE
        )
      } else {
        data.frame(
          ec50 = NA_real_, top = NA_real_, bottom = NA_real_, hill = NA_real_,
          slope = NA_real_, intercept = NA_real_, r_squared = NA_real_,
          n = sum(positive), model = "none", converged = FALSE,
          stringsAsFactors = FALSE
        )
      }
    }
  })

  out <- do.call(rbind, fits)
  labels_df <- do.call(rbind, lapply(split_data, function(df) df[1, group_cols, drop = FALSE]))
  out <- cbind(labels_df, out)
  rownames(out) <- NULL

  if ("top" %in% colnames(out) && "bottom" %in% colnames(out)) {
    out$direction <- ifelse(
      is.finite(out$top) & is.finite(out$bottom),
      ifelse(out$top > out$bottom, "up", "down"),
      NA_character_
    )
  }

  out
}

#' Plot dose-response curves
#'
#' Draws the observed data and the fitted dose-response curve for each feature.
#'
#' @param data The long data frame used for fitting.
#' @param fits Optional output of [fit_dose_response()]. If `NULL`, curves are
#'   fitted automatically.
#' @param dose_col Name of the dose column.
#' @param value_col Name of the response column.
#' @param group_cols Columns that identify a feature.
#' @param features Optional character vector of feature labels to show (as
#'   produced by pasting `group_cols` with `" | "`).
#' @param ncol Number of facet columns.
#'
#' @return A `ggplot` object.
#' @export
plot_dose_response <- function(data, fits = NULL, dose_col = "Dose",
                               value_col = "Value",
                               group_cols = c("Protein.Name", "Peptide.Note"),
                               features = NULL, ncol = 2) {
  if (is.null(fits)) {
    fits <- fit_dose_response(
      data,
      dose_col = dose_col,
      value_col = value_col,
      group_cols = group_cols
    )
  }

  data <- as.data.frame(data, stringsAsFactors = FALSE)
  data[[dose_col]] <- suppressWarnings(as.numeric(data[[dose_col]]))
  data[[value_col]] <- suppressWarnings(as.numeric(data[[value_col]]))
  data$feature <- .hm_feature_label(data, group_cols)

  if (!is.null(features)) {
    data <- data[data$feature %in% features, , drop = FALSE]
    fits <- fits[.hm_feature_label(fits, group_cols) %in% features, , drop = FALSE]
  }

  if (nrow(data) == 0) {
    return(ggplot2::ggplot() +
      ggplot2::geom_text(ggplot2::aes(x = 0, y = 0, label = "No dose-response data"), size = 5) +
      ggplot2::theme_void())
  }

  fits$feature <- .hm_feature_label(fits, group_cols)

  curve_rows <- lapply(seq_len(nrow(fits)), function(i) {
    row <- fits[i, , drop = FALSE]
    observed <- data[data$feature == row$feature, , drop = FALSE]
    positive <- observed[[dose_col]][observed[[dose_col]] > 0]
    if (length(positive) == 0) return(NULL)
    grid <- exp(seq(log(min(positive)), log(max(positive)), length.out = 100))
    if (identical(row$model, "4pl") && is.finite(row$ec50) && is.finite(row$hill)) {
      predicted <- row$bottom + (row$top - row$bottom) / (1 + exp((row$ec50 - grid) / row$hill))
    } else if (identical(row$model, "linear") && is.finite(row$slope)) {
      predicted <- row$intercept + row$slope * log10(grid)
    } else {
      return(NULL)
    }
    data.frame(feature = row$feature, dose = grid, predicted = predicted)
  })
  curve <- do.call(rbind, curve_rows)

  p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[dose_col]], y = .data[[value_col]])) +
    ggplot2::geom_point(alpha = 0.6, size = 1.8, colour = "#4C72B0") +
    ggplot2::facet_wrap(~feature, scales = "free_y", ncol = ncol) +
    ggplot2::scale_x_log10() +
    ggplot2::labs(x = dose_col, y = value_col) +
    ggplot2::theme_minimal(base_family = "sans") +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 0.6),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      strip.text = ggplot2::element_text(size = 9)
    )

  if (!is.null(curve) && nrow(curve) > 0) {
    p <- p + ggplot2::geom_line(
      data = curve,
      ggplot2::aes(x = dose, y = predicted),
      colour = "#D55E00",
      linewidth = 0.9,
      inherit.aes = FALSE
    )
  }

  p
}
