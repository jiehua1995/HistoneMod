##### Time-course analysis #####
# Fit simple kinetic models to histone PTM time-course experiments.

# Internal helper: fit a single feature's time course.
.hm_fit_time_series <- function(x, y, model = "auto", min_points = 4L) {
  ok <- is.finite(x) & is.finite(y) & x >= 0
  x <- x[ok]
  y <- y[ok]
  order_index <- order(x)
  x <- x[order_index]
  y <- y[order_index]

  if (length(x) < max(3L, min_points) || length(unique(x)) < 3L) {
    return(.hm_empty_time_fit(model = "none", converged = FALSE, n = length(x)))
  }

  y0_start <- y[which.min(x)]
  plateau_start <- y[which.max(x)]
  if (!is.finite(y0_start)) y0_start <- mean(y, na.rm = TRUE)
  if (!is.finite(plateau_start)) plateau_start <- mean(y, na.rm = TRUE)

  fit_model <- function(kind) {
    if (identical(kind, "decay")) {
      formula <- y ~ plateau + (y0 - plateau) * exp(-k * x)
    } else {
      formula <- y ~ y0 + (plateau - y0) * (1 - exp(-k * x))
    }
    suppressWarnings(tryCatch(
      stats::nls(
        formula,
        data = data.frame(x = x, y = y),
        start = list(y0 = y0_start, plateau = plateau_start, k = 0.1),
        algorithm = "port",
        lower = c(y0 = -Inf, plateau = -Inf, k = 1e-6),
        control = stats::nls.control(maxiter = 300, warnOnly = TRUE)
      ),
      error = function(e) NULL
    ))
  }

  candidates <- list()
  if (model %in% c("auto", "decay")) {
    fit <- fit_model("decay")
    if (!is.null(fit)) {
      candidates[["decay"]] <- list(fit = fit, aic = stats::AIC(fit), kind = "decay")
    }
  }
  if (model %in% c("auto", "approach")) {
    fit <- fit_model("approach")
    if (!is.null(fit)) {
      candidates[["approach"]] <- list(fit = fit, aic = stats::AIC(fit), kind = "approach")
    }
  }

  if (length(candidates) > 0) {
    best <- candidates[[which.min(vapply(candidates, `[[`, numeric(1), "aic"))]]
    coefs <- stats::coef(best$fit)
    predicted <- tryCatch(stats::fitted(best$fit), error = function(e) rep(NA_real_, length(y)))
    r2 <- .hm_r_squared(y, predicted)
    k <- unname(coefs[["k"]])
    if (is.finite(k) && k > 0 && is.finite(r2) && r2 >= 0) {
      return(data.frame(
        model = best$kind,
        half_life = log(2) / k,
        k = k,
        y0 = unname(coefs[["y0"]]),
        plateau = unname(coefs[["plateau"]]),
        slope = NA_real_,
        intercept = NA_real_,
        r_squared = r2,
        n = length(x),
        converged = TRUE,
        direction = if (unname(coefs[["plateau"]]) >= unname(coefs[["y0"]])) "increase" else "decrease",
        stringsAsFactors = FALSE
      ))
    }
  }

  linear <- tryCatch(stats::lm(y ~ x), error = function(e) NULL)
  if (!is.null(linear)) {
    coefs <- stats::coef(linear)
    return(data.frame(
      model = "linear",
      half_life = NA_real_,
      k = NA_real_,
      y0 = NA_real_,
      plateau = NA_real_,
      slope = unname(coefs[2]),
      intercept = unname(coefs[1]),
      r_squared = summary(linear)$r.squared,
      n = length(x),
      converged = TRUE,
      direction = if (unname(coefs[2]) >= 0) "increase" else "decrease",
      stringsAsFactors = FALSE
    ))
  }

  .hm_empty_time_fit(model = "none", converged = FALSE, n = length(x))
}

.hm_empty_time_fit <- function(model, converged, n) {
  data.frame(
    model = model,
    half_life = NA_real_,
    k = NA_real_,
    y0 = NA_real_,
    plateau = NA_real_,
    slope = NA_real_,
    intercept = NA_real_,
    r_squared = NA_real_,
    n = n,
    converged = converged,
    direction = NA_character_,
    stringsAsFactors = FALSE
  )
}

#' Fit time-course profiles
#'
#' Fits an exponential decay or approach-to-plateau model (with a linear
#' fallback) to each feature in a time-course experiment and reports the
#' half-life.
#'
#' @param data Long data frame with a numeric time column, a response column
#'   and one or more grouping columns.
#' @param time_col Name of the time column.
#' @param value_col Name of the response column.
#' @param group_cols Columns that identify a feature.
#' @param model One of `"auto"`, `"decay"`, `"approach"` or `"linear"`.
#' @param min_points Minimum number of distinct time points required.
#'
#' @return A data frame with one row per feature and columns for `model`,
#'   `half_life`, `k`, `y0`, `plateau`, `slope`, `intercept`, `r_squared`, `n`,
#'   `converged` and `direction`.
#' @export
fit_time_course <- function(data, time_col = "Time", value_col = "Value",
                            group_cols = c("Protein.Name", "Peptide.Note"),
                            model = c("auto", "decay", "approach", "linear"),
                            min_points = 4L) {
  model <- match.arg(model)
  if (!is.data.frame(data)) stop("`data` must be a data.frame.", call. = FALSE)
  required <- c(time_col, value_col, group_cols)
  missing_cols <- setdiff(required, colnames(data))
  if (length(missing_cols) > 0) {
    stop("`data` is missing column(s): ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  data <- as.data.frame(data, stringsAsFactors = FALSE)
  data[[time_col]] <- suppressWarnings(as.numeric(data[[time_col]]))
  data[[value_col]] <- suppressWarnings(as.numeric(data[[value_col]]))
  data <- data[is.finite(data[[time_col]]) & is.finite(data[[value_col]]), , drop = FALSE]
  if (nrow(data) == 0) stop("No finite time/response values to fit.", call. = FALSE)

  labels <- .hm_feature_label(data, group_cols)
  split_data <- split(data, labels)

  rows <- lapply(split_data, function(df) {
    fit <- .hm_fit_time_series(
      df[[time_col]],
      df[[value_col]],
      model = model,
      min_points = min_points
    )
    cbind(df[1, group_cols, drop = FALSE], fit)
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Plot time-course profiles
#'
#' @param data The long data frame used for fitting.
#' @param fits Optional output of [fit_time_course()].
#' @param time_col Name of the time column.
#' @param value_col Name of the response column.
#' @param group_cols Columns that identify a feature.
#' @param features Optional feature labels to show.
#' @param ncol Number of facet columns.
#'
#' @return A `ggplot` object.
#' @export
plot_time_course <- function(data, fits = NULL, time_col = "Time",
                             value_col = "Value",
                             group_cols = c("Protein.Name", "Peptide.Note"),
                             features = NULL, ncol = 2) {
  if (is.null(fits)) {
    fits <- fit_time_course(data, time_col = time_col, value_col = value_col,
                            group_cols = group_cols)
  }

  data <- as.data.frame(data, stringsAsFactors = FALSE)
  data[[time_col]] <- suppressWarnings(as.numeric(data[[time_col]]))
  data[[value_col]] <- suppressWarnings(as.numeric(data[[value_col]]))
  data$feature <- .hm_feature_label(data, group_cols)

  if (!is.null(features)) {
    data <- data[data$feature %in% features, , drop = FALSE]
    fits <- fits[.hm_feature_label(fits, group_cols) %in% features, , drop = FALSE]
  }

  if (nrow(data) == 0) {
    return(ggplot2::ggplot() +
      ggplot2::geom_text(ggplot2::aes(x = 0, y = 0, label = "No time-course data"), size = 5) +
      ggplot2::theme_void())
  }

  fits$feature <- .hm_feature_label(fits, group_cols)

  curve_rows <- lapply(seq_len(nrow(fits)), function(i) {
    row <- fits[i, , drop = FALSE]
    observed <- data[data$feature == row$feature, , drop = FALSE]
    times <- observed[[time_col]]
    if (length(times) < 2) return(NULL)
    grid <- seq(min(times), max(times), length.out = 100)
    if (row$model %in% c("decay", "approach") && is.finite(row$k)) {
      if (identical(row$model, "decay")) {
        predicted <- row$plateau + (row$y0 - row$plateau) * exp(-row$k * grid)
      } else {
        predicted <- row$y0 + (row$plateau - row$y0) * (1 - exp(-row$k * grid))
      }
    } else if (identical(row$model, "linear") && is.finite(row$slope)) {
      predicted <- row$intercept + row$slope * grid
    } else {
      return(NULL)
    }
    data.frame(feature = row$feature, time = grid, predicted = predicted)
  })
  curve <- do.call(rbind, curve_rows)

  p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[time_col]], y = .data[[value_col]])) +
    ggplot2::geom_point(alpha = 0.6, size = 1.8, colour = "#4C72B0") +
    ggplot2::facet_wrap(~feature, scales = "free_y", ncol = ncol) +
    ggplot2::labs(x = time_col, y = value_col) +
    ggplot2::theme_minimal(base_family = "sans") +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 0.6),
      strip.text = ggplot2::element_text(size = 9)
    )

  if (!is.null(curve) && nrow(curve) > 0) {
    p <- p + ggplot2::geom_line(
      data = curve,
      ggplot2::aes(x = time, y = predicted),
      colour = "#D55E00",
      linewidth = 0.9,
      inherit.aes = FALSE
    )
  }

  p
}
