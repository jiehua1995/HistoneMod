test_that("fit_dose_response fits a synthetic logistic curve", {
  set.seed(42)
  dose <- c(0.01, 0.03, 0.1, 0.3, 1, 3, 10)
  truth <- 50 - 45 / (1 + exp((0.3 - dose) / 0.2))
  data <- data.frame(
    Protein.Name = "H3",
    Peptide.Note = "H3K9me3",
    Dose = rep(dose, each = 3),
    Percentage = rep(truth, each = 3) + stats::rnorm(length(dose) * 3, 0, 0.8)
  )

  fits <- fit_dose_response(data, value_col = "Percentage")
  expect_s3_class(fits, "data.frame")
  expect_true(all(c("ec50", "top", "bottom", "hill", "r_squared", "model") %in% names(fits)))
  expect_equal(nrow(fits), 1L)
  expect_true(fits$converged)
  expect_true(is.finite(fits$r_squared))
  expect_gte(fits$r_squared, 0)

  plot <- plot_dose_response(data, fits, value_col = "Percentage")
  expect_s3_class(plot, "ggplot")
})

test_that("fit_dose_response falls back gracefully with limited data", {
  data <- data.frame(
    Protein.Name = "H4",
    Peptide.Note = "H4K16ac",
    Dose = c(0.1, 1),
    Percentage = c(10, 30)
  )
  fits <- fit_dose_response(data, value_col = "Percentage")
  expect_equal(nrow(fits), 1L)
  expect_true(is.na(fits$ec50))
  expect_true(fits$model %in% c("linear", "none"))
})

test_that("simulate_histone_data returns a plausible long table", {
  sim <- simulate_histone_data(n_replicates = 3, n_features = 15, seed = 7)
  expect_s3_class(sim, "data.frame")
  expect_equal(nrow(sim), 2 * 3 * 15)
  expect_true(all(c("Replicate.Name", "Group", "Protein.Name",
                    "Peptide.Note", "Percentage", "TrueEffect") %in% names(sim)))
  expect_true(all(sim$Percentage >= 0))
  totals <- stats::aggregate(Percentage ~ Replicate.Name, sim, sum)
  expect_true(all(abs(totals$Percentage - 100) < 1e-8))
})

test_that("estimate_power returns a monotone-ish power table", {
  power <- estimate_power(
    n_replicates = c(3, 5),
    n_features = 20,
    n_sim = 12,
    seed = 11
  )
  expect_s3_class(power, "data.frame")
  expect_equal(nrow(power), 2L)
  expect_true(all(power$power >= 0 & power$power <= 1))

  plot <- plot_power_curve(power)
  expect_s3_class(plot, "ggplot")
})
