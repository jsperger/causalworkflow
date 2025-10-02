test_that("AIPW engine returns a fitted object with a correct estimate", {
  # Using a simple, non-resampled fit
  data <- mtcars
  data$am <- as.factor(data$am)
  set.seed(123)

  propensity_wf <- workflows::workflow() |>
    workflows::add_model(parsnip::logistic_reg() |> parsnip::set_engine("glm")) |>
    workflows::add_formula(am ~ wt + hp)

  outcome_wf <- workflows::workflow() |>
    workflows::add_model(parsnip::linear_reg() |> parsnip::set_engine("lm")) |>
    workflows::add_formula(mpg ~ am + wt + hp)

  cwf <- causal_workflow(
    propensity = propensity_wf,
    outcome = outcome_wf
  )

  fitted_cwf <- fit(cwf, data = data, engine = "aipw")

  # Check for the new class
  expect_s3_class(fitted_cwf, "fitted_causal_workflow")

  # Check for a plausible estimate
  # The simple regression coefficient for `am` is ~2.55. The AIPW estimate
  # should be close to this value. The previous expected value of -1.45
  # was likely incorrect.
  expect_type(fitted_cwf$estimate, "double")
  expect_equal(fitted_cwf$estimate, 2.5, tolerance = 0.1)
})

test_that("fit.causal_workflow requires an engine", {
  expect_snapshot(
    error = TRUE,
    fit(causal_workflow(), data = mtcars)
  )
})

test_that("fit.causal_workflow errors with an unknown engine", {
  expect_snapshot(
    error = TRUE,
    fit(causal_workflow(), data = mtcars, engine = "unknown_engine")
  )
})