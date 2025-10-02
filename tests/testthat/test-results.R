# Helper function to create a fitted object for testing
fit_test_aipw <- function() {
  data <- mtcars
  data$am <- as.factor(data$am)
  set.seed(123)

  propensity_wf <- workflows::workflow() |>
    workflows::add_model(parsnip::logistic_reg() |> parsnip::set_engine("glm")) |>
    workflows::add_formula(am ~ wt + hp)

  outcome_wf <- workflows::workflow() |>
    workflows::add_model(parsnip::linear_reg() |> parsnip::set_engine("lm")) |>
    workflows::add_formula(mpg ~ am + wt + hp)

  causal_workflow(propensity = propensity_wf, outcome = outcome_wf) |>
    fit(data = data, engine = "aipw")
}

test_that("collect_metrics returns a tibble with the correct format", {
  fitted_cwf <- fit_test_aipw()
  metrics <- collect_metrics(fitted_cwf)

  expect_s3_class(metrics, "tbl_df")
  expect_equal(
    names(metrics),
    c(".metric", ".estimator", ".estimate")
  )
  expect_equal(nrow(metrics), 1)
  expect_equal(metrics$.metric, "ate")
  expect_equal(metrics$.estimator, "aipw")
  expect_equal(metrics$.estimate, 2.5, tolerance = 0.1)
})

test_that("collect_metrics errors if called on an unfitted workflow", {
  cwf <- causal_workflow()
  expect_snapshot(
    error = TRUE,
    collect_metrics(cwf)
  )
})