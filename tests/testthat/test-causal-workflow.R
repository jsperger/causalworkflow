# Setup for tests
# ------------------------------------------------------------------------------
# Create some sample data
set.seed(1234)
n <- 100
treatment_coef <- 0.5
sim_data <- tibble::tibble(
  covar1 = rnorm(n),
  covar2 = rnorm(n),
  treatment = rbinom(n, 1, plogis(treatment_coef * covar1)),
  outcome = 10 + 2 * treatment + 5 * covar1 + 3 * covar2 + rnorm(n)
) |>
  dplyr::mutate(treatment = factor(treatment, levels = c(0, 1), labels = c("control", "treated")))

# Define component workflows
pscore_wflow <-
  workflows::workflow() |>
  workflows::add_model(parsnip::logistic_reg()) |>
  workflows::add_formula(treatment ~ covar1 + covar2)

outcome_wflow <-
  workflows::workflow() |>
  workflows::add_model(parsnip::linear_reg()) |>
  workflows::add_formula(outcome ~ treatment + covar1 + covar2)

# Tests for the constructor and add_* verbs
# ------------------------------------------------------------------------------
test_that("causal_workflow constructor works", {
  wflow <- causal_workflow()
  expect_s3_class(wflow, "causal_workflow")
  expect_null(wflow$propensity_model)
  expect_null(wflow$outcome_model)
})

test_that("add_propensity_model and add_outcome_model work", {
  wflow <- causal_workflow() |>
    add_propensity_model(pscore_wflow) |>
    add_outcome_model(outcome_wflow)

  expect_s3_class(wflow$propensity_model, "workflow")
  expect_s3_class(wflow$outcome_model, "workflow")

  # Test error conditions
  expect_error(add_propensity_model(wflow, "not a workflow"))
  expect_error(add_outcome_model(wflow, "not a workflow"))
  expect_error(add_propensity_model("not a causal workflow", pscore_wflow))
})

# Tests for fit and predict methods
# ------------------------------------------------------------------------------
test_that("fit.causal_workflow and predict.fitted_causal_workflow work", {
  skip_if_not_installed("parsnip")
  skip_if_not_installed("recipes")
  skip_if_not_installed("rsample")
  skip_if_not_installed("dplyr")

  aipw_spec <- causal_workflow() |>
    add_propensity_model(pscore_wflow) |>
    add_outcome_model(outcome_wflow)

  # Test fitting
  fitted_wflow <- fit(aipw_spec, data = sim_data)
  expect_s3_class(fitted_wflow, "fitted_causal_workflow")
  expect_true(!is.null(fitted_wflow$estimates))
  expect_true(!is.null(fitted_wflow$variances))
  expect_equal(nrow(fitted_wflow$eif), nrow(sim_data))
  expect_equal(ncol(fitted_wflow$eif), 2)

  # Test prediction types
  pred_est <- predict(fitted_wflow, type = "potential_outcome")
  expect_true(tibble::is_tibble(pred_est))
  expect_equal(names(pred_est), c("level", ".pred", ".std_err"))
  expect_equal(nrow(pred_est), 2)

  pred_if <- predict(fitted_wflow, type = "if")
  expect_true(tibble::is_tibble(pred_if))
  expect_equal(names(pred_if), c("eif_pom_control", "eif_pom_treated"))
  expect_equal(nrow(pred_if), nrow(sim_data))

  pred_comp <- predict(fitted_wflow, type = "components")
  expect_true(tibble::is_tibble(pred_comp))
  expect_true(all(c("g_hat_control", "g_hat_treated", "q_hat_control", "q_hat_treated") %in% names(pred_comp)))
  expect_equal(nrow(pred_comp), nrow(sim_data))
})