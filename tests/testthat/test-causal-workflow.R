# Setup for tests
# ------------------------------------------------------------------------------
# Create some sample data
set.seed(1234)
n <- 100
sim_data <- tibble::tibble(
  covar1 = rnorm(n),
  covar2 = rnorm(n),
  treatment = rbinom(n, 1, plogis(0.5 * covar1)),
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
  expect_true(!is.null(fitted_wflow$estimate))
  expect_true(!is.null(fitted_wflow$variance))
  expect_equal(length(fitted_wflow$eif), nrow(sim_data))

  # Test prediction types
  pred_est <- predict(fitted_wflow, type = "estimate")
  expect_true(tibble::is_tibble(pred_est))
  expect_equal(names(pred_est), c(".pred", ".std_err"))
  expect_equal(nrow(pred_est), 1)

  pred_if <- predict(fitted_wflow, type = "if")
  expect_true(tibble::is_tibble(pred_if))
  expect_equal(names(pred_if), ".pred_if")
  expect_equal(nrow(pred_if), nrow(sim_data))

  pred_comp <- predict(fitted_wflow, type = "components")
  expect_true(tibble::is_tibble(pred_comp))
  expect_true(all(c("g_hat", "q1_hat", "q0_hat") %in% names(pred_comp)))
  expect_equal(nrow(pred_comp), nrow(sim_data))
})

# Tests for tuning
# ------------------------------------------------------------------------------
test_that("tune_grid.causal_workflow works for greedy tuning", {
  skip_if_not_installed("dials")
  skip_if_not_installed("tune")
  skip_if_not_installed("ranger")

  # Tunable workflow
  pscore_wflow_tune <-
    workflows::workflow() |>
    workflows::add_model(
      parsnip::logistic_reg(penalty = tune::tune()) |> parsnip::set_engine("glmnet")
    ) |>
    workflows::add_formula(treatment ~ covar1 + covar2)

  aipw_spec_tune <- causal_workflow() |>
    add_propensity_model(pscore_wflow_tune) |>
    add_outcome_model(outcome_wflow)

  resamples <- rsample::vfold_cv(sim_data, v = 2)
  grid <- dials::grid_regular(dials::penalty(), levels = 2)

  # Test tuning dispatch
  tune_res <- tune::tune_grid(aipw_spec_tune, resamples = resamples, grid = grid)
  expect_s3_class(tune_res, "tune_results")

  # Test error for ambiguous grid
  outcome_wflow_tune <-
    workflows::workflow() |>
    workflows::add_model(
      parsnip::linear_reg(penalty = tune::tune()) |> parsnip::set_engine("glmnet")
    ) |>
    workflows::add_formula(outcome ~ treatment + covar1 + covar2)

  aipw_spec_ambiguous <- causal_workflow() |>
    add_propensity_model(pscore_wflow_tune) |>
    add_outcome_model(outcome_wflow_tune)

  expect_error(
    tune::tune_grid(aipw_spec_ambiguous, resamples = resamples, grid = grid),
    "Ambiguous tuning grid"
  )
})