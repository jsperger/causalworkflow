# Setup for tests - reusing and extending the setup from other files
# ------------------------------------------------------------------------------
set.seed(1234)
n <- 100
treatment_coef <- 0.5
sim_data <- tibble::tibble(
  covar1 = rnorm(n),
  covar2 = rnorm(n),
  treatment = rbinom(n, 1, plogis(treatment_coef * covar1)),
  outcome = 10 + 2 * treatment + 5 * covar1 + 3 * covar2 + rnorm(n)
) |>
  dplyr::mutate(
    treatment = factor(treatment, levels = c(0, 1), labels = c("control", "treated")),
    .row = dplyr::row_number()
  )

# Base (non-tunable) workflows
pscore_wflow <-
  workflows::workflow() |>
  workflows::add_model(parsnip::logistic_reg()) |>
  workflows::add_formula(treatment ~ covar1 + covar2)

outcome_wflow <-
  workflows::workflow() |>
  workflows::add_model(parsnip::linear_reg()) |>
  workflows::add_formula(outcome ~ treatment + covar1 + covar2)

# Tunable workflow for propensity model
pscore_wflow_tune <-
  workflows::workflow() |>
  workflows::add_model(
    parsnip::logistic_reg(penalty = tune::tune()) |> parsnip::set_engine("glmnet")
  ) |>
  workflows::add_formula(treatment ~ covar1 + covar2)

# Workflow set for outcome model
outcome_wf_set <-
  workflowsets::workflow_set(
    preproc = list(base = outcome ~ treatment + covar1 + covar2),
    models = list(
      lm = parsnip::linear_reg(),
      rf = parsnip::rand_forest(mtry = 2) |>
        parsnip::set_engine("ranger") |>
        parsnip::set_mode("regression")
    ),
    cross = FALSE
  )

# Tests for tune_nested
# ------------------------------------------------------------------------------
test_that("tune_nested works with a single tunable workflow", {
  skip_if_not_installed("tune")
  skip_if_not_installed("dials")
  skip_if_not_installed("glmnet")

  aipw_spec_tune <- causal_workflow() |>
    add_propensity_model(pscore_wflow_tune) |>
    add_outcome_model(outcome_wflow)

  outer_resamples <- rsample::vfold_cv(sim_data, v = 2)

  # Test nested tuning
  # Note: A grid is not supplied, relying on tune's default grid creation
  set.seed(4321)
  tuned_wflow <- tune_nested(
    aipw_spec_tune,
    resamples = outer_resamples,
    treatment_var = "treatment",
    outcome_var = "outcome",
    inner_v = 2
  )

  expect_s3_class(tuned_wflow, "fitted_causal_workflow")
  expect_true(tibble::is_tibble(tuned_wflow$estimates))
  expect_equal(nrow(tuned_wflow$estimates), 2)
  expect_equal(names(tuned_wflow$estimates), c("level", ".pred"))
})

test_that("tune_nested works with a workflow_set and stacks", {
  skip_if_not_installed("workflowsets")
  skip_if_not_installed("stacks")
  skip_if_not_installed("ranger")
  skip_if_not_installed("tune")

  aipw_spec_stack <- causal_workflow() |>
    add_propensity_model(pscore_wflow) |>
    add_outcome_model(outcome_wf_set)

  outer_resamples <- rsample::vfold_cv(sim_data, v = 2)

  # Test nested ensembling
  set.seed(4321)
  stacked_wflow <- tune_nested(
    aipw_spec_stack,
    resamples = outer_resamples,
    treatment_var = "treatment",
    outcome_var = "outcome",
    inner_v = 2
  )

  expect_s3_class(stacked_wflow, "fitted_causal_workflow")
  expect_true(tibble::is_tibble(stacked_wflow$estimates))
  expect_equal(nrow(stacked_wflow$estimates), 2)
})

test_that("add_propensity_model and add_outcome_model accept workflow_set", {
  skip_if_not_installed("workflowsets")

  wflow <- causal_workflow() |>
    add_propensity_model(pscore_wflow) |>
    add_outcome_model(outcome_wf_set)

  expect_s3_class(wflow$propensity_model, "workflow")
  expect_s3_class(wflow$outcome_model, "workflow_set")
})