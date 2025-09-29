# Tests for cv_tmle() functionality

# --- Test Data and Model Setup ---

# Create a simple, 2-stage synthetic dataset for testing
create_test_data <- function(n = 200) {
  set.seed(123)
  tibble::tibble(
    .row = 1:n,
    X1 = rnorm(n),
    A1 = factor(sample(c("a", "b"), n, replace = TRUE)),
    X2 = rnorm(n) + 0.5 * X1,
    A2 = factor(sample(c("c", "d"), n, replace = TRUE)),
    Y = rnorm(n) + as.numeric(A1 == "b") + as.numeric(A2 == "d")
  )
}

# Define simple workflows for nuisance models
q_wflow <- workflows::workflow() |>
  workflows::add_model(parsnip::linear_reg() |> parsnip::set_engine("lm")) |>
  workflows::add_formula(Y ~ .)

g_wflow <- workflows::workflow() |>
  workflows::add_model(parsnip::logistic_reg() |> parsnip::set_engine("glm")) |>
  workflows::add_formula(A1 ~ .)


test_that("cv_tmle runs and produces correctly structured output", {
  test_data <- create_test_data()
  resamples <- rsample::vfold_cv(test_data, v = 2) # Use 2 folds for speed

  # Define staged models
  q_models <- staged_workflow() |>
    add_stage_model(
      q_wflow |> workflows::update_formula(Y ~ X1 + A1 + X2 + A2),
      stage = 2
    ) |>
    add_stage_model(
      q_wflow |> workflows::update_formula(Y ~ X1 + A1),
      stage = 1
    )

  g_models <- staged_workflow() |>
    add_stage_model(
      g_wflow |> workflows::update_formula(A2 ~ X1 + A1 + X2),
      stage = 2
    ) |>
    add_stage_model(
      g_wflow |> workflows::update_formula(A1 ~ X1),
      stage = 1
    )

  # Run the estimator
  tmle_fit <- cv_tmle(
    data = test_data,
    resamples = resamples,
    q_models = q_models,
    g_models = g_models,
    actions = c("A1", "A2"),
    outcome = "Y"
  )

  # --- Assertions ---
  # Check class
  expect_s3_class(tmle_fit, "cv_tmle_fit")

  # Check top-level elements
  expect_named(tmle_fit, c(".estimate", ".se", ".conf.low", ".conf.high", "influence_curve", "nuisance_predictions"))

  # Check types of scalar results
  expect_true(is.numeric(tmle_fit$.estimate) && length(tmle_fit$.estimate) == 1)
  expect_true(is.numeric(tmle_fit$.se) && length(tmle_fit$.se) == 1)
  expect_true(is.numeric(tmle_fit$.conf.low) && length(tmle_fit$.conf.low) == 1)
  expect_true(is.numeric(tmle_fit$.conf.high) && length(tmle_fit$.conf.high) == 1)

  # Check structure of tibble results
  expect_s3_class(tmle_fit$influence_curve, "tbl_df")
  expect_equal(nrow(tmle_fit$influence_curve), nrow(test_data))
  expect_named(tmle_fit$influence_curve, c(".row", "ic"))

  expect_s3_class(tmle_fit$nuisance_predictions, "tbl_df")
  expect_equal(nrow(tmle_fit$nuisance_predictions), nrow(test_data))

  # Check that key columns from the algorithm exist in the nuisance predictions
  expect_true(all(c("q1_star_a", "q1_star_b", "q2_star_c", "q2_star_d", "D0", "D1", "D2") %in% names(tmle_fit$nuisance_predictions)))
})


test_that("cv_tmle works with workflows containing recipes", {
  test_data <- create_test_data()
  resamples <- rsample::vfold_cv(test_data, v = 2)

  # Define a recipe that creates dummy variables
  q_recipe <- recipes::recipe(Y ~ X1 + A1 + X2 + A2, data = test_data) |>
    recipes::step_dummy(A1, A2, one_hot = TRUE)

  # Create a workflow using this recipe
  q_wflow_recipe <- workflows::workflow() |>
    workflows::add_model(parsnip::linear_reg() |> parsnip::set_engine("lm")) |>
    workflows::add_recipe(q_recipe)

  # Define staged models, using the recipe-based workflow for stage 2
  q_models <- staged_workflow() |>
    add_stage_model(
      q_wflow_recipe, # Using the recipe workflow here
      stage = 2
    ) |>
    add_stage_model(
      q_wflow |> workflows::update_formula(Y ~ X1 + A1), # Keep stage 1 simple
      stage = 1
    )

  g_models <- staged_workflow() |>
    add_stage_model(
      g_wflow |> workflows::update_formula(A2 ~ X1 + A1 + X2),
      stage = 2
    ) |>
    add_stage_model(
      g_wflow |> workflows::update_formula(A1 ~ X1),
      stage = 1
    )

  # Run the estimator and expect it to work without errors
  expect_no_error({
    tmle_fit_recipe <- cv_tmle(
      data = test_data,
      resamples = resamples,
      q_models = q_models,
      g_models = g_models,
      actions = c("A1", "A2"),
      outcome = "Y"
    )
  })
})

test_that("cv_tmle works with workflow_sets for ensembling", {
  testthat::skip_if_not_installed("stacks")
  testthat::skip_if_not_installed("workflowsets")

  test_data <- create_test_data()
  resamples <- rsample::vfold_cv(test_data, v = 2)

  # Define a workflow set for the stage 1 propensity model
  g_wflow_set <- workflowsets::workflow_set(
    preproc = list(A1 ~ X1),
    models = list(
      logistic = parsnip::logistic_reg(),
      mars = parsnip::mars(mode = "classification") |> parsnip::set_engine("earth")
    )
  )

  # Define staged models
  q_models <- staged_workflow() |>
    add_stage_model(q_wflow |> workflows::update_formula(Y ~ X1 + A1 + X2 + A2), stage = 2) |>
    add_stage_model(q_wflow |> workflows::update_formula(Y ~ X1 + A1), stage = 1)

  g_models <- staged_workflow() |>
    add_stage_model(g_wflow |> workflows::update_formula(A2 ~ X1 + A1 + X2), stage = 2) |>
    add_stage_model(g_wflow_set, stage = 1) # Use the workflow_set here

  # Run the estimator and expect it to work without errors
  expect_no_error({
    tmle_fit_wfset <- cv_tmle(
      data = test_data,
      resamples = resamples,
      q_models = q_models,
      g_models = g_models,
      actions = c("A1", "A2"),
      outcome = "Y"
    )
  })
})