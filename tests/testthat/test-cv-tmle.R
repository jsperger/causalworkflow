# Tests for cv_tmle() functionality - Corrected and Final Version

# --- Test Data and Model Setup ---
create_test_data <- function(n = 200) {
  set.seed(123)
  tibble::tibble(
    .row = 1:n,
    X1 = rnorm(n),
    A1 = factor(ifelse(plogis(0.5 * rnorm(n) + 0.5 * X1) > 0.5, "b", "a")),
    X2 = rnorm(n) + 0.5 * X1,
    A2 = factor(ifelse(plogis(0.5 * rnorm(n) + 0.2 * X2) > 0.5, "d", "c")),
    Y = rnorm(n, sd = 2) + as.numeric(A1 == "b") + as.numeric(A2 == "d") + 0.5 * X1
  )
}

q_wflow <- workflows::workflow() |>
  workflows::add_model(parsnip::linear_reg() |> parsnip::set_engine("lm"))
g_wflow <- workflows::workflow() |>
  workflows::add_model(parsnip::logistic_reg() |> parsnip::set_engine("glm"))

# --- Tests ---

test_that("cv_tmle runs and produces correctly structured output with simple workflows", {
  test_data <- create_test_data()
  resamples <- rsample::vfold_cv(test_data, v = 2)

  stage1_wflow <- causal_workflow() |>
    add_propensity_model(g_wflow |> workflows::add_formula(A1 ~ X1)) |>
    add_outcome_model(q_wflow |> workflows::add_formula(Y ~ X1 + A1))

  stage2_wflow <- causal_workflow() |>
    add_propensity_model(g_wflow |> workflows::add_formula(A2 ~ X1 + A1 + X2)) |>
    add_outcome_model(q_wflow |> workflows::add_formula(Y ~ X1 + A1 + X2 + A2))

  stages <- list("1" = stage1_wflow, "2" = stage2_wflow)

  tmle_fit <- cv_tmle(
    data = test_data,
    resamples = resamples,
    stages = stages,
    actions = c("A1", "A2"),
    outcome = "Y"
  )

  expect_s3_class(tmle_fit, "cv_tmle_fit")
  expect_named(tmle_fit, c(".estimate", ".se", ".conf.low", ".conf.high", "influence_curve", "nuisance_predictions"))
  expect_true(is.numeric(tmle_fit$.estimate))
})

test_that("cv_tmle works with workflows containing recipes", {
  test_data <- create_test_data()
  resamples <- rsample::vfold_cv(test_data, v = 2)

  q_recipe <- recipes::recipe(Y ~ X1 + A1 + X2 + A2, data = test_data) |>
    recipes::step_dummy(A1, A2)
  q_wflow_recipe <- workflows::workflow() |>
    workflows::add_model(parsnip::linear_reg() |> parsnip::set_engine("lm")) |>
    workflows::add_recipe(q_recipe)

  stage1_wflow <- causal_workflow() |>
    add_propensity_model(g_wflow |> workflows::add_formula(A1 ~ X1)) |>
    add_outcome_model(q_wflow |> workflows::add_formula(Y ~ X1 + A1))
  stage2_wflow <- causal_workflow() |>
    add_propensity_model(g_wflow |> workflows::add_formula(A2 ~ X1 + A1 + X2)) |>
    add_outcome_model(q_wflow_recipe)
  stages <- list("1" = stage1_wflow, "2" = stage2_wflow)

  expect_no_error({
    cv_tmle(
      data = test_data,
      resamples = resamples,
      stages = stages,
      actions = c("A1", "A2"),
      outcome = "Y"
    )
  })
})

test_that("cv_tmle works with workflow_sets for ensembling", {
  testthat::skip_if_not_installed("stacks")
  testthat::skip_if_not_installed("workflowsets")
  testthat::skip_if_not_installed("earth")
  testthat::skip_if_not_installed("glmnet")

  test_data <- create_test_data()
  resamples <- rsample::vfold_cv(test_data, v = 2)

  # Using a workflow_set for the Q-model as it's more stable
  q_wflow_set <- workflowsets::workflow_set(
    preproc = list(main = Y ~ X1 + A1),
    models = list(
      lm = parsnip::linear_reg(),
      mars = parsnip::mars(mode = "regression") |> parsnip::set_engine("earth")
    )
  )

  stage1_wflow <- causal_workflow() |>
    add_propensity_model(g_wflow |> workflows::add_formula(A1 ~ X1)) |>
    add_outcome_model(q_wflow_set)
  stage2_wflow <- causal_workflow() |>
    add_propensity_model(g_wflow |> workflows::add_formula(A2 ~ X1 + A1 + X2)) |>
    add_outcome_model(q_wflow |> workflows::add_formula(Y ~ X1 + A1 + X2 + A2))
  stages <- list("1" = stage1_wflow, "2" = stage2_wflow)

  expect_no_error({
    cv_tmle(
      data = test_data,
      resamples = resamples,
      stages = stages,
      actions = c("A1", "A2"),
      outcome = "Y"
    )
  })
})