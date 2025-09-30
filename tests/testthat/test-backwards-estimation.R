# --- Unit tests for `backwards_estimation_step` -------------------------------

library(parsnip)
library(recipes)
library(workflows)
library(testthat)

# --- Test Data ----------------------------------------------------------------

# A simple dataset for a 2-stage DTR
set.seed(123)
sim_data <- data.frame(
  id = rep(1:10, each = 2),
  stage = rep(1:2, 10),
  covar1 = rnorm(20),
  covar2 = rnorm(20),
  action = factor(sample(c("A", "B"), 20, replace = TRUE)),
  outcome = rnorm(20)
)

# --- Test `backwards_estimation_step` -----------------------------------------

test_that("`backwards_estimation_step` is consistent with `fit`", {
  testthat::skip_if_not_installed("broom")
  # --- Setup: Define a simple 2-stage workflow ---
  lm_wflow <- workflow() |>
    add_model(linear_reg()) |>
    add_formula(outcome ~ covar1 + covar2 + action)

  spec <- staged_workflow() |>
    add_stage_model(lm_wflow, stages = 1:2)

  actions <- levels(sim_data$action)

  # --- Method 1: Use the top-level `fit()` function ---
  fitted_spec_auto <- fit(spec, data = sim_data)

  # --- Method 2: Use `backwards_estimation_step()` manually ---
  # Fit stage 2 (the last stage)
  fitted_model_2 <- backwards_estimation_step(
    object = spec,
    data = sim_data,
    k = 2,
    next_stage_model = NULL,
    actions = actions
  )

  # Fit stage 1, using the fitted model from stage 2
  fitted_model_1 <- backwards_estimation_step(
    object = spec,
    data = sim_data,
    k = 1,
    next_stage_model = fitted_model_2,
    actions = actions
  )

  # --- Comparison ---
  # The manually fitted models should have identical coefficients
  coefs_auto_1 <- broom::tidy(fitted_spec_auto$models$`1`)
  coefs_manual_1 <- broom::tidy(fitted_model_1)
  expect_equal(coefs_auto_1, coefs_manual_1)

  coefs_auto_2 <- broom::tidy(fitted_spec_auto$models$`2`)
  coefs_manual_2 <- broom::tidy(fitted_model_2)
  expect_equal(coefs_auto_2, coefs_manual_2)
})

test_that("`backwards_estimation_step` handles `causal_workflow` correctly", {
  # --- Setup: A compositional workflow ---
  stage_1_aipw <- causal_workflow() |>
    add_outcome_model(
      workflow() |>
        add_model(linear_reg()) |>
        add_formula(outcome ~ covar1 + covar2 + action)
    ) |>
    add_propensity_model(
      workflow() |>
        add_model(logistic_reg()) |>
        add_formula(action ~ covar1 + covar2)
    )

  stage_2_q <- workflow() |>
    add_model(linear_reg()) |>
    add_formula(outcome ~ covar1 + covar2 + action)

  spec <- staged_workflow() |>
    add_stage_model(stage_1_aipw, stage = 1) |>
    add_stage_model(stage_2_q, stage = 2)

  actions <- levels(sim_data$action)

  # --- Manual fitting ---
  fitted_model_2 <- backwards_estimation_step(
    object = spec,
    data = sim_data,
    k = 2,
    next_stage_model = NULL,
    actions = actions
  )

  fitted_model_1 <- backwards_estimation_step(
    object = spec,
    data = sim_data,
    k = 1,
    next_stage_model = fitted_model_2,
    actions = actions
  )

  # --- Comparison ---
  # The fitted model for stage 1 should be a `fitted_causal_workflow`
  expect_s3_class(fitted_model_1, "fitted_causal_workflow")
  expect_s3_class(fitted_model_2, "workflow")
  expect_true(fitted_model_2$trained)
})