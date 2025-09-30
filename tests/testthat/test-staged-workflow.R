# --- Unit tests for refactored staged_workflow fitting logic ----------------

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

# --- Test `fit.staged_workflow` with new recursive engine ---------------------

test_that("`fit.staged_workflow` runs and returns the correct structure", {
  lm_wflow <- workflow() |>
    add_model(linear_reg()) |>
    add_formula(outcome ~ covar1 + covar2 + action)

  spec <- staged_workflow() |>
    add_stage_model(lm_wflow, stages = 1:2)

  fitted_spec <- fit(spec, data = sim_data)

  expect_s3_class(fitted_spec, "fitted_staged_workflow")
  expect_equal(length(fitted_spec$models), 2)
  expect_s3_class(fitted_spec$models$`1`, "workflow")
  expect_true(fitted_spec$models$`1`$trained)
  expect_true(fitted_spec$models$`2`$trained)
})

# --- Test `fit_next_stage` and resumable fitting ------------------------------

test_that("`fit_next_stage` and resumable fits work correctly", {
  # A 3-stage dataset
  set.seed(456)
  sim_data_3_stage <- data.frame(
    id = rep(1:10, each = 3),
    stage = rep(1:3, 10),
    covar1 = rnorm(30),
    covar2 = rnorm(30),
    action = factor(sample(c("A", "B"), 30, replace = TRUE)),
    outcome = rnorm(30)
  )

  lm_wflow <- workflow() |>
    add_model(linear_reg()) |>
    add_formula(outcome ~ covar1 + covar2 + action)

  spec <- staged_workflow() |>
    add_stage_model(lm_wflow, stages = 1:3) # A 3-stage workflow

  # 1. Fit only the last stage (stage 3)
  fitted_stage_3 <- fit_next_stage(spec, data = sim_data_3_stage)
  expect_s3_class(fitted_stage_3, "fitted_staged_workflow")
  expect_equal(length(fitted_stage_3$models), 1)
  expect_true("3" %in% names(fitted_stage_3$models))
  expect_false("2" %in% names(fitted_stage_3$models))

  # 2. Fit the next stage (stage 2)
  fitted_stage_3_2 <- fit_next_stage(fitted_stage_3, data = sim_data_3_stage)
  expect_equal(length(fitted_stage_3_2$models), 2)
  expect_true(all(c("3", "2") %in% names(fitted_stage_3_2$models)))
  expect_false("1" %in% names(fitted_stage_3_2$models))

  # 3. Resume the fit from the partially-fitted object to complete it
  fitted_all <- fit(fitted_stage_3_2, data = sim_data_3_stage)
  expect_equal(length(fitted_all$models), 3)
  expect_true(all(c("3", "2", "1") %in% names(fitted_all$models)))
  expect_true(fitted_all$models$`1`$trained)
})

# --- Test fitting with `causal_workflow` and `tmle_workflow` stages ---------

test_that("`staged_workflow` can fit `causal_workflow` and `tmle_workflow`", {
  # A tmle_workflow spec for stage 1
  stage_1_tmle <- tmle_workflow() |>
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

  # A standard Q-learning spec for stage 2
  stage_2_q <- workflow() |>
    add_model(linear_reg()) |>
    add_formula(outcome ~ covar1 + covar2 + action)

  # Create and fit the compositional workflow
  longitudinal_spec <- staged_workflow() |>
    add_stage_model(stage_1_tmle, stage = 1) |>
    add_stage_model(stage_2_q, stage = 2)

  fitted_longitudinal <- fit(longitudinal_spec, data = sim_data)

  # Check the structure of the fitted object
  expect_s3_class(fitted_longitudinal, "fitted_staged_workflow")
  expect_equal(length(fitted_longitudinal$models), 2)
  expect_s3_class(fitted_longitudinal$models$`1`, "fitted_tmle_workflow")
  expect_s3_class(fitted_longitudinal$models$`2`, "workflow")
  expect_true(!is.null(fitted_longitudinal$models$`1`$epsilon))
  expect_true("tbl_df" %in% class(fitted_longitudinal$models$`1`$targeted_predictions))
})

test_that("TMLE fit aborts for > 2 stages", {
  stage_1_tmle <- tmle_workflow() |>
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

  three_stage_spec <- staged_workflow() |>
    add_stage_model(stage_1_tmle, stage = 1) |>
    add_stage_model(stage_2_q, stage = 2) |>
    add_stage_model(stage_2_q, stage = 3)

  expect_error(
    fit(three_stage_spec, data = sim_data),
    "CV-TMLE for more than 2 stages is not yet supported."
  )
})