# --- Unit tests for `staged_workflow` -----------------------------------------

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

# --- Test `staged_workflow` constructor and verbs -----------------------------

test_that("`staged_workflow` constructor works", {
  spec <- staged_workflow()
  expect_s3_class(spec, "staged_workflow")
  expect_equal(length(spec$stages), 0)
  expect_null(spec$exclusions)
})

test_that("`add_stage_model` works correctly", {
  lm_wflow <- workflow() |>
    add_model(linear_reg()) |>
    add_formula(outcome ~ covar1 + action)

  spec <- staged_workflow() |>
    add_stage_model(lm_wflow, stage = 1)

  expect_equal(length(spec$stages), 1)
  expect_s3_class(spec$stages$`1`, "workflow")

  spec <- spec |>
    add_stage_model(lm_wflow, stages = 2:3)

  expect_equal(length(spec$stages), 3)
  expect_s3_class(spec$stages$`2`, "workflow")
  expect_s3_class(spec$stages$`3`, "workflow")
})

test_that("`set_action_exclusions` works correctly", {
  spec <- staged_workflow() |>
    set_action_exclusions(~ action == "A" & covar1 > 0)

  expect_s3_class(spec$exclusions, "formula")
  expect_equal(rlang::f_rhs(spec$exclusions), quote(action == "A" & covar1 > 0))
})


# --- Test `fit.staged_workflow` -----------------------------------------------

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
})

test_that("`fit.staged_workflow` respects one-sided vs. two-sided formulas", {
  # Two-sided formula for stage 1
  wflow_two_sided <- workflow() |>
    add_model(linear_reg()) |>
    add_formula(outcome ~ covar1 + action)

  # One-sided formula for stage 1
  wflow_one_sided <- workflow() |>
    add_model(linear_reg()) |>
    add_formula(~ covar1 + action)

  # Common workflow for stage 2
  wflow_stage_2 <- workflow() |>
    add_model(linear_reg()) |>
    add_formula(outcome ~ covar1 + action)

  spec_two_sided <- staged_workflow() |>
    add_stage_model(wflow_two_sided, stage = 1) |>
    add_stage_model(wflow_stage_2, stage = 2)

  spec_one_sided <- staged_workflow() |>
    add_stage_model(wflow_one_sided, stage = 1) |>
    add_stage_model(wflow_stage_2, stage = 2)

  expect_no_error(fit(spec_two_sided, data = sim_data))
  expect_no_error(fit(spec_one_sided, data = sim_data))
})


# --- Test `predict.fitted_staged_workflow` ------------------------------------

test_that("`predict.fitted_staged_workflow` works for all types", {
  lm_wflow <- workflow() |>
    add_model(linear_reg()) |>
    add_formula(outcome ~ covar1 + covar2 + action)

  spec <- staged_workflow() |>
    add_stage_model(lm_wflow, stages = 1:2)

  fitted_spec <- fit(spec, data = sim_data)

  new_data <- sim_data[sim_data$stage == 1, ]

  pred_action <- predict(fitted_spec, new_data, stage = 1, type = "action")
  expect_s3_class(pred_action, "tbl_df")
  expect_equal(names(pred_action), ".pred_action")
  expect_equal(nrow(pred_action), 10)

  pred_value <- predict(fitted_spec, new_data, stage = 1, type = "value")
  expect_s3_class(pred_value, "tbl_df")
  expect_equal(names(pred_value), ".pred_value")
  expect_equal(nrow(pred_value), 10)
})


# --- Test `multi_predict` -----------------------------------------------------

test_that("`multi_predict.fitted_staged_workflow` works correctly", {
  lm_wflow <- workflow() |>
    add_model(linear_reg()) |>
    add_formula(outcome ~ covar1 + covar2 + action)

  spec <- staged_workflow() |>
    add_stage_model(lm_wflow, stages = 1:2)

  fitted_spec <- fit(spec, data = sim_data)

  initial_data <- sim_data[sim_data$stage == 1, ]

  seq_preds <- multi_predict(fitted_spec, new_data = initial_data)

  expect_s3_class(seq_preds, "tbl_df")
  expect_equal(
    names(seq_preds),
    c(".pred_value_1", ".pred_action_1", ".pred_value_2", ".pred_action_2")
  )
  expect_equal(nrow(seq_preds), 10)
})