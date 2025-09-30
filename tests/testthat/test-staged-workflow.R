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
  expect_s3_class(spec$stages$`1`$wflow, "workflow")
  expect_equal(spec$stages$`1`$type, "single_model")

  spec <- spec |>
    add_stage_model(lm_wflow, stages = 2:3)

  expect_equal(length(spec$stages), 3)
  expect_s3_class(spec$stages$`2`$wflow, "workflow")
  expect_equal(spec$stages$`2`$type, "single_model")
  expect_s3_class(spec$stages$`3`$wflow, "workflow")
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
  expect_equal(names(pred_action), ".pred")
  expect_equal(nrow(pred_action), 10)
  expect_s3_class(pred_action$.pred, "factor")

  pred_value <- predict(fitted_spec, new_data, stage = 1, type = "value")
  expect_s3_class(pred_value, "tbl_df")
  expect_equal(names(pred_value), ".pred")
  expect_equal(nrow(pred_value), 10)
  expect_true(is.numeric(pred_value$.pred))
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

  seq_preds_wrapped <- multi_predict(fitted_spec, new_data = initial_data)

  expect_s3_class(seq_preds_wrapped, "tbl_df")
  expect_equal(names(seq_preds_wrapped), ".pred")
  expect_equal(nrow(seq_preds_wrapped), 10)
  expect_true(is.list(seq_preds_wrapped$.pred))

  seq_preds <- seq_preds_wrapped$.pred[[1]]
  expect_s3_class(seq_preds, "tbl_df")
  expect_equal(names(seq_preds), c(".stage", ".pred_value", ".pred_action"))
  expect_equal(nrow(seq_preds), 2)
})


# --- Test Phase 3: Compositional Workflows ------------------------------------

test_that("`staged_workflow` can fit a compositional `causal_workflow`", {
  # A multi-component (AIPW) spec for stage 1
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

  # A standard Q-learning spec for stage 2
  stage_2_q <- workflow() |>
    add_model(linear_reg()) |>
    add_formula(outcome ~ covar1 + covar2 + action)

  # Create and fit the compositional workflow
  longitudinal_spec <- staged_workflow() |>
    add_stage_model(stage_1_aipw, stage = 1) |>
    add_stage_model(stage_2_q, stage = 2)

  expect_no_error(
    fitted_longitudinal <- fit(longitudinal_spec, data = sim_data)
  )

  # Check the structure of the fitted object
  expect_s3_class(fitted_longitudinal, "fitted_staged_workflow")
  expect_equal(length(fitted_longitudinal$models), 2)
  expect_s3_class(fitted_longitudinal$models$`1`, "fitted_causal_workflow")
  expect_s3_class(fitted_longitudinal$models$`2`, "workflow")
  expect_true(fitted_longitudinal$models$`2`$trained)
  expect_s3_class(fitted_longitudinal$models$`1`$outcome_model, "workflow")

  # Test predict dispatch
  new_data_s1 <- sim_data[sim_data$stage == 1, ]
  new_data_s2 <- sim_data[sim_data$stage == 2, ]

  # Predict from stage 1 (causal_workflow)
  # Test type = "potential_outcome"
  pred_po_s1 <- predict(
    fitted_longitudinal,
    new_data = new_data_s1,
    stage = 1,
    type = "potential_outcome"
  )
  expect_s3_class(pred_po_s1, "tbl_df")
  expect_equal(names(pred_po_s1), ".pred")
  expect_true(is.list(pred_po_s1$.pred))
  expect_equal(nrow(pred_po_s1), nrow(new_data_s1))
  first_po <- pred_po_s1$.pred[[1]]
  expect_true(tibble::is_tibble(first_po))
  expect_equal(names(first_po), c("level", ".pred"))
  expect_equal(nrow(first_po), 2)

  # Test type = "value"
  pred_val_s1 <- predict(
    fitted_longitudinal,
    new_data = new_data_s1,
    stage = 1,
    type = "value"
  )
  expect_s3_class(pred_val_s1, "tbl_df")
  expect_equal(names(pred_val_s1), ".pred")
  expect_true(is.numeric(pred_val_s1$.pred))
  expect_equal(nrow(pred_val_s1), nrow(new_data_s1))

  # Test type = "action"
  pred_act_s1 <- predict(
    fitted_longitudinal,
    new_data = new_data_s1,
    stage = 1,
    type = "action"
  )
  expect_s3_class(pred_act_s1, "tbl_df")
  expect_equal(names(pred_act_s1), ".pred")
  expect_true(is.factor(pred_act_s1$.pred))
  expect_equal(nrow(pred_act_s1), nrow(new_data_s1))


  # Predict from stage 2 (standard workflow) should work
  pred_action_s2 <- predict(
    fitted_longitudinal,
    new_data = new_data_s2,
    stage = 2,
    type = "action"
  )
  expect_s3_class(pred_action_s2, "tbl_df")
  expect_equal(names(pred_action_s2), ".pred")
  expect_s3_class(pred_action_s2$.pred, "factor")

  # Test multi_predict error
  expect_error(
    multi_predict(fitted_longitudinal, new_data = new_data_s1),
    "is only supported for staged workflows where every stage is a standard"
  )
})