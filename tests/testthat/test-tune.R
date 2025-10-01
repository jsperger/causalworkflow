# --- Unit tests for tuning and workflow sets with refactored engine ---

library(parsnip)
library(recipes)
library(workflows)
library(workflowsets)
library(testthat)
library(tune)
library(lifecycle)

# --- Test Data ----------------------------------------------------------------

# Reusing sim_data from other tests
set.seed(123)
sim_data_staged <- data.frame(
  id = rep(1:50, each = 2),
  stage = rep(1:2, 50),
  covar1 = rnorm(100),
  covar2 = rnorm(100),
  action = factor(sample(c("A", "B"), 100, replace = TRUE)),
  outcome = rnorm(100)
)

# --- Test tunable workflows and workflowsets --------------------------------

test_that("A staged_workflow with a tunable tmle_workflow can be fitted", {
  skip_if_not_installed("tune")
  skip_if_not_installed("rsample")
  skip_if_not_installed("glmnet")

  # A tmle_workflow with a tunable model
  glmnet_logit_mod <- parsnip::logistic_reg(penalty = tune(), mixture = 1) |>
    parsnip::set_engine("glmnet")

  stage_1_tmle_tuned <- tmle_workflow() |>
    add_outcome_model(
      workflow() |>
        add_model(linear_reg()) |>
        add_formula(outcome ~ covar1 + action)
    ) |>
    add_propensity_model(
      workflow() |>
        add_model(glmnet_logit_mod) |>
        add_formula(action ~ covar1 + covar2)
    )

  stage_2_q <- workflow() |>
    add_model(linear_reg()) |>
    add_formula(outcome ~ covar1 + action)

  spec <- staged_workflow() |>
    add_stage_model(stage_1_tmle_tuned, stage = 1) |>
    add_stage_model(stage_2_q, stage = 2)

  # fit() should automatically handle the tuning via internal CV
  expect_no_error(
    fitted_spec <- fit(spec, data = sim_data_staged)
  )

  # Check that the propensity model in stage 1 was actually tuned and fitted
  g_fit_s1 <- fitted_spec$models$`1`$propensity_model_fit
  expect_s3_class(g_fit_s1, "model_fit")
  expect_true(g_fit_s1$trained)
})

test_that("A staged_workflow can use a workflow_set", {
  skip_if_not_installed("workflowsets")
  skip_if_not_installed("stacks")
  skip_if_not_installed("rsample")

  # A workflow_set for the outcome model
  outcome_models <- workflow_set(
    preproc = list(
      base = recipe(
        outcome ~ covar1 + action,
        data = sim_data_staged[sim_data_staged$stage == 1, ]
      )
    ),
    models = list(lm = linear_reg(), glmnet = linear_reg(penalty = 0.1))
  )

  stage_1_wfs <- tmle_workflow() |>
    add_outcome_model(outcome_models) |>
    add_propensity_model(
      workflow() |>
        add_model(logistic_reg()) |>
        add_formula(action ~ covar1)
    )

  stage_2_q <- workflow() |>
    add_model(linear_reg()) |>
    add_formula(outcome ~ covar1 + action)

  spec <- staged_workflow() |>
    add_stage_model(stage_1_wfs, stage = 1) |>
    add_stage_model(stage_2_q, stage = 2)

  # fit() should handle the workflow_set by creating a stack
  expect_no_error(
    fitted_spec <- fit(spec, data = sim_data_staged)
  )

  # Check that the outcome model in stage 1 is a fitted stack
  q_fit_s1 <- fitted_spec$models$`1`$outcome_model_fit
  expect_s3_class(q_fit_s1, "model_stack")
  expect_true(!is.null(q_fit_s1$coefs))
})