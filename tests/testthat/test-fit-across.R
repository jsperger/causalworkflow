# Setup for tests - reusing the same setup as test-causal-workflow.R
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
  dplyr::mutate(treatment = factor(treatment, levels = c(0, 1), labels = c("control", "treated")))

pscore_wflow <-
  workflows::workflow() |>
  workflows::add_model(parsnip::logistic_reg()) |>
  workflows::add_formula(treatment ~ covar1 + covar2)

outcome_wflow <-
  workflows::workflow() |>
  workflows::add_model(parsnip::linear_reg()) |>
  workflows::add_formula(outcome ~ treatment + covar1 + covar2)

# Tests for fit_across
# ------------------------------------------------------------------------------
test_that("fit_across.causal_workflow works", {
  skip_if_not_installed("parsnip")
  skip_if_not_installed("rsample")
  skip_if_not_installed("dplyr")

  aipw_spec <- causal_workflow() |>
    add_propensity_model(pscore_wflow) |>
    add_outcome_model(outcome_wflow)

  # Test cross-fitted estimation
  fitted_wflow <- fit_across(aipw_spec, data = sim_data)

  expect_s3_class(fitted_wflow, "fitted_causal_workflow")
  expect_true(!is.null(fitted_wflow$estimates))
  expect_true(!is.null(fitted_wflow$variances))
  expect_true(tibble::is_tibble(fitted_wflow$estimates))
  expect_equal(nrow(fitted_wflow$estimates), 2)
  expect_equal(names(fitted_wflow$estimates), c("level", ".pred"))

  # Check EIF object
  expect_true(!is.null(fitted_wflow$eif_pom))
  expect_true(tibble::is_tibble(fitted_wflow$eif_pom))
  expect_equal(nrow(fitted_wflow$eif_pom), nrow(sim_data))
  expect_equal(ncol(fitted_wflow$eif_pom), 2)
  expect_true(all(grepl("eif_pom_", names(fitted_wflow$eif_pom))))

  # Check nuisance predictions (out-of-sample)
  expect_true(!is.null(fitted_wflow$nuisance_predictions))
  expect_true(tibble::is_tibble(fitted_wflow$nuisance_predictions))
  expect_equal(nrow(fitted_wflow$nuisance_predictions), nrow(sim_data))
  expect_true(
    all(c(
      "g_hat_control", "g_hat_treated",
      "q_hat_control", "q_hat_treated"
    ) %in% names(fitted_wflow$nuisance_predictions))
  )

  # Check that predict methods still work
  pred_est <- predict(fitted_wflow, type = "potential_outcome")
  expect_true(tibble::is_tibble(pred_est))
  expect_equal(names(pred_est), c("level", ".pred", ".std_err"))
})

# Test fit_across.staged_workflow placeholder
# ------------------------------------------------------------------------------
test_that("fit_across.staged_workflow placeholder throws an error", {
  lm_wflow <- workflows::workflow() |>
    workflows::add_model(parsnip::linear_reg()) |>
    workflows::add_formula(outcome ~ covar1 + covar2)

  staged_spec <- staged_workflow() |>
    add_stage_model(lm_wflow, stage = 1)

  # Use a simplified version of the staged data
  sim_data_staged <- sim_data |>
    dplyr::mutate(
      stage = 1,
      action = treatment,
      outcome = outcome
    )

  expect_error(
    fit_across(staged_spec, data = sim_data_staged),
    "TMLE fold processing is not yet implemented."
  )
})