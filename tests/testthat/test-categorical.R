# Setup for categorical treatment tests
# ------------------------------------------------------------------------------
# Create sample data with a 3-level treatment
set.seed(12345)
n_cat <- 150
sim_data_cat <- tibble::tibble(
  covar1 = rnorm(n_cat),
  covar2 = rnorm(n_cat),
  treatment = factor(sample(c("a", "b", "c"), n_cat, replace = TRUE)),
  outcome = 10 +
            as.numeric(treatment == "b") * 2 +
            as.numeric(treatment == "c") * 4 +
            5 * covar1 + 3 * covar2 + rnorm(n_cat)
)

# Define component workflows for categorical treatment
pscore_wflow_cat <-
  workflows::workflow() |>
  workflows::add_model(parsnip::multinom_reg() |> parsnip::set_engine("nnet")) |>
  workflows::add_formula(treatment ~ covar1 + covar2)

outcome_wflow_cat <-
  workflows::workflow() |>
  workflows::add_model(parsnip::linear_reg()) |>
  workflows::add_formula(outcome ~ treatment + covar1 + covar2)

# Tests for fit and predict with categorical treatments
# ------------------------------------------------------------------------------
test_that("fit.causal_workflow works with categorical treatments", {
  skip_if_not_installed("parsnip")
  skip_if_not_installed("rsample")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("nnet")

  aipw_spec_cat <- causal_workflow() |>
    add_propensity_model(pscore_wflow_cat) |>
    add_outcome_model(outcome_wflow_cat)

  fitted_wflow_cat <- fit(aipw_spec_cat, data = sim_data_cat)

  expect_s3_class(fitted_wflow_cat, "fitted_causal_workflow")
  expect_equal(fitted_wflow_cat$treatment, "treatment")
  expect_equal(fitted_wflow_cat$treatment_levels, c("a", "b", "c"))
  expect_s3_class(fitted_wflow_cat$eif_pom, "tbl_df")
  expect_equal(ncol(fitted_wflow_cat$eif_pom), 3)
  expect_equal(nrow(fitted_wflow_cat$eif_pom), n_cat)
})

test_that("predict.fitted_causal_workflow works with categorical treatments", {
  skip_if_not_installed("parsnip")
  skip_if_not_installed("rsample")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("nnet")

  aipw_spec_cat <- causal_workflow() |>
    add_propensity_model(pscore_wflow_cat) |>
    add_outcome_model(outcome_wflow_cat)

  fitted_wflow_cat <- fit(aipw_spec_cat, data = sim_data_cat)

  # Test type = "value"
  pred_val <- predict(fitted_wflow_cat, new_data = sim_data_cat, type = "value")
  expect_s3_class(pred_val, "tbl_df")
  expect_equal(names(pred_val), ".pred")
  expect_true(is.numeric(pred_val$.pred))
  expect_equal(nrow(pred_val), n_cat)

  # Test type = "action"
  pred_act <- predict(fitted_wflow_cat, new_data = sim_data_cat, type = "action")
  expect_s3_class(pred_act, "tbl_df")
  expect_equal(names(pred_act), ".pred")
  expect_true(is.factor(pred_act$.pred))
  expect_equal(levels(pred_act$.pred), c("a", "b", "c"))
  expect_equal(nrow(pred_act), n_cat)

  # Test type = "potential_outcome"
  pred_po <- predict(
    fitted_wflow_cat,
    new_data = sim_data_cat,
    type = "potential_outcome"
  )
  expect_s3_class(pred_po, "tbl_df")
  expect_equal(names(pred_po), ".pred")
  expect_true(is.list(pred_po$.pred))
  expect_equal(nrow(pred_po), n_cat)
  # Check nested structure
  first_po <- pred_po$.pred[[1]]
  expect_s3_class(first_po, "tbl_df")
  expect_equal(names(first_po), c("level", ".pred"))
  expect_equal(nrow(first_po), 3)
  expect_equal(first_po$level, c("a", "b", "c"))
})