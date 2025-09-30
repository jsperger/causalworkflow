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
  expect_equal(fitted_wflow_cat$treatment_levels, c("a", "b", "c"))
  expect_s3_class(fitted_wflow_cat$eif_pom, "tbl_df")
  expect_equal(ncol(fitted_wflow_cat$eif_pom), 3)
  expect_equal(nrow(fitted_wflow_cat$eif_pom), n_cat)
})

test_that("predict.fitted_causal_workflow works for type = 'potential_outcome'", {
  skip_if_not_installed("parsnip")
  skip_if_not_installed("rsample")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("nnet")

  aipw_spec_cat <- causal_workflow() |>
    add_propensity_model(pscore_wflow_cat) |>
    add_outcome_model(outcome_wflow_cat)

  fitted_wflow_cat <- fit(aipw_spec_cat, data = sim_data_cat)
  pred_po_wrapped <- predict(fitted_wflow_cat, type = "potential_outcome")

  expect_s3_class(pred_po_wrapped, "tbl_df")
  expect_equal(names(pred_po_wrapped), ".pred")
  expect_true(is.list(pred_po_wrapped$.pred))

  pred_po <- pred_po_wrapped$.pred[[1]]
  expect_s3_class(pred_po, "tbl_df")
  expect_equal(names(pred_po), c("level", ".pred", ".std_err"))
  expect_equal(nrow(pred_po), 3)
  expect_equal(pred_po$level, c("a", "b", "c"))
})

test_that("predict.fitted_causal_workflow works for type = 'blip_ref'", {
  skip_if_not_installed("parsnip")
  skip_if_not_installed("rsample")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("nnet")

  aipw_spec_cat <- causal_workflow() |>
    add_propensity_model(pscore_wflow_cat) |>
    add_outcome_model(outcome_wflow_cat)

  fitted_wflow_cat <- fit(aipw_spec_cat, data = sim_data_cat)
  pred_blip_ref_wrapped <- predict(fitted_wflow_cat, type = "blip_ref", ref_level = "a")

  expect_s3_class(pred_blip_ref_wrapped, "tbl_df")
  expect_equal(names(pred_blip_ref_wrapped), ".pred")
  expect_true(is.list(pred_blip_ref_wrapped$.pred))

  pred_blip_ref <- pred_blip_ref_wrapped$.pred[[1]]
  expect_s3_class(pred_blip_ref, "tbl_df")
  expect_equal(names(pred_blip_ref), c("level", ".pred", ".std_err"))
  expect_equal(nrow(pred_blip_ref), 2)
  expect_equal(pred_blip_ref$level, c("b", "c"))
})

test_that("predict.fitted_causal_workflow works for type = 'blip_avg'", {
  skip_if_not_installed("parsnip")
  skip_if_not_installed("rsample")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("nnet")

  aipw_spec_cat <- causal_workflow() |>
    add_propensity_model(pscore_wflow_cat) |>
    add_outcome_model(outcome_wflow_cat)

  fitted_wflow_cat <- fit(aipw_spec_cat, data = sim_data_cat)
  pred_blip_avg_wrapped <- predict(fitted_wflow_cat, type = "blip_avg")

  expect_s3_class(pred_blip_avg_wrapped, "tbl_df")
  expect_equal(names(pred_blip_avg_wrapped), ".pred")
  expect_true(is.list(pred_blip_avg_wrapped$.pred))

  pred_blip_avg <- pred_blip_avg_wrapped$.pred[[1]]
  expect_s3_class(pred_blip_avg, "tbl_df")
  expect_equal(names(pred_blip_avg), c("level", ".pred", ".std_err"))
  expect_equal(nrow(pred_blip_avg), 3)
})

test_that("predict with blip_ref errors correctly", {
  skip_if_not_installed("parsnip")
  skip_if_not_installed("rsample")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("nnet")

  aipw_spec_cat <- causal_workflow() |>
    add_propensity_model(pscore_wflow_cat) |>
    add_outcome_model(outcome_wflow_cat)

  fitted_wflow_cat <- fit(aipw_spec_cat, data = sim_data_cat)

  expect_error(predict(fitted_wflow_cat, type = "blip_ref"))
  expect_error(predict(fitted_wflow_cat, type = "blip_ref", ref_level = "d"))
})