# NOTE on extensibility: The current `fit.causal_workflow` implementation is
# specialized for a binary treatment and the AIPW estimator. Future extensions
# should aim to generalize this logic. For example, handling a multi-level
# categorical treatment would involve iterating over treatment levels to compute
# pairwise ATEs or other contrasts. This would likely require modifications to
# the counterfactual prediction loop and the final EIF calculation. Similarly,
# supporting other estimators would require replacing the EIF calculation with
# the appropriate influence function. The overall cross-fitting structure,
# however, should remain a valid foundation for these extensions.

#' @importFrom generics fit
#' @export
generics::fit

#' Fit a causal workflow
#'
#' @description
#' `fit()` for a `causal_workflow` object performs a cross-fitted estimation
#' of causal effects. It supports both binary and categorical treatments.
#' It uses out-of-sample predictions for the nuisance models (propensity and
#' outcome models) to construct the efficient influence function for each
#' treatment level, providing robust estimates of potential outcomes.
#'
#' @param object A `causal_workflow` object that has been configured with
#'   a propensity model and an outcome model.
#' @param data A data frame containing the training data, including the
#'   treatment, outcome, and covariate variables.
#' @param ... Not used.
#'
#' @return A `fitted_causal_workflow` object. This object contains:
#'   - `propensity_model_fit`, `outcome_model_fit`: The final nuisance models
#'     fitted on the full dataset.
#'   - `treatment_levels`: A character vector of the treatment levels.
#'   - `estimates`: A tibble with the estimated potential outcome for each
#'     treatment level.
#'   - `variances`: A tibble with the variance of the potential outcome
#'     estimator for each level.
#'   - `eif`: A tibble of the observation-level efficient influence function
#'     (EIF) values, with one column for each treatment level.
#'   - `nuisance_predictions`: A tibble of the out-of-sample nuisance
#'     predictions from the cross-fitting procedure.
#'
#' @export
fit.causal_workflow <- function(object, data, ...) {
  # 1. Validate inputs
  .check_fit_inputs(object, data)

  # 2. Extract workflows and variable names
  pscore_wflow <- object$propensity_model
  outcome_wflow <- object$outcome_model

  treatment_formula <- pscore_wflow$pre$actions$formula$formula
  treatment_var <- rlang::f_lhs(treatment_formula) |> rlang::as_name()

  outcome_formula <- outcome_wflow$pre$actions$formula$formula
  outcome_var <- rlang::f_lhs(outcome_formula) |> rlang::as_name()

  # Ensure treatment is a factor and get levels for counterfactual prediction
  data[[treatment_var]] <- as.factor(data[[treatment_var]])
  treatment_levels <- levels(data[[treatment_var]])

  # 3. Set up cross-fitting
  data_with_row <- data |> dplyr::mutate(.row = dplyr::row_number())
  folds <- rsample::vfold_cv(data_with_row)

  # 4. Perform cross-fitting to get nuisance predictions
  nuisance_preds <-
    purrr::map_dfr(
      folds$splits,
      ~ .fit_predict_one_fold(
          .x,
          pscore_wflow = pscore_wflow,
          outcome_wflow = outcome_wflow,
          treatment_var = treatment_var,
          treatment_levels = treatment_levels
        )
    )

  # 5. Join predictions back to original data
  data_with_preds <-
    dplyr::left_join(data_with_row, nuisance_preds, by = ".row")

  # 6. Calculate EIF for each treatment level
  Y <- data_with_preds[[outcome_var]]
  A <- data_with_preds[[treatment_var]]

  eif_list <-
    purrr::map(
      treatment_levels,
      function(lvl) {
        g_hat_lvl <- data_with_preds[[paste0("g_hat_", lvl)]]
        q_hat_lvl <- data_with_preds[[paste0("q_hat_", lvl)]]
        indicator <- as.numeric(A == lvl)

        eif <- (indicator / g_hat_lvl) * (Y - q_hat_lvl) + q_hat_lvl
        eif
      }
    )

  names(eif_list) <- paste0("eif_", treatment_levels)
  eif_tibble <- tibble::as_tibble(eif_list)

  # 7. Calculate potential outcome estimates and their variance
  potential_outcomes <- colMeans(eif_tibble, na.rm = TRUE)
  variance_estimates <- apply(eif_tibble, 2, stats::var, na.rm = TRUE) / colSums(!is.na(eif_tibble))

  # 8. Fit final models on full data
  final_g_fit <- parsnip::fit(pscore_wflow, data = data)
  final_q_fit <- parsnip::fit(outcome_wflow, data = data)

  # 9. Construct return object
  estimates_tbl <- tibble::enframe(potential_outcomes, name = "level", value = ".pred") |>
    dplyr::mutate(level = sub("eif_", "", level))

  variances_tbl <- tibble::enframe(variance_estimates, name = "level", value = ".variance") |>
    dplyr::mutate(level = sub("eif_", "", level))

  fitted_obj <-
    list(
      propensity_model_fit = final_g_fit,
      outcome_model_fit = final_q_fit,
      original_workflows = object,
      treatment_levels = treatment_levels,
      estimates = estimates_tbl,
      variances = variances_tbl,
      eif = eif_tibble,
      nuisance_predictions = data_with_preds |> dplyr::select(-.row)
    )

  class(fitted_obj) <- "fitted_causal_workflow"

  return(fitted_obj)
}

.check_fit_inputs <- function(object, data) {
  if (is.null(object$propensity_model) || is.null(object$outcome_model)) {
    rlang::abort("Both a propensity model and an outcome model must be added to the workflow.")
  }
  if (!is.data.frame(data)) {
    rlang::abort("`data` must be a data frame.")
  }
}

# Helper function to process one fold of the cross-fitting procedure
.fit_predict_one_fold <- function(split, pscore_wflow, outcome_wflow, treatment_var, treatment_levels) {
  analysis_data <- rsample::analysis(split)
  assessment_data <- rsample::assessment(split)

  g_fit <- parsnip::fit(pscore_wflow, data = analysis_data)
  q_fit <- parsnip::fit(outcome_wflow, data = analysis_data)

  # Get propensity scores for the assessment set and rename them
  g_preds <- predict(g_fit, new_data = assessment_data, type = "prob") |>
    dplyr::rename_with(~ paste0("g_hat_", sub(".pred_", "", .x)), .cols = dplyr::starts_with(".pred_"))

  # Get potential outcome predictions for each treatment level
  q_hat_preds <-
    purrr::map(
      treatment_levels,
      function(lvl) {
        counterfactual_data <- assessment_data
        counterfactual_data[[treatment_var]] <- factor(lvl, levels = treatment_levels)
        predict(q_fit, new_data = counterfactual_data) |>
          dplyr::rename(!!paste0("q_hat_", lvl) := .pred)
      }
    ) |>
    dplyr::bind_cols()

  result <-
    tibble::tibble(.row = assessment_data$.row) |>
    dplyr::bind_cols(g_preds) |>
    dplyr::bind_cols(q_hat_preds)

  return(result)
}