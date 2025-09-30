# NOTE on extensibility: The current `fit_across.causal_workflow` implementation is
# specialized for a binary treatment and the AIPW estimator. Future extensions
# should aim to generalize this logic. For example, handling a multi-level
# categorical treatment would involve iterating over treatment levels to compute
# pairwise ATEs or other contrasts. This would likely require modifications to
# the counterfactual prediction loop and the final EIF calculation. Similarly,
# supporting other estimators would require replacing the EIF calculation with
# the appropriate influence function. The overall cross-fitting structure,
# however, should remain a valid foundation for these extensions.

#' Fit a causal workflow with cross-fitting
#'
#' @description
#' [fit_across()] for a `causal_workflow` object performs a cross-fitted estimation
#' of causal effects. It uses out-of-sample predictions for the nuisance models
#' (propensity and outcome models) to construct the efficient influence
#' function (EIF) for the potential outcome mean (POM), providing robust
#' estimates of potential outcomes.
#'
#' @details
#' Cross-fitting (also known as cross-estimation) is a sample-splitting
#' technique that mitigates the bias that can arise from using the same data to
#' fit and predict with a model, which is particularly important when using
#' flexible or machine learning-based nuisance models. The data is split into
#' K folds. For each fold, the propensity and outcome models are fitted on the
#' other K-1 folds (the analysis set) and then used to generate predictions for
#' the held-out fold (the assessment set). This results in a full set of
#' out-of-sample predictions for all observations in the original dataset.
#'
#' These out-of-sample predictions are then used to calculate the EIF for the
#' Potential Outcome Mean (POM) for each treatment level, leading to more
#' robust and less biased estimates of the causal effect.
#'
#' @param object A `causal_workflow` object that has been configured with
#'   a propensity model and an outcome model.
#' @param data A data frame containing the training data, including the
#'   treatment, outcome, and covariate variables.
#' @param ... Not used.
#'
#' @return A `fitted_causal_workflow` object.
#'
#' @seealso [fit.causal_workflow()], [tune_nested()]
#' @export
fit_across <- function(object, ...) {
  UseMethod("fit_across")
}

#' @export
fit_across.causal_workflow <- function(object, data, ...) {
  # 1. Validate inputs
  .check_fit_inputs(object, data)

  # 2. Extract workflows and variable names
  pscore_wflow <- object$propensity_model
  outcome_wflow <- object$outcome_model

  treatment_formula <- hardhat::extract_preprocessor(pscore_wflow)
  treatment_var <- rlang::f_lhs(treatment_formula) |> rlang::as_name()

  outcome_formula <- hardhat::extract_preprocessor(outcome_wflow)
  outcome_var <- rlang::f_lhs(outcome_formula) |> rlang::as_name()

  # Ensure treatment is a factor and get levels for counterfactual prediction
  data[[treatment_var]] <- as.factor(data[[treatment_var]])
  treatment_levels <- levels(data[[treatment_var]])

  # 3. Set up cross-fitting
  data_with_row <- data |> dplyr::mutate(.row = dplyr::row_number())
  # Use a fixed number of folds to avoid LOO-CV issues with small N.
  n_folds <- min(5, nrow(data_with_row))
  folds <- rsample::vfold_cv(data_with_row, v = n_folds)

  # 4. Perform cross-fitting to get out-of-sample nuisance predictions
  nuisance_preds <-
    purrr::map(
      folds$splits,
      ~ .fit_predict_one_fold(
        .x,
        pscore_wflow = pscore_wflow,
        outcome_wflow = outcome_wflow,
        treatment_var = treatment_var,
        treatment_levels = treatment_levels
      )
    ) |>
    purrr::list_rbind()

  # 5. Join predictions back to original data
  data_with_preds <-
    dplyr::left_join(data_with_row, nuisance_preds, by = ".row")

  # 6. Calculate EIF for the Potential Outcome Mean (POM)
  eif_tibble <- .calculate_eif_pom(
    data = data_with_preds,
    treatment_var = treatment_var,
    outcome_var = outcome_var,
    treatment_levels = treatment_levels
  )

  # 7. Calculate potential outcome estimates and their variance
  potential_outcomes <- colMeans(eif_tibble, na.rm = TRUE)
  variance_estimates <- apply(eif_tibble, 2, stats::var, na.rm = TRUE) /
    colSums(!is.na(eif_tibble))

  # 8. Fit final models on full data
  final_g_fit <- parsnip::fit(pscore_wflow, data = data)
  final_q_fit <- parsnip::fit(outcome_wflow, data = data)

  # 9. Construct return object
  estimates_tbl <- tibble::enframe(
    potential_outcomes,
    name = "level",
    value = ".pred"
  ) |>
    dplyr::mutate(level = sub("eif_pom_", "", level))

  variances_tbl <- tibble::enframe(
    variance_estimates,
    name = "level",
    value = ".variance"
  ) |>
    dplyr::mutate(level = sub("eif_pom_", "", level))

  fitted_obj <-
    list(
      propensity_model_fit = final_g_fit,
      outcome_model_fit = final_q_fit,
      original_workflows = object,
      treatment_levels = treatment_levels,
      estimates = estimates_tbl,
      variances = variances_tbl,
      eif_pom = eif_tibble,
      nuisance_predictions = data_with_preds |> dplyr::select(-.row)
    )

  class(fitted_obj) <- "fitted_causal_workflow"

  return(fitted_obj)
}

# Helper function to process one fold of the cross-fitting procedure
.fit_predict_one_fold <- function(
  split,
  pscore_wflow,
  outcome_wflow,
  treatment_var,
  treatment_levels
) {
  analysis_data <- rsample::analysis(split)
  assessment_data <- rsample::assessment(split)

  g_fit <- parsnip::fit(pscore_wflow, data = analysis_data)
  q_fit <- parsnip::fit(outcome_wflow, data = analysis_data)

  nuisance_preds <- .get_nuisance_preds(
    g_fit = g_fit,
    q_fit = q_fit,
    data = assessment_data,
    treatment_var = treatment_var,
    treatment_levels = treatment_levels
  )

  result <-
    tibble::tibble(.row = assessment_data$.row) |>
    dplyr::bind_cols(nuisance_preds)

  return(result)
}

#' @export
fit_across.staged_workflow <- function(object, data, ...) {
  # 1. Validate inputs
  # .check_staged_fit_inputs(object, data, discount) # Need to decide how to handle discount here

  # 2. Set up cross-fitting folds
  data_with_row <- data |> dplyr::mutate(.row = dplyr::row_number())
  n_folds <- min(5, nrow(data_with_row))
  folds <- rsample::vfold_cv(data_with_row, v = n_folds)

  # 3. Perform cross-fitting to get out-of-sample targeted predictions
  #    and influence curve components.
  #    This will involve calling fit_recursive on the analysis set and
  #    a new predict method on the assessment set.
  fold_results <- purrr::map(
    folds$splits,
    ~ .fit_predict_tmle_fold(.x, object = object)
  ) # |> purrr::list_rbind()

  # 4. Pool results and compute final estimate and variance.
  #    This logic will be extracted from the current cv_tmle function.

  cli::cli_abort("`fit_across.staged_workflow` is not yet implemented.")
}

.fit_predict_tmle_fold <- function(split, object) {
  analysis_data <- rsample::analysis(split)
  assessment_data <- rsample::assessment(split)

  # Fit the full staged workflow on the analysis data
  # The recursive engine needs to be enhanced to handle TMLE specifics
  fitted_staged_wflow <- fit(object, data = analysis_data)

  # Predict on the assessment data to get targeted outcomes and IC components
  # A new predict method for fitted_staged_workflow will be needed.
  # predict(fitted_staged_wflow, new_data = assessment_data)

  cli::cli_abort("TMLE fold processing is not yet implemented.")
}