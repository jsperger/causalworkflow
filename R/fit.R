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
#' of the specified causal effect (e.g., ATE). It uses out-of-sample
#' predictions for the nuisance models (propensity and outcome models) to
#' construct the efficient influence function, providing a robust estimate.
#'
#' @param object A `causal_workflow` object that has been configured with
#'   a propensity model and an outcome model.
#' @param data A data frame containing the training data, including the
#'   treatment, outcome, and covariate variables.
#' @param ... Not used.
#'
#' @return A `fitted_causal_workflow` object. This object contains the
#'   fitted nuisance models (trained on the full dataset), the final point
#'   estimate and its variance, the observation-level efficient influence
#'   function values, and the out-of-sample nuisance predictions.
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

  if (length(treatment_levels) != 2) {
    rlang::abort("The treatment variable must have exactly two levels.")
  }

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

  # 6. Calculate EIF
  Y <- data_with_preds[[outcome_var]]
  A <- as.numeric(data_with_preds[[treatment_var]] == treatment_levels[2])
  g_hat <- data_with_preds$g_hat
  q1_hat <- data_with_preds$q1_hat
  q0_hat <- data_with_preds$q0_hat

  # AIPW EIF calculation
  term1 <- q1_hat - q0_hat
  term2 <- (A / g_hat) * (Y - q1_hat)
  term3 <- ((1 - A) / (1 - g_hat)) * (Y - q0_hat)

  eif_values <- term1 + term2 - term3

  # 7. Calculate final estimate and variance
  if (any(is.na(eif_values))) {
    rlang::warn(
      paste(
        "NA values were found in the efficient influence function.",
        "These are often caused by near-zero propensity scores and have been",
        "removed from the final estimate."
      )
    )
  }
  ate_estimate <- mean(eif_values, na.rm = TRUE)
  ate_variance <- stats::var(eif_values, na.rm = TRUE) / sum(!is.na(eif_values))

  # 8. Fit final models on full data
  final_g_fit <- parsnip::fit(pscore_wflow, data = data)
  final_q_fit <- parsnip::fit(outcome_wflow, data = data)

  # 9. Construct return object
  fitted_obj <-
    list(
      propensity_model_fit = final_g_fit,
      outcome_model_fit = final_q_fit,
      original_workflows = object,
      estimate = ate_estimate,
      variance = ate_variance,
      eif = eif_values,
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

  # Get propensity scores for the assessment set
  g_preds <- predict(g_fit, new_data = assessment_data, type = "prob")

  # Create counterfactual datasets for outcome prediction
  assessment_data_a1 <- assessment_data
  assessment_data_a1[[treatment_var]] <- factor(treatment_levels[2], levels = treatment_levels)

  assessment_data_a0 <- assessment_data
  assessment_data_a0[[treatment_var]] <- factor(treatment_levels[1], levels = treatment_levels)

  # Get counterfactual outcome predictions
  q1_preds <- predict(q_fit, new_data = assessment_data_a1)
  q0_preds <- predict(q_fit, new_data = assessment_data_a0)

  # Propensity score for the treated level (A=1)
  pscore_col <- paste0(".pred_", treatment_levels[2])

  tibble::tibble(
    .row = assessment_data$.row,
    g_hat = g_preds[[pscore_col]],
    q1_hat = q1_preds$.pred,
    q0_hat = q0_preds$.pred
  )
}