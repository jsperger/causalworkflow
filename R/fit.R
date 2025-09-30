# NOTE on extensibility: This basic `fit` method is designed for simple,
# non-regularized models where in-sample prediction does not introduce
# significant bias. For models requiring regularization or hyperparameter
# tuning, the cross-fitting (`fit_across()`) or nested tuning (`tune_nested()`)
# methods should be used to ensure robust, unbiased estimation of causal
# effects.

#' @importFrom generics fit
#' @export
generics::fit

#' Fit a causal workflow
#'
#' @description
#' [{.fn fit}] for a [{.cls causal_workflow}] object performs a straightforward,
#' non-cross-fitted estimation of causal effects. It fits the propensity and
#' outcome models on the full dataset and uses in-sample predictions to
#' construct the efficient influence function (EIF) for the potential outcome
#' mean (POM).
#'
#' @details
#' This method is best suited for simple, non-regularized models (e.g.,
#' unpenalized logistic or linear regression) where the risk of overfitting is
#' low. For more complex or regularized models, using in-sample predictions can
#' lead to biased estimates. In those cases, it is strongly recommended to use
#' [{.fn fit_across}] for cross-fitted estimation or [{.fn tune_nested}] for
#' estimation with hyperparameter tuning.
#'
#' The method calculates the EIF for the Potential Outcome Mean (POM) for each
#' treatment level. The POM is the average outcome that would be observed if all
#' individuals in the population received a specific treatment.
#'
#' @param object A [{.cls causal_workflow}] object that has been configured with
#'   a propensity model and an outcome model.
#' @param data A data frame containing the training data, including the
#'   treatment, outcome, and covariate variables.
#' @param ... Not used.
#'
#' @return A `fitted_causal_workflow` object. This object contains:
#'   - `propensity_model_fit`, `outcome_model_fit`: The nuisance models
#'     fitted on the full dataset.
#'   - `treatment_levels`: A character vector of the treatment levels.
#'   - `estimates`: A tibble with the estimated potential outcome for each
#'     treatment level.
#'   - `variances`: A tibble with the variance of the potential outcome
#'     estimator for each level.
#'   - `eif_pom`: A tibble of the observation-level efficient influence function
#'     (EIF) values for the POM, with one column for each treatment level.
#'   - `nuisance_predictions`: A tibble of the in-sample nuisance
#'     predictions.
#'
#' @seealso [{.fn fit_across}], [{.fn tune_nested}]
#' @export
fit.causal_workflow <- function(object, data, ...) {
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

  # 3. Fit models on the full dataset
  g_fit <- parsnip::fit(pscore_wflow, data = data)
  q_fit <- parsnip::fit(outcome_wflow, data = data)

  # 4. Generate in-sample nuisance predictions
  g_preds <- predict(g_fit, new_data = data, type = "prob") |>
    dplyr::rename_with(
      ~ paste0("g_hat_", sub(".pred_", "", .x)),
      .cols = dplyr::starts_with(".pred_")
    )

  q_hat_preds <-
    purrr::map(
      treatment_levels,
      function(lvl) {
        counterfactual_data <- data
        counterfactual_data[[treatment_var]] <- factor(
          lvl,
          levels = treatment_levels
        )
        predict(q_fit, new_data = counterfactual_data) |>
          dplyr::rename(!!paste0("q_hat_", lvl) := .pred)
      }
    ) |>
    dplyr::bind_cols()

  data_with_preds <- dplyr::bind_cols(data, g_preds, q_hat_preds)

  # 5. Calculate EIF for the Potential Outcome Mean (POM) for each level
  Y <- data_with_preds[[outcome_var]]
  A <- data_with_preds[[treatment_var]]

  eif_list <-
    purrr::map(
      treatment_levels,
      function(lvl) {
        g_hat_lvl <- data_with_preds[[paste0("g_hat_", lvl)]]
        q_hat_lvl <- data_with_preds[[paste0("q_hat_", lvl)]]
        indicator <- as.numeric(A == lvl)

        eif_pom <- (indicator / g_hat_lvl) * (Y - q_hat_lvl) + q_hat_lvl
        eif_pom
      }
    )

  names(eif_list) <- paste0("eif_pom_", treatment_levels)
  eif_tibble <- tibble::as_tibble(eif_list)

  # 6. Calculate potential outcome estimates and their variance
  potential_outcomes <- colMeans(eif_tibble, na.rm = TRUE)
  variance_estimates <- apply(eif_tibble, 2, stats::var, na.rm = TRUE) /
    nrow(eif_tibble)

  # 7. Construct return object
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
      propensity_model_fit = g_fit,
      outcome_model_fit = q_fit,
      original_workflows = object,
      treatment_levels = treatment_levels,
      estimates = estimates_tbl,
      variances = variances_tbl,
      eif_pom = eif_tibble,
      nuisance_predictions = data_with_preds
    )

  class(fitted_obj) <- "fitted_causal_workflow"

  return(fitted_obj)
}

.check_fit_inputs <- function(object, data, call = rlang::caller_env()) {
  if (is.null(object$propensity_model)) {
    cli::cli_abort(
      c(
        "The causal workflow is not complete.",
        "x" = "A propensity model has not been added.",
        "i" = "Use {.fn add_propensity_model} to add a propensity model."
      ),
      call = call
    )
  }
  if (is.null(object$outcome_model)) {
    cli::cli_abort(
      c(
        "The causal workflow is not complete.",
        "x" = "An outcome model has not been added.",
        "i" = "Use {.fn add_outcome_model} to add an outcome model."
      ),
      call = call
    )
  }
  if (!is.data.frame(data)) {
    cli::cli_abort(
      c(
        "{.arg data} must be a data frame.",
        "x" = "You've supplied a {.cls {class(data)[[1]]}}."
      ),
      call = call
    )
  }
}
