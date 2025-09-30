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
#' `fit` for a `causal_workflow` object performs a straightforward,
#' non-cross-fitted estimation of causal effects. It fits the propensity and
#' outcome models on the full dataset and uses in-sample predictions to
#' construct the efficient influence function (EIF) for the potential outcome
#' mean (POM).
#'
#' This method is best suited for simple, non-regularized models (e.g.,
#' unpenalized logistic or linear regression) where the risk of overfitting is
#' low. For more complex or regularized models, using in-sample predictions can
#' lead to biased estimates. In those cases, it is strongly recommended to use
#' [fit_across()] for cross-fitted estimation or [tune_nested()] for
#' estimation with hyperparameter tuning.
#'
#' When called within `fit.staged_workflow()`, this function will be fitted
#' using a pseudo-outcome.
#'
#' @param object A `causal_workflow` object that has been configured with
#'   a propensity model and an outcome model.
#' @param data A data frame containing the training data. When used inside
#'   [fit.staged_workflow()], the response variable is expected to be in a
#'   column named `outcome`.
#' @param ... Not used.
#'
#' @return A `fitted_causal_workflow` object.
#'
#' @seealso [fit_across()], [tune_nested()], [fit.staged_workflow()]
#' @export
fit.causal_workflow <- function(object, data, ...) {
  # 1. Validate inputs
  .check_fit_inputs(object, data)

  # 2. Extract workflows and variable names
  pscore_wflow <- object$propensity_model
  outcome_wflow <- object$outcome_model

  treatment_formula <- hardhat::extract_preprocessor(pscore_wflow)
  treatment_var <- rlang::f_lhs(treatment_formula) |> rlang::as_name()

  # The recursive engine standardizes the response to the `outcome` column.
  # A direct call to `fit` uses the original formula's LHS.
  outcome_formula <- hardhat::extract_preprocessor(outcome_wflow)
  outcome_var <- rlang::f_lhs(outcome_formula) |> rlang::as_name()

  # Ensure treatment is a factor and get levels for counterfactual prediction
  data[[treatment_var]] <- as.factor(data[[treatment_var]])
  treatment_levels <- levels(data[[treatment_var]])

  # 3. Fit models on the full dataset
  g_fit <- parsnip::fit(pscore_wflow, data = data)
  q_fit <- parsnip::fit(outcome_wflow, data = data)

  # 4. Generate in-sample nuisance predictions
  nuisance_preds <- .get_nuisance_preds(
    g_fit = g_fit,
    q_fit = q_fit,
    data = data,
    treatment_var = treatment_var,
    treatment_levels = treatment_levels
  )

  data_with_preds <- dplyr::bind_cols(data, nuisance_preds)

  # 5. Calculate EIF for the Potential Outcome Mean (POM)
  eif_tibble <- .calculate_eif_pom(
    data = data_with_preds,
    treatment_var = treatment_var,
    outcome_var = outcome_var,
    treatment_levels = treatment_levels
  )

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

# --- Internal Helpers for Nuisance Models and EIF ---

#' Get nuisance model predictions
#' @param g_fit A fitted propensity score model.
#' @param q_fit A fitted outcome model.
#' @param data The data to predict on.
#' @param treatment_var A character string of the treatment variable name.
#' @param treatment_levels A character vector of the treatment levels.
#' @return A tibble of nuisance predictions.
#' @keywords internal
.get_nuisance_preds <- function(g_fit, q_fit, data, treatment_var, treatment_levels) {
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
    purrr::list_cbind()

  dplyr::bind_cols(g_preds, q_hat_preds)
}

#' Calculate the EIF for the Potential Outcome Mean (POM)
#' @param data A data frame with outcome, treatment, and nuisance predictions.
#' @param treatment_var A character string of the treatment variable name.
#' @param outcome_var A character string of the outcome variable name.
#' @param treatment_levels A character vector of the treatment levels.
#' @return A tibble of observation-level EIF values.
#' @keywords internal
.calculate_eif_pom <- function(data, treatment_var, outcome_var, treatment_levels) {
  Y <- data[[outcome_var]]
  A <- data[[treatment_var]]

  eif_list <-
    purrr::map(
      treatment_levels,
      function(lvl) {
        g_hat_lvl <- data[[paste0("g_hat_", lvl)]]
        q_hat_lvl <- data[[paste0("q_hat_", lvl)]]
        indicator <- as.numeric(A == lvl)

        # Truncate propensities to avoid instability
        g_hat_lvl[g_hat_lvl < 0.025] <- 0.025

        eif_pom <- (indicator / g_hat_lvl) * (Y - q_hat_lvl) + q_hat_lvl
        eif_pom
      }
    )

  names(eif_list) <- paste0("eif_pom_", treatment_levels)
  tibble::as_tibble(eif_list)
}