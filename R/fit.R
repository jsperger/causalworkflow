# NOTE on extensibility: This basic `fit` method is designed for simple,
# non-regularized models where in-sample prediction does not introduce
# significant bias. For models requiring regularization or hyperparameter
# tuning, the cross-fitting (`fit_resamples()`) or nested tuning (`tune_nested()`)
# methods should be used to ensure robust, unbiased estimation of causal
# effects.

#' @importFrom generics fit
#' @export
generics::fit

#' Fit a causal workflow
#'
#' @description
#' `fit` for a `causal_workflow` object performs a straightforward,
#' non-cross-fitted estimation of causal effects.
#'
#' If the `propensity_model` or `outcome_model` contains tunable hyperparameters
#' (e.g., from `tune()`) or is a `workflow_set`, this function will
#' automatically perform internal cross-validation to tune the models or build
#' an ensemble before fitting the final models on the full dataset.
#'
#' @param object A `causal_workflow` object that has been configured with
#'   a propensity model and an outcome model.
#' @param data A data frame containing the training data.
#' @param ... Not used.
#' @param control A `control_fit` object. Currently unused but reserved for
#'   future enhancements.
#'
#' @return A `fitted_causal_workflow` object.
#'
#' @seealso [tune_nested()], [fit.staged_workflow()]
#' @export
fit.causal_workflow <- function(object, data, ..., control = control_fit()) {
  # 1. Validate inputs
  .check_fit_inputs(object, data)

  # 2. Extract workflows and variable names
  pscore_spec <- object$propensity_model
  outcome_spec <- object$outcome_model

  treatment_formula <- hardhat::extract_preprocessor(pscore_spec)
  treatment_var <- rlang::f_lhs(treatment_formula) |> rlang::as_name()

  outcome_formula <- hardhat::extract_preprocessor(outcome_spec)
  outcome_var <- rlang::f_lhs(outcome_formula) |> rlang::as_name()

  data[[treatment_var]] <- as.factor(data[[treatment_var]])
  treatment_levels <- levels(data[[treatment_var]])

  # 3. Fit models, with internal tuning if necessary
  g_fit <- .fit_nuisance_spec(pscore_spec, resamples = NULL, training_data = data)
  q_fit <- .fit_nuisance_spec(outcome_spec, resamples = NULL, training_data = data)

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
    cli::cli_abort("The causal workflow must have a propensity model.", call = call)
  }
  if (is.null(object$outcome_model)) {
    cli::cli_abort("The causal workflow must have an outcome model.", call = call)
  }
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame.", call = call)
  }
}

# --- Internal Helpers for Nuisance Models and EIF ---
.get_nuisance_preds <- function(g_fit, q_fit, data, treatment_var, treatment_levels) {
  g_preds <- predict(g_fit, new_data = data, type = "prob") |>
    dplyr::rename_with(~ paste0("g_hat_", sub(".pred_", "", .x)), .cols = dplyr::starts_with(".pred_"))

  q_hat_preds <- purrr::map(
    treatment_levels,
    function(lvl) {
      counterfactual_data <- data
      counterfactual_data[[treatment_var]] <- factor(lvl, levels = treatment_levels)
      predict(q_fit, new_data = counterfactual_data) |>
        dplyr::rename(!!paste0("q_hat_", lvl) := .pred)
    }
  ) |> purrr::list_cbind()

  dplyr::bind_cols(g_preds, q_hat_preds)
}

.calculate_eif_pom <- function(data, treatment_var, outcome_var, treatment_levels) {
  Y <- data[[outcome_var]]
  A <- data[[treatment_var]]

  eif_list <- purrr::map(
    treatment_levels,
    function(lvl) {
      g_hat_lvl <- data[[paste0("g_hat_", lvl)]]
      q_hat_lvl <- data[[paste0("q_hat_", lvl)]]
      indicator <- as.numeric(A == lvl)
      g_hat_lvl[g_hat_lvl < 0.025] <- 0.025
      (indicator / g_hat_lvl) * (Y - q_hat_lvl) + q_hat_lvl
    }
  )

  names(eif_list) <- paste0("eif_pom_", treatment_levels)
  tibble::as_tibble(eif_list)
}

.spec_needs_tuning <- function(spec) {
  if (inherits(spec, "workflow_set")) {
    return(TRUE)
  }
  if (inherits(spec, "workflow") && nrow(tune::tunable(spec)) > 0) {
    return(TRUE)
  }
  FALSE
}