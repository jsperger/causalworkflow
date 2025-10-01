#' Initialize a TMLE Workflow
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' The `tmle_workflow()` function initializes a `tmle_workflow` object.
#' This object is a specialized type of `causal_workflow` that signals to the
#' fitting engine that TMLE-specific steps (e.g., model targeting or
#' fluctuation) are required.
#'
#' Like a `causal_workflow`, it serves as a container for the propensity and
#' outcome models, which are added with [add_propensity_model()] and
#' [add_outcome_model()].
#'
#' @inheritParams causal_workflow
#' @return A `tmle_workflow` object, which also inherits from `causal_workflow`.
#' @export
tmle_workflow <- function(...) {
  check_empty_ellipses(...)

  wflow <-
    structure(
      list(
        propensity_model = NULL,
        outcome_model = NULL
      ),
      class = c("tmle_workflow", "causal_workflow")
    )

  if (causal_workflow_constr(wflow)) {
    wflow
  }
}

#' @export
add_propensity_model.tmle_workflow <- function(x, spec) {
  check_causal_workflow(x)
  check_spec(spec)

  x$propensity_model <- spec

  if (causal_workflow_constr(x)) {
    x
  }
}

#' @export
add_outcome_model.tmle_workflow <- function(x, spec) {
  check_causal_workflow(x)
  check_spec(spec)

  x$outcome_model <- spec

  if (causal_workflow_constr(x)) {
    x
  }
}

#' @export
fit.tmle_workflow <- function(object, data, ...) {
  # 1. Fit initial nuisance models, creating resamples if needed for tuning
  g_spec <- object$propensity_model
  q_spec <- object$outcome_model

  g_resamples <- if (.spec_needs_tuning(g_spec)) rsample::vfold_cv(data) else NULL
  q_resamples <- if (.spec_needs_tuning(q_spec)) rsample::vfold_cv(data) else NULL

  g_fit <- .fit_nuisance_spec(g_spec, resamples = g_resamples, training_data = data)
  q_fit <- .fit_nuisance_spec(q_spec, resamples = q_resamples, training_data = data)

  # 2. Extract components and generate initial predictions
  treatment_var <- .extract_var_name(object$propensity_model)
  treatment_levels <- levels(data[[treatment_var]])
  outcome_var <- "outcome" # Standardized by recursive engine

  initial_preds <- .get_nuisance_preds(g_fit, q_fit, data, treatment_var, treatment_levels)
  nuisance_preds <- dplyr::bind_cols(data, initial_preds)

  # 3. Determine optimal policy `d_k(H_k)` based on initial Q_k_hat
  q_hat_cols <- dplyr::select(nuisance_preds, dplyr::starts_with("q_hat_"))
  optimal_action <- colnames(q_hat_cols)[apply(q_hat_cols, 1, which.max)] |>
    sub("q_hat_", "", x = _)

  # 4. Calculate the clever covariate `C_k`
  g_hat_observed <- .get_observed_prob(
    data = nuisance_preds,
    model_prefix = "g_hat",
    observed_actions = data[[treatment_var]]
  ) |> pmax(0.025)

  clever_cov <- as.numeric(data[[treatment_var]] == optimal_action) / g_hat_observed

  # 5. Fit the fluctuation model to find `epsilon_k`
  q_hat_observed <- .get_observed_prob(
    data = nuisance_preds,
    model_prefix = "q_hat",
    observed_actions = data[[treatment_var]]
  )

  fluctuation_data <- tibble::tibble(
    outcome = data[[outcome_var]],
    q_hat_observed = q_hat_observed,
    clever_cov = clever_cov
  )

  epsilon <- stats::coef(stats::glm(
    outcome ~ -1 + offset(q_hat_observed) + clever_cov,
    data = fluctuation_data,
    family = "gaussian"
  ))
  if (is.na(epsilon)) epsilon <- 0

  # 6. Create targeted predictions: Q_k_star = Q_k_hat + epsilon_k * C_k
  q_star_cols <- tibble::as_tibble(q_hat_cols + (epsilon * clever_cov))
  colnames(q_star_cols) <- sub("q_hat", "q_star", colnames(q_hat_cols))

  # 7. Construct the final `fitted_tmle_workflow` object
  res <- list(
    propensity_model_fit = g_fit,
    outcome_model_fit = q_fit,
    original_workflows = object,
    treatment_levels = treatment_levels,
    epsilon = epsilon,
    targeted_predictions = q_star_cols,
    nuisance_predictions = dplyr::bind_cols(nuisance_preds, q_star_cols)
  )
  class(res) <- c("fitted_tmle_workflow", "fitted_causal_workflow")
  return(res)
}

#' Helper to extract the predicted probability for the observed action
#' @keywords internal
.get_observed_prob <- function(data, model_prefix, observed_actions) {
  pred_cols <- dplyr::select(data, dplyr::starts_with(model_prefix))
  action_levels <- sub(paste0(model_prefix, "_"), "", colnames(pred_cols))
  as.matrix(pred_cols)[cbind(seq_len(nrow(data)), match(observed_actions, action_levels))]
}