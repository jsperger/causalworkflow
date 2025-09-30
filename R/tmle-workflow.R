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
fit.tmle_workflow <- function(object, data, ...) {
  # 1. Perform initial fit using the causal_workflow method
  # This gives us g-hat and Q-hat for the current stage `k`.
  initial_fit <- fit.causal_workflow(object, data, ...)

  # 2. Extract necessary components from initial fit
  nuisance_preds <- initial_fit$nuisance_predictions
  treatment_var <- rlang::f_lhs(hardhat::extract_preprocessor(object$propensity_model)) |>
    rlang::as_name()

  # The recursive engine standardizes the response to `outcome`.
  outcome_var <- "outcome"

  # 3. Determine optimal policy `d_k(H_k)` based on initial Q_k_hat
  q_hat_cols <- dplyr::select(nuisance_preds, dplyr::starts_with("q_hat_"))
  optimal_action <- colnames(q_hat_cols)[apply(q_hat_cols, 1, which.max)] |>
    sub("q_hat_", "", x = _)

  # 4. Calculate the clever covariate `C_k`
  g_hat_observed <- .get_observed_prob(
    data = nuisance_preds,
    model_prefix = "g_hat",
    action_col = treatment_var,
    observed_actions = data[[treatment_var]]
  ) |> pmax(0.025) # Truncate for stability

  clever_cov <- as.numeric(data[[treatment_var]] == optimal_action) / g_hat_observed

  # 5. Fit the fluctuation model to find `epsilon_k`
  q_hat_observed <- .get_observed_prob(
    data = nuisance_preds,
    model_prefix = "q_hat",
    action_col = treatment_var,
    observed_actions = data[[treatment_var]]
  )

  fluctuation_data <- tibble::tibble(
    outcome = data[[outcome_var]],
    q_hat_observed = q_hat_observed,
    clever_cov = clever_cov
  )

  # The fluctuation is a simple regression on the clever covariate with
  # the initial Q-hat as an offset.
  epsilon <- stats::coef(stats::glm(
    outcome ~ -1 + offset(q_hat_observed) + clever_cov,
    data = fluctuation_data,
    family = "gaussian"
  ))

  # If clever_cov is singular (e.g., all zeros), epsilon will be NA.
  # In this case, no update is possible, so epsilon is effectively 0.
  if (is.na(epsilon)) {
    epsilon <- 0
  }

  # 6. Create targeted predictions: Q_k_star = Q_k_hat + epsilon_k * C_k
  q_star_cols <- tibble::as_tibble(q_hat_cols + (epsilon * clever_cov))
  colnames(q_star_cols) <- sub("q_hat", "q_star", colnames(q_hat_cols))

  # 7. Construct the final `fitted_tmle_workflow` object
  res <- c(
    initial_fit,
    list(
      epsilon = epsilon,
      targeted_predictions = q_star_cols
    )
  )

  # Replace nuisance_predictions with a more complete tibble
  res$nuisance_predictions <- dplyr::bind_cols(
    initial_fit$nuisance_predictions,
    q_star_cols
  )

  class(res) <- c("fitted_tmle_workflow", class(initial_fit))

  return(res)
}

#' Helper to extract the predicted probability for the observed action
#' @param data A tibble of predictions.
#' @param model_prefix The prefix for the prediction columns (e.g., "g_hat").
#' @param action_col The name of the column with the observed actions.
#' @param observed_actions A vector of the observed actions.
#' @return A numeric vector of probabilities.
#' @keywords internal
.get_observed_prob <- function(data, model_prefix, action_col, observed_actions) {
  pred_cols <- dplyr::select(data, dplyr::starts_with(model_prefix))
  action_levels <- sub(paste0(model_prefix, "_"), "", colnames(pred_cols))

  as.matrix(pred_cols)[cbind(
    seq_len(nrow(data)),
    match(observed_actions, action_levels)
  )]
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