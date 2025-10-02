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
#' Like a `causal_workflow`, it serves as a container for the various modeling
#' components, which are added with [add_component()]. For TMLE, this typically
#' includes a "propensity" and an "outcome" model.
#'
#' @inheritParams causal_workflow
#' @return A `tmle_workflow` object, which also inherits from `causal_workflow`.
#' @export
tmle_workflow <- function(...) {
  res <- causal_workflow(...)
  class(res) <- c("tmle_workflow", class(res))
  res
}


#' @export
fit.tmle_workflow <- function(object, data, ...) {
  # 1. Fit initial nuisance models, creating resamples if needed for tuning
  g_spec <- object$component[object$component_id == "propensity"][[1]]
  q_spec <- object$component[object$component_id == "outcome"][[1]]

  g_resamples <- if (.spec_needs_tuning(g_spec)) {
    rsample::vfold_cv(data)
  } else {
    NULL
  }
  q_resamples <- if (.spec_needs_tuning(q_spec)) {
    rsample::vfold_cv(data)
  } else {
    NULL
  }

  g_fit <- .fit_nuisance_spec(
    g_spec,
    resamples = g_resamples,
    training_data = data
  )
  q_fit <- .fit_nuisance_spec(
    q_spec,
    resamples = q_resamples,
    training_data = data
  )

  # 2. Extract components and generate initial predictions
  treatment_var <- .extract_var_name(g_spec)
  treatment_levels <- levels(data[[treatment_var]])
  outcome_var <- "outcome" # Standardized by recursive engine

  initial_preds <- .get_nuisance_preds(
    g_fit,
    q_fit,
    data,
    treatment_var,
    treatment_levels
  )
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
  ) |>
    pmax(0.025)

  clever_cov <- as.numeric(data[[treatment_var]] == optimal_action) /
    g_hat_observed

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
  if (is.na(epsilon)) {
    epsilon <- 0
  }

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
  as.matrix(pred_cols)[cbind(
    seq_len(nrow(data)),
    match(observed_actions, action_levels)
  )]
}

.spec_needs_tuning <- function(spec) {
  if (inherits(spec, "workflow_set")) {
    return(TRUE)
  }
  if (inherits(spec, "workflow")) {
    return(hardhat::has_blueprint(spec$pre$actions$recipe) &&
             !is.null(spec$pre$actions$recipe$recipe$steps) &&
             any(vapply(spec$pre$actions$recipe$recipe$steps,
                        function(x) inherits(x, "step_hyper"), logical(1))))
  }
  return(FALSE)
}

.fit_nuisance_spec <- function(spec, resamples, training_data) {
  if (.spec_needs_tuning(spec)) {
    tune::tune_grid(spec, resamples = resamples)
  } else {
    parsnip::fit(spec, data = training_data)
  }
}

.extract_var_name <- function(spec) {
  if (inherits(spec, "workflow_set")) {
    # Assuming all workflows in the set have the same outcome
    spec <- spec$info[[1]]$workflow[[1]]
  }
  # This is a bit brittle, but should work for most cases
  spec$pre$actions$formula$formula[[2]] |> as.character()
}

.get_nuisance_preds <- function(g_fit, q_fit, data, treatment_var, treatment_levels) {
  # This is a placeholder for a more robust prediction function
  # that would handle workflow_sets, etc.
  g_preds <- predict(g_fit, new_data = data, type = "prob")
  names(g_preds) <- paste0("g_hat_", names(g_preds))

  q_preds_list <- purrr::map(treatment_levels, function(lvl) {
    new_data_lvl <- data
    new_data_lvl[[treatment_var]] <- lvl
    predict(q_fit, new_data = new_data_lvl)
  })

  q_preds <- dplyr::bind_cols(q_preds_list)
  names(q_preds) <- paste0("q_hat_", treatment_levels)

  dplyr::bind_cols(g_preds, q_preds)
}