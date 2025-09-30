# This file contains the new, unified recursive fitting engine for
# staged_workflow objects. This engine is the core of the refactored
# fitting process, handling single-stage, multi-stage, and partially fitted
# workflows, as well as different estimation strategies like cross-fitting
# and TMLE.

#' Fit a staged workflow using a recursive engine
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This is the core, internal recursive engine for fitting `staged_workflow`
#' objects. It is designed to be called by user-facing `fit()` methods like
#' `fit.staged_workflow()` and `fit_next_stage()`.
#'
#' The engine works backwards from the last stage to the first. It can handle
#' both full fitting processes (from stage `K` to `1`) and partial fits
#' (resuming from an intermediate stage `k`).
#'
#' @param object A `staged_workflow` or `fitted_staged_workflow` object.
#' @param data A data frame containing all necessary variables for all stages.
#' @param ... Not used.
#' @param discount A numeric value between 0 and 1 for discounting future
#'   outcomes.
#' @param control A control object to manage the fitting process.
#' @param single_stage A logical indicating whether to fit only the next
#'   stage in the sequence.
#'
#' @return A `fitted_staged_workflow` object.
#' @keywords internal
fit_recursive <- function(
  object,
  data,
  ...,
  discount = 1,
  control = control_fit(),
  single_stage = FALSE
) {
  # Abort for TMLE fits with > 2 stages until implemented
  is_tmle_fit <- any(sapply(object$stages, \(s) inherits(s$wflow, "tmle_workflow")))
  if (is_tmle_fit && length(object$stages) > 2) {
    cli::cli_abort(
      "CV-TMLE for more than 2 stages is not yet supported."
    )
  }

  # Prepare a partially-fitted object for recursion
  if (inherits(object, "fitted_staged_workflow")) {
    object$fitted_models <- object$models
    object$models <- NULL
  }

  # 1. Determine the sequence of stages to fit.
  stage_ids <- as.numeric(names(object$stages))
  fitted_ids <- if (!is.null(object$fitted_models)) {
    as.numeric(names(object$fitted_models))
  } else {
    numeric(0)
  }
  stages_to_fit <- sort(setdiff(stage_ids, fitted_ids), decreasing = TRUE)

  if (length(stages_to_fit) == 0) {
    cli::cli_warn("All stages have already been fitted.")
    return(object)
  }

  # Initialize fitted_models from the potentially partially-fitted object
  fitted_models <- if (!is.null(object$fitted_models)) {
    object$fitted_models
  } else {
    list()
  }

  # 2. Loop through stages in reverse order.
  for (k in stages_to_fit) {
    stage_spec <- object$stages[[as.character(k)]]
    stage_k_wflow <- stage_spec$wflow
    stage_k_data <- data[data$stage == k, ]

    # 3. Calculate the pseudo-outcome
    next_stage_model <-
      if ((k + 1) %in% stage_ids) {
        fitted_models[[as.character(k + 1)]]
      } else {
        NULL
      }

    target_outcome <- .calculate_pseudo_outcome(
      next_stage_model = next_stage_model,
      current_stage_spec = stage_spec,
      current_stage_data = stage_k_data,
      data = data,
      discount = discount
    )

    fit_data <- stage_k_data
    fit_data$outcome <- target_outcome

    # If the original formula was one-sided, update it
    stage_k_wflow <- .update_outcome_in_wflow(stage_k_wflow, "outcome")

    # 4. Fit the model for the current stage.
    fitted_k_wflow <- parsnip::fit(stage_k_wflow, data = fit_data)

    # 5. Store the fitted model.
    fitted_models[[as.character(k)]] <- fitted_k_wflow

    # 6. If single_stage is TRUE, exit after one iteration.
    if (single_stage) {
      break
    }
  }

  res <- list(
    stages = object$stages,
    models = fitted_models[order(as.numeric(names(fitted_models)))],
    exclusions = object$exclusions,
    actions = levels(data$action)
  )

  class(res) <- "fitted_staged_workflow"
  res
}

.calculate_pseudo_outcome <- function(
  next_stage_model,
  current_stage_spec,
  current_stage_data,
  data,
  discount
) {
  if (is.null(next_stage_model)) {
    return(current_stage_data$outcome)
  }

  value_k_plus_1 <-
    if (inherits(next_stage_model, "fitted_tmle_workflow")) {
      q_star_preds <- next_stage_model$targeted_predictions
      apply(q_star_preds, 1, max, na.rm = TRUE)
    } else if (inherits(next_stage_model, "fitted_causal_workflow")) {
      next_stage_model$estimate
    } else {
      actions <- unique(data$action)
      preds_over_actions <- lapply(actions, function(act) {
        future_data <- current_stage_data
        future_data$action <- act
        stats::predict(next_stage_model, new_data = future_data)$.pred
      })
      do.call(pmax, preds_over_actions)
    }

  wflow_formula <- hardhat::extract_preprocessor(current_stage_spec$wflow)
  has_outcome <- !is.null(rlang::f_lhs(wflow_formula))

  if (has_outcome) {
    current_stage_data$outcome + discount * value_k_plus_1
  } else {
    discount * value_k_plus_1
  }
}

.update_outcome_in_wflow <- function(wflow, outcome_name) {
  if (!inherits(wflow, "workflow")) {
    return(wflow)
  }

  wflow_formula <- hardhat::extract_preprocessor(wflow)
  if (is.null(rlang::f_lhs(wflow_formula))) {
    new_formula <- rlang::new_formula(
      lhs = rlang::sym(outcome_name),
      rhs = rlang::f_rhs(wflow_formula)
    )
    wflow <- wflow |>
      workflows::remove_formula() |>
      workflows::add_formula(new_formula)
  }
  wflow
}