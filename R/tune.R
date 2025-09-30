#' @importFrom tune tune_grid
#' @export
tune::tune_grid

#' Tune a causal workflow component
#'
#' @description
#' This method supports "greedy" hyperparameter tuning for a single component
#' of a [causal_workflow()]. It identifies which component (propensity or outcome
#' model) the provided `grid` of hyperparameters belongs to and dispatches
#' the tuning job to [tune::tune_grid()] for that component.
#'
#' @param object A [causal_workflow()] object.
#' @param resamples An `rsample` object, such as one created by [rsample::vfold_cv()].
#' @param grid A data frame of tuning hyperparameters, created by a `dials`
#'   grid function.
#' @param ... Additional arguments passed to [tune::tune_grid()].
#'
#' @return A `tune_results` object from the underlying call to [tune::tune_grid()].
#' The user is responsible for using this result to finalize the appropriate
#' workflow and update the [causal_workflow()] object.
#'
#' @export
tune_grid.causal_workflow <- function(object, resamples, grid, ...) {
  # 1. Validate inputs
  .check_tune_inputs(object)

  # 2. Identify which component to tune
  pscore_wflow <- object$propensity_model
  outcome_wflow <- object$outcome_model

  pscore_params <- .get_workflow_params(pscore_wflow)
  outcome_params <- .get_workflow_params(outcome_wflow)

  grid_params <- names(grid)

  is_pscore_tuning <- all(grid_params %in% pscore_params)
  is_outcome_tuning <- all(grid_params %in% outcome_params)

  # 3. Dispatch to the correct component
  if (is_pscore_tuning && !is_outcome_tuning) {
    tune::tune_grid(pscore_wflow, resamples = resamples, grid = grid, ...)
  } else if (is_outcome_tuning && !is_pscore_tuning) {
    tune::tune_grid(outcome_wflow, resamples = resamples, grid = grid, ...)
  } else if (is_pscore_tuning && is_outcome_tuning) {
    # This can happen if both have the same tunable param name, e.g. `mtry`
    cli::cli_abort(
      c(
        "Ambiguous tuning grid: The specified parameters exist in both the propensity and outcome models.",
        "i" = "This function only supports tuning one component at a time.",
        "x" = "Parameters found in both models: {.val {grid_params}}."
      )
    )
  } else {
    cli::cli_abort(
      c(
        "Grid parameters do not match tunable parameters in either model.",
        "i" = "The provided grid parameters are: {.val {grid_params}}.",
        "i" = "The propensity model has parameters: {.val {pscore_params}}.",
        "i" = "The outcome model has parameters: {.val {outcome_params}}."
      )
    )
  }
}

.check_tune_inputs <- function(object) {
  if (is.null(object$propensity_model) && is.null(object$outcome_model)) {
    cli::cli_abort(
      c(
        "The causal workflow must have at least one component model to tune.",
        "i" = "Add a model with {.fn add_propensity_model} or {.fn add_outcome_model}."
      )
    )
  }
}

# Helper to get tunable parameter names from a workflow
.get_workflow_params <- function(wflow) {
  if (is.null(wflow)) {
    return(character(0))
  }

  tunable_params <- tune::tunable(wflow)

  if (nrow(tunable_params) == 0) {
    return(character(0))
  }

  return(tunable_params$name)
}
