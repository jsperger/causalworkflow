# --- Internal Helpers for Fitting Nuisance Models ---

#' Fit a nuisance model specification (workflow or workflow_set)
#'
#' @description
#' This internal helper function is the core engine for fitting nuisance models.
#' It can handle both standard `workflow` objects and `workflow_set` objects,
#' and will automatically perform hyperparameter tuning if the workflow is
#' tunable.
#'
#' If a workflow is tunable but no `resamples` are provided, it will create
#' a default 5-fold cross-validation set from the `training_data`.
#'
#' @param spec A `workflow` or `workflow_set` object.
#' @param resamples An `rsample` object to use for tuning or ensembling. Can be
#'   `NULL`.
#' @param training_data The data to use for the final fit after tuning.
#' @param metric A character string for the metric to optimize during tuning.
#'
#' @return A fitted `workflow` or `stack` object.
#' @keywords internal
.fit_nuisance_spec <- function(spec, resamples, training_data, metric = NULL) {
  is_wflow <- inherits(spec, "workflow")
  is_wflow_set <- inherits(spec, "workflow_set")

  if (!is_wflow && !is_wflow_set) {
    cli::cli_abort(
      c(
        "{.arg spec} must be a {.cls workflow} or {.cls workflow_set} object.",
        "x" = "You've supplied a {.cls {class(spec)[[1]]}}."
      )
    )
  }

  is_tunable <- (is_wflow && nrow(tune::tunable(spec)) > 0) || is_wflow_set

  if (!is_tunable) {
    return(parsnip::fit(spec, data = training_data))
  }

  # --- If we reach here, tuning or stacking is required ---

  if (is.null(resamples)) {
    resamples <- rsample::vfold_cv(training_data, v = 5)
  }

  if (is_wflow) {
    # It's a tunable workflow
    tuned <- tune::tune_grid(spec, resamples = resamples)
    best_params <- tune::select_best(tuned, metric = metric)
    return(
      tune::finalize_workflow(spec, best_params) |>
        parsnip::fit(data = training_data)
    )
  }

  if (is_wflow_set) {
    # It's a workflow_set, build a stack
    wf_set_trained <-
      workflowsets::workflow_map(
        spec,
        "tune_grid",
        resamples = resamples,
        control = stacks::control_stack_grid(),
        verbose = FALSE
      )

    return(
      stacks::stacks() |>
        stacks::add_candidates(wf_set_trained) |>
        stacks::blend_predictions() |>
        stacks::fit_members()
    )
  }
}