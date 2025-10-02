#' Control options for causal workflow components
#'
#' These functions create classed lists of options that can be supplied to the
#' `options` argument of [add_options()] to control how different components
#' of a `causal_workflow` are handled during fitting.
#'
#' @param truncate A numeric value between 0 and 1 for truncating propensity
#'   scores.
#' @param control_tune A `tune` control object, such as [tune::control_grid()],
#'   used when a component is a `<workflow_set>`.
#' @param fluctuation The type of fluctuation model to use in TMLE. Currently,
#'   only `"logistic"` is supported.
#' @param ... Not currently used.
#'
#' @return A `causal_workflow_control` object.
#' @name control_causal_workflow
NULL

#' @export
#' @rdname control_causal_workflow
control_propensity <- function(truncate = 0.01, ...) {
  checkmate::assert_number(truncate, lower = 0, upper = 1)
  res <- list(truncate = truncate)
  class(res) <- "causal_workflow_control_propensity"
  res
}

#' @export
#' @rdname control_causal_workflow
control_outcome <- function(control_tune = tune::control_grid(), ...) {
  res <- list(control_tune = control_tune)
  class(res) <- "causal_workflow_control_outcome"
  res
}

#' @export
#' @rdname control_causal_workflow
control_targeting <- function(fluctuation = "logistic", ...) {
  res <- list(fluctuation = fluctuation)
  class(res) <- "causal_workflow_control_targeting"
  res
}


#' Add options to a causal workflow component
#'
#' This function allows you to add a `control` object to a specific component
#' within a `causal_workflow`.
#'
#' @param x A `causal_workflow` object.
#' @param component_id A character string identifying the component.
#' @param stage A single integer for the analysis stage. Defaults to `1L`.
#' @param options A `control` object, such as one created by
#'   [control_propensity()].
#'
#' @return An updated `causal_workflow` object.
#' @export
add_options <- function(x, component_id, options, stage = 1L) {
  check_causal_workflow(x)
  checkmate::assert_string(component_id)
  checkmate::assert_int(stage, lower = 1)

  row_ind <- which(x$component_id == component_id & x$stage == stage)

  if (length(row_ind) == 0) {
    cli::cli_abort("Component '{component_id}' not found in stage {stage}.")
  }

  x$options[[row_ind]] <- options
  x
}