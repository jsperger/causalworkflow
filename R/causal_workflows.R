#' Initialize a Causal Workflow
#'
#' @description
#' The `causal_workflow()` function initializes a `causal_workflow` object.
#' This object serves as a container for the various component models
#' required for a causal analysis, such as a propensity score model and an
#' outcome model.
#'
#' The resulting `causal_workflow` object is then passed to "verb" functions
#' like `add_propensity_model()` and `add_outcome_model()` to specify the
#' analysis.
#'
#' @details
#' This initial version of `causal_workflow` is designed for single-stage,
#' point-treatment estimators with binary treatments (e.g., AIPW). The
#' underlying architecture is designed to be extensible to more complex
#' scenarios in future versions, including:
#' - Multi-level categorical or continuous treatments.
#' - More generic weighting schemes, such as inverse probability of censoring
#'   weights.
#' - Multi-stage dynamic treatment regimes.
#'
#' @param ... Additional arguments. Currently ignored.
#'
#' @return A `causal_workflow` object.
#' @export
causal_workflow <- function(...) {
  check_empty_ellipses(...)

  wflow <-
    structure(
      list(
        propensity_model = NULL,
        outcome_model = NULL
      ),
      class = "causal_workflow"
    )

  if (causal_workflow_constr(wflow)) {
    wflow
  }
}

#' Add a propensity model to a causal workflow
#'
#' @param x A `causal_workflow` object.
#' @param wflow A `tidymodels` `workflow` object for the propensity model.
#'
#' @return An updated `causal_workflow` object.
#' @export
add_propensity_model <- function(x, wflow) {
  check_causal_workflow(x)
  check_workflow(wflow)

  x$propensity_model <- wflow

  if (causal_workflow_constr(x)) {
    x
  }
}

#' Add an outcome model to a causal workflow
#'
#' @param x A `causal_workflow` object.
#' @param wflow A `tidymodels` `workflow` object for the outcome model.
#'
#' @return An updated `causal_workflow` object.
#' @export
add_outcome_model <- function(x, wflow) {
  check_causal_workflow(x)
  check_workflow(wflow)

  x$outcome_model <- wflow

  if (causal_workflow_constr(x)) {
    x
  }
}

check_causal_workflow <- function(x, call = rlang::caller_env()) {
  if (!inherits(x, "causal_workflow")) {
    rlang::abort(
      "`x` must be a `causal_workflow` object.",
      call = call
    )
  }
}

check_workflow <- function(wflow, call = rlang::caller_env()) {
  if (!inherits(wflow, "workflow")) {
    rlang::abort(
      "`wflow` must be a `workflow` object.",
      call = call
    )
  }
}