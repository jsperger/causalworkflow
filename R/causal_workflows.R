#' Create a Causal Workflow
#'
#' @description
#' Initializes a `causal_workflow` object. This function creates a tibble-based
#' structure that serves as a blueprint for a causal analysis. Components are
#' added to this object using [add_component()] or [add_stage()].
#'
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Named arguments where the
#'   name defines the `component_id` and the value is a `<workflow>`,
#'   `<workflow_set>`, or `<tailor>` object.
#' @param .stage A single integer defining the analysis stage for the
#'   provided components. Defaults to `1L`.
#'
#' @returns A `causal_workflow` object.
#' @export
causal_workflow <- function(..., .stage = 1L) {
  res <- new_causal_workflow()
  components <- rlang::list2(...)

  if (rlang::is_empty(components)) {
    return(res)
  }

  for (nm in names(components)) {
    res <- add_component(res, nm, components[[nm]], stage = .stage)
  }

  res
}

new_causal_workflow <- function(x = NULL) {
  if (is.null(x)) {
    x <- tibble::tibble(
      stage = integer(0),
      component_id = character(0),
      component = list(),
      options = list(),
      result = list()
    )
  }

  tibble::new_tibble(x, class = "causal_workflow")
}

check_causal_workflow <- function(x, call = rlang::caller_env()) {
  if (!inherits(x, "causal_workflow")) {
    cli::cli_abort(
      "{.arg x} must be a {.cls causal_workflow} object, not a {.cls {class(x)[[1]]}}.",
      call = call
    )
  }
}

check_spec <- function(spec, call = rlang::caller_env()) {
  is_wflow <- inherits(spec, "workflow")
  is_wflow_set <- inherits(spec, "workflow_set")

  if (!is_wflow && !is_wflow_set) {
    cli::cli_abort(
      "{.arg spec} must be a {.cls workflow} or {.cls workflow_set} object, not a {.cls {class(spec)[[1]]}}.",
      call = call
    )
  }
}