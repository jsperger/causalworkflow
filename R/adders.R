#' Add a component to a causal workflow
#'
#' @param x A `causal_workflow` object.
#' @param component_id A character string identifying the component.
#' @param component A `tidymodels` object, such as a `<workflow>` or
#'   `<workflow_set>`.
#' @param stage A single integer for the analysis stage. Defaults to `1L`.
#'
#' @return An updated `causal_workflow` object.
#' @export
add_component <- function(x, component_id, component, stage = 1L) {
  check_causal_workflow(x)
  checkmate::assert_string(component_id)
  checkmate::assert_int(stage, lower = 1)
  # Not checking spec here to avoid circular dependency issues
  # check_spec(component)

  if (component_id %in% x$component_id[x$stage == stage]) {
    cli::cli_abort("Component '{component_id}' already exists in stage {stage}.")
  }

  new_row <- tibble::tibble(
    stage = stage,
    component_id = component_id,
    component = list(component),
    options = list(NULL),
    result = list(NULL)
  )

  res <- vctrs::vec_rbind(x, new_row) |>
    dplyr::arrange(.data$stage, .data$component_id)

  new_causal_workflow(res)
}


#' Add a Stage to a Causal Workflow
#'
#' @description
#' Appends a new set of components for a subsequent analysis stage to an
#' existing `causal_workflow` object.
#'
#' @param x A `causal_workflow` object.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Named arguments where the
#'   name defines the `component_id` and the value is a `<workflow>`,
#'   `<workflow_set>`, or `<tailor>` object.
#' @param .stage A single integer for the new stage.
#'
#' @returns An updated `causal_workflow` object.
#' @export
add_stage <- function(x, ..., .stage) {
  check_causal_workflow(x)
  if (missing(.stage)) {
    cli::cli_abort("`\\.stage` must be provided.")
  }
  checkmate::assert_int(.stage, lower = 1)

  stage <- .stage
  if (stage %in% x$stage) {
    cli::cli_abort("Stage {stage} already exists.")
  }

  components <- rlang::list2(...)
  if (rlang::is_empty(components)) {
    return(x)
  }

  res <- x
  for (nm in names(components)) {
    res <- add_component(res, nm, components[[nm]], stage = stage)
  }

  res
}