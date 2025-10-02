#' @importFrom hardhat extract_preprocessor
#' @export
hardhat::extract_preprocessor

#' Extract a preprocessor from a causal workflow
#'
#' @param x A `causal_workflow` object.
#' @param component_id A character string identifying the component from which
#'   to extract the preprocessor.
#' @param stage A single integer for the analysis stage. Defaults to `1L`.
#' @param ... Not used.
#'
#' @return The preprocessor object, typically a `recipe`.
#' @export
extract_preprocessor.causal_workflow <- function(x, component_id, stage = 1L, ...) {
  row_ind <- which(x$component_id == component_id & x$stage == stage)

  if (length(row_ind) == 0) {
    cli::cli_abort("Component '{component_id}' not found in stage {stage}.")
  }

  # Use pluck for safe extraction from the list column
  component <- purrr::pluck(x, "component", row_ind)

  hardhat::extract_preprocessor(component)
}