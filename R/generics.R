#' @importFrom hardhat extract_preprocessor
#' @export
hardhat::extract_preprocessor

#' @export
extract_preprocessor.causal_workflow <- function(x, ...) {
  # The relevant preprocessor for a causal_workflow in the context of staged
  # fitting is the one from the outcome model, as it defines the response.
  outcome_comp <- x$component[x$component_id == "outcome"]

  if (rlang::is_empty(outcome_comp)) {
    cli::cli_abort("The causal workflow must have an 'outcome' component.")
  }

  # In case there are multiple outcome models (e.g. in different stages)
  # we'll just take the first one. This might need refinement later.
  hardhat::extract_preprocessor(outcome_comp[[1]])
}