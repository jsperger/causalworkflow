causal_workflow_constr <- function(x) {
  # TODO: replace with proper constructor
  TRUE
}

check_dots_empty <- function(..., call = rlang::caller_env()) {
  if (rlang::dots_n(...) > 0) {
    rlang::warn("The `...` are not used in this function.", call = call)
  }
  invisible(NULL)
}