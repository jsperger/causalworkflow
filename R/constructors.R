causal_workflow_constr <- function(causal_workflow) {
  # check that the components are either NULL or a workflow/workflow_set
  .check <- function(spec, component_name) {
    if (is.null(spec)) {
      return(invisible(TRUE))
    }
    is_wflow <- inherits(spec, "workflow")
    is_wflow_set <- inherits(spec, "workflow_set")
    if (!is_wflow && !is_wflow_set) {
      cli::cli_abort(
        "{.arg {component_name}} must be a {.cls workflow} or {.cls workflow_set} object, not a {.cls {class(spec)[[1]]}}."
      )
    }
  }

  .check(causal_workflow$propensity_model, "propensity_model")
  .check(causal_workflow$outcome_model, "outcome_model")

  invisible(TRUE)
}
