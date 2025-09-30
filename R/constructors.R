data_stack_constr <- function(data_stack) {
  check_inherits(attr(data_stack, "rs_hash"), "character")
  check_inherits(attr(data_stack, "outcome"), "character")
  check_inherits(attr(data_stack, "mode"), "character")
  check_inherits(attr(data_stack, "train"), "tbl_df")
  check_inherits(attr(data_stack, "splits"), "tbl_df")

  purrr::map(attr(data_stack, "cols_map"), check_inherits, "character")
  purrr::map(attr(data_stack, "model_defs"), check_inherits, "workflow")
  purrr::map(attr(data_stack, "model_metrics"), check_inherits, "tbl_df")

  invisible(TRUE)
}

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


model_stack_constr <- function(model_stack) {
  check_inherits(model_stack[["coefs"]], "model_fit")
  check_inherits(model_stack[["equations"]], "list")
  check_inherits(model_stack[["train"]], "tbl_df")
  check_inherits(model_stack[["data_stack"]], "tbl_df")
  check_inherits(model_stack[["mode"]], "character")
  check_inherits(model_stack[["outcome"]], "character")
  check_inherits(model_stack[["splits"]], "tbl_df")
  check_inherits(model_stack[["penalty"]], "list")

  purrr::map(model_stack[["model_defs"]], check_inherits, "workflow")
  purrr::map(model_stack[["cols_map"]], check_inherits, "character")
  purrr::map(model_stack[["model_metrics"]], check_inherits, "tbl_df")
  purrr::map(model_stack[["member_fits"]], check_inherits, "workflow")

  invisible(TRUE)
}
