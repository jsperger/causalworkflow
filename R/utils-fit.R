# --- Internal Helpers for Fitting Nuisance Models ---

#' Fit a nuisance model specification (workflow or workflow_set)
#'
#' @description
#' This internal helper function is the core engine for fitting nuisance models.
#' It can handle both standard `workflow` objects and `workflow_set` objects,
#' and will automatically perform hyperparameter tuning if the workflow is
#' tunable.
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

  needs_tuning <- .spec_needs_tuning(spec)

  if (!needs_tuning) {
    return(parsnip::fit(spec, data = training_data))
  }

  # --- If we reach here, tuning or stacking is required ---
  if (needs_tuning && is.null(resamples)) {
    cli::cli_abort("{.arg resamples} must be provided when tuning is required")
  }

  if (is_wflow) {
    # It's a tunable workflow
    tuned <- tune::tune_grid(spec, resamples = resamples)
    best_params <- tune::select_best(tuned, metric = metric)
    final_spec <- tune::finalize_workflow(spec, best_params)

    return(parsnip::fit(final_spec, data = training_data))
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

# --- Input validation helpers -------------------------------------------------

.check_staged_fit_inputs <- function(
  object,
  data,
  discount,
  call = rlang::caller_env()
) {
  # Allow both unfitted and partially-fitted staged workflows
  valid_class <- inherits(object, "staged_workflow") ||
    inherits(object, "fitted_staged_workflow")

  if (!valid_class) {
    cli::cli_abort(
      c(
        "{.arg object} must be a {.cls staged_workflow} or {.cls fitted_staged_workflow} object.",
        "x" = "You've supplied a {.cls {class(object)[[1]]}}."
      ),
      call = call
    )
  }
  if (!is.data.frame(data)) {
    cli::cli_abort(
      c(
        "{.arg data} must be a data frame.",
        "x" = "You've supplied a {.cls {class(data)[[1]]}}."
      ),
      call = call
    )
  }
  required_cols <- c("stage", "action", "outcome")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    cli::cli_abort(
      c(
        "{.arg data} must contain the required columns.",
        "x" = "Missing column{?s}: {.val {missing_cols}}."
      ),
      call = call
    )
  }
  if (!is.factor(data$action)) {
    cli::cli_abort(
      c(
        "Column {.arg action} in {.arg data} must be a factor.",
        "x" = "It is a {.cls {class(data$action)[[1]]}}."
      ),
      call = call
    )
  }
  if (any(is.na(data$action))) {
    cli::cli_abort(
      "Column {.arg action} in {.arg data} must not contain missing values.",
      call = call
    )
  }
  if (!is.numeric(discount) || length(discount) != 1) {
    cli::cli_abort(
      c(
        "{.arg discount} must be a single number.",
        "x" = "You've supplied a {.cls {class(discount)[[1]]}} of length {length(discount)}."
      ),
      call = call
    )
  }
  if (discount < 0 || discount > 1) {
    cli::cli_abort(
      c(
        "{.arg discount} must be between 0 and 1.",
        "x" = "You've supplied {.val {discount}}."
      ),
      call = call
    )
  }
}

.check_fit_inputs <- function(object, data, call = rlang::caller_env()) {
  if (is.null(object$propensity_model)) {
    cli::cli_abort(
      "The causal workflow must have a propensity model.",
      call = call
    )
  }
  if (is.null(object$outcome_model)) {
    cli::cli_abort(
      "The causal workflow must have an outcome model.",
      call = call
    )
  }
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame.", call = call)
  }
}

.extract_var_name <- function(spec) {
  if (inherits(spec, "workflow_set")) {
    ids <- spec$wflow_id
    formula_list <- purrr::map(ids, \(wf_id) {
      hardhat::extract_preprocessor(spec, id = wf_id)
    })
    vars <- purrr::map_chr(
      formula_list,
      ~ rlang::as_name(rlang::f_lhs(.x))
    )
    all_vars_equal <- all(vars == vars[[1]])
    if (!all_vars_equal) {
      cli::cli_abort(
        "All outcome/treatment variables for the same component must be the same."
      )
    }
    return(vars[[1]])
  } else {
    formula <- hardhat::extract_preprocessor(spec)
    return(rlang::f_lhs(formula) |> rlang::as_name())
  }
}

.extract_spec_vars <- function(object) {
  treatment_var <- .extract_var_name(object$propensity_model)
  outcome_var <- .extract_var_name(object$outcome_model)

  list(
    treatment_var = treatment_var,
    outcome_var = outcome_var
  )
}

.spec_needs_tuning <- function(spec) {
  if (inherits(spec, "workflow_set")) {
    return(TRUE)
  }
  if (inherits(spec, "workflow") && nrow(tune::tunable(spec)) > 0) {
    return(TRUE)
  }
  FALSE
}
