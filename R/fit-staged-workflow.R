# --- `fit` method for `staged_workflow` -----------------------------------------

#' Fit a `staged_workflow`
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This function fits the sequence of models defined in a [staged_workflow()]
#' object using a backwards recursive algorithm. It serves as a wrapper around
#' the core `fit_recursive()` engine.
#'
#' @param object A [staged_workflow()] or [fitted_staged_workflow()] object.
#'   If a fitted object is provided, the function will resume the fitting
#'   process from where it left off.
#' @param data A data frame containing all necessary variables for all stages.
#' @param ... Not used.
#' @param discount A numeric value between 0 and 1 for discounting future
#'   outcomes. Defaults to 1 (no discounting).
#' @param control A `control_fit` object to manage the fitting process.
#'
#' @details
#' The fitting process proceeds in reverse order of the stages (`K` down to `1`).
#' For any stage `k < K`, the model is fit to a "pseudo-outcome" calculated from
#' the predictions of the model at stage `k+1`. See the underlying recursive
#' engine for more details.
#'
#' @return A `fitted_staged_workflow` object containing the ordered list of all
#'   fitted stage models.
#' @importFrom generics fit
#' @export
fit.staged_workflow <- function(
  object,
  data,
  ...,
  discount = 1,
  control = control_fit()
) {
  .check_staged_fit_inputs(object, data, discount)

  fit_recursive(
    object = object,
    data = data,
    discount = discount,
    control = control,
    single_stage = FALSE
  )
}

#' @rdname fit.staged_workflow
#' @export
fit.fitted_staged_workflow <- function(
  object,
  data,
  ...,
  discount = 1,
  control = control_fit()
) {
  .check_staged_fit_inputs(object, data, discount)

  fit_recursive(
    object = object,
    data = data,
    discount = discount,
    control = control,
    single_stage = FALSE
  )
}


# --- New `fit_next_stage` function --------------------------------------------

#' Fit the next stage of a `staged_workflow`
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This function fits the next sequential model in a `staged_workflow` object.
#' It is a valuable tool for debugging and for manually stepping through the
#' backwards recursive fitting process.
#'
#' @inheritParams fit.staged_workflow
#'
#' @details
#' Given a `staged_workflow` that may be partially fitted, this function
#' identifies the next stage that needs to be fitted (the highest-numbered
#' unfitted stage) and fits only that single stage. It uses the same recursive
#' engine as `fit.staged_workflow()`, but instructs it to stop after one
#' iteration.
#'
#' For example, if you have a 4-stage workflow where stages 4 and 3 are already
#' fitted, calling `fit_next_stage()` will fit stage 2 and return the updated
#' object with stages 4, 3, and 2 fitted.
#'
#' @return A `fitted_staged_workflow` object with one additional stage fitted.
#' @export
fit_next_stage <- function(
  object,
  data,
  ...,
  discount = 1,
  control = control_fit()
) {
  .check_staged_fit_inputs(object, data, discount)

  fit_recursive(
    object = object,
    data = data,
    discount = discount,
    control = control,
    single_stage = TRUE
  )
}

# --- Input validation helper -------------------------------------------------

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