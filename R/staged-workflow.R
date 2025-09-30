# --- `staged_workflow` constructor and helpers ---------------------------------

#' `staged_workflow`
#'
#' Initializes a `staged_workflow` object.
#'
#' @details
#' A `staged_workflow` is a container for a sequence of stage-specific
#' `tidymodels` workflows, designed for fitting multi-stage models like
#' those used in Dynamic Treatment Regimes (DTRs).
#'
#' The core components are:
#' - `stages`: A list mapping each stage to a [workflows::workflow].
#' - `exclusions`: A formula specifying conditions under which certain
#'   actions are unavailable.
#'
#' @return
#' A new `staged_workflow` object.
#' @export
#' @examples
#' \dontrun{
#' library(parsnip)
#' library(recipes)
#' library(workflows)
#'
#' # Define a simple linear model workflow
#' lm_wflow <- workflow() |>
#'   add_model(linear_reg()) |>
#'   add_formula(outcome ~ covar1 + covar2 + action)
#'
#' # Initialize a staged workflow and add the model for two stages
#' q_spec <- staged_workflow() |>
#'   add_stage_model(lm_wflow, stages = 1:2)
#' }
staged_workflow <- function() {
  res <- list(
    stages = list(),
    exclusions = NULL
  )

  class(res) <- "staged_workflow"
  res
}

#' Add a stage-specific model to a `staged_workflow`
#'
#' This verb assigns a `tidymodels` workflow to one or more stages in a
#' `staged_workflow` object.
#'
#' @param x A `staged_workflow` object.
#' @param wflow A `tidymodels` `workflow` or a `causal_workflow` object to be
#'   assigned to the stage(s).
#' @param stage An integer specifying a single stage.
#' @param stages An integer vector specifying multiple stages.
#'
#' @details
#' Use either the `stage` or `stages` argument to specify the assignment. If a
#' workflow is already present for a given stage, it will be overwritten.
#'
#' When a [causal_workflow] is provided, the fitting process for that stage
#' will use the multi-component estimation procedure defined by Phase 1.
#' When a standard [workflows::workflow] is provided, the fitting process will use the
#' single-model Q-learning procedure from Phase 2.
#'
#' @return
#' An updated `staged_workflow` object.
#' @export
#' @examples
#' \dontrun{
#' library(parsnip)
#' library(recipes)
#' library(workflows)
#'
#' # A different model for each stage
#' stage_2_wflow <- workflow() |>
#'   add_model(linear_reg()) |>
#'   add_formula(outcome ~ covar1 + covar2 + action)
#'
#' stage_1_wflow <- workflow() |>
#'   add_model(rand_forest() |> set_engine("ranger")) |>
#'   add_formula(outcome ~ covar1 + covar2 + action)
#'
#' q_spec <- staged_workflow() |>
#'   add_stage_model(stage_2_wflow, stage = 2) |>
#'   add_stage_model(stage_1_wflow, stage = 1)
#'
#' # The same model for all stages
#' general_wflow <- workflow() |>
#'   add_model(linear_reg()) |>
#'   add_formula(outcome ~ covar1 + covar2 + action)
#'
#' q_spec_2 <- staged_workflow() |>
#'   add_stage_model(general_wflow, stages = 1:2)
#' }
add_stage_model <- function(x, wflow, stage = NULL, stages = NULL) {
  if (!inherits(x, "staged_workflow")) {
    cli::cli_abort(
      c(
        "{.arg x} must be a {.cls staged_workflow} object.",
        "x" = "You've supplied a {.cls {class(x)[[1]]}}."
      )
    )
  }
  if (!inherits(wflow, "workflow") &&
      !inherits(wflow, "causal_workflow") &&
      !inherits(wflow, "workflow_set")) {
    cli::cli_abort(
      c(
        "{.arg wflow} must be a {.cls workflow}, {.cls causal_workflow}, or {.cls workflow_set} object.",
        "x" = "You've supplied a {.cls {class(wflow)[[1]]}}."
      )
    )
  }

  if (is.null(stage) && is.null(stages)) {
    cli::cli_abort("Either {.arg stage} or {.arg stages} must be specified.")
  }

  if (!is.null(stage)) {
    if (!is.numeric(stage) || length(stage) != 1 || stage < 1 || stage %% 1 != 0) {
      cli::cli_abort("{.arg stage} must be a single positive integer.")
    }
    stages <- stage
  }

  if (!is.numeric(stages) || any(stages < 1) || any(stages %% 1 != 0)) {
    cli::cli_abort("{.arg stages} must be a vector of positive integers.")
  }

  wflow_type <- if (
    inherits(wflow, "causal_workflow") || inherits(wflow, "workflow_set")
  ) {
    "multi_component"
  } else {
    "single_model"
  }

  stage_spec <- list(
    wflow = wflow,
    type = wflow_type
  )

  for (s in stages) {
    x$stages[[as.character(s)]] <- stage_spec
  }

  x
}

#' Set action exclusion rules for a `staged_workflow`
#'
#' This verb defines rules for actions that are not available in certain states.
#'
#' @param x A `staged_workflow` object.
#' @param formula A formula describing the exclusion rules. The LHS should be
#'   empty, and the RHS should be a logical expression.
#'
#' @details
#' The formula is used during the fitting process to identify valid actions
#' when calculating pseudo-outcomes. For example, `~ action == "chemo" &
#' tumor_size > 5` would exclude the "chemo" action for patients with a tumor
#' size greater than 5.
#'
#' This is currently a placeholder and will be used in a future version.
#'
#' @return
#' An updated `staged_workflow` object.
#' @export
#' @examples
#' \dontrun{
#' q_spec <- staged_workflow() |>
#'   set_action_exclusions(~ action == "chemo" & tumor_size > 5)
#' }
set_action_exclusions <- function(x, formula) {
  if (!inherits(x, "staged_workflow")) {
    cli::cli_abort(
      c(
        "{.arg x} must be a {.cls staged_workflow} object.",
        "x" = "You've supplied a {.cls {class(x)[[1]]}}."
      )
    )
  }
  if (!inherits(formula, "formula")) {
    cli::cli_abort(
      c(
        "{.arg formula} must be a formula.",
        "x" = "You've supplied a {.cls {class(formula)[[1]]}}."
      )
    )
  }

  if (!is.null(rlang::f_lhs(formula))) {
    cli::cli_abort("The {.arg formula} must be one-sided.")
  }

  x$exclusions <- formula
  x
}
