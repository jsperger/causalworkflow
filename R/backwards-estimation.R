#' Perform a Single Stage of a Backwards Estimation Procedure
#'
#' @description
#' This function executes a single step of the backwards estimation algorithm
#' used to fit a `staged_workflow`. It fits the model for a specified stage `k`,
#' using the fitted model from stage `k+1` to calculate a pseudo-outcome if
#' necessary.
#'
#' Its primary purpose is for debugging multi-stage `causal_workflow` and
#' `staged_workflow`s.
#'
#' @param object A `staged_workflow` object.
#' @param data A data frame containing all necessary variables. It must include
#'   columns for `stage`, `action`, and `outcome`.
#' @param k The integer index of the stage to fit.
#' @param next_stage_model The fitted model from stage `k+1`. If `NULL` (the
#'   default), the model for stage `k` is assumed to be the final stage and is
#'   fit to the observed outcome.
#' @param discount A numeric value between 0 and 1 for discounting future
#'   outcomes. Defaults to 1 (no discounting).
#' @param actions A character vector of all possible action levels.
#'
#' @return
#' A fitted `workflow` or `causal_workflow` for stage `k`.
#'
#' @export
backwards_estimation_step <- function(object,
                                      data,
                                      k,
                                      next_stage_model = NULL,
                                      discount = 1,
                                      actions) {
  # Input validation
  checkmate::assert_class(object, "staged_workflow")
  checkmate::assert_data_frame(data)
  checkmate::assert_names(
    names(data),
    must.include = c("stage", "action", "outcome")
  )
  checkmate::assert_factor(data$action, any.missing = FALSE, min.levels = 1)
  checkmate::assert_int(k)
  # next_stage_model can be NULL or a fitted model
  checkmate::assert_number(discount, lower = 0, upper = 1)
  checkmate::assert_character(actions, any.missing = FALSE, min.len = 1)

  stage_spec <- object$stages[[as.character(k)]]
  if (is.null(stage_spec)) {
    rlang::abort(paste0("No model specification found for stage ", k, "."))
  }
  stage_k_wflow <- stage_spec$wflow
  stage_k_data <- data[data$stage == k, ]

  target_outcome <-
    if (is.null(next_stage_model)) {
      # This is the last stage (K), use observed outcome
      stage_k_data$outcome
    } else {
      # This is stage k < K.
      # First, calculate the expected future value from stage k+1.
      value_k_plus_1 <-
        if (inherits(next_stage_model, "fitted_causal_workflow")) {
          # For a causal_workflow, the value is the single point estimate.
          next_stage_model$estimate
        } else {
          # For a standard workflow, the value is the max predicted Q-value.
          preds_over_actions <- lapply(actions, function(act) {
            future_data <- stage_k_data
            future_data$action <- factor(act, levels = actions)
            stats::predict(next_stage_model, new_data = future_data)$.pred
          })
          do.call(pmax, preds_over_actions)
        }

      # Second, calculate the pseudo-outcome for stage k based on its own type.
      if (stage_spec$type == "multi_component") {
        # A causal_workflow always updates the observed outcome.
        stage_k_data$outcome + discount * value_k_plus_1
      } else {
        # A standard workflow checks for a one-sided vs two-sided formula.
        wflow_formula <- hardhat::extract_preprocessor(stage_k_wflow)
        if (!is.null(rlang::f_lhs(wflow_formula))) {
          stage_k_data$outcome + discount * value_k_plus_1
        } else {
          discount * value_k_plus_1
        }
      }
    }

  fit_data <- stage_k_data
  fit_data$outcome <- target_outcome

  # If the original formula was one-sided, update it (only for standard workflows)
  if (inherits(stage_k_wflow, "workflow")) {
    wflow_formula <- hardhat::extract_preprocessor(stage_k_wflow)
    if (is.null(rlang::f_lhs(wflow_formula))) {
      new_formula <- rlang::new_formula(
        lhs = rlang::sym("outcome"),
        rhs = rlang::f_rhs(wflow_formula)
      )
      stage_k_wflow <- stage_k_wflow |>
        workflows::remove_formula() |>
        workflows::add_formula(new_formula)
    }
  }

  # S3 dispatch will call fit.workflow or fit.causal_workflow
  fitted_k_wflow <- parsnip::fit(stage_k_wflow, data = fit_data)

  fitted_k_wflow
}