# --- `fit` method for `staged_workflow` -----------------------------------------

#' Fit a `staged_workflow`
#'
#' This function fits the sequence of models defined in a `staged_workflow`
#' object using a backwards recursive algorithm.
#'
#' @param object A `staged_workflow` object.
#' @param data A data frame containing all necessary variables for all stages.
#'   It must include columns for `stage`, `action`, and `outcome`.
#' @param ... Not used.
#' @param discount A numeric value between 0 and 1 for discounting future
#'   outcomes. Defaults to 1 (no discounting).
#'
#' @details
#' The fitting process proceeds in reverse order of the stages (`K` down to `1`).
#'
#' - For the final stage (`K`), the model is fit using the observed `outcome`.
#' - For any preceding stage `k < K`, the model is fit to a "pseudo-outcome"
#'   calculated from the predictions of the model at stage `k+1`.
#'
#' The calculation of the pseudo-outcome depends on the formula specified in the
#' workflow for stage `k`:
#' - **Two-sided formula (`outcome ~ ...`)**: The pseudo-outcome is
#'   `data$outcome + discount * max_q_k_plus_1`.
#' - **One-sided formula (`~ ...`)**: The pseudo-outcome is
#'   `discount * max_q_k_plus_1`.
#'
#' where `max_q_k_plus_1` is the maximum predicted value from the stage `k+1`
#' model over all possible actions.
#'
#' @return
#' A `fitted_staged_workflow` object containing the ordered list of all
#' fitted stage models.
#' @importFrom generics fit
#' @export
fit.staged_workflow <- function(object, data, ..., discount = 1) {
  checkmate::assert_class(object, "staged_workflow")
  checkmate::assert_data_frame(data)
  checkmate::assert_names(
    names(data),
    must.include = c("stage", "action", "outcome")
  )
  checkmate::assert_factor(data$action, any.missing = FALSE, min.levels = 1)
  checkmate::assert_number(discount, lower = 0, upper = 1)

  stage_nums <- sort(as.numeric(names(object$stages)), decreasing = TRUE)
  actions <- levels(data$action)

  fitted_models <- list()
  next_stage_model <- NULL

  for (k in stage_nums) {
    fitted_k_wflow <- backwards_estimation_step(
      object = object,
      data = data,
      k = k,
      next_stage_model = next_stage_model,
      discount = discount,
      actions = actions
    )

    fitted_models[[as.character(k)]] <- fitted_k_wflow
    next_stage_model <- fitted_k_wflow
  }

  res <- list(
    models = fitted_models[order(as.numeric(names(fitted_models)))],
    exclusions = object$exclusions,
    actions = levels(data$action)
  )

  class(res) <- "fitted_staged_workflow"
  res
}
