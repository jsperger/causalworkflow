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
#' @export
fit.staged_workflow <- function(object, data, ..., discount = 1) {
  checkmate::assert_class(object, "staged_workflow")
  checkmate::assert_data_frame(data)
  checkmate::assert_names(names(data), must.include = c("stage", "action", "outcome"))
  checkmate::assert_factor(data$action, any.missing = FALSE, min.levels = 1)
  checkmate::assert_number(discount, lower = 0, upper = 1)

  stage_nums <- sort(as.numeric(names(object$stages)), decreasing = TRUE)

  fitted_models <- list()
  next_stage_model <- NULL

  for (k in stage_nums) {
    stage_k_wflow <- object$stages[[as.character(k)]]
    stage_k_data <- data[data$stage == k, ]

    target_outcome <-
      if (is.null(next_stage_model)) {
        # This is the last stage (K)
        stage_k_data$outcome
      } else {
        # This is stage k < K, calculate pseudo-outcome
        actions <- unique(data$action)

        # Predict Q-values for each possible action using model from k+1
        preds_over_actions <- lapply(actions, function(act) {
          future_data <- stage_k_data
          future_data$action <- act

          stats::predict(next_stage_model, new_data = future_data)$.pred
        })

        # Find the max Q-value for each observation
        max_q_k_plus_1 <- do.call(pmax, preds_over_actions)

        # Calculate pseudo-outcome based on the formula
        wflow_formula <- hardhat::extract_preprocessor(stage_k_wflow)

        if (!is.null(rlang::f_lhs(wflow_formula))) {
          # Two-sided formula: outcome ~ ...
          stage_k_data$outcome + discount * max_q_k_plus_1
        } else {
          # One-sided formula: ~ ...
          discount * max_q_k_plus_1
        }
      }

    # Prepare data for fitting
    fit_data <- stage_k_data
    fit_data$outcome <- target_outcome

    # If the original formula was one-sided, update it
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

    # Fit the model for stage k
    fitted_k_wflow <- parsnip::fit(stage_k_wflow, data = fit_data)

    # Store the fitted model
    fitted_models[[as.character(k)]] <- fitted_k_wflow
    next_stage_model <- fitted_k_wflow
  }

  # Return a final object
  res <- list(
    models = fitted_models[order(as.numeric(names(fitted_models)))],
    exclusions = object$exclusions,
    actions = levels(data$action)
  )

  class(res) <- "fitted_staged_workflow"
  res
}