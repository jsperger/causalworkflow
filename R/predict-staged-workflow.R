# --- `predict` methods for `fitted_staged_workflow` ---------------------------

#' Predict from a `fitted_staged_workflow`
#'
#' This function generates predictions for a specific stage from a fitted
#' `staged_workflow` object.
#'
#' @param object A `fitted_staged_workflow` object.
#' @param new_data A data frame containing the predictor values for the
#'   specified stage.
#' @param stage An integer specifying the stage for which to generate
#'   predictions.
#' @param type A character string indicating the type of prediction.
#'   - `"action"`: Returns the action with the maximum predicted Q-value.
#'   - `"value"`: Returns the maximum predicted Q-value.
#' @param ... Not used.
#'
#' @details
#' The method retrieves the fitted model for the specified `stage` and predicts
#' the Q-value for each possible action. Based on the `type` argument, it
#' returns either the optimal action or the corresponding value.
#'
#' @return
#' A tibble with the predictions. For `type = "action"`, the column is
#' `.pred_action`. For `type = "value"`, the column is `.pred_value`.
#' @export
predict.fitted_staged_workflow <-
  function(object,
           new_data,
           stage,
           type = "action",
           ...) {
    checkmate::assert_class(object, "fitted_staged_workflow")
    checkmate::assert_data_frame(new_data)
    checkmate::assert_int(stage, lower = 1)

    stage_model <- object$models[[as.character(stage)]]
    if (is.null(stage_model)) {
      stop("No model found for stage ", stage)
    }

    # Dispatch to the appropriate prediction method based on model type
    if (inherits(stage_model, "fitted_causal_workflow")) {
      # Phase 3: Predict from a multi-component model
      # The `type` argument is passed via `...` to the underlying method
      return(stats::predict(stage_model, new_data = new_data, type = type, ...))
    } else {
      # Phase 2: Predict from a single model (Q-learning)
      checkmate::assert_choice(type, c("action", "value"))
      actions <- object$actions

      preds_over_actions <- lapply(actions, function(act) {
        data_for_action <- new_data
        data_for_action$action <- factor(act, levels = actions)
        stats::predict(stage_model, new_data = data_for_action)$.pred
      })

      pred_matrix <- do.call(cbind, preds_over_actions)

      if (type == "value") {
        max_values <- do.call(pmax, as.data.frame(pred_matrix))
        return(tibble::tibble(.pred_value = max_values))
      }

      if (type == "action") {
        max_indices <- apply(pred_matrix, 1, which.max)
        best_actions <- actions[max_indices]
        return(tibble::tibble(.pred_action = factor(best_actions, levels = actions)))
      }
    }
  }

#' Predict a sequence of optimal actions
#'
#' This function predicts the optimal action and value for each stage in a
#' `fitted_staged_workflow`.
#'
#' @param object A `fitted_staged_workflow` object.
#' @param new_data A data frame containing the initial state for the first
#'   stage of prediction.
#' @param ... Not used.
#'
#' @details
#' This method iteratively predicts the optimal action for each stage, updates
#' the state based on that action, and passes the new state to the next stage's
#' model. This is a "greedy" prediction that assumes the optimal action is taken
#' at each step.
#'
#' @return
#' A tibble with one row per observation in `new_data` and columns for the
#' predicted action and value at each stage (e.g., `.pred_action_1`,
#' `.pred_value_1`, etc.).
#' @importFrom parsnip multi_predict
#' @export
multi_predict.fitted_staged_workflow <- function(object, new_data, ...) {
  checkmate::assert_class(object, "fitted_staged_workflow")
  checkmate::assert_data_frame(new_data)

  # Check if any stage has a multi-component model, which is not supported
  # for sequential action prediction.
  has_causal_model <- purrr::some(object$models, ~ inherits(.x, "fitted_causal_workflow"))
  if (has_causal_model) {
    stop(
      "`multi_predict()` is only supported for staged workflows where every ",
      "stage is a standard `workflow` (single-model Q-learning)."
    )
  }

  stage_nums <- as.numeric(names(object$models))

  results <- list()
  current_data <- new_data

  for (k in stage_nums) {
    stage_model <- object$models[[as.character(k)]]
    actions <- object$actions

    preds_over_actions <- lapply(actions, function(act) {
      data_for_action <- current_data
      data_for_action$action <- factor(act, levels = actions)
      stats::predict(stage_model, new_data = data_for_action)$.pred
    })

    pred_matrix <- do.call(cbind, preds_over_actions)

    # Get values and actions for stage k
    max_values <- do.call(pmax, as.data.frame(pred_matrix))
    max_indices <- apply(pred_matrix, 1, which.max)
    best_actions <- actions[max_indices]

    # Store results
    results[[paste0(".pred_value_", k)]] <- max_values
    results[[paste0(".pred_action_", k)]] <- factor(best_actions, levels = actions)

    # Update data for the next stage
    current_data$action <- factor(best_actions, levels = actions)
  }

  tibble::as_tibble(results)
}
