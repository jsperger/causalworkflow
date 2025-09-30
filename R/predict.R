#' Predict from a fitted causal workflow
#'
#' @description
#' The behavior of `predict()` for a `fitted_causal_workflow` object depends on
#' how the object was created.
#'
#' - If the object was created using `fit()`, `predict()` generates predictions
#'   on new data.
#' - If the object was created using `fit_across()` or `tune_nested()`,
#'   `predict()` extracts the results of the estimation procedure (e.g.,
#'   potential outcome means, influence functions).
#'
#' @param object A `fitted_causal_workflow` object.
#' @param new_data A new data frame for which to generate predictions. Required
#'   only when the object was fitted with `fit()`.
#' @param type A character string specifying the type of output. The valid
#'   options depend on how the object was fitted.
#'
#'   For objects from `fit()` (prediction):
#'   - `"value"`: (Default) The maximum estimated potential outcome.
#'   - `"action"`: The action that leads to the maximum estimated potential outcome.
#'   - `"potential_outcome"`: The estimated potential outcome for each treatment level.
#'
#'   For objects from `fit_across()` or `tune_nested()` (extraction):
#'   - `"potential_outcome"`: (Default) The estimated potential outcome for each
#'     treatment level, with standard errors.
#'   - `"if"`: The observation-level efficient influence function (EIF) values.
#'   - `"components"`: The out-of-sample nuisance predictions.
#'   - `"blip_ref"`: The treatment effect relative to a reference level.
#'   - `"blip_avg"`: The treatment effect relative to the average of all levels.
#' @param ... Additional arguments, such as `ref_level` for `type = "blip_ref"`.
#'
#' @return A [tibble::tibble()] with a single column: `.pred`.
#'   - For predictive types `"value"` and `"action"`, `.pred` is a numeric or
#'     factor vector, respectively.
#'   - For other types, `.pred` is a list-column containing the results
#'     (e.g., tibbles of potential outcomes or influence functions).
#'
#' @export
predict.fitted_causal_workflow <- function(object,
                                           new_data = NULL,
                                           type = NULL,
                                           ...) {
  # is_predictive is TRUE if the object was created by fit()
  is_predictive <- !is.null(object$.fitted_by) && object$.fitted_by == "fit"

  if (is_predictive) {
    # --- Predictive Branch (for objects from fit()) ---
    if (is.null(type)) {
      type <- "value"
    }
    valid_types <- c("value", "action", "potential_outcome")
    type <- rlang::arg_match(type, valid_types)

    if (is.null(new_data)) {
      rlang::abort(
        paste(
          "`new_data` is required for prediction from a `fitted_causal_workflow`",
          "that was created by `fit()`."
        )
      )
    }

    treatment_var <- object$treatment
    treatment_lvls <- object$treatment_levels
    outcome_model <- object$outcome_model_fit

    preds_list <- purrr::map(treatment_lvls, function(lvl) {
      new_data_lvl <- new_data
      new_data_lvl[[treatment_var]] <- factor(lvl, levels = treatment_lvls)
      stats::predict(outcome_model, new_data = new_data_lvl)$.pred
    })

    preds_df <- do.call(cbind, preds_list)
    colnames(preds_df) <- treatment_lvls

    res <- switch(
      type,
      "value" = {
        pred <- do.call(pmax, as.data.frame(preds_df))
        tibble::tibble(.pred = pred)
      },
      "action" = {
        max_indices <- apply(preds_df, 1, which.max)
        pred <- factor(treatment_lvls[max_indices], levels = treatment_lvls)
        tibble::tibble(.pred = pred)
      },
      "potential_outcome" = {
        nested_preds <- purrr::map(seq_len(nrow(preds_df)), function(i) {
          tibble::tibble(
            level = treatment_lvls,
            .pred = as.numeric(preds_df[i, ])
          )
        })
        tibble::tibble(.pred = nested_preds)
      }
    )
    return(res)
  } else {
    # --- Extractor Branch (for objects from fit_across(), etc.) ---
    if (is.null(type)) {
      type <- "potential_outcome"
    }
    dots <- list(...)
    valid_types <- c(
      "potential_outcome", "if", "components", "blip_ref", "blip_avg"
    )
    type <- rlang::arg_match(type, valid_types)

    if (!is.null(new_data)) {
      rlang::warn("`new_data` is ignored when predicting from a `fitted_causal_workflow` that was not created by `fit()`.")
    }

    if_object <- object$eif_pom

    res <- switch(
      type,
      "potential_outcome" = {
        res <- dplyr::left_join(object$estimates, object$variances, by = "level")
        res <- dplyr::mutate(res, .std_err = sqrt(.data$.variance))
        dplyr::select(res, "level", ".pred", ".std_err")
      },
      "if" = {
        if_object
      },
      "components" = {
        object$nuisance_predictions
      },
      "blip_ref" = {
        ref_level <- dots$ref_level
        if (is.null(ref_level)) {
          rlang::abort("`ref_level` must be specified for `type = 'blip_ref'`.")
        }
        if (!is.character(ref_level) || length(ref_level) != 1) {
          rlang::abort("`ref_level` must be a single string.")
        }
        if (!ref_level %in% object$treatment_levels) {
          rlang::abort(
            paste0(
              "`ref_level` must be one of the following treatment levels: ",
              paste(object$treatment_levels, collapse = ", ")
            )
          )
        }

        eif_ref_col <- paste0("eif_pom_", ref_level)
        eif_ref <- if_object[[eif_ref_col]]
        contrast_levels <- setdiff(object$treatment_levels, ref_level)

        purrr::map_dfr(contrast_levels, function(lvl) {
          eif_lvl_col <- paste0("eif_pom_", lvl)
          eif_lvl <- if_object[[eif_lvl_col]]
          blip_eif <- eif_lvl - eif_ref

          pred <- mean(blip_eif, na.rm = TRUE)
          variance <- stats::var(blip_eif, na.rm = TRUE) / sum(!is.na(blip_eif))

          tibble::tibble(
            level = lvl,
            .pred = pred,
            .std_err = sqrt(variance)
          )
        })
      },
      "blip_avg" = {
        avg_eif <- rowMeans(if_object, na.rm = TRUE)

        purrr::map_dfr(object$treatment_levels, function(lvl) {
          eif_lvl_col <- paste0("eif_pom_", lvl)
          eif_lvl <- if_object[[eif_lvl_col]]
          blip_eif <- eif_lvl - avg_eif

          pred <- mean(blip_eif, na.rm = TRUE)
          variance <- stats::var(blip_eif, na.rm = TRUE) / sum(!is.na(blip_eif))

          tibble::tibble(
            level = lvl,
            .pred = pred,
            .std_err = sqrt(variance)
          )
        })
      }
    )
    return(tibble::tibble(.pred = list(res)))
  }
}

#' @importFrom generics augment
#' @export
generics::augment