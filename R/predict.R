#' Predict from a fitted causal workflow
#'
#' @param object A `fitted_causal_workflow` object.
#' @param new_data A new data frame for which to generate predictions.
#'   This is not yet used but is included for API consistency.
#' @param type A character string specifying the type of prediction to return.
#'   Valid options are:
#'   - `"potential_outcome"`: (Default) The estimated potential outcome for each
#'     treatment level.
#'   - `"blip_ref"`: The treatment effect relative to a reference level (a
#'     "pseudo-blip"). Requires the `ref_level` argument.
#'   - `"blip_avg"`: The treatment effect relative to the average of all
#'     treatment levels.
#'   - `"if"`: The observation-level efficient influence function (EIF) values
#'     for the potential outcome mean (POM) for each treatment level.
#'   - `"components"`: The out-of-sample nuisance predictions from the
#'     cross-fitting procedure.
#' @param ... Additional arguments passed to the prediction method.
#'   - `ref_level`: A character string specifying the reference treatment level
#'     when `type = "blip_ref"`.
#'
#' @return A tibble with the requested prediction type.
#'
#' @export
predict.fitted_causal_workflow <- function(object, new_data = NULL, type = "potential_outcome", ...) {
  # Capture additional arguments
  dots <- list(...)

  # Argument matching for type
  valid_types <- c("potential_outcome", "if", "components", "blip_ref", "blip_avg")
  type <- rlang::arg_match(type, valid_types)

  # Use eif_pom as the influence function object
  if_object <- object$eif_pom

  switch(
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
}

#' @importFrom generics augment
#' @export
generics::augment