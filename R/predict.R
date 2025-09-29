#' Predicting with a model stack
#'
#' Apply a model stack to create different types of predictions.
#'
#' @param object A model stack with fitted members outputted from [fit_members()].
#' @param new_data A rectangular data object, such as a data frame.
#' @param type Format of returned predicted values—one of "numeric", "class",
#'   or "prob". When NULL, `predict()` will
#'   choose an appropriate value based on the model's mode.
#' @param members Logical. Whether or not to additionally return the predictions
#'   for each of the ensemble members.
#' @param opts A list of optional arguments to the underlying predict
#'   function passed on to [parsnip::predict.model_fit] for each member.
#' @inheritParams stacks
#'
#' @template note_example_data
#'
#' @examplesIf (stacks:::should_run_examples(suggests = c("ranger", "kernlab")))
#'
#' # see the "Example Data" section above for
#' # clarification on the data and tuning results
#' # objects used in these examples!
#'
#' data(tree_frogs_reg_test)
#' data(tree_frogs_class_test)
#'
#' # build and fit a regression model stack
#' reg_st <-
#'   stacks() |>
#'   add_candidates(reg_res_lr) |>
#'   add_candidates(reg_res_sp) |>
#'   blend_predictions() |>
#'   fit_members()
#'
#' reg_st
#'
#' # predict on the tree frogs testing data
#' predict(reg_st, tree_frogs_reg_test)
#'
#' # include the predictions from the members
#' predict(reg_st, tree_frogs_reg_test, members = TRUE)
#'
#' # build and fit a classification model stack
#' class_st <-
#'   stacks() |>
#'   add_candidates(class_res_nn) |>
#'   add_candidates(class_res_rf) |>
#'   blend_predictions() |>
#'   fit_members()
#'
#' class_st
#'
#' # predict reflex, first as a class, then as
#' # class probabilities
#' predict(class_st, tree_frogs_class_test)
#' predict(class_st, tree_frogs_class_test, type = "prob")
#'
#' # returning the member predictions as well
#' predict(
#'   class_st,
#'   tree_frogs_class_test,
#'   type = "prob",
#'   members = TRUE
#' )
#'
#' @importFrom stats predict
#' @method predict model_stack
#' @export predict.model_stack
#' @export
predict.model_stack <- function(
  object,
  new_data,
  type = NULL,
  members = FALSE,
  opts = list(),
  ...
) {
  check_fitted(object)
  type <- check_pred_type(object, type)
  check_inherits(members, "logical")
  check_inherits(opts, "list")

  coefs <-
    .get_glmn_coefs(object[["coefs"]][["fit"]]) |>
    dplyr::select(terms, estimate)

  member_type <-
    switch(type, class = , prob = "prob", numeric = "numeric")

  member_preds <-
    rlang::call2(
      paste0("predict_members_", object[["mode"]]),
      model_stack = object,
      coefs = coefs,
      new_data = new_data,
      opts = opts,
      type = member_type
    ) |>
    rlang::eval_tidy()

  member_preds <- setNames(member_preds, make.names(names(member_preds)))

  res <- stack_predict(object$equations[[type]], member_preds)

  if (members) {
    if (type == "class") {
      member_preds <-
        purrr::map_dfc(
          names(object[["member_fits"]]),
          parse_member_probs,
          member_preds,
          levels(object[["data_stack"]][[object[["outcome"]]]])
        )
    }
    res <- dplyr::bind_cols(res, member_preds)
  }

  res
}

#' Predicting with a model stack
#'
#' @description
#' The data stack must be evaluated with [blend_predictions()] and its member
#' models fitted with [fit_members()] to predict on new data.
#'
#' @param object A data stack.
#' @inheritParams stacks
#'
#' @importFrom stats predict
#' @method predict data_stack
#' @export predict.data_stack
#' @export
predict.data_stack <- function(object, ...) {
  cli_abort(
    "To predict with a stacked ensemble, the supplied data stack must be 
     evaluated with `blend_predictions()` and its member models fitted with 
     `fit_members()` to predict on new data."
  )
}

check_pred_type <- function(object, type) {
  if (is.null(type)) {
    if (object[["mode"]] == "regression") {
      type <- "numeric"
    } else {
      type <- "class"
    }
  }

  type
}

predict_members_regression <- function(
  model_stack,
  coefs,
  new_data,
  opts,
  type
) {
  predictions <-
    purrr::map(
      model_stack[["member_fits"]],
      predict,
      new_data = new_data,
      type = "numeric",
      opts = opts
    ) |>
    purrr::map(dplyr::pull) |>
    tibble::as_tibble()

  predictions
}

predict_members_classification <- function(
  model_stack,
  coefs,
  new_data,
  opts,
  type
) {
  member_preds <-
    purrr::map(
      model_stack[["member_fits"]],
      predict,
      new_data = new_data,
      type = "prob",
      opts = opts
    ) |>
    purrr::map(tibble::rowid_to_column) |>
    tibble::enframe() |>
    tidyr::unnest(cols = value)
  member_preds <- 
    member_preds |>
    tidyr::pivot_wider(
      id_cols = rowid,
      names_from = name,
      values_from = 3:ncol(member_preds)
    ) |>
    dplyr::select(-rowid)

  member_preds
}

parse_member_probs <- function(member_name, member_probs, levels) {
  member_probs[, grepl(member_name, colnames(member_probs))] |>
    multi_net_helper() |>
    dplyr::transmute(
      !!paste0(".pred_class_", member_name) := factor(
        levels[idx],
        levels = levels
      )
    )
}

check_fitted <- function(model_stack, call = caller_env()) {
  if (is.null(model_stack[["member_fits"]])) {
    cli_abort(
      "The supplied model stack hasn't been fitted yet. 
       Please fit the necessary members with fit_members() to predict on new data.",
      call = call
    )
  }
}

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
#'     for each treatment level.
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

  switch(
    type,
    "potential_outcome" = {
      res <- dplyr::left_join(object$estimates, object$variances, by = "level")
      res <- dplyr::mutate(res, .std_err = sqrt(.data$.variance))
      dplyr::select(res, "level", ".pred", ".std_err")
    },
    "if" = {
      object$eif
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

      eif_ref_col <- paste0("eif_", ref_level)
      eif_ref <- object$eif[[eif_ref_col]]
      contrast_levels <- setdiff(object$treatment_levels, ref_level)

      purrr::map_dfr(contrast_levels, function(lvl) {
        eif_lvl_col <- paste0("eif_", lvl)
        eif_lvl <- object$eif[[eif_lvl_col]]
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
      avg_eif <- rowMeans(object$eif, na.rm = TRUE)

      purrr::map_dfr(object$treatment_levels, function(lvl) {
        eif_lvl_col <- paste0("eif_", lvl)
        eif_lvl <- object$eif[[eif_lvl_col]]
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

#' Augment a model stack
#'
#' @param x A fitted model stack; see [fit_members()].
#' @inheritParams predict.model_stack
#' @param ... Additional arguments passed to `predict.model_stack`. In
#' particular, see `type` and `members`.
#'
#' @seealso The [collect_parameters()] function is analogous to a [`tidy()`][generics::tidy()]
#' method for model stacks.
#'
#' @method augment model_stack
#' @name augment.model_stack
#' @export
augment.model_stack <- function(x, new_data, ...) {
  dots <- list(...)
  outcome <- x[["outcome"]]
  member_cols <- unlist(x[["cols_map"]])

  res <- dplyr::bind_cols(new_data, predict(x, new_data = new_data, ...))

  if (mode_is_regression(x) & isTRUE(dots[["members"]])) {
    res <- dplyr::rename_with(
      res,
      ~ paste0(".pred_", .x),
      any_of(unname(member_cols))
    )
  }

  if (mode_is_regression(x) & outcome %in% colnames(new_data)) {
    res <-
      dplyr::mutate(
        res,
        across(
          starts_with(".pred"),
          ~ !!rlang::sym(outcome) - .x,
          .names = ".resid{gsub('.pred', '', .col)}"
        )
      )
  }

  tibble::as_tibble(res)
}
