#' @importFrom tune tune_grid
#' @export
tune::tune_grid

#' Tune and estimate a causal workflow with nested resampling
#'
#' @description
#' [tune_nested()] for a [causal_workflow()] object performs nested resampling to
#' either tune the hyperparameters of the nuisance models or to fit an ensemble
#' of models using `stacks`. It provides robust, unbiased causal effect
#' estimates for complex, regularized, or non-parametric models that require
#' tuning.
#'
#' @details
#' Nested resampling is essential when a modeling process involves both
#' hyperparameter tuning and estimation of a final causal effect. It consists
#' of two loops:
#' - An **outer loop** that splits the data into folds for final estimation.
#'   The models are never trained on the assessment sets of these folds,
#'   ensuring the final estimates are unbiased.
#' - An **inner loop** that uses the analysis sets from the outer loop to
#'   perform hyperparameter tuning or ensemble fitting.
#'
#' This function handles two main scenarios:
#' 1.  When a component (propensity or outcome model) is a single [workflows::workflow()]
#'     with tunable hyperparameters, `tune_nested()` uses the inner resamples
#'     to run [tune::tune_grid()] to find the best hyperparameter combination.
#' 2.  When a component is a [workflowsets::workflow_set()], `tune_nested()` uses the inner
#'     resamples to fit an ensemble using the `stacks` package. It trains each
#'     candidate model in the set, blends their predictions into a stacked
#'     ensemble, and fits the final members.
#'
#' The best-performing model or the fitted ensemble from the inner loop is then
#' used to generate out-of-sample predictions for the outer loop's assessment
#' set. These predictions are used to calculate the final, doubly robust causal
#' effect estimates.
#'
#' @inheritParams fit_across.causal_workflow
#' @param object A [causal_workflow()] object.
#' @param resamples An `rsample` object for the outer folds of nested
#'   resampling, such as one created by [rsample::vfold_cv()].
#' @param inner_resamples An `rsample` object for the inner folds, which will
#'   be created from the analysis set of each outer fold.
#' @param ... Additional arguments passed to the underlying tuning or fitting
#'   functions.
#'
#' @return A `fitted_causal_workflow` object, similar to [fit_across()], but
#'   where the nuisance predictions are generated from models that have been
#'   tuned or ensembled within the nested resampling procedure.
#'
#' @export
tune_nested <- function(object, ...) {
  UseMethod("tune_nested")
}

#' @export
tune_nested.causal_workflow <- function(
  object,
  resamples,
  treatment_var,
  outcome_var,
  inner_v = 5,
  ...
) {
  # 1. Validate inputs
  .check_fit_inputs(object, resamples)
  tune::check_rset(resamples)
  if (!is.numeric(inner_v) || inner_v < 2) {
    cli::cli_abort(
      "{.arg inner_v} must be an integer greater than or equal to 2, not {.val {inner_v}}."
    )
  }
  # treatment_var and outcome_var are now required arguments

  # 2. Extract workflows and variable names
  pscore_spec <- object$propensity_model
  outcome_spec <- object$outcome_model

  # Ensure treatment is a factor and get levels for counterfactual prediction
  data <- resamples$splits[[1]]$data # Use first split to get variable info
  data[[treatment_var]] <- as.factor(data[[treatment_var]])
  treatment_levels <- levels(data[[treatment_var]])

  # 3. Perform nested cross-fitting to get nuisance predictions
  nuisance_preds <-
    purrr::map_dfr(
      resamples$splits,
      ~ .fit_predict_one_nested_fold(
        split = .x,
        pscore_spec = pscore_spec,
        outcome_spec = outcome_spec,
        inner_v = inner_v,
        treatment_var = treatment_var,
        treatment_levels = treatment_levels
      )
    )

  # 4. Join predictions back to original data
  # This relies on the data used to create resamples having a .row column
  data_with_row <- resamples$splits[[1]]$data
  data_with_preds <-
    dplyr::left_join(data_with_row, nuisance_preds, by = ".row")

  # 5. Calculate EIF for the Potential Outcome Mean (POM)
  Y <- data_with_preds[[outcome_var]]
  A <- data_with_preds[[treatment_var]]

  eif_list <-
    purrr::map(
      treatment_levels,
      function(lvl) {
        g_hat_lvl <- data_with_preds[[paste0("g_hat_", lvl)]]
        q_hat_lvl <- data_with_preds[[paste0("q_hat_", lvl)]]
        indicator <- as.numeric(A == lvl)

        eif_pom <- (indicator / g_hat_lvl) * (Y - q_hat_lvl) + q_hat_lvl
        eif_pom
      }
    )

  names(eif_list) <- paste0("eif_pom_", treatment_levels)
  eif_tibble <- tibble::as_tibble(eif_list)

  # 6. Calculate potential outcome estimates and their variance
  potential_outcomes <- colMeans(eif_tibble, na.rm = TRUE)
  variance_estimates <- apply(eif_tibble, 2, stats::var, na.rm = TRUE) /
    colSums(!is.na(eif_tibble))

  # 7. Fit final models on full data (using the tuned/ensembled spec)
  # This part is complex: for now, we return NULL for the final fits,
  # as the primary output is the robust estimate. A full implementation
  # would re-run the tuning/stacking on the full dataset.
  final_g_fit <- NULL
  final_q_fit <- NULL

  # 8. Construct return object
  estimates_tbl <- tibble::enframe(
    potential_outcomes,
    name = "level",
    value = ".pred"
  ) |>
    dplyr::mutate(level = sub("eif_pom_", "", level))

  variances_tbl <- tibble::enframe(
    variance_estimates,
    name = "level",
    value = ".variance"
  ) |>
    dplyr::mutate(level = sub("eif_pom_", "", level))

  fitted_obj <-
    list(
      propensity_model_fit = final_g_fit,
      outcome_model_fit = final_q_fit,
      original_workflows = object,
      treatment_levels = treatment_levels,
      estimates = estimates_tbl,
      variances = variances_tbl,
      eif_pom = eif_tibble,
      nuisance_predictions = data_with_preds |> dplyr::select(-.row)
    )

  class(fitted_obj) <- "fitted_causal_workflow"

  return(fitted_obj)
}

# Helper for one outer fold of nested resampling
.fit_predict_one_nested_fold <- function(
  split,
  pscore_spec,
  outcome_spec,
  inner_v,
  treatment_var,
  treatment_levels
) {
  analysis_data <- rsample::analysis(split)
  assessment_data <- rsample::assessment(split)

  # Create inner folds from the analysis data
  inner_folds <- rsample::vfold_cv(analysis_data, v = inner_v)

  # Fit nuisance models using the inner folds for tuning/stacking
  g_fit <- .fit_nuisance_spec(pscore_spec, inner_folds, analysis_data)
  q_fit <- .fit_nuisance_spec(outcome_spec, inner_folds, analysis_data)

  # Generate predictions on the assessment set
  g_preds <- predict(g_fit, new_data = assessment_data, type = "prob") |>
    dplyr::rename_with(
      ~ paste0("g_hat_", sub(".pred_", "", .x)),
      .cols = dplyr::starts_with(".pred_")
    )

  q_hat_preds <-
    purrr::map(
      treatment_levels,
      function(lvl) {
        counterfactual_data <- assessment_data
        counterfactual_data[[treatment_var]] <- factor(
          lvl,
          levels = treatment_levels
        )
        predict(q_fit, new_data = counterfactual_data) |>
          dplyr::rename(!!paste0("q_hat_", lvl) := .pred)
      }
    ) |>
    dplyr::bind_cols()

  tibble::tibble(.row = assessment_data$.row) |>
    dplyr::bind_cols(g_preds) |>
    dplyr::bind_cols(q_hat_preds)
}

# Helper to fit a nuisance model spec (workflow or workflow_set)
.fit_nuisance_spec <- function(spec, resamples, training_data) {
  if (inherits(spec, "workflow")) {
    # If it's a workflow, tune it if it has tunable parameters
    if (nrow(tune::tunable(spec)) > 0) {
      # tune_grid uses a default grid if not specified
      tuned <- tune::tune_grid(spec, resamples = resamples)
      best_params <- tune::select_best(tuned)
      tune::finalize_workflow(spec, best_params) |>
        parsnip::fit(data = training_data)
    } else {
      # Otherwise, just fit it on the full analysis set
      parsnip::fit(spec, data = training_data)
    }
  } else if (inherits(spec, "workflow_set")) {
    # If it's a workflow_set, build a stack
    wf_set_trained <-
      workflowsets::workflow_map(
        spec,
        "tune_grid",
        resamples = resamples,
        control = stacks::control_stack_grid(),
        verbose = FALSE
      )

    stacks::stacks() |>
      stacks::add_candidates(wf_set_trained) |>
      stacks::blend_predictions() |>
      stacks::fit_members()
  }
}
