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
#' The best-performing model or the fitted ensemble from the inner loop is then
#' used to generate out-of-sample predictions for the outer loop's assessment
#' set. These predictions are used to calculate the final, doubly robust causal
#' effect estimates.
#'
#' @param object A [causal_workflow()] object.
#' @param resamples An `rsample` object for the outer folds of nested
#'   resampling, such as one created by [rsample::vfold_cv()].
#' @param ... Additional arguments passed to the underlying tuning or fitting
#'   functions.
#' @param inner_v The number of folds for the inner resampling loop.
#' @param metric A character string for the metric to optimize during tuning.
#'
#' @return A `fitted_causal_workflow` object.
#'
#' @export
tune_nested <- function(
  object,
  resamples,
  ...,
  inner_v = 5,
  metric = NULL
) {
  # 1. Validate inputs
  .check_fit_inputs(object, resamples$splits[[1]]$data)
  tune::check_rset(resamples)

  # 2. Extract workflows and variable names
  pscore_spec <- object$propensity_model
  outcome_spec <- object$outcome_model

  treatment_formula <- hardhat::extract_preprocessor(pscore_spec)
  treatment_var <- rlang::f_lhs(treatment_formula) |> rlang::as_name()
  outcome_formula <- hardhat::extract_preprocessor(outcome_spec)
  outcome_var <- rlang::f_lhs(outcome_formula) |> rlang::as_name()

  data <- resamples$splits[[1]]$data
  data[[treatment_var]] <- as.factor(data[[treatment_var]])
  treatment_levels <- levels(data[[treatment_var]])

  # 3. Perform nested cross-fitting to get nuisance predictions
  nuisance_preds <-
    purrr::map(
      resamples$splits,
      ~ .fit_predict_one_nested_fold(
        split = .x,
        pscore_spec = pscore_spec,
        outcome_spec = outcome_spec,
        inner_v = inner_v,
        treatment_var = treatment_var,
        treatment_levels = treatment_levels,
        metric = metric
      )
    ) |>
    purrr::list_rbind()

  # 4. Join predictions back to original data
  data_with_row <- resamples$splits[[1]]$data |>
    dplyr::mutate(.row = dplyr::row_number())
  data_with_preds <-
    dplyr::left_join(data_with_row, nuisance_preds, by = ".row")

  # 5. Calculate EIF for the Potential Outcome Mean (POM)
  eif_tibble <- .calculate_eif_pom(
    data = data_with_preds,
    treatment_var = treatment_var,
    outcome_var = outcome_var,
    treatment_levels = treatment_levels
  )

  # 6. Calculate potential outcome estimates and their variance
  potential_outcomes <- colMeans(eif_tibble, na.rm = TRUE)
  variance_estimates <- apply(eif_tibble, 2, stats::var, na.rm = TRUE) /
    colSums(!is.na(eif_tibble))

  # 7. Fit final models on full data
  final_g_fit <- .fit_nuisance_spec(pscore_spec, resamples, data, metric)
  final_q_fit <- .fit_nuisance_spec(outcome_spec, resamples, data, metric)

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
  treatment_levels,
  metric
) {
  analysis_data <- rsample::analysis(split)
  assessment_data <- rsample::assessment(split)

  inner_folds <- rsample::vfold_cv(analysis_data, v = inner_v)

  g_fit <- .fit_nuisance_spec(pscore_spec, inner_folds, analysis_data, metric)
  q_fit <- .fit_nuisance_spec(outcome_spec, inner_folds, analysis_data, metric)

  nuisance_preds <- .get_nuisance_preds(
    g_fit = g_fit,
    q_fit = q_fit,
    data = assessment_data,
    treatment_var = treatment_var,
    treatment_levels = treatment_levels
  )

  tibble::tibble(.row = assessment_data$.row) |>
    dplyr::bind_cols(nuisance_preds)
}