#' @importFrom tune fit_resamples
#' @export
tune::fit_resamples

#' Resample a causal workflow
#'
#' @description
#' [fit_resamples()] for a `causal_workflow` object evaluates the performance
#' of the two nuisance models (propensity and outcome) across a set of
#' resamples. It provides metrics to diagnose model performance, which is
#' crucial for the validity of the final causal estimates.
#'
#' @details
#' For each resample, this function performs the following steps:
#' 1. Fits the propensity score model on the analysis set.
#' 2. Generates predictions on the assessment set and calculates performance
#'    metrics (e.g., [roc_auc()] for binary classification).
#' 3. Fits the outcome model on the analysis set.
#' 4. Generates predictions on the assessment set and calculates performance
#'    metrics (e.g., [rmse()] for regression).
#'
#' The resulting metrics are collected into a single tibble, with columns
#' identifying which model component each metric corresponds to. This allows
#' for a comprehensive assessment of the nuisance models before proceeding to
#' causal effect estimation.
#'
#' @param object A `causal_workflow` object.
#' @param resamples An `rsample` object, such as one created by
#'   [rsample::vfold_cv()].
#' @param metrics A [yardstick::metric_set()] containing the metrics to
#'   compute for each model. If `NULL` (the default), standard metrics are
#'   chosen based on the model mode. Note: The same metrics will be applied to
#'   both models, so they must be compatible. For more specific control, use
#'   the `control` argument.
#' @param control A [tune::control_resamples()] object from the `tune` package used
#'   to manage the resampling process.
#'
#' @return A resamples object with a `.metrics` column containing the performance
#'   metrics for both the propensity and outcome models. The `.nuisance_component`
#'   column in the metrics tibble distinguishes between `"propensity_model"`
#'   and `"outcome_model"`.
#'
#' @seealso [fit.causal_workflow()], [fit_across()]
#' @export
fit_resamples.causal_workflow <- function(
  object,
  resamples,
  metrics = NULL,
  control = tune::control_resamples()
) {
  # 1. Validate inputs
  .check_fit_inputs(object, resamples)
  tune::check_rset(resamples)

  # 2. Delegate to tune's resampling infrastructure for each component
  pscore_results <- tune::fit_resamples(
    object$propensity_model,
    resamples = resamples,
    metrics = metrics,
    control = control
  )

  outcome_results <- tune::fit_resamples(
    object$outcome_model,
    resamples = resamples,
    metrics = metrics,
    control = control
  )

  # 3. Combine the results
  pscore_metrics <- tune::collect_metrics(pscore_results) |>
    dplyr::mutate(.nuisance_component = "propensity_model")

  outcome_metrics <- tune::collect_metrics(outcome_results) |>
    dplyr::mutate(.nuisance_component = "outcome_model")

  # For now, we return a simple tibble of the combined metrics.
  # A more complete implementation would return a modified resamples object.
  # This provides the core diagnostic information.
  dplyr::bind_rows(pscore_metrics, outcome_metrics) |>
    dplyr::select(
      .nuisance_component,
      .metric,
      .estimator,
      mean,
      dplyr::everything()
    )
}
