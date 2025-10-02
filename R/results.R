#' Collect metrics from a fitted causal workflow
#'
#' @param x A `fitted_causal_workflow` object.
#' @param ... Not currently used.
#'
#' @return A tibble with the calculated causal estimates. The tibble will have
#'   the following columns:
#'   - `.metric`: The name of the metric (e.g., "ate").
#'   - `.estimator`: The name of the causal estimator used (e.g., "aipw").
#'   - `.estimate`: The numeric value of the estimate.
#' @export
collect_metrics <- function(x, ...) {
  UseMethod("collect_metrics")
}

#' @export
collect_metrics.fitted_causal_workflow <- function(x, ...) {
  # This is a simple implementation for the single estimate from .engine_aipw
  # It will be expanded as more complex engines are added.
  tibble::tibble(
    .metric = "ate",
    .estimator = "aipw",
    .estimate = x$estimate
  )
}

#' @export
collect_metrics.causal_workflow <- function(x, ...) {
  cli::cli_abort("Cannot collect metrics from an unfitted causal workflow.")
}