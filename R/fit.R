#' Fit a causal workflow
#'
#' @param object A `causal_workflow` object.
#' @param data A data frame containing the data for the analysis.
#' @param engine A character string specifying the estimation engine.
#'   Currently, only `"aipw"` is supported.
#' @param ... Additional arguments passed to the engine.
#'
#' @return A fitted `causal_workflow` object with the `result` column
#'   populated.
#' @export
fit.causal_workflow <- function(object, data, engine = "aipw", ...) {
  if (missing(engine)) {
    cli::cli_abort("The `engine` argument must be specified.")
  }
  checkmate::assert_string(engine)

  engine_fun <- paste0(".engine_", engine)
  if (!exists(engine_fun, envir = asNamespace("causalworkflows"), mode = "function")) {
    cli::cli_abort("Engine '{engine}' is not a recognized engine.")
  }

  engine_fun <- get(engine_fun, envir = asNamespace("causalworkflows"))
  engine_fun(object, data, ...)
}