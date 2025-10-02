#' @details
#' `causalworkflows` is an R package for estimating causal effects using
#' multi-stage, modular modeling procedures that align with the `tidymodels`
#' framework. The package provides a flexible structure for defining and
#' fitting causal estimators that rely on nuisance models, such as the
#' propensity score and outcome models.
#'
#' The core of the package is the `causal_workflow()` object, which allows you
#' to specify separate `tidymodels` workflows for each component of your
#' estimator. The package can then be used to construct robust estimates of
#' causal effects, such as the Average Treatment Effect (ATE), using estimators
#' like the Augmented Inverse Propensity Weighting (AIPW) estimator.
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom generics augment
#' @importFrom generics glance
#' @importFrom generics tidy
## usethis namespace: end
NULL