#' Toy Two-Stage Trial Datasets
#'
#' These datasets are provided only to facilitate examples. They are not based
#'   on or representative of any real-world applications.
#'
#' @name data
#' @rdname data
#'
#' @usage data(twoStageCont)
#'
#' @format twoStageCont is a dataset generated to mimic a simple two-stage
#'   trial. It is taken from the `DTRreg` package for concordance testing. The data.frame contains 1000 observations with 5 columns:
#' \describe{
#' \item{X1}{The first stage covariate. A normally distributed continuous variable.}
#' \item{A1}{The first stage treatment. A binary variable.}
#' \item{X2}{The second stage covariate. A normally distributed continuous variable.}
#' \item{A2}{The second stage treatment. A binary variable.}
#' \item{Y}{The outcome. A continuous variable.}
#' }
#'
#' @keywords datasets
"twoStageCont"


#' @noRd
#' @keywords internal
.generateTwoStageCont <- function(seed = 1234L, n = 1000L) {
  set.seed(seed)

  # expit function
  expit <- function(x) {
    1.0 / (1.0 + exp(-x))
  }

  # variables (X = patient information, A = treatment)
  X1 <- rnorm(n)
  A1 <- rbinom(n, 1, expit(X1))
  X2 <- rnorm(n)
  A2 <- rbinom(n, 1, expit(X2))

  # blip functions
  gamma1 <- A1 * (1 + X1)
  gamma2 <- A2 * (1 + X2)

  # observed outcome: treatment-free outcome plus blip functions
  Y <- exp(X1) + exp(X2) + gamma1 + gamma2 + rnorm(n)

  data.frame("X1" = X1, "A1" = A1, "X2" = X2, "A2" = A2, "Y" = Y)
}


#' Example Objects
#'
#' stacks provides some resampling objects and datasets for use in examples
#' and vignettes derived from a study on 1212 red-eyed tree frog embryos!
#'
#' Red-eyed tree frog (RETF) embryos can hatch earlier than their normal
#' 7ish days if they detect potential predator threat. Researchers wanted
#' to determine how, and when, these tree frog embryos were able to detect
#' stimulus from their environment. To do so, they subjected the embryos
#' at varying developmental stages to "predator stimulus" by jiggling
#' the embryos with a blunt probe. Beforehand, though some of the embryos
#' were treated with gentamicin, a compound that knocks out their lateral
#' line (a sensory organ.) Researcher Julie Jung and her crew found that
#' these factors inform whether an embryo hatches prematurely or not!
#'
#' Note that the data included with the stacks package is not necessarily
#' a representative or unbiased subset of the complete dataset, and is
#' only for demonstrative purposes.
#'
#' `reg_folds` and `class_folds` are `rset` cross-fold validation objects
#' from `rsample`, splitting the training data into for the regression
#' and classification model objects, respectively. `tree_frogs_reg_test` and
#' `tree_frogs_class_test` are the analogous testing sets.
#'
#' `reg_res_lr`, `reg_res_svm`, and `reg_res_sp` contain regression tuning results
#' for a linear regression, support vector machine, and spline model, respectively,
#' fitting \code{latency} (i.e. how long the embryos took to hatch in response
#' to the jiggle) in the \code{tree_frogs} data, using most all of the other
#' variables as predictors. Note that the data underlying these models is
#' filtered to include data only from embryos that hatched in response to
#' the stimulus.
#'
#' `class_res_rf` and `class_res_nn` contain multiclass classification tuning
#' results for a random forest and neural network classification model,
#' respectively, fitting \code{reflex} (a measure of ear function) in the
#' data using most all of the other variables as predictors.
#'
#' `log_res_rf` and `log_res_nn`, contain binary classification tuning results
#' for a random forest and neural network classification model, respectively,
#' fitting \code{hatched} (whether or not the embryos hatched in response
#' to the stimulus) using most all of the other variables as predictors.
#'
#' The source code for generating these objects is given below.
#'
#' @includeRmd man-roxygen/example_models.Rmd
#'
#' @source
#' Julie Jung et al. (2020) Multimodal mechanosensing enables treefrog
#' embryos to escape egg-predators. \doi{10.1242/jeb.236141}
#'
#' @name example_data
NULL

#' @rdname example_data
"reg_res_svm"
#' @rdname example_data
"reg_res_sp"
#' @rdname example_data
"reg_res_lr"
#' @rdname example_data
"reg_folds"
#' @rdname example_data
"class_res_nn"
#' @rdname example_data
"class_res_rf"
#' @rdname example_data
"class_folds"
#' @rdname example_data
"log_res_nn"
#' @rdname example_data
"log_res_rf"

#' @name tree_frogs_reg_test
#' @docType data
#' @keywords datasets
#' @rdname example_data
NULL

#' @name tree_frogs_class_test
#' @docType data
#' @keywords datasets
#' @rdname example_data
NULL
