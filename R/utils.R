# Re-exports
# ------------------------------------------------------------------------

# Imports
#' @importFrom cli cli_inform
#' @importFrom cli cli_warn
#' @importFrom cli cli_abort
#' @importFrom rlang caller_env %||%
#' @import workflows

# Global Variables
# ------------------------------------------------------------------------
utils::globalVariables(c(
  ":=",
  ".",
  ".config",
  ".metric",
  ".pred",
  ".pred_class",
  "across",
  "any_of",
  "as.formula",
  "contains",
  "data",
  "estimate",
  "id",
  "idx",
  "n",
  "name",
  "na.omit",
  "new",
  "rowid",
  "setNames",
  "splits",
  "starts_with",
  "type",
  "value",
  "where"
))

# Checks and Prompts
# ------------------------------------------------------------------------
# adapted from tune
check_empty_ellipses <- function(...) {
  dots <- rlang::enquos(...)
  if (length(dots) > 0) {
    needs_name <- names(dots) == ""
    names(dots)[needs_name] <-
      dots[needs_name] |>
      purrr::map(
        rlang::get_expr
      ) |>
      unlist()

    cli_warn(
      "The `...` are not used in this function but {?an/}
       argument{?s} {.arg {names(dots)}} {?was/were} passed."
    )
  }
  invisible(NULL)
}

check_inherits <- function(x, what, call = caller_env()) {
  cl <- match.call()

  if (!inherits(x, what)) {
    cli_abort(
      "Element {.val {cl$x}} needs to inherit from {.var {what}}, but its
       class is {.var {class(x)}}.",
      call = call
    )
  }

  invisible(TRUE)
}

# adapted from ps:::is_cran_check()
is_cran_check <- function() {
  if (identical(Sys.getenv("NOT_CRAN"), "true")) {
    FALSE
  } else {
    Sys.getenv("_R_CHECK_PACKAGE_NAME_", "") != ""
  }
}

# suggests: a character vector of package names, giving packages
#           listed in Suggests that are needed for the example.
# for use a la `@examplesIf (tune:::should_run_examples())`
should_run_examples <- function(suggests = NULL) {
  has_needed_installs <- TRUE

  if (!is.null(suggests)) {
    has_needed_installs <- rlang::is_installed(suggests)
  }

  has_needed_installs && !is_cran_check()
}
