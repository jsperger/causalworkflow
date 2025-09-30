.max_or_na <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  max(x, na.rm = TRUE)
}

check_empty_ellipses <- function(..., call = rlang::caller_env()) {
  if (rlang::dots_n(...) > 0) {
    rlang::warn("The `...` are not used in this function.", call = call)
  }
  invisible(NULL)
}

check_inherits <- function(x, what, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  if (!inherits(x, what)) {
    rlang::abort(
      paste0("`", arg, "` must inherit from `", what, "`, not `", class(x)[1], "`."),
      call = call
    )
  }
  invisible(TRUE)
}