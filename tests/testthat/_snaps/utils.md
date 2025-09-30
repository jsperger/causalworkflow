# misc. utilities work

    Code
      check_inherits("howdy", "numeric")
    Condition
      Error:
      ! `"howdy"` must inherit from `numeric`, not `character`.

---

    Code
      res <- check_empty_ellipses(yall)
    Condition
      Warning:
      The `...` are not used in this function.

---

    Code
      res <- check_empty_ellipses(hey = yall, what = "is", going)
    Condition
      Warning:
      The `...` are not used in this function.

