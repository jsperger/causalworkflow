# misc. utilities work

    Code
      check_inherits("howdy", "numeric")
    Condition
      Error:
      ! An object has the wrong class.
      i Expected an object inheriting from {.cls numeric}.
      x Instead, it has class {.cls character}.

---

    Code
      res <- check_empty_ellipses(yall)
    Condition
      Warning:
      The `...` are not used in this function but an argument `yall` was passed.

---

    Code
      res <- check_empty_ellipses(hey = yall, what = "is", going)
    Condition
      Warning:
      The `...` are not used in this function but arguments `hey`, `what`, and `going` were passed.

