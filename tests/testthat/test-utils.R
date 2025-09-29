test_that("misc. utilities work", {
  skip_on_cran()

  expect_snapshot(error = TRUE, check_inherits("howdy", "numeric"))
  expect_true(check_inherits("howdy", "character"))

  yall <- "y'all"

  expect_snapshot(
    res <- check_empty_ellipses(yall)
  )

  going <- "on"

  expect_snapshot(
    res <- check_empty_ellipses(hey = yall, what = "is", going)
  )
})