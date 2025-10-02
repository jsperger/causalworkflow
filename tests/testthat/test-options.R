# Basic setup from the other test file
make_wflow <- function() {
  workflowsets::workflow_set(
    preproc = list(mpg = recipes::recipe(mpg ~ cyl, mtcars)),
    models = list(lm = parsnip::linear_reg() |> parsnip::set_engine("lm"))
  )
}

test_that("control_propensity creates a control object", {
  ctrl <- control_propensity(truncate = 0.05)
  expect_s3_class(ctrl, "causal_workflow_control_propensity")
  expect_equal(ctrl$truncate, 0.05)
})

test_that("control_outcome creates a control object", {
  tune_ctrl <- tune::control_grid()
  ctrl <- control_outcome(control_tune = tune_ctrl)
  expect_s3_class(ctrl, "causal_workflow_control_outcome")
  expect_identical(ctrl$control_tune, tune_ctrl)
})

test_that("control_targeting creates a control object", {
  ctrl <- control_targeting(fluctuation = "logistic")
  expect_s3_class(ctrl, "causal_workflow_control_targeting")
  expect_equal(ctrl$fluctuation, "logistic")
})


test_that("add_options adds a control object to the correct component", {
  wf <- make_wflow()
  ctrl <- control_propensity(truncate = 0.05)

  cwf <- causal_workflow(propensity = wf) |>
    add_options(component_id = "propensity", options = ctrl)

  expect_s3_class(cwf$options[[1]], "causal_workflow_control_propensity")
  expect_equal(cwf$options[[1]]$truncate, 0.05)
})

test_that("add_options can overwrite existing options", {
  wf <- make_wflow()
  ctrl1 <- control_propensity(truncate = 0.01)
  ctrl2 <- control_propensity(truncate = 0.05)

  cwf <- causal_workflow(propensity = wf) |>
    add_options(component_id = "propensity", options = ctrl1) |>
    add_options(component_id = "propensity", options = ctrl2)

  expect_equal(cwf$options[[1]]$truncate, 0.05)
})

test_that("add_options errors with an invalid component_id", {
  expect_snapshot(
    error = TRUE,
    causal_workflow() |>
      add_options(component_id = "nonexistent", options = control_propensity())
  )
})

test_that("add_options can target a specific stage", {
  wf <- make_wflow()
  ctrl <- control_propensity()

  cwf <- causal_workflow(propensity = wf) |>
    add_stage(propensity = wf, .stage = 2) |>
    add_options(component_id = "propensity", stage = 2, options = ctrl)

  expect_null(cwf$options[[1]])
  expect_s3_class(cwf$options[[2]], "causal_workflow_control_propensity")
})