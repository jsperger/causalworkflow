# Helper function to create a basic workflow for testing
make_wflow_with_recipe <- function() {
  rec <- recipes::recipe(mpg ~ cyl, mtcars)
  wf <- workflows::workflow() |>
    workflows::add_recipe(rec) |>
    workflows::add_model(parsnip::linear_reg() |> parsnip::set_engine("lm"))
  list(wf = wf, rec = rec)
}

test_that("extract_preprocessor can extract from a specific component and stage", {
  setup <- make_wflow_with_recipe()
  cwf <-
    causal_workflow(propensity = setup$wf) |>
    add_stage(outcome = setup$wf, .stage = 2)

  # Extract from stage 2
  extracted_rec <- extract_preprocessor(
    cwf,
    component_id = "outcome",
    stage = 2
  )
  expect_identical(extracted_rec, setup$rec)

  # Extract from stage 1
  extracted_rec_2 <- extract_preprocessor(
    cwf,
    component_id = "propensity",
    stage = 1
  )
  expect_identical(extracted_rec_2, setup$rec)
})


test_that("extract_preprocessor errors if component is not found", {
  setup <- make_wflow_with_recipe()
  cwf <- causal_workflow(propensity = setup$wf)

  expect_snapshot(
    error = TRUE,
    extract_preprocessor(cwf, component_id = "nonexistent")
  )
})

test_that("extract_preprocessor errors if stage is not found", {
  setup <- make_wflow_with_recipe()
  cwf <- causal_workflow(propensity = setup$wf)

  expect_snapshot(
    error = TRUE,
    extract_preprocessor(cwf, component_id = "propensity", stage = 99)
  )
})