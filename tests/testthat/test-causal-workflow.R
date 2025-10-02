# Helper function to create a basic workflow for testing
make_wflow <- function() {
  workflowsets::workflow_set(
    preproc = list(mpg = recipes::recipe(mpg ~ cyl, mtcars)),
    models = list(lm = parsnip::linear_reg() |> parsnip::set_engine("lm"))
  )
}

test_that("causal_workflow constructor works", {
  res <- causal_workflow()
  expect_s3_class(res, "causal_workflow")
  expect_true(tibble::is_tibble(res))
  expect_equal(
    names(res),
    c("stage", "component_id", "component", "options", "result")
  )
  expect_equal(nrow(res), 0)
})

test_that("can add a component to a causal_workflow", {
  wf <- make_wflow()
  cwf <- causal_workflow() |>
    add_component("propensity", wf)

  expect_equal(nrow(cwf), 1)
  expect_equal(cwf$component_id, "propensity")
  expect_equal(cwf$stage, 1)
  expect_identical(cwf$component[[1]], wf)
})

test_that("can add a component with a specified stage", {
  wf <- make_wflow()
  cwf <- causal_workflow() |>
    add_component("propensity", wf, stage = 2)

  expect_equal(nrow(cwf), 1)
  expect_equal(cwf$stage, 2)
})

test_that("can add a stage to a causal_workflow", {
  wf1 <- make_wflow()
  wf2 <- make_wflow()
  cwf <- causal_workflow() |>
    add_component("propensity", wf1, stage = 1) |>
    add_stage(
      outcome = wf2,
      .stage = 2
    )

  expect_equal(nrow(cwf), 2)
  expect_equal(cwf$stage, c(1, 2))
  expect_equal(cwf$component_id, c("propensity", "outcome"))
})

test_that("add_stage requires .stage argument", {
  expect_snapshot(
    error = TRUE,
    causal_workflow() |> add_stage(outcome = make_wflow())
  )
})

test_that("add_stage prevents duplicate stages", {
  expect_snapshot(
    error = TRUE,
    causal_workflow() |>
      add_component("propensity", make_wflow(), stage = 1) |>
      add_stage(outcome = make_wflow(), .stage = 1)
  )
})

test_that("add_component prevents duplicate component_id within a stage", {
  expect_snapshot(
    error = TRUE,
    causal_workflow() |>
      add_component("propensity", make_wflow(), stage = 1) |>
      add_component("propensity", make_wflow(), stage = 1)
  )
})