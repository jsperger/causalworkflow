# AIPW Engine
#
# This is a simplified, non-cross-validated implementation of the AIPW
# estimator.
.engine_aipw <- function(cwf, data, ...) {
  # 1. Extract propensity and outcome workflows
  propensity_wf <- purrr::pluck(
    cwf,
    "component",
    which(cwf$component_id == "propensity" & cwf$stage == 1)
  )
  outcome_wf <- purrr::pluck(
    cwf,
    "component",
    which(cwf$component_id == "outcome" & cwf$stage == 1)
  )

  if (rlang::is_empty(propensity_wf)) {
    cli::cli_abort("A 'propensity' component is required for the AIPW engine.")
  }
  if (rlang::is_empty(outcome_wf)) {
    cli::cli_abort("An 'outcome' component is required for the AIPW engine.")
  }

  # 2. Fit the nuisance models
  propensity_fit <- parsnip::fit(propensity_wf, data = data)
  outcome_fit <- parsnip::fit(outcome_wf, data = data)

  # 3. Extract variable names and treatment levels
  # This is brittle and will be improved later.
  outcome_var <- outcome_wf$pre$actions$formula$formula[[2]] |> as.character()
  treatment_var <- propensity_wf$pre$actions$formula$formula[[2]] |> as.character()

  if (!is.factor(data[[treatment_var]])) {
    cli::cli_abort("{.var {treatment_var}} must be a factor.")
  }
  lvls <- levels(data[[treatment_var]])
  if (length(lvls) != 2) {
    cli::cli_abort("{.var {treatment_var}} must have exactly two levels.")
  }

  # 4. Generate nuisance predictions
  g_hat_1 <- predict(propensity_fit, new_data = data, type = "prob") |>
    dplyr::pull(paste0(".pred_", lvls[2]))

  data_1 <- data
  data_1[[treatment_var]] <- factor(lvls[2], levels = lvls)
  q_hat_1 <- predict(outcome_fit, new_data = data_1) |> dplyr::pull(.data$.pred)

  data_0 <- data
  data_0[[treatment_var]] <- factor(lvls[1], levels = lvls)
  q_hat_0 <- predict(outcome_fit, new_data = data_0) |> dplyr::pull(.data$.pred)

  y <- data[[outcome_var]]
  a <- as.numeric(data[[treatment_var]] == lvls[2])

  # 5. Calculate AIPW estimate
  prop_opts <- purrr::pluck(cwf, "options", which(cwf$component_id == "propensity"))
  truncate_val <- purrr::pluck(prop_opts[[1]], "truncate", .default = 0.01)
  g_hat_1 <- pmax(pmin(g_hat_1, 1 - truncate_val), truncate_val)

  mu_1 <- mean(a / g_hat_1 * (y - q_hat_1) + q_hat_1)
  mu_0 <- mean((1 - a) / (1 - g_hat_1) * (y - q_hat_0) + q_hat_0)
  ate_estimate <- mu_1 - mu_0

  # 6. Construct fitted object
  res <- list(
    causal_workflow = cwf,
    propensity_fit = propensity_fit,
    outcome_fit = outcome_fit,
    estimate = ate_estimate
  )
  class(res) <- "fitted_causal_workflow"

  res
}