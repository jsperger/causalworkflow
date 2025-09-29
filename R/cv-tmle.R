#' Estimate the value of a dynamic treatment regime using CV-TMLE
#'
#' @description
#' `cv_tmle()` implements the Cross-Validated Targeted Maximum Likelihood
#' Estimator (CV-TMLE) for the value of an estimated two-stage optimal
#' Dynamic Treatment Regime (DTR).
#'
#' @details
#' This function performs K-fold cross-validation to estimate the value of a
#' DTR. The process involves:
#' 1.  **Initial Nuisance Estimation**: Within each training fold, it fits
#'     stage-specific models for the Q-functions (outcome models) and
#'     propensity scores (treatment models) using the provided `staged_workflow`
#'     objects.
#' 2.  **DTR Estimation**: The fitted Q-models are used to derive an estimate
#'     of the optimal DTR for that fold.
#' 3.  **Targeting (Fluctuation)**: On the corresponding validation fold, the
#'     initial Q-function estimates are updated (fluctuated) sequentially,
#'     using "clever covariates" derived from the propensity scores and the
#'     estimated DTR. This step aims to reduce bias in the final estimate.
#' 4.  **Pooling and Inference**: The targeted estimates from all folds are
#'     pooled to compute the final CV-TMLE point estimate. Inference is based
#'     on the empirical variance of the estimated influence curve (IC).
#'
#' The function supports both standard `tidymodels` workflows and `workflow_sets`
#' for ensembling nuisance models via the `stacks` package, adapting the
#' nested resampling logic from `tune_nested()`.
#'
#' @param data A `data.frame` containing the observational data.
#' @param resamples An `rsample` object for K-fold cross-validation, such as
#'   one created by `rsample::vfold_cv()`.
#' @param q_models A `staged_workflow` object defining the Q-function models
#'   for each stage.
#' @param g_models A `staged_workflow` object defining the propensity score
#'   models for each stage.
#' @param actions A character vector of the column names for the treatment
#'   variables at each stage (e.g., `c("A_1", "A_2")`).
#' @param outcome A character string of the column name for the final outcome
#'   variable (e.g., `"Y"`).
#' @param ... Additional arguments passed to underlying fitting functions.
#'
#' @return
#' A `cv_tmle_fit` object containing:
#' - `.estimate`: The final CV-TMLE point estimate of the DTR's value.
#' - `.se`: The standard error of the estimate.
#' - `.conf.low`, `.conf.high`: The 95% confidence interval.
#' - `influence_curve`: A tibble with the estimated influence curve for each
#'   observation.
#' - `nuisance_predictions`: A tibble with the out-of-sample nuisance
#'   predictions from all folds.
#' @export
cv_tmle <- function(data, resamples, q_models, g_models, actions, outcome, inner_v = 5, ...) {
  # 1. Input validation
  # TODO: Add comprehensive input checks

  # 2. Iterate over each fold to get targeted predictions and IC components
  fold_results <- purrr::map_dfr(
    resamples$splits,
    ~ .process_one_fold(
      split = .x,
      q_models = q_models,
      g_models = g_models,
      actions = actions,
      outcome = outcome,
      inner_v = inner_v,
      ...
    )
  ) |>
    # Ensure results are ordered by original row index for correct IC calculation
    dplyr::arrange(.row)


  # 3. Calculate final estimate and inference from pooled results
  V_hat_cvtmle <- mean(fold_results$Q1_star_max, na.rm = TRUE)

  # Combine IC components and subtract the final estimate for the full IC
  ic_full <- (fold_results$D2 + fold_results$D1 + fold_results$D0) - V_hat_cvtmle

  n_obs <- nrow(fold_results)
  var_hat <- stats::var(ic_full, na.rm = TRUE) / n_obs
  se_hat <- sqrt(var_hat)

  # 4. Construct final result object
  result <- list(
    .estimate = V_hat_cvtmle,
    .se = se_hat,
    .conf.low = V_hat_cvtmle - 1.96 * se_hat,
    .conf.high = V_hat_cvtmle + 1.96 * se_hat,
    influence_curve = tibble::tibble(.row = fold_results$.row, ic = ic_full),
    nuisance_predictions = fold_results
  )

  class(result) <- "cv_tmle_fit"
  result
}

#' Process a single fold for CV-TMLE
#'
#' This helper function executes the main steps of the CV-TMLE algorithm on a
#' single split of the data: initial estimation on the analysis set, and
#' targeting on the assessment set.
#'
#' @inheritParams cv_tmle
#' @param split An `rsplit` object from the main `resamples`.
#' @return A tibble containing the targeted predictions and influence curve
#'   components for the observations in the assessment set of the split.
.process_one_fold <- function(split, q_models, g_models, actions, outcome, inner_v, ...) {
  analysis_data <- rsample::analysis(split)
  assessment_data <- rsample::assessment(split)

  # Create inner folds for tuning/stacking
  inner_resamples <- rsample::vfold_cv(analysis_data, v = inner_v)

  # Step 1: Fit nuisance models and get initial predictions
  preds_init <- .fit_predict_nuisance_models(
    analysis_data = analysis_data,
    assessment_data = assessment_data,
    q_models = q_models,
    g_models = g_models,
    actions = actions,
    outcome = outcome,
    inner_resamples = inner_resamples
  )
  data_for_targeting <- dplyr::left_join(assessment_data, preds_init, by = ".row")

  # Step 2: Targeting (fluctuation) step
  targeted_results <- .target_nuisance_estimates(
    data = data_for_targeting,
    actions = actions,
    outcome = outcome
  )

  # Step 3: Calculate Influence Curve components
  .calculate_ic(
    data = targeted_results,
    actions = actions,
    outcome = outcome
  )
}

#' Calculate the components of the influence curve
#'
#' @param data A data frame containing the targeted estimates (`Q_star`) and
#'   other necessary variables.
#' @inheritParams cv_tmle
#' @return A tibble with the IC components for each observation.
.calculate_ic <- function(data, actions, outcome) {
  # Get observed Q_star values
  q2_star_observed <- .get_observed_prob(data, "q2_star", actions[2])
  q1_star_observed <- .get_observed_prob(data, "q1_star", actions[1])

  # Get maximized Q_star values
  q2_star_max <- data |>
    dplyr::select(dplyr::starts_with("q2_star_")) |>
    apply(1, max, na.rm = TRUE)
  q1_star_max <- data |>
    dplyr::select(dplyr::starts_with("q1_star_")) |>
    apply(1, max, na.rm = TRUE)

  # Calculate the three components of the EIF
  D2 <- data$C2 * (data[[outcome]] - q2_star_observed)
  D1 <- data$C1 * (q2_star_max - q1_star_observed)
  D0 <- q1_star_max # The final estimate is subtracted from this later

  data |>
    dplyr::mutate(
      Q1_star_max = q1_star_max,
      D2 = D2,
      D1 = D1,
      D0 = D0
    )
}

#' Fluctuate initial nuisance estimates using the targeting step
#'
#' @param data A data frame containing the assessment data for a fold, augmented
#'   with the initial nuisance predictions (`q_hat` and `g_hat`).
#' @inheritParams cv_tmle
#' @return A tibble containing the targeted Q-function estimates (`Q_star`) and
#'   other components needed for the final IC calculation.
.target_nuisance_estimates <- function(data, actions, outcome) {

  # --- 1. Derive Estimated Optimal DTR (pi-hat) from initial Q-hats ---
  q1_hat_cols <- data |> dplyr::select(dplyr::starts_with("q1_hat_"))
  pi_hat_1 <- colnames(q1_hat_cols)[apply(q1_hat_cols, 1, which.max)] |> sub("q1_hat_", "", x = _)

  q2_hat_cols <- data |> dplyr::select(dplyr::starts_with("q2_hat_"))
  pi_hat_2 <- colnames(q2_hat_cols)[apply(q2_hat_cols, 1, which.max)] |> sub("q2_hat_", "", x = _)

  data <- data |> dplyr::mutate(pi_hat_1 = pi_hat_1, pi_hat_2 = pi_hat_2)

  # --- 2. Calculate Clever Covariates (C1 and C2) ---
  # Get observed g_hats
  g1_hat_observed <- .get_observed_prob(data, model_prefix = "g1_hat", action_col = actions[1])
  g2_hat_observed <- .get_observed_prob(data, model_prefix = "g2_hat", action_col = actions[2])

  # Calculate C1 and C2
  C1 <- as.numeric(data[[actions[1]]] == data$pi_hat_1) / g1_hat_observed
  C2 <- C1 * (as.numeric(data[[actions[2]]] == data$pi_hat_2) / g2_hat_observed)
  data <- data |> dplyr::mutate(C1 = C1, C2 = C2)

  # --- 3. Sequential Targeting ---
  # ** Stage 2 Targeting (Q2_hat -> Q2_star) **
  q2_hat_observed <- .get_observed_prob(data, model_prefix = "q2_hat", action_col = actions[2])
  fluctuation_model_2 <- stats::glm(
    formula = as.formula(paste(outcome, "~ -1 + offset(q2_hat_observed) + C2")),
    data = data,
    family = "gaussian"
  )
  epsilon_2 <- stats::coef(fluctuation_model_2)

  # Update all Q2_hat estimates to get Q2_star
  q2_star_cols <- q2_hat_cols + (epsilon_2 * data$C2)
  colnames(q2_star_cols) <- sub("q2_hat", "q2_star", colnames(q2_hat_cols))

  # ** Stage 1 Targeting (Q1_hat -> Q1_star) **
  # Calculate pseudo-outcome using *targeted* Q2_star
  pseudo_outcome_1_targeted <- apply(q2_star_cols, 1, max, na.rm = TRUE)
  q1_hat_observed <- .get_observed_prob(data, model_prefix = "q1_hat", action_col = actions[1])
  fluctuation_model_1 <- stats::glm(
    formula = pseudo_outcome_1_targeted ~ -1 + offset(q1_hat_observed) + C1,
    family = "gaussian"
  )
  epsilon_1 <- stats::coef(fluctuation_model_1)

  # Update all Q1_hat estimates to get Q1_star
  q1_star_cols <- q1_hat_cols + (epsilon_1 * data$C1)
  colnames(q1_star_cols) <- sub("q1_hat", "q1_star", colnames(q1_hat_cols))


  # --- 4. Combine and return all results ---
  dplyr::bind_cols(
    data,
    q1_star_cols,
    q2_star_cols
  )
}

#' Helper to get the probability corresponding to the observed action
#' @param data A data frame.
#' @param model_prefix The prefix for the prediction columns (e.g., "g1_hat").
#' @param action_col The column name of the observed action.
#' @return A numeric vector of probabilities.
.get_observed_prob <- function(data, model_prefix, action_col) {
  pred_cols <- data |> dplyr::select(dplyr::starts_with(model_prefix))
  action_levels <- sub(paste0(model_prefix, "_"), "", colnames(pred_cols))

  # Create an index matrix for fast subsetting
  col_indices <- match(data[[action_col]], action_levels)
  row_indices <- seq_len(nrow(data))

  # Extract the probabilities using matrix indexing
  as.matrix(pred_cols)[cbind(row_indices, col_indices)]
}


#' Fit nuisance models on training data and predict on validation data
#'
#' @param analysis_data The training data for a fold.
#' @param assessment_data The validation data for a fold.
#' @inheritParams cv_tmle
#' @return A tibble of out-of-sample nuisance predictions for the assessment_data.
.fit_predict_nuisance_models <- function(analysis_data, assessment_data, q_models, g_models, actions, outcome, inner_resamples) {
  # --- Fit Q-models (sequentially, backwards from final stage) ---
  q_fit_2 <- .fit_nuisance_spec(
    spec = q_models$stages$`2`$wflow,
    resamples = inner_resamples,
    training_data = analysis_data
  )
  # Create pseudo-outcome for stage 1
  action_2_levels <- levels(analysis_data[[actions[2]]])
  q2_preds_train <-
    purrr::map(action_2_levels, function(lvl) {
      counterfactual_data <- analysis_data
      counterfactual_data[[actions[2]]] <- factor(lvl, levels = action_2_levels)
      predict(q_fit_2, new_data = counterfactual_data)
    }) |>
    dplyr::bind_cols() |>
    as.matrix()

  pseudo_outcome_1 <- apply(q2_preds_train, 1, max)
  analysis_data_stage1 <- analysis_data |>
    dplyr::mutate(.pseudo_outcome = pseudo_outcome_1)

  # Fit stage 1 Q-model using the pseudo-outcome
  orig_q1_wflow <- q_models$stages$`1`$wflow
  orig_q1_formula <- hardhat::extract_preprocessor(orig_q1_wflow)
  new_q1_formula <- rlang::new_formula(
    lhs = rlang::sym(".pseudo_outcome"),
    rhs = rlang::f_rhs(orig_q1_formula)
  )
  q1_wflow <- orig_q1_wflow |>
    workflows::update_formula(new_q1_formula)
  q_fit_1 <- .fit_nuisance_spec(
    spec = q1_wflow,
    resamples = inner_resamples,
    training_data = analysis_data_stage1
  )

  # --- Fit g-models (propensity scores) ---
  g_fit_1 <- .fit_nuisance_spec(
    spec = g_models$stages$`1`$wflow,
    resamples = inner_resamples,
    training_data = analysis_data
  )
  g_fit_2 <- .fit_nuisance_spec(
    spec = g_models$stages$`2`$wflow,
    resamples = inner_resamples,
    training_data = analysis_data
  )

  # --- Generate predictions on the assessment data ---
  q2_preds_assess <-
    purrr::map_dfc(action_2_levels, function(lvl) {
      counterfactual_data <- assessment_data
      counterfactual_data[[actions[2]]] <- factor(lvl, levels = action_2_levels)
      predict(q_fit_2, new_data = counterfactual_data) |>
        dplyr::rename(!!paste0("q2_hat_", lvl) := .pred)
    })

  action_1_levels <- levels(assessment_data[[actions[1]]])
  q1_preds_assess <-
    purrr::map_dfc(action_1_levels, function(lvl) {
      counterfactual_data <- assessment_data
      counterfactual_data[[actions[1]]] <- factor(lvl, levels = action_1_levels)
      predict(q_fit_1, new_data = counterfactual_data) |>
        dplyr::rename(!!paste0("q1_hat_", lvl) := .pred)
    })

  g1_preds_assess <- predict(g_fit_1, new_data = assessment_data, type = "prob") |>
    dplyr::rename_with(~ paste0("g1_hat_", sub(".pred_", "", .x)), .cols = dplyr::starts_with(".pred_"))
  g2_preds_assess <- predict(g_fit_2, new_data = assessment_data, type = "prob") |>
    dplyr::rename_with(~ paste0("g2_hat_", sub(".pred_", "", .x)), .cols = dplyr::starts_with(".pred_"))

  tibble::tibble(.row = assessment_data$.row) |>
    dplyr::bind_cols(q1_preds_assess, q2_preds_assess, g1_preds_assess, g2_preds_assess)
}


# Helper to fit a nuisance model spec (workflow or workflow_set)
# This is adapted from tune_nested.R
.fit_nuisance_spec <- function(spec, resamples, training_data) {
  if (inherits(spec, "workflow")) {
    if (nrow(tune::tunable(spec)) > 0) {
      tuned <- tune::tune_grid(spec, resamples = resamples)
      best_params <- tune::select_best(tuned)
      tune::finalize_workflow(spec, best_params) |>
        parsnip::fit(data = training_data)
    } else {
      parsnip::fit(spec, data = training_data)
    }
  } else if (inherits(spec, "workflow_set")) {
    wf_set_trained <-
      workflowsets::workflow_map(
        spec,
        "tune_grid",
        resamples = resamples,
        control = stacks::control_stack_grid(),
        verbose = FALSE
      )

    stacks::stacks() |>
      stacks::add_candidates(wf_set_trained) |>
      stacks::blend_predictions() |>
      stacks::fit_members()
  } else {
    rlang::abort("Unsupported specification in .fit_nuisance_spec. Must be a `workflow` or `workflow_set`.")
  }
}