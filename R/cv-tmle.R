#' Estimate the value of a dynamic treatment regime using CV-TMLE
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' [[cv_tmle()]] implements the Cross-Validated Targeted Maximum Likelihood
#' Estimator (CV-TMLE) for the value of an estimated two-stage optimal
#' Dynamic Treatment Regime (DTR).
#'
#' @details
#' This function performs K-fold cross-validation to estimate the value of a
#' DTR. The process involves:
#' 1.  **Initial Nuisance Estimation**: Within each training fold, it fits
#'     stage-specific models for the Q-functions (outcome models) and
#'     propensity scores (treatment models) using the provided
#'     [`causal_workflow`] objects. This step uses nested resampling to
#'     tune or ensemble the models.
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
#' @param data A `data.frame` containing the observational data.
#' @param resamples An `rsample` object for K-fold cross-validation.
#' @param stages A named list of [`causal_workflow`] objects, one for each
#'   stage.
#' @param actions A character vector of the column names for the treatment
#'   variables at each stage.
#' @param outcome A character string of the column name for the final outcome.
#' @param inner_v The number of folds for the inner resampling loop used for
#'   tuning or ensembling.
#' @param lower_bound A small numeric value to truncate propensity scores,
#'   preventing numerical instability.
#' @param ... Additional arguments passed to underlying fitting functions.
#'
#' @return A `cv_tmle_fit` object.
#' @export
cv_tmle <- function(
  data,
  resamples,
  stages,
  actions,
  outcome,
  inner_v = 5,
  lower_bound = 0.025,
  ...
) {
  # 1. Iterate over each fold to get targeted predictions and IC components
  fold_results <- purrr::map_dfr(
    resamples$splits,
    ~ .process_one_fold(
      split = .x,
      stages = stages,
      actions = actions,
      outcome = outcome,
      inner_v = inner_v,
      lower_bound = lower_bound,
      ...
    )
  ) |>
    dplyr::arrange(.row)

  # 2. Calculate final estimate and inference from pooled results
  V_hat_cvtmle <- mean(fold_results$Q1_star_max, na.rm = TRUE)
  ic_full <- (fold_results$D2 + fold_results$D1 + fold_results$D0) -
    V_hat_cvtmle
  n_obs <- nrow(fold_results)
  var_hat <- stats::var(ic_full, na.rm = TRUE) / n_obs
  se_hat <- sqrt(var_hat)

  # 3. Construct final result object
  list(
    .estimate = V_hat_cvtmle,
    .se = se_hat,
    .conf.low = V_hat_cvtmle - 1.96 * se_hat,
    .conf.high = V_hat_cvtmle + 1.96 * se_hat,
    influence_curve = tibble::tibble(.row = fold_results$.row, ic = ic_full),
    nuisance_predictions = fold_results
  ) |>
    structure(class = "cv_tmle_fit")
}

# --- Internal Helper Functions ---

.safe_max <- function(x) {
  # `max` with `na.rm = TRUE` returns -Inf if all values are NA
  # This helper returns NA instead to avoid warnings and downstream errors.
  if (all(is.na(x))) {
    return(NA_real_)
  }
  max(x, na.rm = TRUE)
}

.process_one_fold <- function(
  split,
  stages,
  actions,
  outcome,
  inner_v,
  lower_bound,
  ...
) {
  analysis_data <- rsample::analysis(split)
  assessment_data <- rsample::assessment(split)
  inner_resamples <- rsample::vfold_cv(analysis_data, v = inner_v)

  preds_init <- .fit_predict_nuisance_models(
    analysis_data,
    assessment_data,
    stages,
    actions,
    inner_resamples,
    inner_v = inner_v
  )
  data_for_targeting <- dplyr::left_join(
    assessment_data,
    preds_init,
    by = ".row",
    multiple = "all"
  )

  targeted_results <- .target_nuisance_estimates(
    data_for_targeting,
    actions,
    outcome,
    lower_bound
  )
  .calculate_ic(targeted_results, actions, outcome)
}

.fit_predict_nuisance_models <- function(
  analysis_data,
  assessment_data,
  stages,
  actions,
  inner_resamples,
  inner_v
) {
  q_spec_2 <- stages$`2`$outcome_model
  g_spec_2 <- stages$`2`$propensity_model
  q_spec_1 <- stages$`1`$outcome_model
  g_spec_1 <- stages$`1`$propensity_model

  q_fit_2 <- .fit_nuisance_spec(q_spec_2, inner_resamples, analysis_data)
  g_fit_2 <- .fit_nuisance_spec(g_spec_2, inner_resamples, analysis_data)

  action_2_levels <- levels(analysis_data[[actions[2]]])
  q2_preds_train <- purrr::map(
    action_2_levels,
    ~ predict(
      q_fit_2,
      new_data = dplyr::mutate(
        analysis_data,
        !!actions[2] := factor(.x, levels = action_2_levels)
      )
    )
  ) |>
    dplyr::bind_cols() |>
    as.matrix()
  pseudo_outcome_1 <- apply(q2_preds_train, 1, .safe_max)
  analysis_data_stage1 <- dplyr::mutate(
    analysis_data,
    .pseudo_outcome = pseudo_outcome_1
  )

  q1_wflow <- .update_q_workflow(q_spec_1)
  inner_resamples_stage1 <- rsample::vfold_cv(analysis_data_stage1, v = inner_v)
  q_fit_1 <- .fit_nuisance_spec(
    q1_wflow,
    inner_resamples_stage1,
    analysis_data_stage1
  )
  g_fit_1 <- .fit_nuisance_spec(g_spec_1, inner_resamples, analysis_data)

  action_1_levels <- levels(assessment_data[[actions[1]]])
  q2_preds_assess <- purrr::map_dfc(
    action_2_levels,
    ~ predict(
      q_fit_2,
      new_data = dplyr::mutate(
        assessment_data,
        !!actions[2] := factor(.x, levels = action_2_levels)
      )
    )
  )
  q1_preds_assess <- purrr::map_dfc(
    action_1_levels,
    ~ predict(
      q_fit_1,
      new_data = dplyr::mutate(
        assessment_data,
        !!actions[1] := factor(.x, levels = action_1_levels)
      )
    )
  )
  g1_preds_assess <- predict(g_fit_1, new_data = assessment_data, type = "prob")
  g2_preds_assess <- predict(g_fit_2, new_data = assessment_data, type = "prob")

  names(q2_preds_assess) <- paste0("q2_hat_", action_2_levels)
  names(q1_preds_assess) <- paste0("q1_hat_", action_1_levels)
  g1_preds_assess <- dplyr::rename_with(
    g1_preds_assess,
    ~ paste0("g1_hat_", sub(".pred_", "", .x))
  )
  g2_preds_assess <- dplyr::rename_with(
    g2_preds_assess,
    ~ paste0("g2_hat_", sub(".pred_", "", .x))
  )

  tibble::tibble(.row = assessment_data$.row) |>
    dplyr::bind_cols(
      q1_preds_assess,
      q2_preds_assess,
      g1_preds_assess,
      g2_preds_assess
    )
}

.target_nuisance_estimates <- function(data, actions, outcome, lower_bound) {
  q1_hat_cols <- dplyr::select(data, dplyr::starts_with("q1_hat_"))
  q2_hat_cols <- dplyr::select(data, dplyr::starts_with("q2_hat_"))
  pi_hat_1 <- colnames(q1_hat_cols)[apply(q1_hat_cols, 1, which.max)] |>
    sub("q1_hat_", "", x = _)
  pi_hat_2 <- colnames(q2_hat_cols)[apply(q2_hat_cols, 1, which.max)] |>
    sub("q2_hat_", "", x = _)
  data <- dplyr::mutate(data, pi_hat_1 = pi_hat_1, pi_hat_2 = pi_hat_2)

  g1_hat_observed <- .get_observed_prob(data, "g1_hat", actions[1]) |>
    pmax(lower_bound)
  g2_hat_observed <- .get_observed_prob(data, "g2_hat", actions[2]) |>
    pmax(lower_bound)
  C1 <- as.numeric(data[[actions[1]]] == data$pi_hat_1) / g1_hat_observed
  C2 <- C1 * (as.numeric(data[[actions[2]]] == data$pi_hat_2) / g2_hat_observed)

  data <- dplyr::mutate(
    data,
    C1 = C1,
    C2 = C2,
    q2_hat_observed = .get_observed_prob(data, "q2_hat", actions[2]),
    q1_hat_observed = .get_observed_prob(data, "q1_hat", actions[1])
  )

  epsilon_2 <- stats::coef(stats::glm(
    as.formula(paste(outcome, "~ -1 + offset(q2_hat_observed) + C2")),
    data = data,
    family = "gaussian"
  ))
  q2_star_cols <- q2_hat_cols + (epsilon_2 * data$C2)
  colnames(q2_star_cols) <- sub("q2_hat", "q2_star", colnames(q2_hat_cols))

  data_q1_fluct <- dplyr::bind_cols(data, q2_star_cols)
  data_q1_fluct$pseudo_outcome_1_targeted <- apply(
    dplyr::select(data_q1_fluct, dplyr::starts_with("q2_star_")),
    1,
    .safe_max
  )

  epsilon_1 <- stats::coef(stats::glm(
    pseudo_outcome_1_targeted ~ -1 + offset(q1_hat_observed) + C1,
    data = data_q1_fluct,
    family = "gaussian"
  ))
  q1_star_cols <- q1_hat_cols + (epsilon_1 * data$C1)
  colnames(q1_star_cols) <- sub("q1_hat", "q1_star", colnames(q1_hat_cols))

  dplyr::bind_cols(data, q1_star_cols)
}

.calculate_ic <- function(data, actions, outcome) {
  q2_star_observed <- .get_observed_prob(data, "q2_star", actions[2])
  q1_star_observed <- .get_observed_prob(data, "q1_star", actions[1])
  q2_star_max <- apply(
    dplyr::select(data, dplyr::starts_with("q2_star_")),
    1,
    .safe_max
  )

  q1_star_max <- apply(
    dplyr::select(data, dplyr::starts_with("q1_star_")),
    1,
    .safe_max
  )

  D2 <- data$C2 * (data[[outcome]] - q2_star_observed)
  D1 <- data$C1 * (q2_star_max - q1_star_observed)
  D0 <- q1_star_max

  dplyr::mutate(data, Q1_star_max = q1_star_max, D2 = D2, D1 = D1, D0 = D0)
}

.get_observed_prob <- function(data, model_prefix, action_col) {
  pred_cols <- dplyr::select(data, dplyr::starts_with(model_prefix))
  action_levels <- sub(paste0(model_prefix, "_"), "", colnames(pred_cols))
  as.matrix(pred_cols)[cbind(
    seq_len(nrow(data)),
    match(data[[action_col]], action_levels)
  )]
}

.update_q_workflow <- function(spec) {
  if (inherits(spec, "workflow")) {
    orig_formula <- hardhat::extract_preprocessor(spec)
    new_formula <- rlang::new_formula(
      rlang::sym(".pseudo_outcome"),
      rlang::f_rhs(orig_formula)
    )
    return(workflows::update_formula(spec, new_formula))
  } else if (inherits(spec, "workflow_set")) {
    spec$info <- purrr::map(spec$info, function(info_df) {
      info_df$workflow <- purrr::map(info_df$workflow, .update_q_workflow)
      info_df
    })
    return(spec)
  }
  spec
}

.fit_nuisance_spec <- function(
  spec,
  resamples,
  training_data,
  call = rlang::caller_env()
) {
  if (inherits(spec, "workflow")) {
    if (nrow(tune::tunable(spec)) > 0) {
      tuned <- tune::tune_grid(spec, resamples = resamples, grid = 10)
      best_params <- tune::select_best(tuned)
      return(
        tune::finalize_workflow(spec, best_params) |>
          parsnip::fit(data = training_data)
      )
    } else {
      return(parsnip::fit(spec, data = training_data))
    }
  } else if (inherits(spec, "workflow_set")) {
    wf_set_trained <- workflowsets::workflow_map(
      "tune_grid",
      spec,
      resamples = resamples,
      control = stacks::control_stack_grid()
    )
    return(
      stacks::stacks() |>
        stacks::add_candidates(wf_set_trained) |>
        stacks::blend_predictions() |>
        stacks::fit_members()
    )
  }
  cli::cli_abort(
    c(
      "{.arg spec} must be a {.cls workflow} or {.cls workflow_set} object.",
      "x" = "You supplied a {.cls {class(spec)[[1]]}}."
    ),
    call = call
  )
}
