# Refactoring the Fitting Methods in CausalWorkflows

This document outlines the current state of the fitting functions in the `CausalWorkflows` package and proposes a refactoring strategy to unify them into a more general, extensible, and maintainable framework.

## 1. Current State of Fitting Implementations

The package currently has several distinct fitting functions, each with a specialized purpose:

-   **`fit.R` (`fit.causal_workflow`):** This is the most basic fitting method. It performs a straightforward, in-sample estimation by fitting the propensity and outcome models on the entire dataset. It then uses these in-sample predictions to construct the efficient influence function (EIF) and estimate the potential outcome means. This method is simple but can be biased when used with flexible or machine learning models due to overfitting.

-   **`fit-across.R` (`fit_across.causal_workflow`):** This method implements cross-fitting to mitigate the bias issues of the basic `fit` method. It splits the data into K folds, and for each fold, it fits the nuisance models on the other K-1 folds to generate out-of-sample predictions. These predictions are then used to construct the EIF, leading to more robust estimates. Its current implementation is specialized for single-stage `causal_workflow` objects.

-   **`fit-staged-workflow.R` (`fit.staged_workflow`):** This function introduces a backwards-recursive algorithm for fitting multi-stage dynamic treatment regimes (DTRs). It iterates from the final stage (K) down to the first, using the predicted value of the stage `k+1` model as a "pseudo-outcome" for fitting the stage `k` model. This recursive logic is a powerful concept that forms the foundation of the proposed refactoring.

-   **`cv-tmle.R` (`cv_tmle`):** This is a highly complex and specialized implementation of the Cross-Validated Targeted Maximum Likelihood Estimator (CV-TMLE) for a two-stage DTR. It orchestrates an outer cross-validation loop for estimation and an inner loop for nuisance model fitting (e.g., via `workflow_set` and `stacks`). It then performs a sequential targeting (fluctuation) step to update the initial outcome model estimates. While powerful, its logic is monolithic, tightly coupled, and difficult to extend to more than two stages or different estimators.

-   **`fit-resamples.R` (`fit_resamples.causal_workflow`):** This is a diagnostic utility, not an estimation function. It leverages `tune::fit_resamples()` to evaluate the performance of the nuisance models across resamples, which is crucial for assessing the validity of the final causal estimates.

## 2. New Requirements for the Refactored Design

The refactored design must satisfy the following requirements:

1.  **Support for Arbitrary Number of Stages:** The core fitting engine must be able to fit a `staged_workflow` with any number of stages (`K >= 1`) without modification. The `fit.staged_workflow` method should work seamlessly for K-stage problems.

2.  **Abort for Unsupported Estimators:** For estimators that are currently only defined for two stages (like the included CV-TMLE), the fitting process must explicitly abort with an informative error if a `staged_workflow` with `K > 2` is provided. This applies to `cv_tmle` and any `causal_workflow` fit that relies on a specific influence function.

3.  **Debugging with `fit_next_stage()`:** A new user-facing function, `fit_next_stage()`, must be provided. This function will allow a user to fit only the next stage in the sequence, given a partially fitted `staged_workflow` object. This is invaluable for debugging and understanding the model-fitting process one step at a time.

4.  **Resumable/Partial Fitting:** The main `fit()` method must be able to accept a `staged_workflow` object that has already been partially fitted. For a K-stage problem, if the models for stages `k+1, ..., K` have already been fitted, calling `fit()` on this object should intelligently resume the process, fitting stage `k`, then `k-1`, and so on, down to stage 1.

## 3. Proposal for Refactoring

The proposed refactoring centers on creating a unified, extensible recursive engine that generalizes the logic currently in `fit.staged_workflow` and modularizes the logic from `cv_tmle`.

1.  **Unified Recursive Engine:**
    -   A new internal function, `fit_recursive()`, will be created, likely in a new file `R/fit-recursive.R`. This function will be the core of the new design, containing the backward recursion logic.
    -   It will accept a `staged_workflow` object and determine the next stage to fit by inspecting the `fitted_models` slot of the object. This enables both full and partial/resumable fits.

2.  **Decomposition and Integration of `cv_tmle`:**
    -   The complex `cv_tmle` function will be broken down. The outer cross-validation loop will become a `fit_across` method for `staged_workflow` objects.
    -   The inner TMLE logic (initial nuisance estimation, targeting, and influence curve calculation) will be modularized. This can be achieved by creating a new stage type (e.g., a `tmle_workflow`) or by using a control object (e.g., `control_tmle()`) that instructs the recursive engine to perform the necessary targeting and fluctuation steps after the initial model fit for a given stage.

3.  **Generalization of `fit` and `fit_across`:**
    -   `fit.staged_workflow` will become a simple, elegant wrapper around the `fit_recursive()` engine.
    -   `fit.causal_workflow` and `fit_across.causal_workflow` will be refactored to first construct a single-stage `staged_workflow` and then call the corresponding `staged_workflow` method (`fit` or `fit_across`). This eliminates significant code duplication and ensures that all fitting flows through the same unified engine.

4.  **Implementation of `fit_next_stage()`:**
    -   This new function will also be a wrapper around `fit_recursive()`, but it will be configured to execute only a single step of the recursion and return the updated, partially-fitted object.

5.  **Pseudo-Outcome Calculation:**
    -   The recursive engine will be responsible for calculating the correct pseudo-outcome for each stage `k` based on the fitted model from stage `k+1`. This logic will need to handle different scenarios:
        -   **Standard `workflow`:** The pseudo-outcome is `r_k + discount * V_{k+1}`, where `V_{k+1}` is the maximum predicted value from the stage `k+1` model.
        -   **TMLE stage:** The pseudo-outcome for stage `k-1` is the value function derived from the *targeted* Q-model, `Q_k^*`.
        -   **AIPW/EIF `causal_workflow`:** This will be handled as a single-stage process where the final "outcome" is the EIF itself, not a pseudo-outcome for a prior stage.