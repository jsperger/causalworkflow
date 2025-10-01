# Fitting and Tuning function review
## Notes on existing functions

This section summarizes the state of the fitting and tuning functions prior to the refactoring.

### `fit-helpers.R`
- **Responsibility**: Contains the `.fit_nuisance_spec()` internal helper function.
- **Functionality**: This is a core utility for fitting individual model components (`workflow` or `workflow_set`). It intelligently handles both standard fitting and hyperparameter tuning (if the workflow is tunable). It can build a model stack if a `workflow_set` is provided.
- **Shortcomings**: The name `fit-helpers.R` is a bit generic. The logic for tuning is intertwined with basic fitting.

### `fit-recursive.R`
- **Responsibility**: Contains `fit_recursive()`, the primary engine for fitting `staged_workflow` objects.
- **Functionality**: Implements a backwards recursive algorithm to fit stages from `K` down to `1`. It calculates the correct pseudo-outcome for each stage based on the fitted model from the subsequent stage (`k+1`). It supports both full and partial (resumable) fits. It has special handling for different workflow types (e.g., `tmle_workflow`).
- **Shortcomings**: The logic is complex and handles many different cases. The pseudo-outcome calculation is a critical but dense piece of logic within this file.

### `fit-resamples.R`
- **Responsibility**: Provides a `fit_resamples.causal_workflow()` S3 method.
- **Functionality**: Its purpose is to evaluate the performance of the two nuisance models (propensity and outcome) in a `causal_workflow` across a set of resamples. It returns diagnostic metrics to help users assess the quality of their nuisance models before proceeding to causal estimation.
- **Shortcomings**: It is specific to `causal_workflow` and doesn't have a clear counterpart for the more general `staged_workflow`.

### `fit-staged-workflow.R`
- **Responsibility**: Provides user-facing wrappers for the recursive fitting engine.
- **Functionality**: Contains the `fit.staged_workflow()` S3 method, which is the main entry point for fitting a full `staged_workflow`. It also provides `fit_next_stage()` for fitting only a single stage, which is useful for debugging and manual control.
- **Shortcomings**: The file is mostly boilerplate that delegates to `fit_recursive()`.

### `fit.R`
- **Responsibility**: Provides the `fit.causal_workflow()` S3 method.
- **Functionality**: This performs a direct, non-cross-fitted estimation of the nuisance models and the final causal effect (EIF-based). It uses `.fit_nuisance_spec()` internally to handle any required tuning of the component models before the final fit.
- **Shortcomings**: This represents a "single-pass" estimation strategy that is distinct from the nested/cross-fit approaches, but its relationship to them could be clearer.

### `tune-nested.R`
- **Responsibility**: Contains the `tune_nested()` function for `causal_workflow` objects.
- **Functionality**: This is the key function for robust estimation. It implements a nested resampling procedure where an outer loop separates data for estimation, and an inner loop is used for hyperparameter tuning of the nuisance models. This prevents information leakage and provides unbiased causal effect estimates.
- **Shortcomings**: The implementation is long and complex. It mixes the orchestration of the nested loops with the details of fitting and predicting.

### `tune.R`
- **Responsibility**: Provides a `tune_grid.causal_workflow()` S3 method.
- **Functionality**: A "greedy" or "convenience" tuner. It allows the user to tune the hyperparameters of *one* of the nuisance models (propensity or outcome) at a time. It inspects the provided grid to determine which model to tune.
- **Shortcomings**: This is a limited form of tuning and might be confusing to users compared to the more robust `tune_nested()` approach. Its utility is questionable in a world with `tune_nested()`.

## Requirements
1.  **Support for Arbitrary Number of Stages:** The core fitting engine must be able to fit any number of stages (`K >= 1`) without modification.
- it may be reasonable to specify the number of stages to help manage memory size with recursion and facilitate logging

2.  **Abort for Unsupported Estimators:** For estimators that are currently only defined for one or two stages (like the included CV-TMLE), the fitting process must explicitly abort with an informative error if a  number of stages greater than 2 is provided. 

3.  **Support stage by stage fitting** Currently this is implemented in  `fit_next_stage()`; maybe rename this to `fit_stage_iteration` to avoid confusion about "next" in the context of a backward recursive algorithm

4.  **Resumable/Partial Fitting:** The main `fit()` method must be able to accept a `staged_workflow` object that has already been partially fitted. For a K-stage problem, if the models for stages `k+1, ..., K` have already been fitted, calling `fit()` on this object should intelligently resume the process, fitting stage `k`, then `k-1`, and so on, down to stage 1.

5. **Support targeted adjustment using component models for CV-TMLE**

## Refactoring Proposal

This section outlines the proposed new architecture for the fitting and tuning functions. The goal is to increase modularity, reduce complexity, and improve the user experience.

### 1. New File Structure

The existing files will be reorganized to better group related functionalities.

- **`R/fit.R`**: This will become the central file for all user-facing `fit()` S3 methods.
  - `fit.staged_workflow()`: The primary entry point for fitting multi-stage dynamic treatment regimes.
  - `fit.causal_workflow()`: For single-pass, non-cross-fitted causal effect estimation (AIPW/EIF).
  - `fit.tmle_workflow()`: A new, dedicated method for handling the TMLE fitting logic.

- **`R/fit-engine.R`**: This file will house the core, internal fitting logic.
  - `fit_recursive()`: The backwards recursive engine, refactored for clarity.
  - `fit_stage_iteration()`: The new name for `fit_next_stage`, for fitting a single stage.
  - `.calculate_pseudo_outcome()`: The pseudo-outcome calculation logic will be extracted into this internal helper.

- **`R/fit-nested.R`**: This new file will contain the primary user-facing function for robust, nested tuning and estimation.
  - `fit_nested()`: The new, `tidymodels`-idiomatic name for the function that replaces `tune_nested()`. It orchestrates the nested resampling process.

- **`R/resample.R`**: A new file dedicated to resampling-based diagnostics.
  - `fit_resamples.causal_workflow()`: Will be moved here. Its purpose is to provide performance metrics for nuisance models without performing a full nested tuning process.

- **`R/utils-fit.R`**: A new file for shared helper functions related to fitting and tuning.
  - `.fit_nuisance_spec()`: The core utility for fitting/tuning a single `workflow` or `workflow_set`, moved from `fit-helpers.R`.
  - Other helpers related to input validation, formula manipulation, etc.

- **Deleted Files**: `fit-helpers.R`, `fit-recursive.R`, `fit-resamples.R`, `fit-staged-workflow.R`, `tune.R` and `tune-nested.R` will be removed after their logic has been migrated.

### 2. Core Design Principles

- **Separation of Concerns**:
  - **User-Facing API (`fit.R`, `fit-nested.R`)**: These functions will be clean, well-documented, and focused on providing a good user experience. They will handle input validation and then delegate to the internal engines.
  - **Internal Engine (`fit-engine.R`)**: This will contain the complex, recursive logic, completely separated from the user API.
  - **Tuning vs. Fitting**: `fit_nested()` will be explicitly for hyperparameter tuning and robust estimation via nested resampling. `fit()` will be for direct fitting (either single-pass EIF or recursive DTR).

- **Modularity and Reusability**:
  - The `.fit_nuisance_spec()` helper will be the single, reusable component for fitting any individual model specification (workflow or workflow_set), used by all higher-level functions (`fit`, `fit_nested`, etc.).
  - The pseudo-outcome calculation will be a distinct, testable function.

### 3. Key Function Signature Changes

- **`fit_stage_iteration(object, data, ...)`**: Replaces `fit_next_stage`.
- **`fit_nested(object, resamples, ...)`**: Replaces `tune_nested`. This new name is more descriptive and aligns better with the `tidymodels` ecosystem.
- **`fit.tmle_workflow(object, data, ...)`**: A new S3 method will be created to encapsulate the specific logic for the TMLE procedure (e.g., the fluctuation step), making the main `fit_recursive` engine simpler.