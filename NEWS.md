# CausalWorkflows 0.2.0

This version represents a major refactoring of the package's core data structures and API.

## Major Changes

*   The `causal_workflow` object has been redesigned from a simple S3 list to a more flexible and powerful tibble-based structure. This aligns the package more closely with `tidymodels` principles and allows for greater extensibility.
*   The old `add_propensity_model()` and `add_outcome_model()` functions have been removed. They are replaced by the more general `add_component()` function.
*   The concept of multi-stage analyses is now explicitly supported through the `add_stage()` function and the `stage` column in the `causal_workflow` object.
*   A new system for managing component-specific options has been introduced, with `control_*` functions and `add_options()`.
*   The fitting process is now managed by a dispatching `fit()` method that calls different "engines" based on user specification. A simple, non-cross-validated AIPW engine has been implemented.
*   A `collect_metrics()` function has been added to extract results from fitted objects in a tidy format.

## Deprecations and Removals

*   The `staged_workflow` object and all associated functions (`fit_stage_iteration`, `predict-staged-workflow`, etc.) have been removed from the package. This functionality was a remnant of the package's history and is no longer part of the core design.

# CausalWorkflows 0.1.0

* Initial release of `CausalWorkflows`.
* This package provides a framework for estimating causal effects using multi-stage, modular modeling procedures.
* Core functionality includes `causal_workflow()` and `staged_workflow()` for specifying and fitting causal models.
* The package is a refactoring of the `stacks` package, with all model stacking functionality removed to focus on causal inference.