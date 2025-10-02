# CausalWorkflows Revision Plan

## 1. Introduction

This document outlines a phased plan to refactor the `CausalWorkflows` package. The goal is to transition from the current simple, S3-based implementation to the more flexible and powerful tibble-based `causal_workflow` object described in the `README.md`. This revision will better align the package with the `tidymodels` ecosystem and provide a solid foundation for future extensions.

## 2. Current Implementation

The current implementation of the `causal_workflow` object is a basic S3 object that acts as a list with two slots:

- `propensity_model`
- `outcome_model`

This object is manipulated by functions like `add_propensity_model()` and `add_outcome_model()`. While functional for simple, single-stage analyses, this design has several limitations:

- **Lack of Extensibility:** It is difficult to add new components (e.g., for targeting, censoring, or multiple stages) without modifying the core object structure.
- **Limited Configuration:** There is no standardized way to manage component-specific options (e.g., propensity score truncation, tuning parameters for outcome models).
- **Departure from `tidymodels` Principles:** The design does not follow the `tidymodels` pattern of using tibbles to organize and manage complex modeling workflows (e.g., `workflow_set`).

## 3. Proposed Design

The `README.md` outlines a new design for the `causal_workflow` object as a specialized tibble. This design is highly modular and extensible, with the following key features:

- **Tibble-based Structure:** The `causal_workflow` object will be a tibble with columns for `stage`, `component_id`, `component`, `options`, and `result`.
- **Modularity:** Each row in the tibble represents a distinct modeling component, allowing for a flexible and extensible analysis pipeline.
- **Multi-stage Support:** The `stage` column provides a natural way to define and manage multi-stage analyses, such as dynamic treatment regimes.
- **Standardized Configuration:** The `options` column provides a consistent mechanism for managing component-specific configurations using `control_*` objects.
- **Separation of Definition and Execution:** The `causal_workflow` object serves as a blueprint for the analysis, which is then executed by the `fit()` function. This separation makes the analysis easier to reason about and debug.

## 4. Phased Refactoring Plan

The refactoring process will be broken down into the following phases to ensure a smooth transition and allow for testing at each stage.

### Phase 1: Core Object and Constructors

- **Implement the new `causal_workflow` object:**
    - Create the core tibble-based structure with the specified columns.
    - Define the `causal_workflow` class.
- **Create the constructor functions:**
    - Implement `causal_workflow()` to initialize the object.
    - Implement `add_stage()` to add new stages to the workflow.
- **Deprecate the old S3 object:**
    - Mark the old `causal_workflow()` function and the `add_propensity_model()` and `add_outcome_model()` functions as deprecated.

### Phase 2: Options and Configuration

- **Implement the `control_*` functions:**
    - Create `control_propensity()`, `control_outcome()`, and `control_targeting()` to manage component-specific options.
- **Create the `add_options()` helper function:**
    - This function will provide a user-friendly way to add control objects to components.

### Phase 3: Fitting Engines

- **Implement the `fit.causal_workflow()` generic:**
    - This function will dispatch to the appropriate engine based on the `engine` argument.
- **Develop the orchestration engines:**
    - Implement `.engine_aipw()` for cross-validated doubly robust estimation.
    - Implement `.engine_cvtmle()` for cross-validated targeted maximum likelihood estimation, including the logic for handling nested resampling.

### Phase 4: Results and Utilities

- **Implement the results functions:**
    - Create `collect_metrics()` to extract and tidy the final causal estimates.
    - Create `collect_predictions()` to extract and tidy out-of-sample predictions.
- **Implement the `autoplot()` method:**
    - This function will provide a way to visualize the results of the analysis.

### Phase 5: Documentation, Testing, and Cleanup

- **Update documentation:**
    - Revise all documentation to reflect the new API.
    - Create new vignettes to demonstrate the new workflow.
- **Write comprehensive unit tests:**
    - Ensure that all new functionality is thoroughly tested.
- **Remove deprecated code:**
    - Once the new API is stable and well-tested, remove the old S3 object and its associated functions.

## 5. Potential Revisions to the Design

After reviewing the repository, the proposed design in the `README.md` appears to be very well-thought-out and aligns with the goals of the project. At this time, I do not have any suggested revisions to the design. The phased approach outlined above should provide a clear path for implementing this design.