## 1\. Causal Workflow Design Document

This document outlines the architecture and design for a `causal_workflow` object, a new structure for the `tidymodels` ecosystem designed to streamline complex causal inference analyses. The goal is to provide a flexible, extensible, and user-friendly framework for methods like CV-TMLE and multi-stage dynamic treatment regimes, while adhering to the principles of the `tidymodels` ecosystem.

`causalworkflows` is an R package for estimating causal effects using multi-stage, modular modeling procedures that align with the `tidymodels` framework. The package provides a flexible structure for defining and fitting causal estimators that rely on nuisance models, such as the propensity score and outcome models.

The core of the package is the `causal_workflow()` object, which allows you to specify separate `tidymodels` workflows for each component of your estimator. The package then uses cross-fitting to construct robust estimates of causal effects, such as the Average Treatment Effect (ATE), using estimators like the Augmented Inverse Propensity Weighting (AIPW) estimator.

### 2\. The `causal_workflow` Object

The core of the package is the `causal_workflow` object, a specialized tibble that acts as a blueprint for a causal analysis. Each row in this tibble represents a distinct modeling component required for the analysis.

#### 2.1. Structure

A `causal_workflow` object will be a tibble with a custom subclass (`causal_workflow`) and the following columns:

| Column | Type | Description |
| :--- | :--- | :--- |
| `stage` | `numeric` | An integer indicating the analysis stage. For a single time-point analysis, this will be `1`. Essential for dynamic treatment regimes. |
| `component_id` | `character` | A user-defined, unique identifier for the component within a given `stage`. Examples: `"propensity"`, `"outcome_sl"`, `"targeting"`. |
| `component` | `list` | A list-column containing the core `tidymodels` object for that component. This can be a `<workflow>`, `<workflow_set>`, `stacks`, or `<tailor>` object. |
| `options` | `list` | A list-column holding component-specific control objects that manage fitting, prediction, and other options. |
| `result` | `list` | A list-column that is initially empty and will be populated by `fit()` with fitted objects, predictions, and metrics. |

#### 2.2. Example Structure (Single-Stage TMLE)

```
# A tibble: 3 × 5
  stage component_id component        options        result
  <int> <chr>        <list>           <list>         <list>
1     1 propensity   <workflow>       <control_prop> <list>
2     1 outcome      <workflow_set>   <control_out>  <list>
3     1 targeting    <tailor>         <control_targ> <list>
```

-----

### 3\. Constructor Functions

Manually creating the `causal_workflow` tibble would be cumbersome. A set of user-friendly constructor and helper functions is essential.

#### 3.1. `causal_workflow()`

This is the main constructor. It initializes the object and is designed to be used with pipes.

  * **Usage**: It takes named arguments, where the name becomes the `component_id` and the value is the `tidymodels` object (`workflow`, `workflow_set`, or `tailor`).
  * An optional `.stage` argument sets the stage for all components being added. It defaults to `1`.

<!-- end list -->

```r
#' Create a Causal Workflow
#'
#' @description
#' Initializes a `causal_workflow` object. Subsequent components or stages
#' should be added with helper functions like `add_stage()`.
#'
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Named arguments where the
#'   name defines the `component_id` and the value is a `<workflow>`,
#'   `<workflow_set>`, or `<tailor>` object.
#' @param .stage A single integer defining the analysis stage for the
#'   provided components. Defaults to `1`.
#'
#' @returns A `causal_workflow` object.
causal_workflow <- function(..., .stage = 1L) {
  # Implementation to create the initial tibble
}
```

#### 3.2. `add_stage()`

This helper function allows for the construction of multi-stage analyses in a readable, sequential manner.

  * **Usage**: It takes an existing `causal_workflow` object and adds a new set of components for a specified stage.

<!-- end list -->

```r
#' Add a Stage to a Causal Workflow
#'
#' @description
#' Appends a new set of components for a subsequent analysis stage to an
#' existing `causal_workflow` object.
#'
#' @param x A `causal_workflow` object.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Named arguments for the
#'   new components.
#' @param .stage A single integer for the new stage. The function will error
#'   if the stage number already exists.
#'
#' @returns An updated `causal_workflow` object.
add_stage <- function(x, ..., .stage) {
  # Implementation to add rows to the tibble
}
```

#### 3.3. Example Construction

```r
# Define component objects (workflows, etc.)
propensity_wf <- ...
outcome_wfs <- ...
targeting_tailor <- ...

# Construct the causal_workflow
causal_wf <-
  causal_workflow(
    propensity = propensity_wf,
    outcome = outcome_wfs,
    targeting = targeting_tailor,
    .stage = 1
  )
```

-----

### 4\. Evaluation and Orchestration

The separation of definition from execution is a core design principle. The `fit()` generic will be the primary entry point for evaluation, dispatching to different computational engines based on the user's requested causal method.

#### 4.1. `fit.causal_workflow()`

  * **Usage**: This function takes the `causal_workflow` object, data, and an optional resampling object. A key argument, `engine`, will specify which causal estimation method to use.
  * **Resampling**: The function will expect an `rsample` object for any cross-validated estimation engine. For CV-TMLE, the user must provide a **nested resampling object** (e.g., from `rsample::nested_cv()`). The orchestration engine is responsible for correctly navigating the nested structure.

<!-- end list -->

```r
#' Fit a Causal Workflow
#'
#' @param x A `causal_workflow` object.
#' @param data A data frame.
#' @param resamples An optional `rsample` object, required for
#'   cross-validated engines.
#' @param engine A character string specifying the estimation engine.
#'   Defaults to "cvtmle".
#' @param ... Additional arguments passed to the engine.
#'
#' @returns A `causal_workflow` object with the `result` column populated.
fit.causal_workflow <- function(x, data, resamples = NULL, engine = "cvtmle", ...) {
  # Dispatch to the correct engine function, e.g., .engine_cvtmle()
}
```

#### 4.2. Orchestration Engines

These are the internal functions that execute the complex fitting logic.

  * **`.engine_cvtmle(causal_wf, resamples, ...)`**:

    1.  **Input Validation**: Checks that `resamples` is a nested CV object.
    2.  **Outer Loop**: Iterates through the outer folds. The assessment set of each outer fold is reserved for the final parameter estimation.
    3.  **Inner Loop**: Iterates through the inner folds to generate out-of-sample nuisance predictions.
          * Fits `propensity` and `outcome` models on the inner analysis sets. If a component is a `<workflow_set>`, this step performs model tuning and selects the best model based on the options provided.
          * Generates predictions on the inner assessment sets.
    4.  **Targeting**: Uses the full set of out-of-sample predictions from the inner loop to fit the `targeting` model/tailor.
    5.  **Estimation**: Applies the fitted targeting step to the outer assessment set to compute the fold-specific causal estimate.
    6.  **Aggregation**: After the outer loop, aggregates the fold-specific estimates to produce the final CV-TMLE estimate and its variance.

  * **`.engine_aipw(causal_wf, resamples, ...)`**:

      * A simpler engine for a non-targeted, cross-validated doubly robust estimate. It would perform the inner loop logic to get out-of-sample nuisance predictions and then directly compute the AIPW estimate on the outer assessment folds.

-----

### 5\. Managing Options and Configuration

Granular control over each component is necessary. This will be managed via the `options` column, which will store custom `control` objects. This is analogous to the `control` arguments in `tune` and the `option` column in `workflowsets`.

#### 5.1. Control Objects

A series of constructor functions will create standardized option objects.

  * **`control_propensity(truncate = 0.01, ...)`**: Returns a control object with options specific to propensity score models, such as truncation levels.
  * **`control_outcome(control_tune = control_grid(), ...)`**: Returns a control object for outcome models. If the component is a `<workflow_set>`, this is where the user would provide the `tune` control object to manage the hyperparameter search.
  * **`control_targeting(...)`**: Options for the targeting step.

#### 5.2. Usage

The user would add these control objects to the `component` objects before creating the `causal_workflow`. A helper function, `add_options()`, could facilitate this. The orchestration engines would then parse these options to guide the fitting process.

```r
# Example of setting options
propensity_wf_opts <-
  propensity_wf |>
  add_options(control_propensity(truncate = 0.025))

outcome_wfs_opts <-
  outcome_wfs |>
  add_options(control_outcome(control_tune = control_grid(save_pred = TRUE)))

# The options are carried into the final object
causal_wf <- causal_workflow(
  propensity = propensity_wf_opts,
  outcome = outcome_wfs_opts,
  ...
)
```

-----

### 6\. Proposed Function Organization

| Category | Functions | Purpose |
| :--- | :--- | :--- |
| **Core** | `causal_workflow()`, `add_stage()` | User-friendly constructors for the main object. |
| **Evaluation** | `fit()`, `predict()` | High-level generics for fitting the analysis and getting predictions. |
| **Results** | `collect_metrics()`, `collect_predictions()` | Functions to extract and tidy results, analogous to `tune`. |
| **Options** | `add_options()`, `control_propensity()`, `control_outcome()` | Constructors for managing component-specific options. |
| **Plotting** | `autoplot()` | A method for visualizing results, similar to `workflowsets`. |
| **Internal** | `.engine_cvtmle()`, `.engine_aipw()` | Non-exported functions that contain the core fitting logic. |

### 7 Known Constraints & Assumptions
Reliance on tidymodels: This architecture is fundamentally dependent on the tidymodels ecosystem. Users must be familiar with parsnip, recipes, and workflows to specify their nuisance models.

Focus on Orchestration: The package's core responsibility is the coordination of the causal procedures for AIPW, CV-TMLE, and similar while being extensible. It delegates the actual model fitting algorithms to parsnip engines. The performance and correctness of the underlying model fits are the responsibility of those external packages.

## Example usage 
```r
# 1. LOAD LIBRARIES
# -----------------
library(tidymodels)
library(causalworkflow) # Your new package
library(rsample)

# 2. PREPARE DATA & RESAMPLING
# ----------------------------
# Load data where 'A' is treatment, 'Y' is outcome, 'W' are confounders
data <- read_csv("my_data.csv")

# CV-TMLE requires nested cross-validation for unbiased estimation
set.seed(123)
nested_cv <- nested_cv(data, outside = vfold_cv(v = 5), inside = vfold_cv(v = 5))

# 3. DEFINE MODELING COMPONENTS
# -----------------------------
# a. Propensity Model (predicts A ~ W)
propensity_spec <- logistic_reg() |> set_engine("glm")
propensity_wf <- workflow() |>
  add_model(propensity_spec) |>
  add_formula(A ~ W1 + W2)

# b. Outcome Model (predicts Y ~ A + W)
# Use a workflow_set to tune between two different models
outcome_spec_glm <- linear_reg() |> set_engine("glm")
outcome_spec_rf <- rand_forest() |> set_engine("ranger") |> set_mode("regression")

outcome_wfs <- workflow_set(
  preproc = list(formula = Y ~ A + W1 + W2),
  models = list(glm = outcome_spec_glm, rf = outcome_spec_rf)
)

# c. Targeting Model (fluctuation step)
# Use a pre-defined logistic tailor for the fluctuation
targeting_comp <- tailor_logistic()

# 4. CONSTRUCT THE CAUSAL WORKFLOW
# --------------------------------
causal_wf <- causal_workflow(
  propensity = propensity_wf,
  outcome = outcome_wfs,
  targeting = targeting_comp
)

# Print the object to see the analysis plan
# > # A causal workflow
# > # A tibble: 3 × 5
# >   stage component_id component        options result
# >   <int> <chr>        <list>           <list>  <list>
# > 1     1 propensity   <workflow>       <NULL>  <NULL>
# > 2     1 outcome      <workflow_set>   <NULL>  <NULL>
# > 3     1 targeting    <tailor>         <NULL>  <NULL>

# 5. FIT THE CAUSAL WORKFLOW
# --------------------------
# The engine handles the complex nested CV orchestration
fitted_causal_wf <- fit(
  causal_wf,
  data = data,
  resamples = nested_cv,
  engine = "cvtmle"
)

# 6. INSPECT THE RESULTS
# ----------------------
# The result column is now populated
print(fitted_causal_wf)

# Collect the final, cross-validated causal estimate (e.g., ATE)
# The function knows to aggregate the fold-wise estimates
collect_metrics(fitted_causal_wf)

# > # A tibble: 1 × 5
# >   .metric .estimator .estimate .std_err .p_value
# >   <chr>   <chr>          <dbl>    <dbl>    <dbl>
# > 1 ate     tmle           0.123   0.0456   0.0067
```
