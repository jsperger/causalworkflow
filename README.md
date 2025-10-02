# CausalWorkflows

`causalworkflows` is an R package for estimating causal effects using multi-stage, modular modeling procedures that align with the `tidymodels` framework. The package provides a flexible structure for defining and fitting causal estimators that rely on nuisance models, such as the propensity score and outcome models.

The core of the package is the `causal_workflow()` object, which allows you to specify separate `tidymodels` workflows for each component of your estimator. The package can then be used to construct robust estimates of causal effects, such as the Average Treatment Effect (ATE), using estimators like the Augmented Inverse Propensity Weighting (AIPW) estimator.

## Installation

You can install the development version of `causalworkflows` from GitHub with:

```r
# install.packages("devtools")
devtools::install_github("jsperger/causalworkflows")
```

## Example

This is a basic example of how to use `causalworkflows` to estimate the Average Treatment Effect (ATE) of switching from an automatic to a manual transmission on fuel efficiency (`mpg`) in the `mtcars` dataset.

### 1. Load Libraries

```r
library(causalworkflows)
library(parsnip)
library(workflows)
library(dplyr)
```

### 2. Prepare Data

```r
data <- mtcars
data$am <- as.factor(data$am)
```

### 3. Define Modeling Components

We need two models for an AIPW estimator: a propensity model to predict the treatment (`am`) and an outcome model to predict the outcome (`mpg`).

```r
# a. Propensity Model (predicts am ~ wt + hp)
propensity_wf <- workflow() |>
  add_model(logistic_reg() |> set_engine("glm")) |>
  add_formula(am ~ wt + hp)

# b. Outcome Model (predicts mpg ~ am + wt + hp)
outcome_wf <- workflow() |>
  add_model(linear_reg() |> set_engine("lm")) |>
  add_formula(mpg ~ am + wt + hp)
```

### 4. Construct the Causal Workflow

The `causal_workflow()` object is a container for the different modeling components.

```r
cwf <- causal_workflow(
  propensity = propensity_wf,
  outcome = outcome_wf
)

# Print the object to see the analysis plan
# > # A causal workflow: 2 components
# > # A tibble: 2 × 5
# >   stage component_id component  options result
# >   <int> <chr>        <list>     <list>  <list>
# > 1     1 outcome      <workflow> <NULL>  <NULL>
# > 2     1 propensity   <workflow> <NULL>  <NULL>
```

### 5. Fit the Causal Workflow

The `fit()` function takes the `causal_workflow` object and the data, and it dispatches to the correct estimation engine. For this example, we'll use the (non-cross-validated) AIPW engine.

```r
fitted_cwf <- fit(cwf, data = data, engine = "aipw")
```

### 6. Inspect the Results

The `collect_metrics()` function extracts the final causal estimate(s) into a tidy tibble.

```r
collect_metrics(fitted_cwf)

# > # A tibble: 1 × 3
# >   .metric .estimator .estimate
# >   <chr>   <chr>          <dbl>
# > 1 ate     aipw            2.50
```