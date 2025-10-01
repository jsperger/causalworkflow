# Fitting and Tuning function review
## Notes one existing functions
PLACEHOLDER

## Requirements
1.  **Support for Arbitrary Number of Stages:** The core fitting engine must be able to fit any number of stages (`K >= 1`) without modification.
- it may be reasonable to specify the number of stages to help manage memory size with recursion and facilitate logging

2.  **Abort for Unsupported Estimators:** For estimators that are currently only defined for one or two stages (like the included CV-TMLE), the fitting process must explicitly abort with an informative error if a  number of stages greater than 2 is provided. 

3.  **Support stage by stage fitting** Currently this is implemented in  `fit_next_stage()`; maybe rename this to `fit_stage_iteration` to avoid confusion about "next" in the context of a backward recursive algorithm

4.  **Resumable/Partial Fitting:** The main `fit()` method must be able to accept a `staged_workflow` object that has already been partially fitted. For a K-stage problem, if the models for stages `k+1, ..., K` have already been fitted, calling `fit()` on this object should intelligently resume the process, fitting stage `k`, then `k-1`, and so on, down to stage 1.

5. **Support targeted adjustment using component models for CV-TMLE**

## Potential Ideas
The fitting functions should require that all of the necessary components are passed as arguments or are in the `causal_workflow` or `staged_workflow` object

A new function with a name like `fit_nested_workflow` or similar, not an S3 method for `fit`, should be made that handles setting up nested cross validation and orchestrating the fitting calls with the appropriate folds. Try to disentangle some of the fitting and tuning while still providing a convenient interface. A control object passed to this function can determine whether the targeting, and influence curve calculation for CV-TMLE are done. 

4.  **Implementation of `fit_next_stage()`:**
    -   This new function will also be a wrapper around `fit_recursive()`, but it will be configured to execute only a single step of the recursion and return the updated, partially-fitted object.

5.  **Pseudo-Outcome Calculation:**
    -   The recursive engine will be responsible for calculating the correct pseudo-outcome for each stage `k` based on the fitted model from stage `k+1`. This logic will need to handle different scenarios:
        -   **Standard `workflow`:** The pseudo-outcome is `r_k + discount * V_{k+1}`, where `V_{k+1}` is the maximum predicted value from the stage `k+1` model.
        -   **TMLE stage:** The pseudo-outcome for stage `k-1` is the value function derived from the *targeted* Q-model, `Q_k^*`.
        -   **AIPW/EIF `causal_workflow`:** This will be handled as a single-stage process where the final "outcome" is the EIF itself, not a pseudo-outcome for a prior stage.
