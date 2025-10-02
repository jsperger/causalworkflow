# fit.causal_workflow requires an engine

    Code
      fit(causal_workflow(), data = mtcars)
    Condition
      Error in `fit()`:
      ! The `engine` argument must be specified.

# fit.causal_workflow errors with an unknown engine

    Code
      fit(causal_workflow(), data = mtcars, engine = "unknown_engine")
    Condition
      Error in `fit()`:
      ! Engine 'unknown_engine' is not a recognized engine.
