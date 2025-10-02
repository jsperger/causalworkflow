# add_options errors with an invalid component_id

    Code
      add_options(causal_workflow(), component_id = "nonexistent", options = control_propensity())
    Condition
      Error in `add_options()`:
      ! Component 'nonexistent' not found in stage 1.
