# add_stage requires .stage argument

    Code
      add_stage(causal_workflow(), outcome = make_wflow())
    Condition
      Error in `add_stage()`:
      ! `\.stage` must be provided.

# add_stage prevents duplicate stages

    Code
      add_stage(add_component(causal_workflow(), "propensity", make_wflow(), stage = 1),
      outcome = make_wflow(), .stage = 1)
    Condition
      Error in `add_stage()`:
      ! Stage 1 already exists.

# add_component prevents duplicate component_id within a stage

    Code
      add_component(add_component(causal_workflow(), "propensity", make_wflow(),
      stage = 1), "propensity", make_wflow(), stage = 1)
    Condition
      Error in `add_component()`:
      ! Component 'propensity' already exists in stage 1.
