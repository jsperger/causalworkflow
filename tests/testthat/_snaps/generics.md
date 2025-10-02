# extract_preprocessor errors if component is not found

    Code
      extract_preprocessor(cwf, component_id = "nonexistent")
    Condition
      Error in `extract_preprocessor()`:
      ! Component 'nonexistent' not found in stage 1.

# extract_preprocessor errors if stage is not found

    Code
      extract_preprocessor(cwf, component_id = "propensity", stage = 99)
    Condition
      Error in `extract_preprocessor()`:
      ! Component 'propensity' not found in stage 99.
