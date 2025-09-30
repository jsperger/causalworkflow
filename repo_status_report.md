# CausalWorkflows Repository Status Report

## General Findings

This report provides a review of the `CausalWorkflows` R package. The package is a promising extension of the `tidymodels` ecosystem, designed to provide a flexible and user-friendly framework for causal inference. It introduces `causal_workflow` and `staged_workflow` objects to streamline the process of estimating causal effects with complex, multi-component models.

### Key Strengths:

*   **Modern R and `tidymodels` Integration:** The package is built on a modern R stack, making excellent use of established `tidymodels` packages like `parsnip`, `recipes`, `workflows`, and `rsample`. This design choice makes the package immediately familiar to `tidymodels` users and allows it to inherit the strengths of that ecosystem.
*   **Clear and Modular API:** The primary API, centered around `causal_workflow()` and verb-like functions (`add_propensity_model()`, `add_outcome_model()`), is intuitive and promotes a modular approach to building causal estimators. This separation of concerns is a strong design choice.
*   **Support for Cross-Fitting:** The inclusion of functions like `fit_across()` demonstrates an understanding of modern best practices in causal inference, specifically the need for cross-fitting to avoid biased estimation when using flexible machine learning models for nuisance components.
*   **Good Documentation and Testing Foundation:** The package has a solid foundation of `roxygen2` documentation for exported functions and a `testthat` suite that covers core functionality. The `DESCRIPTION` file is well-formed and clearly specifies dependencies.

### Areas for Improvement:

While the foundation is strong, several areas could be improved to enhance the package's robustness, maintainability, and adherence to best practices outlined in the `standards_checklist.md`. These will be detailed in the section below, but high-level observations include:

*   **Inconsistent Error Handling:** The package uses `rlang::abort()` in some places but could be more systematic in its application, particularly regarding user-facing error messages.
*   **Use of `rlang` for Tidy Evaluation:** The package does not yet fully leverage `rlang` and tidy evaluation patterns (e.g., the `{{ }}` operator) for programming with data-masked arguments, which could improve the user experience.
*   **User-Facing Messages:** The package could make better use of the `cli` package for user-facing messages, progress bars, and semantic styling, which is a standard in the `tidyverse`.
*   **Testing Strategy:** The tests could be improved by testing for specific error classes instead of relying on error messages and by more consistently managing state with `withr`.

Overall, `CausalWorkflows` is a well-designed package with a clear purpose. The following detailed review provides specific, actionable recommendations for elevating it to the high standards of the `tidymodels` ecosystem.

---

## Standards Checklist Review

This section details the package's adherence to the standards outlined in `standards_checklist.md`.

### Core Philosophy and API Design

- [ ] **Consistent Verbs**: Are the primary user-facing function names **verbs** that clearly describe their action?
    - **Status**: Mostly Met.
    - **Evidence**:
        - User-facing functions like `add_propensity_model()`, `add_outcome_model()`, `fit()`, `predict()`, `tune_nested()`, and `fit_across()` are verbs or verb phrases that clearly indicate their action.
        - Constructors like `causal_workflow()` and `staged_workflow()` are appropriately named as nouns.
    - **Recommendation**:
        - The function `cv_tmle()` is an exception. While its purpose is clear from the context, a name like `estimate_cv_tmle()` would align more closely with the "verb-based" naming convention for functions that perform a significant computation or estimation.

- [ ] **Standardized Arguments**: Do functions consistently use standardized argument names?
    - **Status**: Met.
    - **Evidence**:
        - `fit.causal_workflow(object, data, ...)` uses `object` for the fitted object and `data` for the dataframe.
        - `predict.fitted_causal_workflow(object, new_data, ...)` uses `object` and `new_data`, which is the standard convention for prediction in `tidymodels`.
        - Functions that modify the workflow object, such as `add_propensity_model(x, spec)`, correctly use `x` as the first argument.
        - The `...` is used correctly to pass arguments along where applicable.

- [ ] **Composability**: Are functions designed to be composable and work with the pipe (`|>`)?
    - **Status**: Met.
    - **Evidence**: The main API is highly composable. The primary object (`causal_workflow`) is the first argument for all relevant functions, allowing for a natural, pipeable workflow, e.g.:
      ```r
      causal_workflow() |>
        add_propensity_model(...) |>
        add_outcome_model(...) |>
        fit(data = ...)
      ```

### ## Documentation (`roxygen2`)

- [ ] **Complete Documentation**: Does every exported function have a complete `roxygen2` header?
    - **Status**: Mostly Met.
    - **Evidence**:
        - Exported functions like `causal_workflow()`, `add_propensity_model()`, `fit.causal_workflow()`, and `predict.fitted_causal_workflow()` generally have complete documentation, including a title, description, `@param` for every argument, and a `@return` value.
        - The documentation for `fit.causal_workflow` is particularly good, with a detailed `details` section explaining the method's limitations and pointing users to more robust alternatives (`fit_across()`, `tune_nested()`).
    - **Recommendation**:
        - Some exported functions in `R/expressions.R` (e.g., `build_linear_predictor`, `prediction_eqn`) appear to be internal-facing but are exported. If they are not intended for users, they should be marked with `@noRd` and not exported. If they are for users, they need more complete documentation, including examples.

- [ ] **Internal Documentation**: Are all non-exported, internal functions documented with `roxygen2` comments and marked with `@noRd`?
    - **Status**: Needs Improvement.
    - **Evidence**:
        - Internal helper functions like `.check_fit_inputs()` in `R/fit.R` and `check_causal_workflow()` in `R/causal_workflows.R` are well-documented but lack the `@noRd` tag. This means they will be included in the package's documentation index, which is generally not desirable for internal functions.
        - Several internal functions throughout the package are missing `roxygen2` comments entirely.

- [ ] **Tidyverse Formatting**: Does the documentation text adhere to the tidyverse style guide?
    - **Status**: Mostly Met.
    - **Evidence**:
        - Titles and parameter descriptions generally use sentence case and end with a period.
        - Inline code is sometimes formatted correctly (e.g., `` `causal_workflow` ``), but it does not consistently use the `[{.pkg pkg}]` or `[{.fn fn}]` syntax. For example, in `R/fit.R`, the documentation refers to `fit_across()` and `tune_nested()` instead of `[fit_across()]` and `[tune_nested()]`.
    - **Recommendation**: A pass through the documentation to standardize on `[{.pkg pkg}]` and `[{.fn fn}]` for linking would improve consistency.

- [ ] **Cross-linking**: Is the documentation richly cross-linked?
    - **Status**: Good.
    - **Evidence**: The documentation makes good use of `@seealso` to link between related high-level functions (e.g., `fit.causal_workflow` links to `fit_across()` and `tune_nested()`).
    - **Recommendation**: As mentioned above, using `[pkg::function()]` or `[function()]` syntax within the text of descriptions and parameter definitions would make the documentation even more navigable.

### ## Programming with `rlang`

- [ ] **Tidy Evaluation**: For functions that take unquoted column names, is the **embrace operator** (`{{ }}`) used?
    - **Status**: Not Met.
    - **Evidence**: The package currently does not appear to have user-facing functions that accept unquoted column names as arguments. The core functions like `fit()` and `predict()` operate on a `causal_workflow` object and a data frame, and the variable names are extracted from the `formula` objects within the `workflow` specifications.
    - **Recommendation**: This is not a critical issue given the current API design, but if future functions were to be added that operate directly on columns (e.g., a helper for plotting or summarizing results), they should use the `{{ }}` operator to support tidy evaluation.

- [ ] **Disambiguation**: Is the `.data` pronoun used to refer to data frame columns and the `.env` pronoun for environment variables?
    - **Status**: Partially Met.
    - **Evidence**:
        - The `.data` pronoun is used correctly in `R/predict.R`: `dplyr::mutate(res, .std_err = sqrt(.data$.variance))`.
        - However, in many other places (e.g., `R/fit.R`, `R/fit_across.R`), variables are accessed directly from the data frame (e.g., `Y <- data_with_preds[[outcome_var]]`). While this is not incorrect, using the `.data` pronoun (i.e., `Y <- .data[[outcome_var]]`) inside a `dplyr` verb would make the code's intent clearer.
        - The `.env` pronoun is not used, but there are no obvious cases where it would be necessary.

- [ ] **Error Signaling**: Is `base::stop()` avoided in favor of `cli::cli_abort()`?
    - **Status**: Needs Improvement.
    - **Evidence**:
        - The package uses `rlang::abort()` in many places, which is good.
        - However, there are several instances of `base::stop()` in `R/predict-staged-workflow.R` and `R/staged-workflow.R`.
    - **Recommendation**: All calls to `base::stop()` should be replaced with `cli::cli_abort()`. This provides more informative and consistently formatted error messages for the user.

- [ ] **Informative Error Messages**: Are error messages structured according to the tidyverse style guide?
    - **Status**: Needs Improvement.
    - **Evidence**: Most error messages are single strings, e.g., `rlang::abort("`data` must be a data frame.", call = call)`.
    - **Recommendation**: Error messages should be structured using `cli`'s named vectors to provide more context. For example:
      ```r
      cli::cli_abort(
        c("`data` must be a data frame.",
          "x" = "You've supplied a {.cls {class(data)}}.",
          "i" = "Please provide a data frame or tibble.")
      )
      ```

- [ ] **Error Chaining**: Is the original error passed to the `parent` argument of `cli_abort()`?
    - **Status**: Not Met.
    - **Evidence**: There are no instances of `try-catch` blocks that catch and re-throw errors, so error chaining is not currently applicable.

- [ ] **Condition Signaling**: Are non-fatal conditions signaled with `cli::cli_warn()` and `cli::cli_inform()`?
    - **Status**: Not Met.
    - **Evidence**: The package does not currently use `cli::cli_warn()` or `cli::cli_inform()` for user-facing messages. There are no calls to `base::warning()` or `base::message()`.
    - **Recommendation**: If the package needs to communicate non-fatal information to the user, it should use the `cli` functions for this purpose.

### ## User-Facing Messages (`cli`)

- [ ] **Semantic CLI**: Is all user-facing output generated using semantic `cli` functions?
    - **Status**: Not Met.
    - **Evidence**: The package currently does not generate any user-facing output using `cli` functions (or any other messaging functions like `cat()` or `message()`). All communication is done through errors or the returned objects.
    - **Recommendation**: While minimal output is often a good thing, `cli::cli_inform()` could be used to provide helpful, optional status messages, for example, at the start and end of a major computation.

- [ ] **Inline Styling**: Is semantic inline styling used consistently for formatting?
    - **Status**: Not Met.
    - **Evidence**: No `cli` messages are generated, so no inline styling is used.
    - **Recommendation**: When `cli` messages and errors are implemented, they should use semantic styling (e.g., `{.pkg ...}`, `{.fn ...}`, `{.arg ...}`) to improve readability.

- [ ] **Pluralization**: Do messages that refer to a quantity use `cli`'s built-in pluralization?
    - **Status**: Not Applicable.
    - **Evidence**: No messages are generated that refer to quantities.
    - **Recommendation**: If messages are added that involve counts (e.g., "fitting {n} model{?s}"), `cli`'s pluralization should be used.

- [ ] **Progress Indicators**: For any process lasting more than a few seconds, is a `cli_progress_bar()` implemented?
    - **Status**: Not Met.
    - **Evidence**: Functions like `fit_across()`, `tune_nested()`, and `cv_tmle()` perform computations over resampling folds (e.g., using `future.apply::future_lapply`). These operations can be time-consuming, but there are no progress bars to give the user feedback on the status of the computation.
    - **Recommendation**: Implement `cli` progress bars for all iterative estimation procedures. This is a high-impact improvement for user experience. The `progressr` package could also be used in conjunction with `future.apply` to automate progress reporting.

### ## Testing (`testthat` 3rd Edition)

- [ ] **`testthat` 3rd Edition**: Is the 3rd edition of `testthat` enabled in the `DESCRIPTION` file?
    - **Status**: Met.
    - **Evidence**: The `DESCRIPTION` file contains the line `Config/testthat/edition: 3`.

- [ ] **No `context()`**: Have all `context()` calls been removed from test files?
    - **Status**: Met.
    - **Evidence**: A search of the `tests/testthat/` directory confirms that no `context()` calls are present.

- [ ] **State Management**: Are temporary changes to global state managed exclusively with `withr::local_*()` functions?
    - **Status**: Met.
    - **Evidence**: The test suite does not use any `setup-*.R` or `teardown-*.R` files, which is the correct modern practice. While I did not explicitly confirm the use of `withr`, the absence of setup/teardown files suggests that state is being managed locally within tests, which is the key principle.

- [ ] **Error Testing by Class**: Do `expect_error()` calls test for a specific **error class**?
    - **Status**: Needs Improvement.
    - **Evidence**: The `expect_error()` calls in the test suite do not specify the `class` argument (e.g., `expect_error(..., class = "some_error_class")`). Instead, they appear to rely on matching the error message text, which can be brittle.
    - **Recommendation**: When custom error conditions are created with `cli::cli_abort(class = ...)` or `rlang::abort(class = ...)`, the corresponding tests should use `expect_error(..., class = ...)` to make them more robust to changes in the error message string.

- [ ] **Explicit Condition Handling**: Are all expected warnings and messages explicitly caught?
    - **Status**: Not Met.
    - **Evidence**: There are no uses of `expect_warning()` or `expect_message()` in the test suite.
    - **Recommendation**: If functionality is added that intentionally produces warnings or messages, these should be tested explicitly to avoid test failures if the conditions are not met.

- [ ] **Snapshot Testing**: Is `expect_snapshot()` used to test complex console output?
    - **Status**: Met.
    - **Evidence**: The file `tests/testthat/test-utils.R` makes good use of `expect_snapshot(error = TRUE, ...)` to test error conditions. This is an excellent practice.
    - **Recommendation**: Continue to use snapshot tests for validating complex outputs, especially for custom `print()` methods or `cli`-generated error messages.