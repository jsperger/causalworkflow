### ## Core Philosophy and API Design

- [ ] [cite_start]**Consistent Verbs**: Are the primary user-facing function names **verbs** that clearly describe their action (e.g., `fit()`, `predict()`, `collect_metrics()`)? [cite: 5, 29]
- [ ] **Standardized Arguments**: Do functions consistently use standardized argument names for common objects?
    - [ ] [cite_start]`data`: for the primary data frame or tibble. [cite: 31]
    - [ ] [cite_start]`object`: for a fitted model or other primary S3 object input. [cite: 33]
    - [ ] [cite_start]`x`, `y`: for predictor and outcome data in non-formula interfaces. [cite: 32]
    - [ ] [cite_start]`...`: for passing arguments to other methods. [cite: 31]
- [ ] [cite_start]**Composability**: Are functions designed to be composable and work with the pipe (`|>`), with the primary data object as the first argument? [cite: 18]

---

### ## Documentation (`roxygen2`)

- [ ] [cite_start]**Complete Documentation**: Does every exported function have a complete `roxygen2` header, including a title, description, `@param` for every argument, and a `@return` value? [cite: 35]
- [ ] [cite_start]**Internal Documentation**: Are all non-exported, internal functions documented with `roxygen2` comments and marked with `@noRd`? [cite: 36]
- [ ] **Tidyverse Formatting**: Does the documentation text adhere to the tidyverse style guide?
    - [ ] [cite_start]Titles and parameter descriptions use sentence case and end with a period. [cite: 37]
    - [ ] [cite_start]Inline code is correctly formatted (e.g., `[dplyr::mutate()]`, `{.pkg forcats}`, `na.rm`). [cite: 37]
- [ ] [cite_start]**Cross-linking**: Is the documentation richly cross-linked to other relevant functions (`[pkg::function()]`) and vignettes (`vignette("topic-name")`) to aid user discovery? [cite: 40, 41]

---

### ## Programming with `rlang`

- [ ] [cite_start]**Tidy Evaluation**: For functions that take unquoted column names, is the **embrace operator** (`{{ }}`) used to pass the argument to data-masking functions? [cite: 45, 46]
- [ ] [cite_start]**Disambiguation**: In programming contexts, is the `.data` pronoun used to explicitly refer to data frame columns and the `.env` pronoun used for environment variables to prevent ambiguity? [cite: 48, 49]
- [ ] [cite_start]**Error Signaling**: Is `base::stop()` avoided entirely in favor of `cli::cli_abort()` for signaling all fatal errors? [cite: 58, 88]
- [ ] **Informative Error Messages**: Are error messages structured according to the tidyverse style guide?
    - [ ] [cite_start]A concise, high-level statement of the problem. [cite: 61]
    - [ ] [cite_start]A bulleted list using `x` for the problem, `i` for context/solutions, and `*` for general points. [cite: 61]
- [ ] [cite_start]**Error Chaining**: When an error is caught and re-thrown, is the original low-level error passed to the `parent` argument of `cli_abort()` to preserve the full backtrace? [cite: 72, 73]
- [ ] [cite_start]**Condition Signaling**: Are non-fatal conditions signaled with `cli::cli_warn()` and `cli::cli_inform()` instead of `base::warning()` and `base::message()`? [cite: 65, 88]

---

### ## User-Facing Messages (`cli`)

- [ ] [cite_start]**Semantic CLI**: Is all user-facing output generated using semantic `cli` functions (e.g., `cli_alert_success()`, `cli_h1()`, `cli_ul()`) rather than unstructured `cat()` or `message()` calls? [cite: 83, 84, 85, 86]
- [ ] [cite_start]**Inline Styling**: Is semantic inline styling used consistently for formatting? [cite: 96, 97]
    - [ ] [cite_start]`{.pkg package_name}` [cite: 98]
    - [ ] [cite_start]`{.fn function_name}` [cite: 98]
    - [ ] [cite_start]`{.arg argument_name}` [cite: 98]
    - [ ] [cite_start]`{.val value}` [cite: 98]
- [ ] [cite_start]**Pluralization**: Do messages that refer to a quantity use `cli`'s built-in pluralization (e.g., `"fit {n} model{?s}"`) to ensure grammatical correctness? [cite: 99]
- [ ] [cite_start]**Progress Indicators**: For any process lasting more than a few seconds, is a `cli_progress_bar()` implemented? [cite: 91]
    - [ ] [cite_start]Is the bar correctly initialized before the loop, updated with `cli_progress_update()` inside the loop, and terminated with `cli_progress_done()` after the loop? [cite: 93]

---

### ## Testing (`testthat` 3rd Edition)

- [ ] [cite_start]**`testthat` 3rd Edition**: Is the 3rd edition of `testthat` enabled in the `DESCRIPTION` file (`Config/testthat/edition: 3`)? [cite: 108, 109]
- [ ] [cite_start]**No `context()`**: Have all `context()` calls been removed from test files? [cite: 110, 111]
- [ ] [cite_start]**State Management**: Are temporary changes to global state (options, environment variables, files) managed exclusively with `withr::local_*()` functions inside `test_that()` blocks, avoiding `setup()` and `teardown()` files? [cite: 116, 117]
- [ ] [cite_start]**Error Testing by Class**: Do `expect_error()` calls test for a specific **error class** and *not* a message string via the `regexp` argument? [cite: 119, 120, 122]
- [ ] [cite_start]**Explicit Condition Handling**: Are all expected warnings and messages explicitly caught with `expect_warning()` or `expect_message()`, or silenced if they are irrelevant to the test? [cite: 112]
- [ ] [cite_start]**Snapshot Testing**: Is `expect_snapshot()` used to test complex console output (e.g., from `print()` methods) instead of fragile character string comparisons? [cite: 124, 127]
