## Using `cli` Features
### Semantic Messages

`cli` provides a set of functions for creating semantically meaningful messages. These functions automatically apply consistent styling to different types of messages.

  - `cli::cli_success()`: For success messages.
  - `cli::cli_info()`: For informational messages.
  - `cli::cli_warn()`: For warnings.
  - `cli::cli_danger()` and `cli_abort()`: For errors.

These functions use inline styling with `{}` similar to the `glue` package. You can use `.cls` to apply CSS-like classes for styling.

```r
# Instead of:
# user_name <- "Alex"
# file_count <- 5
# message(paste0("User '", user_name, "' has ", file_count, " files."))

# Use:
user_name <- "Alex"
file_count <- 5
cli::cli_inform("User {.val {user_name}} has {file_count} files.")

```

### Pluralization

`cli` simplifies the creation of messages that need to handle singular and plural forms. It uses a special syntax `{?}` to automatically select the correct form based on the length of a vector.

For example, `cli_text("Found {length(files)} file{?s}.")` will produce "Found 1 file." if `length(files)` is 1, and "Found 2 files." otherwise. You can also handle irregular plurals, like `cli_text("Found {length(x)} director{?y/ies}.")`.

### Condition Formatting

`cli` provides an improved way to format conditions like errors, warnings, and messages. It is recommended to use `cli::cli_abort()`, `cli::cli_warn()`, and `cli::cli_inform()` instead of their base R or `rlang` counterparts. These functions enable `cli` formatting, which includes features like line wrapping and styled bullets.