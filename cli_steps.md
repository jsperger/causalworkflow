## Overview

The `cli` package provides a powerful and flexible way to create command-line interfaces. It allows for themed output, pluralization, and structured messages, which can significantly improve the user experience of your package.

## Using `cli` Features
### Semantic Messages

`cli` provides a set of functions for creating semantically meaningful messages. These functions automatically apply consistent styling to different types of messages.

  - `cli_success()`: For success messages.
  - `cli_info()`: For informational messages.
  - `cli_warn()`: For warnings.
  - `cli_danger()` and `cli_abort()`: For errors.

These functions use inline styling with `{}` similar to the `glue` package. You can use `.cls` to apply CSS-like classes for styling.

### Pluralization

`cli` simplifies the creation of messages that need to handle singular and plural forms. It uses a special syntax `{?}` to automatically select the correct form based on the length of a vector.

For example, `cli_text("Found {length(files)} file{?s}.")` will produce "Found 1 file." if `length(files)` is 1, and "Found 2 files." otherwise. You can also handle irregular plurals, like `cli_text("Found {length(x)} director{?y/ies}.")`.

### Condition Formatting

`cli` provides an improved way to format conditions like errors, warnings, and messages. It is recommended to use `cli::cli_abort()`, `cli::cli_warn()`, and `cli::cli_inform()` instead of their base R or `rlang` counterparts. These functions enable `cli` formatting, which includes features like line wrapping and styled bullets.

To enable `cli` formatting globally for your package, you can call `cli::local_use_cli()` in your package's `.onLoad()` function.

-----

## Steps to Update a Package

Here are the steps to update your R package to use `cli`:
**Replace Messaging Functions**: Replace base R messaging functions with their `cli` equivalents:

      - [ ] `message()` -\> `cli_info()`
      - [ ] `warning()` -\> `cli_warn()`
      - [ ] `stop()` -\> `cli_abort()`
Examples:
```r
# Instead of:
# stop("Invalid input. 'x' must be a numeric vector.")

# Use:
cli::cli_abort(c(
  "Invalid input.",
  "x" = "Argument {.arg x} must be a numeric vector, not a {.cls {class(x)}}."
))
```

```r
# Instead of:
# message("Processing complete.")

# Use:
cli::cli_success("Processing complete.")
```

**Enhance Messages**: Use `cli`'s features to improve your messages.

    - [ ] Use inline styling to highlight important parts of your messages.
    - [ ] Use pluralization to make your messages grammatically correct.
```r
files <- c("a.txt", "b.txt")
# Instead of:
# message(paste("Found", length(files), "files."))

# Use:
cli::cli_info("Found {length(files)} file{?s}.")
```

    - [ ] Use lists to structure your messages and make them easier to read.

cli has three types of list: ordered, unordered and definition lists, see cli_ol(), cli_ul() and cli_dl():
```r
cli::cli_ol(c("item 1", "item 2", "item 3"))

#> 1. item 1                                                                       
#> 2. item 2                                                                       
#> 3. item 3                                                                       

cli::cli_ul(c("item 1", "item 2", "item 3"))

#> • item 1                                                                        
#> • item 2                                                                        
#> • item 3                                                                        

cli::cli_dl(c("item 1" = "description 1", "item 2" = "description 2",
         "item 3" = "description 3"))

#> item 1: description 1                                                           
#> item 2: description 2                                                           
#> item 3: description 3                                                           
```

## Glue Syntax vs. `paste`

The `cli` package's functions use `glue`-like syntax for string interpolation which is often more readable and concise than using `paste()` or `paste0()`.

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

