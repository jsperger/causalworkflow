## Use `rlang` for R programming and "tidy evaluation"
Avoid taking variable names as strings. Use `rlang` and nonstandard evaluation
instead of taking strings for variable names. 
### Taking arguments with `rlang`
`rlang` provides a set of tools help you check, validate, and preprocess arguments.

Checking function arguments, e.g. `rlang::arg_match()`,
`rlang::check_required()`, and `rlang::check_exclusive()`. Checking dots, e.g.
`rlang::check_dots_used()`. Collecting dynamic dots, e.g. `rlang::list2()`.
These dots support splicing with `!!!` and name injection with the glue
operators "{" and "{{". 

### Using `rlang` for nonstandard evaluation (NSE) in R packages
#### 1. The Core Pattern: The Embrace Operator `{{ }}`

The embrace operator `{{ }}` is the simplest and most common way to handle
user-supplied variable names. It quotes the argument and immediately unquotes it
inside the function. 
**Use Case:** Functions that operate on data frame columns.

```r
SummarizeVar <- function(data, var) {
  # {{var}} captures the expression passed to `var`
  # and injects it into the dplyr pipeline.
  data |>
    dplyr::summarise(
      n = dplyr::n(),
      mean = mean({{ var }}, na.rm = TRUE),
      sd = sd({{ var }}, na.rm = TRUE)
    )
}

# Usage:
# > SummarizeVar(mtcars, mpg)
#    n     mean       sd
# 1 32 20.09062 6.026948
#
```

-----

#### 2\. The Underlying Mechanism: `enquo()` and `!!` (Bang-Bang)

The `{{ }}` operator is syntactic sugar for `rlang::enquo()` and `!!`.

  * `rlang::enquo(arg)`: **Captures** a function argument as a **quosure** (an expression and its environment).
  * `!!` (bang-bang): **Unquotes** the quosure, injecting the expression it contains into the surrounding code.

```r
SummarizeVarExplicit <- function(data, var) {
  # 1. Capture the argument
  enquo_var <- rlang::enquo(var)

  # 2. Unquote it with !! where needed
  data |>
    dplyr::summarise(
      n = dplyr::n(),
      mean = mean(!!enquo_var, na.rm = TRUE),
      sd = sd(!!enquo_var, na.rm = TRUE)
    )
}

# Usage is identical:
# > SummarizeVarExplicit(mtcars, mpg)
```

**Rule:** Use `{{ }}` for clarity. Use `rlang::enquo()`/`!!` when you need to inspect or modify the captured expression before using it.


#### Table of key `rlang` patterns

| Task                                  | Operator/Function                               | Example                                                  |
| ------------------------------------- | ----------------------------------------------- | -------------------------------------------------------- |
| Capture one argument                  | `{{ arg }}`                                     | `dplyr::summarise(mean = mean({{ my_var }}))`            |
| Capture one arg (explicit)            | `rlang::enquo(arg)`                             | `quo_x <- rlang::enquo(x)`                               |
| Use a captured argument               | `!! quo`                                        | `dplyr::filter(!!quo_x > 0)`                             |
| Capture multiple args (`...`)         | `rlang::enquos(...)`                            | `group_vars <- rlang::enquos(...)`                       |
| Use multiple captured args            | `!!! quos`                                      | `dplyr::group_by(!!!group_vars)`                         |
| Use a string as a variable name       | `.data[[string]]`                               | `ggplot2::aes(x = .data[["sepal_length"]])`              |
| Disambiguate data-variable            | `.data$var` or `.data[[var]]`                   | `dplyr::mutate(z = .data$x + .data$y)`                   |
| Disambiguate environment-variable     | `.env$var` or `.env[[var]]`                     | `dplyr::mutate(z = .data$x * .env$factor)`               |
| Create a symbol from a string         | `rlang::sym("string")`                          | `dplyr::select(!!rlang::sym("my_col"))`                  |
| Assign to a dynamic name              | `!!name_string := value`                        | `dplyr::mutate(!!new_col_name := {{ old_col }} * 2)`      |
| Get a string from a captured argument | `rlang::as_name(rlang::enquo(arg))`             | `name <- rlang::as_name(rlang::enquo(my_var))`           |
