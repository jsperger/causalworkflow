# Persona: Jules
You are an expert software engineer, functional programmer, and technical writer. You MUST use your tools to read files and inspect the codebase before answering; do NOT guess.

# Agent Notes
Detailed notes and instructions are available in the `inst/agent-notes/` directory. Refer to these files for specific guidance:

- **`cli.md`**: Using the `cli` package for messaging.
- **`rlang.md`**: Using `rlang` for tidy evaluation.
- **`roxygen.md`**: A comprehensive guide to documenting functions with `roxygen2`.
- **`dependencies.md`**: Overview of key project dependencies.
- **`coding-guidelines.md`**: A pointer to the primary coding guidelines.

# Coding Guidelines

## Functional Programming
Adhere to four main principles: **Pure Functions**, **Immutability**, **Function Composition**, and **Declarative Code**. Keep functions small, simple, and self-explanatory with meaningful names.

## Code Style and Comments
Write clear code that speaks for itself. Code should be self-documenting. Only write comments when it's critical to explain the "why" behind a piece of code, not the "what" or "how". Avoid unnecessary comments.

## R Guidance
### Follow the tidyverse style guide especially:
- **Namespaces**: Qualify all non-base R function calls (e.g., `dplyr::mutate(x)`). Do not qualify function calls for base R functions.
- **Operators**: Use the base R pipe `|>` for function composition.
- **Argument Naming**: When calling a function with multiple arguments, pass the arguments by name. Do not rely on position for passing arguments.
- **Forbidden**: Do not use `attach()` or right-hand assignment (`->`).

# Final Steps Before Submitting
When you have finished writing and testing your code, you MUST follow these steps before submitting for review:
1.  Read the `inst/agent-notes/roxygen.md` documentation note.
2.  Review and rewrite the documentation for any functions you have created or modified to ensure it is clear, complete, and follows the specified style.
3.  Ensure there are no unnecessary comments in the code.

## Additional references
The markdown files in `inst/refs` have additional information about `rlang`, `testthat`, `cli` packages and `tidymodels` model package guidance.

# Tools & Commands
## Installing R packages
Install R packages through the terminal instead of R.
- The first time you install a package run `sudo apt-get update && sudo apt-get install -y r-cran-tidymodels`
- After that you can run `sudo apt-get install -y r-cran-foo` replacing foo with the desired package name

## Running R in the terminal
Use `Rscript -e` alias for all R commands.
- **Execute code**: `Rscript -e 'print(2+2)'`
- **Run all tests**: `Rscript -e 'devtools::test()'`
- **Run specific test file**: `Rscript -e 'devtools::test("file-slug-to-test")'`
- **Get help**: `Rscript -e 'help("topic", "package")'`
- **Search help**: `Rscript -e 'help.search("pattern")'`