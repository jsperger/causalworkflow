# Roxygen Documentation

This guide provides instructions on how to use `roxygen2` to document R functions, following the tidyverse style guide.

## Roxygen Basics

- **Workflow**:
  1. Add roxygen comments (starting with `#'`) to your `.R` files.
  2. Run `devtools::document()` to generate `.Rd` files.
  3. Preview documentation with `?function`.

- **Structure**:
  - Roxygen comments are organized into blocks.
  - Blocks are broken up by tags (e.g., `@param`, `@examples`).
  - The text before the first tag is the introduction (title and description).

- **Markdown**: Use markdown for formatting. Key features:
  - Backticks for inline code: `` `my_function()` ``
  - Square brackets for auto-linked functions: `[my_function()]`
  - Bulleted lists for easier reading.

## Title, Description, and Details

- **Title**: The first sentence of the documentation. Should be in sentence case and not end with a period.
- **Description**: The paragraph after the title. A brief summary of the function.
- **Details (`@details`)**: Optional section for more in-depth explanation. Use markdown headings (`#` or `@section`) to structure long details.

## Documenting Parameters (`@param`)

- Document each parameter with `@param name description`.
- The description should be a sentence, starting with a capital letter and ending with a period.
- If multiple arguments are related, document them together by separating names with commas (e.g., `@param x,y A pair of vectors.`).
- Use `@inheritParams source_function` to inherit parameter documentation from another function.

## Return Value (`@returns`)

- Use `@returns` to describe the function's output. For historical reasons, you can also use `@return`, but `@returns` is preferred.
- Describe the shape of the output (e.g., type and length of a vector, columns of a data frame).

## Examples (`@examples`)

- Provide executable R code to demonstrate the function's usage.
- Keep examples self-contained and focused on the most important features.
- Do not include code that throws errors unless wrapped in `try()` or `\dontrun{}`.
- For code that depends on other packages, use `library()` or `::`.
- Use `@examplesIf condition` to run examples conditionally.

## Cross-Linking

- Use `@seealso` to link to related functions.
- Link to functions in other packages with `[pkg::function()]`.
- Use the `@family` tag to group related functions.

## Re-using Documentation

- Document multiple functions in one topic using `@rdname`.
- Inherit documentation from another function using `@inherit`, `@inheritSection`, and `@inheritDotParams`.
- Use child documents (````{r child = "path/to/file.Rmd"}`) to share content between documentation and vignettes.

## Other Markdown Files

- **`README.md`**: Provides a high-level overview of the package. Create it from `README.Rmd` using `devtools::build_readme()`.
- **`NEWS.md`**: Lists changes for each release. Use `usethis::use_news_md()` to create it.

## Original `agents.md` Roxygen Guidance

Document functions with `#'`. Follow this tag order:
1.  Title (first line)
2.  Description (paragraph after title)
3.  `@param`
4.  `@return`
5.  `@examples` (wrap code in `\dontrun{}`)
*Use `@details` and `@seealso` if necessary. You may use `@inheritParams fn`
instead of documenting the parameters; if `@inheritParams fn` is used only
document parameters that are not shared or that are used in an important and
different way.

Don't include the R function definition in the title.
Be clear and concise, and write for a technical expert audience.