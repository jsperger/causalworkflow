# Key Dependencies

This project relies on a number of key R packages. This document provides a brief overview of the most important ones and their roles in the `causalworkflows` package.

## `dplyr`

`dplyr` is a grammar of data manipulation, providing a consistent set of verbs that help you solve the most common data manipulation challenges.

- **Role in this project**: Used for data manipulation and transformation tasks.
- **Website**: [https://dplyr.tidyverse.org/](https://dplyr.tidyverse.org/)

## `ggplot2`

`ggplot2` is a system for declaratively creating graphics, based on The Grammar of Graphics. You provide the data, tell `ggplot2` how to map variables to aesthetics, what graphical primitives to use, and it takes care of the details.

- **Role in this project**: Used for creating visualizations and plots.
- **Website**: [https://ggplot2.tidyverse.org/](https://ggplot2.tidyverse.org/)

## `parsnip`

`parsnip` is a tidy, unified interface to models. It provides a common syntax for fitting a wide variety of models, from linear regression to complex machine learning algorithms.

- **Role in this project**: Used to specify the models for the causal estimation workflows.
- **Website**: [https://parsnip.tidymodels.org/](https://parsnip.tidymodels.org/)

## `recipes`

The `recipes` package is designed to help you create and manage data preprocessing pipelines. It allows you to define a series of steps for data transformation, such as centering, scaling, and imputation.

- **Role in this project**: Used for preprocessing data before fitting models.
- **Website**: [https://recipes.tidymodels.org/](https://recipes.tidymodels.org/)

## `workflows`

`workflows` bundle your pre-processing, modeling, and post-processing steps into a single object. This makes it easier to manage complex modeling pipelines.

- **Role in this project**: Used to combine `recipes` and `parsnip` models into a single, streamlined workflow.
- **Website**: [https://workflows.tidymodels.org/](https://workflows.tidymodels.org/)

## Other Important Packages

- **`rlang`**: Provides tools for non-standard evaluation and is fundamental to the tidyverse. See `rlang.md` for more details.
- **`cli`**: Used for creating informative and well-styled command-line interfaces. See `cli.md` for more details.