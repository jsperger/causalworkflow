# Argument Names

## Dot Usage

 * If there is a possibility of argument name conflicts between the function and any arguments passed down through `...`, it is strongly suggested that the argument names to the main function be prefixed with a dot (e.g. `.data`, `.x`, etc.)

 * When defining the order of arguments in a function, try to keep the `...` as far to the left as possible to coerce users to explicitly name all arguments to the right of `...`. 

 * Design similar functions using a common prefix so that users can easily [tab complete](https://twitter.com/MilesMcBain/status/1186464948546723840?s=20) in the IDE when searching. [{note}](#tabs) 
 
## Standardization of Arguments

Both within- and between-packages, tidymodels projects standardize on names. Examples follow. 

### Data Arguments

 * `na_rm`: missing data handling. 

 * `new_data`: data to be predicted.

 * `weights`: case weights.

 * For `.data.frame` methods:

    * `x`: predictors or generic data objects. 
 
    * `y`: outcome data.
 
 * For `.formula` methods:
 
    * `formula`: a `y ~ x` formula specifying the outcome and predictors.
    
    * `data`: the data.frame to pull formula variables from.

### Numerical Arguments

 * `times`: the number of bootstraps, simulations, or other replications.
 
### Statistical Quantities 

 * `direction`: the type of hypothesis test alternative. 
 
 * `level`: interval levels (e.g., confidence, credible, prediction, and so on).

 * `link`: link functions for generalized linear models. 
 

### Tuning Parameters

* `activation`: the type of activation function between network layers. 

* `cost`: a cost value for SVM models. 
  
* `cost_complexity`: The cost-complexity parameter in classical CART models. 

* `deg_free`: a parameter for the degrees of freedom. 

* `degree`: the polynomial degree. 
 
* `dropout`: the parameter dropout rate. 
 
* `epochs`: the number of iterations of training. 
 
* `hidden_units`: the number of hidden units in a network layer. 

* `Laplace`: the Laplace correction used to smooth low-frequency counts. 

* `learn_rate`: the rate at which the boosting algorithm adapts from iteration-to-iteration. 
   
* `loss_reduction`:  The reduction in the loss function required to split further. 

* `min_n`: The minimum number of data points in a node that are required for the node to be split further. 
 
* `mixture`: the proportion of L1 regularization in the model. 
 
* `mtry`: The number of predictors that will be randomly sampled at each split when creating the tree models.

* `neighbors`: a parameter for the number of neighbors used in a prototype model. 

* `num_comp`: the number of components in a model (e.g. PCA or PLS components).
 
* `num_terms`: a nonspecific parameter for the number of terms in a model. This can be used with models that include feature selection, such as MARS. 
  
* `prod_degree`: the number of terms to combine into interactions. A value of 1 implies an additive model. Useful for MARS models and some linear models.
    
* `prune`: a logical for whether a tree or set of rules should be pruned. 

* `rbf_sigma`: the sigma parameters of a radial basis function. 

* `penalty`: The amount of regularization used. In cases where different penalty types require to be differentiated, the names `L1` and `L2` are recommended.
   
* `sample_size`: the size of the data set used for modeling within an iteration of the modeling algorithm, such as stochastic gradient boosting.

* `surv_dist`: the statistical distribution of the data in a survival analysis model.
  
* `tree_depth`: The maximum depth of the tree (i.e. number of splits).
   
* `trees`: The number of trees contained in a random forest or boosted  ensemble. In the latter case, this is equal to the number of boosting iterations.  
  
* `weight_func`: The type of kernel function that weights the distances between samples (e.g. in a K-nearest neighbors model).   
 
### Others

* `fn` and `fns` when a single or multiple functions are passed as arguments (respectively). 

 
--- End of File: 06-arguments.Rmd ---


--- Start of File: 01-general-conventions.Rmd ---
Source Path: conventions/01-general-conventions.Rmd
MIME Type: text/plain

# General Conventions

* Code should follow the [tidyverse style conventions](http://style.tidyverse.org/)

* All results must be reproducible from run-to-run.

<div id="messages-back"></div>

* If there is a strong need for producing output during execution, there should be a `verbose` option that defaults to no output. [{notes}](#messages)

--- End of File: 01-general-conventions.Rmd ---


--- Start of File: 05-model-predictions.Rmd ---
Source Path: conventions/05-model-predictions.Rmd
MIME Type: text/plain

# Model Predictions

 * To be consistent with snake_case, `new_data` should be used instead of `newdata`. 

<div id="discuss-back"></div> 

* The function to produce predictions should be a class-specific `predict` method with arguments `object`, `new_data`, and possibly `type`. Other arguments, such as `level`, should be standardized. [{note}](#discuss)

* The main predict method can internally defer to separate, unexported functions (`predict_class`, etc). 

* `type` should also come from a set of pre-defined values such as

| type         	| application                                 	|
|--------------	|---------------------------------------------	|
| `numeric`    	| numeric predictions                         	|
| `class`      	| hard class predictions                      	|
| `prob`       	| class probabilities, survivor probabilities 	|
| `link`       	| `glm` linear predictor                      	|
| `conf_int`   	| confidence intervals                        	|
| `pred_int`   	| prediction intervals                        	|
| `raw`        	| direct access to prediction function        	|
| `param_pred` 	| predictions across tuning parameters        	|
| `quantile`      | quantile predictions                          |

and should be validated using `match.arg()`.

* To determine whether or not to return standard errors for predictions, use a `std_error` argument that takes on `TRUE/FALSE` value. By default, do not report standard error or other measures of uncertainty, as these can be expensive to compute. Clearly document whether any standard errors are for confidence or prediction intervals.

Other values should be assigned with consensus. 

## Input Data

* If `new_data` is not supplied, an error should be thrown. It should **not** default to an archived version of the training set contained in the model object. 

* The data requirements for `new_data` should be the same as those for the orginal model fit function. 

* The model outcome should never be required to be in `new_data`. 

* `new_data` should be tolerant of extra columns. For example, if all variables are in some data frame `dataset`, `predict(object, dataset)` should immediately know which variables are required for prediction, check for their presence, and select only those from `dataset` before proceeding. 

* The prediction code should work whether `new_data` has multiple rows or a single row. 

<div id="splines-trap-back"></div>

* Predictions should not depend on which observations are present in `new_data`. [{note}](#splines-trap).

* When novel factor levels appear in the test set for factor predictors, the default behavior should be to throw an informative error. For models where this is a reasonable way to make predictions on novel factor levels, users need to explicitly specify that they want this behavior, and it's good practice to `message()` for these prediction cases.

## Return Values

* By default, `new_data` should not be returned by the prediction function. 

 <div id="dplyr-like-back"></div>

 * The return value is a tibble with the **same number of rows as the data being predicted** and in the same order. This implies that `na.action` should not affect the dimensions of the outcome object (i.e., it should be ignored).  [{note}](#binding) The class of the tibble can be overloaded to accommodate specialized methods as long as basic data frame functionality is maintained.  [{note}](#dplyr-like). For observations with missing data such that a prediction cannot be generated, we recommend returning `NA`.
 
 * The return tibble can contain extra attributes for values relevant to the prediction (e.g. `level` for intervals) but care should be taken to make sure that these attributes are not destroyed when standard operations are applied to the tibble (e.g. `arrange`, `filter`, etc.). Columns of constant values (e.g. adding `level` as a column) should be avoided. 
 
Specific cases:
 
   * For univariate, numeric point estimates, the column should be named `.pred`. For multivariate numeric predictions (excluding probabilities), the columns should be named `.pred_{outcome name}`.
      
   * Class predictions should be factors with the same levels as the original outcome and named `.pred_class`. 
   
   * For class probability predictions, the columns should be named the same as the factor levels, e.g., `.pred_{level}`, and there should be as many columns as factor levels. 
 
   * If interval estimates are produced (e.g. prediction/confidence/credible), the column names should be `.pred_lower` and `.pred_upper`. If a standard error is produced, the column should be named `.std_error`. If intervals are produced for class probabilities, the levels should be included (e.g., `.pred_lower_{level}`),
 
 <div id="list-cols-back"></div>
 
   * For predictions that are not simple scalars, such as distributions or non-rectangular structures, the `.pred` column should be a list-column [{note}](#list-cols) 
   
 * In cases where the outcome is being _directly_ predicted, the predictions should be on the same scale as the outcome. The same would apply to associated interval estimates. This is equivalent to `type = "response"` for generalized linear models and the like. Reasonable exceptions include estimation of the standard error of prediction (perhaps occurring on the link-level/scale of the linear predictors). 
