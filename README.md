bmi510package
=============

bmi510package is a collection of useful functions for data manipulation, statistical analysis, and performance evaluation. This package is specifically designed to facilitate tasks such as random sampling, scaling data, calculating log-likelihoods, and evaluating classifier performance metrics.  
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

Author
------------
Mingzhe Hu (mingzhehu511@gmail.com)

Source Files
------------
[Template for this package](https://drive.google.com/file/d/109bZOR28oshNCSbml-mYg_7tyqK4kBXm/view?usp=share_link)

[Test cases](https://drive.google.com/file/d/10UJXtxxI705BaEKzZaFvNyP9PEjr9xyq/view?usp=share_link)  
We have passed all the test cases.


Installation
------------

You can install bmi510package directly from GitHub using the `devtools` package. First, make sure you have `devtools` installed:


`install.packages("devtools")`

Next, install the `bmi510package` from GitHub:


`devtools::install_github("MingzheHu-Duke/bmi510_package")`

Finally, load the package in your R session:


`library(bmi510package)`  

**OR**  
To install the `bmi510package` from a local `.tar.gz` file, follow these steps:

1.  Download the `bmi510package_0.1.0.tar.gz` file and save it to a known location on your computer.

2.  In your R session, use the `install.packages()` function with the `repos` argument set to `NULL` and specify the local file path:


`install.packages("path/to/bmi510package_0.1.0.tar.gz", repos = NULL, type = "source")`

Replace `"path/to/bmi510package_0.1.0.tar.gz"` with the actual path to the downloaded `.tar.gz` file on your computer.

Load the package in your R session:

`library(bmi510package)`

Now you're ready to use the functions provided by the `bmi510package`.

Functions
---------

bmi510package includes the following functions:

-   `rando()`: A wrapper around `sample()` for randomly sampling atomic vectors or dataframe-like objects.
-   `is_min()` and `is_max()`: Functions to identify the minimum or maximum values in an atomic vector.
-   `rep_mat()`: A port of the `repmat.m` function from MATLAB, used for replicating matrix rows or columns.
-   `classes()`: Returns a character vector containing the classes of each variable in a tibble.
-   `df_scale()`: Scales the numeric variables in a tibble with optional centering and scaling.
-   `log_likelihood_*()`: A set of functions to calculate log-likelihoods under various distributions (normal, uniform, chi-squared, F, and t).
-   `sensitivity()`, `specificity()`, `precision()`, `recall()`, `accuracy()`, and `f1()`: Functions to calculate various performance metrics for binary classifiers.
-   `minimum_n_per_group()`: Returns the minimum sample size per group needed for a two-sample t-test, based on the expected Cohen's d and desired statistical power.
-   `r2()`: Calculates the R-squared statistic between predicted and ground truth continuous variables.
-   `adj_R2()`: Calculates the adjusted R-squared statistic between predicted and ground truth continuous variables, accounting for the number of model parameters.

Examples
--------
<pre>
```R
`# Randomly sample rows from a data.frame
data(mtcars)
sampled_rows <- rando(mtcars, n = 5, replace = FALSE)

# Calculate log-likelihood under normal distribution
x <- rnorm(100, mean = 0, sd = 1)
log_likelihood_norm(x, mean = 0, sd = 1)

# Evaluate classifier performance
pred <- factor(c(1, 0, 1, 1, 0))
truth <- factor(c(1, 0, 1, 0, 0))
accuracy(pred, truth)`
```
</pre>

For detailed information on each function, refer to the package documentation.

Contributing
------------

Feel free to submit issues, fork the repository, and create pull requests. Contributions are welcome!

License
-------

This project is licensed under the MIT License.
