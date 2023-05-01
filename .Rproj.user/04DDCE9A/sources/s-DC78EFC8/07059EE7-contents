# bmi510.R
library(pwr)

#' Randomly sample elements or rows from a vector or dataframe-like object
#'
#' @param x An atomic vector or dataframe-like object to sample from
#' @param n Number of samples to return (default is 1)
#' @param replace Logical, whether to sample with replacement (default is TRUE)
#'
#' @return A vector or dataframe with n samples or rows, depending on the input type
#' @examples
#' rando(1:10, 5)
#' rando(mtcars, 3)
#' rando(letters, 3, replace = F)
rando = function(x, n = 1, replace = T) {
  if (is.data.frame(x) || is.matrix(x)) {
    return(x[sample(nrow(x), n, replace = replace), ])
  } else {
    return(sample(x, n, replace = replace))
  }
}

#' Check if an element in a vector is equal to the minimum value
#'
#' @param x An atomic vector to check for minimum values
#' @param na.rm Logical, whether to remove NA values before checking (default is TRUE)
#'
#' @return A logical vector with TRUE where x equals its minimum value
#' @examples
#' is_min(c(1, 2, 3, 1))
#' is_min(c(5, NA, 2, 7, 2), na.rm = F)
#' is_min(rnorm(10))
is_min = function(x, na.rm = T) {
  return(x == min(x, na.rm = na.rm))
}

#' Check if an element in a vector is equal to the maximum value
#'
#' @param x An atomic vector to check for maximum values
#' @param na.rm Logical, whether to remove NA values before checking (default is TRUE)
#'
#' @return A logical vector with TRUE where x equals its maximum value
#' @examples
#' is_max(c(1, 2, 3, 1))
#' is_max(c(5, NA, 2, 7, 2), na.rm = F)
#' is_max(rnorm(10))
is_max = function(x, na.rm = T) {
  return(x == max(x, na.rm = na.rm))
}

#' Replicate the rows and columns of a matrix or dataframe
#'
#' @param x A matrix or dataframe to replicate
#' @param M Number of times to replicate the rows (default is 1)
#' @param N Number of times to replicate the columns (default is 1)
#'
#' @return A matrix created by replicating the rows and columns M and N times
#' @examples
#' rep_mat(matrix(1:4, nrow = 2), M = 2, N = 2)
#' rep_mat(mtcars[1:5, 1:3], M = 3)
#' rep_mat(matrix(rnorm(9), nrow = 3), N = 4)
rep_mat = function(x, M = 1, N = 1) {
  return(matrix(rep(x, each = N, times = M), nrow = NROW(x) * M, byrow = TRUE))
}

#' Return the classes of each variable in a tibble
#'
#' @param x A tibble for which to return the classes of each variable
#'
#' @return A character vector containing the classes of each variable in a tibble x
#' @examples
#' classes(iris)
#' classes(mtcars)
#' classes(data.frame(x = 1:10, y = letters[1:10], z = factor(1:10)))
classes = function(x) {
  return(sapply(x, class))
}

#' Scale numeric variables in a tibble
#'
#' @param x A tibble to scale
#' @param center Logical, whether to center the numeric variables (default is TRUE)
#' @param scale Logical, whether to scale the numeric variables (default is TRUE)
#'
#' @return A tibble x in which the numeric variables have been scaled
#' @examples
#' df_scale(iris)
#' df_scale(mtcars, center = F)
#' df_scale(data.frame(x = rnorm(10), y = rnorm(10, mean = 5), z = rnorm(10, sd = 2)), scale = F)
df_scale = function(x, center = T, scale = T) {
  numeric_vars <- sapply(x, is.numeric)
  if (center) {
    x[numeric_vars] <- lapply(x[numeric_vars], scale, center = center, scale = FALSE)
  }
  if (scale) {
    x[numeric_vars] <- lapply(x[numeric_vars], scale, center = FALSE, scale = scale)
  }
  return(x)
}

#' Calculate the log-likelihood of a sample under the normal density
#'
#' @param x A numeric vector of observations
#' @param mean Mean of the normal distribution
#' @param sd Standard deviation of the normal distribution
#'
#' @return The log-likelihood of the sample under the normal density
#' @examples
#' log_likelihood_norm(rnorm(100), 0, 1)
#' log_likelihood_norm(rnorm(100, mean = 5, sd = 2), 5, 2)
#' log_likelihood_norm(c(1, 2, 3, 4, 5), 3, 1)
log_likelihood_norm = function(x, mean, sd) {
  return(sum(dnorm(x, mean, sd, log = T)))
}

#' Calculate the log-likelihood of a sample under the uniform density
#'
#' @param x A numeric vector of observations
#' @param min Minimum value of the uniform distribution
#' @param max Maximum value of the uniform distribution
#'
#' @return The log-likelihood of the sample under the uniform density
#' @examples
#' log_likelihood_unif(runif(100, min = 0, max = 1), 0, 1)
#' log_likelihood_unif(runif(100, min = -2, max = 2), -2, 2)
#' log_likelihood_unif(c(1, 2, 3, 4, 5), 1, 5)
log_likelihood_unif = function(x, min, max) {
  return(sum(dunif(x, min, max, log = T)))
}

#' Calculate the log-likelihood of a sample under the chi-squared density
#'
#' @param x A numeric vector of observations
#' @param df Degrees of freedom of the chi-squared distribution
#'
#' @return The log-likelihood of the sample under the chi-squared density
#' @examples
#' log_likelihood_chisq(rchisq(100, df = 3), 3)
#' log_likelihood_chisq(rchisq(100, df = 10), 10)
#' log_likelihood_chisq(c(1, 2, 3, 4, 5), 2)
log_likelihood_chisq = function(x, df) {
  return(sum(dchisq(x, df, log = T)))
}

#' Calculate the log-likelihood of a sample under the F density
#'
#' @param x A numeric vector of observations
#' @param df1 Degrees of freedom for the numerator of the F distribution
#' @param df2 Degrees of freedom for the denominator of the F distribution
#'
#' @return The log-likelihood of the sample under the F density
#' @examples
#' log_likelihood_f(rf(100, df1 = 3, df2 = 5), 3, 5)
#' log_likelihood_f(rf(100, df1 = 10, df2 = 20), 10, 20)
#' log_likelihood_f(c(1, 2, 3, 4, 5), 2, 4)
log_likelihood_f = function(x, df1, df2) {
  return(sum(df(x, df1, df2, log = T)))
}

#' Calculate the log-likelihood of a sample under the t density
#'
#' @param x A numeric vector of observations
#' @param df Degrees of freedom of the t distribution
#'
#' @return The log-likelihood of the sample under the t density
#' @examples
#' log_likelihood_t(rt(100, df = 3), 3)
#' log_likelihood_t(rt(100, df = 10), 10)
#' log_likelihood_t(c(1, 2, 3, 4, 5), 2)
log_likelihood_t = function(x, df) {
  return(sum(dt(x, df, log = T)))
}

#' Calculate the sensitivity of a binary classifier
#'
#' @param pred A binary vector of predicted labels
#' @param truth A binary vector of true labels
#'
#' @return Sensitivity of the classifier
#' @examples
#' pred <- factor(c(1, 0, 1, 1, 0))
#' truth <- factor(c(1, 0, 1, 0, 0))
#' sensitivity(pred, truth)
#' pred <- rbinom(100, 1, 0.5)
#' truth <- rbinom(100, 1, 0.7)
#' sensitivity(pred, truth)
#' pred <- factor(c(1, 0, 1, 1, 0, 1, 0, 1))
#' truth <- factor(c(1, 1, 1, 1, 0, 0, 0, 0))
#' sensitivity(pred, truth)
sensitivity = function(pred, truth) {
  true_positives <- sum((pred == 1) & (truth == 1))
  false_negatives <- sum((pred == 0) & (truth == 1))
  return(true_positives / (true_positives + false_negatives))
}

#' Calculate the specificity of a binary classifier
#'
#' @param pred A binary vector of predicted labels
#' @param truth A binary vector of true labels
#'
#' @return Specificity of the classifier
#' @examples
#' pred <- factor(c(1, 0, 1, 1, 0))
#' truth <- factor(c(1, 0, 1, 0, 0))
#' specificity(pred, truth)
#' pred <- rbinom(100, 1, 0.5)
#' truth <- rbinom(100, 1, 0.7)
#' specificity(pred, truth)
#' pred <- factor(c(1, 0, 1, 1, 0, 1, 0, 1))
#' truth <- factor(c(1, 1, 1, 1, 0,0, 0, 0))
#' specificity(pred, truth)
specificity = function(pred, truth) {
  true_negatives <- sum((pred == 0) & (truth == 0))
  false_positives <- sum((pred == 1) & (truth == 0))
  return(true_negatives / (true_negatives + false_positives))
}

#' Calculate the precision of a binary classifier
#'
#' @param pred A binary vector of predicted labels
#' @param truth A binary vector of true labels
#'
#' @return Precision of the classifier
#' @examples
#' pred <- factor(c(1, 0, 1, 1, 0))
#' truth <- factor(c(1, 0, 1, 0, 0))
#' precision(pred, truth)
#' pred <- rbinom(100, 1, 0.5)
#' truth <- rbinom(100, 1, 0.7)
#' precision(pred, truth)
#' pred <- factor(c(1, 0, 1, 1, 0, 1, 0, 1))
#' truth <- factor(c(1, 1, 1, 1, 0, 0, 0, 0))
#' precision(pred, truth)
precision = function(pred, truth) {
  true_positives <- sum((pred == 1) & (truth == 1))
  false_positives <- sum((pred == 1) & (truth == 0))
  return(true_positives / (true_positives + false_positives))
}

#' Calculate the recall of a binary classifier
#'
#' @param pred A binary vector of predicted labels
#' @param truth A binary vector of true labels
#'
#' @return Recall of the classifier
#' @examples
#' pred <- factor(c(1, 0, 1, 1, 0))
#' truth <- factor(c(1, 0, 1, 0, 0))
#' recall(pred, truth)
#' pred <- rbinom(100, 1, 0.5)
#' truth <- rbinom(100, 1, 0.7)
#' recall(pred, truth)
#' pred <- factor(c(1, 0, 1, 1, 0, 1, 0, 1))
#' truth <- factor(c(1, 1, 1, 1, 0, 0, 0, 0))
#' recall(pred, truth)
recall = function(pred, truth) {
  return(sensitivity(pred, truth))
}

#' Calculate the accuracy of a binary classifier
#'
#' @param pred A binary vector of predicted labels
#' @param truth A binary vector of true labels
#'
#' @return Accuracy of the classifier
#' @examples
#' pred <- factor(c(1, 0, 1, 1, 0))
#' truth <- factor(c(1, 0, 1, 0, 0))
#' accuracy(pred, truth)
#' pred <- rbinom(100, 1, 0.5)
#' truth <- rbinom(100, 1, 0.7)
#' accuracy(pred, truth)
#' pred <- factor(c(1, 0, 1, 1, 0, 1, 0, 1))
#' truth <- factor(c(1, 1, 1, 1, 0, 0, 0, 0))
#' accuracy(pred, truth)
accuracy = function(pred, truth) {
  return(mean(pred == truth))
}

#' Calculate the F1 score of a binary classifier
#'
#' @param pred A binary vector of predicted labels
#' @param truth A binary vector of true labels
#'
#' @return F1 score of the classifier
#' @examples
#' pred <- factor(c(1, 0, 1, 1, 0))
#' truth <- factor(c(1, 0, 1, 0, 0))
#' f1_score(pred, truth)
#' pred <- rbinom(100, 1, 0.5)
#' truth <- rbinom(100, 1, 0.7)
#' f1_score(pred, truth)
#' pred <- factor(c(1, 0, 1, 1, 0, 1, 0, 1))
#' truth <- factor(c(1, 1, 1, 1, 0, 0, 0, 0))
#' f1_score(pred, truth)
f1 = function(pred, truth) {
  prec <- precision(pred, truth)
  rec <- recall(pred, truth)
  return(2 * prec * rec / (prec + rec))
}

#' Calculate the minimum number of samples per group for a two-sample t-test
#'
#' @param d Expected Cohen's d
#' @param power Desired statistical power (default: 0.8)
#'
#' @return Minimum number of samples per group
#' @examples
#' minimum_n_per_group(0.5)
#' minimum_n_per_group(0.5, power = 0.9)
#' minimum_n_per_group(0.8, power = 0.95)
minimum_n_per_group = function(d, power = 0.8) {
  require(pwr)
  return(ceiling(pwr.t.test(d = d, power = power, sig.level = 0.05, type = "two.sample")$n))
}

#' Calculate the R-squared statistic between predicted and ground truth continuous variables
#'
#' @param pred A numeric vector of predicted values
#' @param truth A numeric vector of true values
#'
#' @return R-squared statistic
#' @examples
#' pred <- c(1, 2, 3, 4, 5)
#' truth <- c(1.2, 2.1, 3.3, 4.4, 4.8)
#' r2(pred, truth)
#' pred <- rnorm(100)
#' truth <- rnorm(100, mean = 3)
#' r2(pred, truth)
#' pred <- runif(50)
#' truth <- runif(50)
#' r2(pred, truth)
r2 = function(pred, truth) {
  return(1 - (sum((truth - pred)^2) / sum((truth - mean(truth))^2)))
}

#' Calculate the adjusted R-squared statistic between predicted and ground truth continuous variables
#'
#' @param pred A numeric vector of predicted values
#' @param truth A numeric vector of true values
#' @param n_p Number of model parameters, excluding the intercept
#'
#' @return Adjusted R-squared statistic
#' @examples
#' pred <- c(1, 2, 3, 4, 5)
#' truth <- c(1.2, 2.1, 3.3, 4.4, 4.8)
#' adj_R2(pred, truth, 2)
#' pred <- rnorm(100)
#' truth <- rnorm(100, mean = 3)
#' adj_R2(pred, truth, 4)
#' pred <- runif(50)
#' truth <- runif(50)
#' adj_R2(pred, truth, 1)
adj_R2 = function(pred, truth, n_p) {
  n <- length(pred)
  r_squared <- r2(pred, truth)
  return(1 - ((1 - r_squared) * (n - 1) / (n - n_p - 1)))
}
