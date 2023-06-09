% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bmi510package.R
\name{log_likelihood_unif}
\alias{log_likelihood_unif}
\title{Calculate the log-likelihood of a sample under the uniform density}
\usage{
log_likelihood_unif(x, min, max)
}
\arguments{
\item{x}{A numeric vector of observations}

\item{min}{Minimum value of the uniform distribution}

\item{max}{Maximum value of the uniform distribution}
}
\value{
The log-likelihood of the sample under the uniform density
}
\description{
Calculate the log-likelihood of a sample under the uniform density
}
\examples{
log_likelihood_unif(runif(100, min = 0, max = 1), 0, 1)
log_likelihood_unif(runif(100, min = -2, max = 2), -2, 2)
log_likelihood_unif(c(1, 2, 3, 4, 5), 1, 5)
}
