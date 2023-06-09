% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bmi510package.R
\name{is_min}
\alias{is_min}
\title{Check if an element in a vector is equal to the minimum value}
\usage{
is_min(x, na.rm = T)
}
\arguments{
\item{x}{An atomic vector to check for minimum values}

\item{na.rm}{Logical, whether to remove NA values before checking (default is TRUE)}
}
\value{
A logical vector with TRUE where x equals its minimum value
}
\description{
Check if an element in a vector is equal to the minimum value
}
\examples{
is_min(c(1, 2, 3, 1))
is_min(c(5, NA, 2, 7, 2), na.rm = F)
is_min(rnorm(10))
}
