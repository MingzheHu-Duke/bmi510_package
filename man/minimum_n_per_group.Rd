% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bmi510package.R
\name{minimum_n_per_group}
\alias{minimum_n_per_group}
\title{Calculate the minimum number of samples per group for a two-sample t-test}
\usage{
minimum_n_per_group(d, power = 0.8)
}
\arguments{
\item{d}{Expected Cohen's d}

\item{power}{Desired statistical power (default: 0.8)}
}
\value{
Minimum number of samples per group
}
\description{
Calculate the minimum number of samples per group for a two-sample t-test
}
\examples{
minimum_n_per_group(0.5)
minimum_n_per_group(0.5, power = 0.9)
minimum_n_per_group(0.8, power = 0.95)
}
