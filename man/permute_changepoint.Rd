% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/permute_changepoint.R
\name{permute_changepoint}
\alias{permute_changepoint}
\title{returns a permutation based distribution of possible changepoint values of X.
fits gam with n knots, find changepoint, loop(new gam for (fitted+shuffled residuals)~x, find changepoint)}
\usage{
permute_changepoint(df, y, x, npoints, iter)
}
\arguments{
\item{df}{dataframe or tibble object}

\item{y}{y variable}

\item{x}{x variable (numeric)}

\item{npoints}{number of changepoints to look for}

\item{iter}{number of permutations/iterations}
}
\description{
I guess if you want to do some hypothesis testing, then do a z test of your theorised changepoint & permuted distribution?
or of your permuted dist vs prior dist?
}
\details{
requires mgcv, tidyverse
}
\examples{
permute_changepoint(dd,rate,year,npoints=3,iter=1000)
}
