% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/glmratio.R
\name{glmratio}
\alias{glmratio}
\title{return OR/RR and confidence intervals for logistic/poisson respectively.}
\usage{
glmratio(mod, method = "profile")
}
\arguments{
\item{mod}{a glm(er) object}

\item{method}{confint method (for mixed models), passed to \code{\link{confint.merMod}}.}
}
\description{
return OR/RR and confidence intervals for logistic/poisson respectively.
}
