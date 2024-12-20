% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/server.R
\name{bestChoice}
\alias{bestChoice}
\title{Find best variable name to match with given input field}
\usage{
bestChoice(patterns, choices)
}
\arguments{
\item{patterns}{character vector, contains all possible patterns, in order
of importance, that will be checked for}

\item{choices}{vector, variable names in which to search for pattern}
}
\value{
one element of \code{choices} that matches best with \code{patterns}
}
\description{
Find best variable name to match with given input field
}
