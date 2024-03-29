% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/after-quarter.R, R/before-quarter.R,
%   R/between-quarter.R, R/on-quarter.R
\name{after_quarter}
\alias{after_quarter}
\alias{before_quarter}
\alias{between_quarters}
\alias{event-quarter}
\alias{on_quarter}
\title{Events related to quarters}
\usage{
after_quarter(x, inclusive = FALSE)

before_quarter(x, inclusive = FALSE)

between_quarters(x, y)

on_quarter(x)
}
\arguments{
\item{x}{\code{[integer(1)]}

A quarter to mark as an event. For \code{on_quarter()}, this is also
allowed to be a vector.}

\item{inclusive}{\code{[logical(1)]}

Should \code{x} count as an event?}

\item{y}{\code{[integer(1)]}

A quarter to mark as an event.}
}
\description{
\itemize{
\item \code{on_quarter()}: Is the date on quarter \code{x}?
\item \code{before_quarter()}: Is the date before quarter \code{x}?
\item \code{after_quarter()}: Is the date after quarter \code{x}?
\item \code{between_quarters()}: Is the date between quarters \code{x} and \code{y}?
}
}
