% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/after-month.R, R/before-month.R,
%   R/between-month.R, R/on-month.R
\name{after_month}
\alias{after_month}
\alias{after_ymonth}
\alias{before_month}
\alias{before_ymonth}
\alias{between_months}
\alias{between_ymonths}
\alias{event-ymonth}
\alias{on_month}
\alias{on_ymonth}
\title{Events related to months of the year}
\usage{
after_month(x, inclusive = FALSE)

after_ymonth(x, inclusive = FALSE)

before_month(x, inclusive = FALSE)

before_ymonth(x, inclusive = FALSE)

between_months(x, y)

between_ymonths(x, y)

on_month(x)

on_ymonth(x)
}
\arguments{
\item{x}{\code{[integer(1) / character(1)]}

A month of the year to mark as an event. For \code{on_month()}, this is also
allowed to be a vector. This can be a full month string like \code{"November"},
or an abbreviation like \code{"Nov"}.}

\item{inclusive}{\code{[logical(1)]}

Should \code{x} count as an event?}

\item{y}{\code{[integer(1) / character(1)]}

A month of the year to mark as an event.}
}
\description{
\itemize{
\item \code{on_month()} / \code{on_ymonth()}: Is the date on month \code{x}?
\item \code{before_month()} / \code{before_ymonth()}: Is the date before month \code{x}?
\item \code{after_month()} / \code{after_ymonth()}: Is the date after month \code{x}?
\item \code{between_months()} / \code{between_ymonths()}: Is the date between months \code{x}
and \code{y}?
}
}
\details{
\code{on_month()} is an alias of the less common, but more verbose, \code{on_ymonth()}.
}
