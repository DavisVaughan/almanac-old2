% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/adjustment.R
\name{adjustments}
\alias{adjustments}
\alias{adj_following}
\alias{adj_preceding}
\alias{adj_modified_following}
\alias{adj_modified_preceding}
\alias{adj_nearest}
\title{Common adjustments}
\usage{
adj_following(x, schedule)

adj_preceding(x, schedule)

adj_modified_following(x, schedule)

adj_modified_preceding(x, schedule)

adj_nearest(x, schedule)
}
\arguments{
\item{x}{\code{[Date]}

A vector of dates.}

\item{schedule}{\code{[schedule / event]}

A schedule or event.}
}
\description{
This family of \code{adj_*()} functions encode business logic for common
adjustments made in the financial world.
\itemize{
\item \code{adj_following()}

Choose the first non-event date after \code{x}. This is equivalent to using
an adjustment of \code{+days(1)}.
\item \code{adj_preceding()}

Choose the first non-event date before \code{x}. This is equivalent to using
an adjustment of \code{-days(1)}.
\item \code{adj_modified_following()}

Choose the first non-event date after \code{x}, unless it falls in a
different month, in which case the first non-event date before \code{x} is
chosen instead.
\item \code{adj_modified_preceding()}

Choose the first non-event date before \code{x}, unless it falls in a
different month, in which case the first non-event date after \code{x} is
chosen instead.
\item \code{adj_nearest()}

Choose the nearest non-event date to \code{x}. If the closest preceding and
following non-event dates are equally far away, the following non-event
date is chosen.
}
}
\details{
For examples, see \code{\link[=sch_adjust]{sch_adjust()}}.
}
