#' Adjust a vector of dates
#'
#' `sch_adjust()` takes an existing vector of dates and shifts it by applying
#' an `adjustment` whenever a date in `x` is also an event defined by
#' the `schedule`.
#'
#' @param x `[Date]`
#'
#'   A vector of dates.
#'
#' @param schedule `[schedule / event]`
#'
#'   A schedule or event.
#'
#' @param adjustment `[Period(1) / integer(1) / function / formula]`
#'
#'   An adjustment to make whenever a date falls on an event.
#'
#'   If this is a lubridate period object, such as [lubridate::days()],
#'   or an integer, then the adjustment is applied as `x + adjustment`.
#'
#'   If this is a function or formula (i.e., a lambda function), then it
#'   should accept 1 argument, the dates to adjust, and should return a `Date`
#'   vector of the same size as the original input.
#'
#' @examples
#' library(lubridate, warn.conflicts = FALSE)
#'
#' # The first of the month is an "event" so we have to adjust
#' # our current date to avoid that.
#' sch_adjust("2019-01-01", on_mday(1))
#'
#' # The adjustment could also be backwards
#' sch_adjust("2019-01-01", on_mday(1), adjustment = -days(1))
#'
#' # Adjustments are applied repeatedly until the next non-event can be found
#' # Here, 2019-01-01 is an event, so we move to 2019-01-02, but that is an
#' # event too, so we move to 2019-01-03
#' sch_adjust("2019-01-01", on_mday(1:2))
#'
#' @export
sch_adjust <- function(x, schedule, adjustment = days(1)) {
  x <- vec_cast_date(x)
  schedule <- as_schedule(schedule)
  adjustment <- check_adjustment(adjustment)

  # Find initial set of events
  env <- init_context(x)
  problem_loc <- sch_in_impl(schedule, env)
  problem_pos <- which(problem_loc)

  # While there are still some events, apply `adjustment` and recheck
  while(length(problem_pos) != 0L) {
    # Apply adjustment
    problems <- x[problem_pos]
    adjusted <- adjustment(problems)

    # Overwrite existing problems (use `vec_slice<-` for type/size stability)
    vec_slice(x, problem_pos) <- adjusted

    # Recheck
    env <- init_context(adjusted)
    problem_loc <- sch_in_impl(schedule, env)

    # Update location of problems
    problem_pos <- problem_pos[problem_loc]
  }

  x
}

check_adjustment <- function(adjustment) {
  # Parse to periods
  if (is.character(adjustment)) {
    adjustment <- period(adjustment)
  }

  # Period -> function
  if (is.period(adjustment)) {
    vec_assert(adjustment, size = 1L)

    if (is_subdaily(adjustment)) {
      abort("`adjustment` must not contain any sub-daily components.")
    }

    adjuster <- function(x) x + adjustment
    return(adjuster)
  }

  # Integer days -> function
  if (is.integer(adjustment) || is.double(adjustment)) {
    adjustment <- vec_cast(adjustment, integer())
    vec_assert(adjustment, size = 1L)

    adjuster <- function(x) x + adjustment
    return(adjuster)
  }

  # formula -> function
  if (is_formula(adjustment, scoped = TRUE, lhs = FALSE)) {
    adjustment <- as_function(adjustment)
  }

  if (!is_function(adjustment)) {
    abort("`adjustment` must be a period or a function.")
  }

  adjustment
}
