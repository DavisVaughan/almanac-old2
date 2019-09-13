#' Is `x` in the schedule?
#'
#' `sch_in()` checks if `x` is in the set of dates defined by the schedule.
#'
#' @details
#'
#' `sch_in()` takes the union of the events in the schedule to construct a
#' single event on which to use [event_in()].
#'
#' @param x `[Date]`
#'
#'    A vector of dates.
#'
#' @param schedule `[schedule]`
#'
#'    A schedule.
#'
#' @export
sch_in <- function(x, schedule) {
  x <- vec_cast_date(x)
  assert_schedule(schedule)

  env <- init_context(x)

  sch_in_impl(schedule, env)
}

sch_in_impl <- function(schedule, env) {
  events <- schedule$events
  event <- reduce(events, event_union)
  event_in_impl(event, env)
}
