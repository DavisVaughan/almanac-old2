#' Generate events in a schedule
#'
#' `sch_seq()` generates all events in a schedule from `from` to `to`,
#' inclusive.
#'
#' @param from,to `[Date(1)]`
#'
#'    Dates defining the range to look for events in the `schedule`.
#'
#' @param schedule `[schedule / event]`
#'
#'    A schedule or event.
#'
#' @examples
#' sch_seq("2019-01-01", "2019-01-31", on_mday(12) | on_wday("Mon"))
#'
#' @export
sch_seq <- function(from, to, schedule) {
  schedule <- as_schedule(schedule)

  from <- vec_cast_date(from, x_arg = "from")
  to <- vec_cast_date(to, x_arg = "to")

  vec_assert(from, size = 1L)
  vec_assert(to, size = 1L)

  dates <- seq(from, to, by = "1 day")

  dates[sch_in(dates, schedule)]
}
