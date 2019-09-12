#' @export
sch_seq <- function(from, to, schedule) {
  assert_schedule(schedule)

  from <- vec_cast_date(from, x_arg = "from")
  to <- vec_cast_date(to, x_arg = "to")

  vec_assert(from, size = 1L)
  vec_assert(to, size = 1L)

  dates <- seq(from, to, by = "1 day")

  dates[sch_in(dates, schedule)]
}
