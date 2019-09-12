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
  event_is_impl(event, env)
}
