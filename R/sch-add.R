#' @export
sch_add <- function(x, event, name) {
  assert_schedule(x, "`x`")
  assert_event(event)
  vec_assert(name, character(), 1L)

  if (vec_in(name, names(x$events))) {
    abort("`name` cannot already exist in the names of the events.")
  }

  events <- c(x$events, list2(!!name := event))
  new_schedule(events)
}
