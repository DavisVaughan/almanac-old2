#' @export
event_is <- function(x, event) {
  x <- vec_cast(x, new_date())
  assert_event(event)

  init_context(x)
  on.exit(reset_context(), add = TRUE)

  event_is_impl(event)
}

event_is_impl <- function(event) {
  out <- event$test()
  vec_assert(out, ptype = logical(), size = current_size())
  out
}
