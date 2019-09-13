#' @export
event_in <- function(x, event) {
  x <- vec_cast_date(x)
  assert_event(event)

  env <- init_context(x)

  event_in_impl(event, env)
}

event_in_impl <- function(event, env) {
  out <- event$test(env)
  vec_assert(out, ptype = logical(), size = current_size(env))
  out
}
