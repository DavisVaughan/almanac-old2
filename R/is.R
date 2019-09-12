#' @export
event_is <- function(x, event) {
  x <- vec_cast_date(x)
  assert_event(event)

  env <- init_context(x)

  event_is_impl(event, env)
}

event_is_impl <- function(event, env) {
  out <- event$test(env)
  vec_assert(out, ptype = logical(), size = current_size(env))
  out
}
