#' Is `x` an event?
#'
#' `event_in()` checks if `x` is in the set of dates defined by `event`.
#'
#' @param x `[Date]`
#'
#'    A vector of dates.
#'
#' @param event `[event]`
#'
#'    An event.
#'
#' @examples
#' event_in("2019-01-01", on_mday(1))
#'
#' event_in("2019-01-01", on_mday(1) & on_wday("Mon"))
#'
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
