# TODO - What happens if you have the case of:
# - Friday is normally trash day
# - Unless Friday is a holiday
# - In which case you'd move to the next non-event date
# - And you want to define weekends as events too, so you'd roll to monday
# So if you use `event_in()` on that monday, it should return TRUE

#' Events related to other events
#'
#' `on_event()` defines an alternative date to use as an event if `event`
#' occurs. It is usually useful as a way to deviate from a normal schedule in
#' the case of an unusual circumstance.
#'
#' @param x `[event]`
#'
#'    An event defining normal behavior.
#'
#' @param event `[event]`
#'
#'    An event defining when a `jump` should occur.
#'
#' @param jump `[Period(1) / integer(1)]`
#'
#'    A jump defining the deviation from normal behavior.
#'
#' @examples
#' # Imagine the garbage truck normally comes every Monday, but if
#' # Monday is a state holiday, then it comes on Tuesday instead. In this case
#' # we can use `event = on_state_holiday` as a trigger to define an alternative
#' # event rule.
#'
#' on_normal_trash_day <- on_wday("Monday")
#' on_state_holiday <- on_month("Sep") & on_mweek(1) & on_wday("Monday")
#'
#' on_trash_day <- on_event(on_normal_trash_day, on_state_holiday, days(1))
#'
#' # A Monday in September
#' event_in("2019-09-09", on_normal_trash_day)
#' event_in("2019-09-09", on_trash_day)
#'
#' # Labor Day Monday should not be trash day
#' event_in("2019-09-02", on_normal_trash_day)
#' event_in("2019-09-02", on_trash_day)
#'
#' # The day after Labor Day Monday is trash day
#' event_in("2019-09-03", on_normal_trash_day)
#' event_in("2019-09-03", on_trash_day)
#'
#' @export
on_event <- function(x, event, jump) {
  jump <- check_jump(jump)
  assert_event(x, "`x`")
  assert_event(event)

  test <- function(env) {
    out <- event_in_impl(x, env)

    not_currently_an_event <- event_in_impl(!event, env)

    # Does the current date lie on an `x` event, but not an `event` event?
    out <- out & not_currently_an_event

    is_event_requiring_jump <- x & event

    dates <- current_date(env)
    dates_unjumped <- dates - jump

    env_unjumped <- init_context(dates_unjumped)

    # Did the date before the jump lie on an `x` event, and an `event` event?
    unjumped_dates_required_jump <- event_in_impl(is_event_requiring_jump, env_unjumped)
    jumped_date_is_event <- not_currently_an_event & unjumped_dates_required_jump

    if (sum(jumped_date_is_event) == 0L) {
      return(out)
    }

    out[jumped_date_is_event] <- TRUE

    out
  }

  new_composite_event(
    description = "Jumped event",
    test = test,
    events = list(x),
    class = "jumped_event"
  )
}

check_jump <- function(jump) {
  if (is.character(jump)) {
    jump <- period(jump)
  }

  if (!is.period(jump)) {
    if (is.double(jump) || is.integer(jump)) {
      jump <- vec_cast(jump, integer())
      jump <- days(jump)
      return(jump)
    }

    abort("`jump` must be a period or integer.")
  }

  if (is_subdaily(jump)) {
    abort("`jump` must not contain any sub-daily components.")
  }

  jump
}

is_subdaily <- function(x) {
  sum(abs(hour(x)), abs(minute(x)), abs(second(x))) != 0L
}
