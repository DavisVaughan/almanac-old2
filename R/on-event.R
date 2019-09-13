# `on_event()` is mainly useful for constructing a schedule where the actual
# event needs to be adjusted if another event happens. i.e. "trash day" is
# normally on Wednesday, but if Wednesday is a holiday then it falls on
# Thursday instead.

# This should not be confused with `sched_adjust()`, which takes an existing
# set of dates and adjusts them to not fall on any `events` located by the
# schedule.

# `jump` here has to be a lubridate period. We need to be able to look
# backwards using the jump to detect if the original date was at the
# intersection of `x` and `event`. In that case, `jump` was applied to
# move the date forward.

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
    abort("`jump` must be a period.")
  }

  if (is_subdaily(jump)) {
    abort("`jump` must not contain any sub-daily components.")
  }

  jump
}

is_subdaily <- function(x) {
  sum(abs(hour(x)), abs(minute(x)), abs(second(x))) != 0L
}
