# `on_event()` is mainly useful for constructing a schedule where the actual
# event needs to be adjusted if another event happens. i.e. "trash day" is
# normally on Wednesday, but if Wednesday is a holiday then it falls on
# Thursday instead.

# This should not be confused with `sched_adjust()`, which takes an existing
# set of dates and adjusts them to not fall on any `events` located by the
# schedule.

# `shift` here has to be a lubridate period. We need to be able to look
# backwards using the shift to detect if the original date was at the
# intersection of `x` and `event`. In that case, `shift` was applied to
# move the date forward.

#' @export
on_event <- function(x, event, shift) {
  shift <- check_period_shift(shift)
  assert_event(x, "`x`")
  assert_event(event)

  test <- function(env) {
    out <- event_is_impl(x, env)

    not_currently_an_event <- event_is_impl(!event, env)

    # Does the current date lie on an `x` event, but not an `event` event?
    out <- out & not_currently_an_event

    is_event_requiring_shift <- x & event

    dates <- current_date(env)
    dates_unshifted <- dates - shift

    env_unshifted <- init_context(dates_unshifted)

    # Did the date before the shift lie on an `x` event, and an `event` event?
    unshifted_dates_required_shift <- event_is_impl(is_event_requiring_shift, env_unshifted)
    shifted_date_is_event <- not_currently_an_event & unshifted_dates_required_shift

    if (sum(shifted_date_is_event) == 0L) {
      return(out)
    }

    out[shifted_date_is_event] <- TRUE

    out
  }

  new_composite_event(
    description = "Shifted event",
    test = test,
    events = list(x),
    class = "shifted_event"
  )
}

check_period_shift <- function(shift) {
  if (is.character(shift)) {
    shift <- period(shift)
  }

  if (!is.period(shift)) {
    abort("`shift` must be a period.")
  }

  shift
}
