#' @export
on_repeat <- function(event, n, since) {
  n <- cast_scalar_integer(n, "n")

  since <- vec_cast_date(since)
  vec_assert(since, size = 1L)

  if (is.character(event)) {
    event <- period(event)
  }

  if (!is_event_object(event) && !is.period(event)) {
    abort("`event` must be an `event` or a `period`.")
  }

  test <- function(env) {
    needles <- current_date(env)
    to <- max(needles)

    if (since > to) {
      return(rep(FALSE, vec_size(needles)))
    }

    if (is_event_object(event)) {
      dates <- seq(since, to, by = "day")
      dates <- dates[event_in(dates, event)]
    } else {
      since_to <- interval(since, to)
      num_periods <- floor(since_to / event)
      dates <- since + seq2(0, num_periods) * event
    }

    n_dates <- length(dates)

    if (n_dates == 0L) {
      return(rep(FALSE, vec_size(needles)))
    }

    locs <- seq(1, n_dates, by = n)
    haystack <- dates[locs]

    vec_in(needles, haystack)
  }

  new_event(
    description = "On every n-th",
    test = test
  )
}
