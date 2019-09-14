# "Every 2 weeks since 1970-01-01"

on_every <- function(period, since, until = NULL) {

  test <- function(env) {
    x <- current_date(env)

    if (is.null(until)) {
      until <- max(x)
    }

    if (since > until) {
      return(rep(FALSE, vec_size(x)))
    }

    since_until <- interval(since, until)

    num_periods <- floor(since_until / period)

    dates <- since + seq2(0, num_periods) * period

    vec_in(x, dates)
  }

  new_event(
    description = "On every",
    test = test
  )
}
