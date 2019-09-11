#' @export
on_event <- function(x, event, shift) {
  if (is.character(shift)) {
    shift <- period(shift)
  }

  if (is_formula(shift, scoped = TRUE, lhs = FALSE)) {
    shift <- as_function(shift)
  }

  if (!is_function(shift) && !is.period(shift)) {
    abort("`shift` must be a period or a function.")
  }

  assert_event(x, "`x`")
  assert_event(event)

  test <- function(env) {
    out <- event_is_impl(x, env)

    problems <- out & event_is_impl(event, env)

    if (sum(problems) == 0L) {
      return(out)
    }

    out[problems] <- FALSE

    dates <- current_date(env)
    problem_dates <- dates[problems]

    if (is_function(shift)) {
      adjusted <- shift(problem_dates)
    } else {
      adjusted <- problem_dates + shift
    }

    solutions <- vec_match(adjusted, dates)

    solutions <- solutions[!is.na(solutions)]

    out[solutions] <- TRUE

    out
  }

  new_composite_event(
    description = "Rolling event",
    test = test,
    events = list(x),
    class = "rolling_event"
  )
}
