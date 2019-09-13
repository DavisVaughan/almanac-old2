#' @rdname event-yday
#' @export
between_days <- function(x, y, start = TRUE) {
  between_ydays(x, y)
}

#' @rdname event-yday
#' @export
between_ydays <- function(x, y, start = TRUE) {
  x <- cast_scalar_integer(x)
  y <- cast_scalar_integer(y, "y")

  if (x > y) {
    glubort("`x` ({x}) must come before `y` ({y}).")
  }

  after <- after_yday(x, inclusive = TRUE, start = start)
  before <- before_yday(y, inclusive = TRUE, start = start)

  test <- function(env) {
    event_in_impl(after, env) & event_in_impl(before, env)
  }

  if (start) {
    desc <- "Between days of the year: {x}-{y}"
  } else {
    desc <- "Between days from the end of the year: {x}-{y}"
  }

  new_event(
    description = glue(desc),
    test = test
  )
}

# ------------------------------------------------------------------------------

#' @rdname event-qday
#' @export
between_qdays <- function(x, y, start = TRUE) {
  x <- cast_scalar_integer(x)
  y <- cast_scalar_integer(y, "y")

  if (x > y) {
    glubort("`x` ({x}) must come before `y` ({y}).")
  }

  after <- after_qday(x, inclusive = TRUE, start = start)
  before <- before_qday(y, inclusive = TRUE, start = start)

  test <- function(env) {
    event_in_impl(after, env) & event_in_impl(before, env)
  }

  if (start) {
    desc <- "Between days of the quarter: {x}-{y}"
  } else {
    desc <- "Between days from the end of the quarter: {x}-{y}"
  }

  new_event(
    description = glue(desc),
    test = test
  )
}

# ------------------------------------------------------------------------------

#' @rdname event-mday
#' @export
between_mdays <- function(x, y, start = TRUE) {
  x <- cast_scalar_integer(x)
  y <- cast_scalar_integer(y, "y")

  if (x > y) {
    glubort("`x` ({x}) must come before `y` ({y}).")
  }

  after <- after_mday(x, inclusive = TRUE, start = start)
  before <- before_mday(y, inclusive = TRUE, start = start)

  test <- function(env) {
    event_in_impl(after, env) & event_in_impl(before, env)
  }

  if (start) {
    desc <- "Between days of the month: {x}-{y}"
  } else {
    desc <- "Between days from the end of the month: {x}-{y}"
  }

  new_event(
    description = glue(desc),
    test = test
  )
}

# ------------------------------------------------------------------------------

#' @rdname event-wday
#' @export
between_wdays <- function(x, y, start = TRUE) {
  x <- wday_normalize(x)
  y <- wday_normalize(y)

  x <- cast_scalar_integer(x)
  y <- cast_scalar_integer(y, "y")

  if (x > y) {
    glubort("`x` ({x}) must come before `y` ({y}).")
  }

  after <- after_wday(x, inclusive = TRUE, start = start)
  before <- before_wday(y, inclusive = TRUE, start = start)

  test <- function(env) {
    event_in_impl(after, env) & event_in_impl(before, env)
  }

  if (start) {
    desc <- "Between days of the week: {weekday_print()[x]}-{weekday_print()[y]}"
  } else {
    desc <- "Between days from the end of the week: {weekday_print()[x]}-{weekday_print()[y]}"
  }

  new_event(
    description = glue(desc),
    test = test
  )
}
