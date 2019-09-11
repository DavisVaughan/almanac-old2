#' @export
between_weeks <- function(x, y) {
  between_yweeks(x, y)
}

#' @export
between_yweeks <- function(x, y, start = TRUE) {
  x <- cast_scalar_integer(x)
  y <- cast_scalar_integer(y, "y")

  if (x > y) {
    glubort("`x` ({x}) must come before `y` ({y}).")
  }

  after <- after_yweek(x, inclusive = TRUE, start = start)
  before <- before_yweek(y, inclusive = TRUE, start = start)

  test <- function(env) {
    event_is_impl(after, env) & event_is_impl(before, env)
  }

  if (start) {
    desc <- "Between weeks of the year: {x}-{y}"
  } else {
    desc <- "Between weeks from the end of the year: {x}-{y}"
  }

  new_event(
    description = glue(desc),
    test = test
  )
}

# ------------------------------------------------------------------------------

#' @export
between_qweeks <- function(x, y, start = TRUE) {
  x <- cast_scalar_integer(x)
  y <- cast_scalar_integer(y, "y")

  if (x > y) {
    glubort("`x` ({x}) must come before `y` ({y}).")
  }

  after <- after_qweek(x, inclusive = TRUE, start = start)
  before <- before_qweek(y, inclusive = TRUE, start = start)

  test <- function(env) {
    event_is_impl(after, env) & event_is_impl(before, env)
  }

  if (start) {
    desc <- "Between weeks of the quarter: {x}-{y}"
  } else {
    desc <- "Between weeks from the end of the quarter: {x}-{y}"
  }

  new_event(
    description = glue(desc),
    test = test
  )
}

# ------------------------------------------------------------------------------

#' @export
between_mweeks <- function(x, y, start = TRUE) {
  x <- cast_scalar_integer(x)
  y <- cast_scalar_integer(y, "y")

  if (x > y) {
    glubort("`x` ({x}) must come before `y` ({y}).")
  }

  after <- after_mweek(x, inclusive = TRUE, start = start)
  before <- before_mweek(y, inclusive = TRUE, start = start)

  test <- function(env) {
    event_is_impl(after, env) & event_is_impl(before, env)
  }

  if (start) {
    desc <- "Between weeks of the month: {x}-{y}"
  } else {
    desc <- "Between weeks from the end of the month: {x}-{y}"
  }

  new_event(
    description = glue(desc),
    test = test
  )
}
