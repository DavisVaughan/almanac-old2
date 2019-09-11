#' @export
between_days <- function(x, y) {
  between_ydays(x, y)
}

#' @export
between_ydays <- function(x, y) {
  x <- cast_scalar_integer(x)
  y <- cast_scalar_integer(y, "y")

  if (x > y) {
    glubort("`x` ({x}) must come before `y` ({y}).")
  }

  after <- after_yday(x, inclusive = TRUE)
  before <- before_yday(y, inclusive = TRUE)

  test <- function(env) {
    event_is_impl(after, env) & event_is_impl(before, env)
  }

  new_event(
    description = glue("Between days of the year: {x}-{y}"),
    test = test
  )
}

# ------------------------------------------------------------------------------

#' @export
between_qdays <- function(x, y) {
  x <- cast_scalar_integer(x)
  y <- cast_scalar_integer(y, "y")

  if (x > y) {
    glubort("`x` ({x}) must come before `y` ({y}).")
  }

  after <- after_qday(x, inclusive = TRUE)
  before <- before_qday(y, inclusive = TRUE)

  test <- function(env) {
    event_is_impl(after, env) & event_is_impl(before, env)
  }

  new_event(
    description = glue("Between days of the quarter: {x}-{y}"),
    test = test
  )
}

# ------------------------------------------------------------------------------

#' @export
between_mdays <- function(x, y) {
  x <- cast_scalar_integer(x)
  y <- cast_scalar_integer(y, "y")

  if (x > y) {
    glubort("`x` ({x}) must come before `y` ({y}).")
  }

  after <- after_mday(x, inclusive = TRUE)
  before <- before_mday(y, inclusive = TRUE)

  test <- function(env) {
    event_is_impl(after, env) & event_is_impl(before, env)
  }

  new_event(
    description = glue("Between days of the month: {x}-{y}"),
    test = test
  )
}

# ------------------------------------------------------------------------------

#' @export
between_wdays <- function(x, y) {
  x <- wday_normalize(x)
  y <- wday_normalize(y)

  x <- cast_scalar_integer(x)
  y <- cast_scalar_integer(y, "y")

  if (x > y) {
    glubort("`x` ({x}) must come before `y` ({y}).")
  }

  after <- after_wday(x, inclusive = TRUE)
  before <- before_wday(y, inclusive = TRUE)

  test <- function(env) {
    event_is_impl(after, env) & event_is_impl(before, env)
  }

  new_event(
    description = glue("Between days of the week: {weekday_print()[x]}-{weekday_print()[y]}"),
    test = test
  )
}
