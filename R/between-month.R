#' @export
between_months <- function(x, y) {
  between_ymonths(x, y)
}

#' @export
between_ymonths <- function(x, y) {
  x <- month_normalize(x)
  y <- month_normalize(y)

  x <- cast_scalar_integer(x)
  y <- cast_scalar_integer(y, "y")

  if (x > y) {
    glubort("`x` ({x}) must come before `y` ({y}).")
  }

  after <- after_ymonth(x, inclusive = TRUE)
  before <- before_ymonth(y, inclusive = TRUE)

  test <- function(env) {
    event_is_impl(after, env) & event_is_impl(before, env)
  }

  new_event(
    description = glue("Between months of the year: {month_print()[x]}-{month_print()[y]}"),
    test = test
  )
}

# ------------------------------------------------------------------------------

#' @export
between_qmonths <- function(x, y) {
  x <- month_normalize(x)
  y <- month_normalize(y)

  x <- cast_scalar_integer(x)
  y <- cast_scalar_integer(y, "y")

  if (x > y) {
    glubort("`x` ({x}) must come before `y` ({y}).")
  }

  after <- after_qmonth(x, inclusive = TRUE)
  before <- before_qmonth(y, inclusive = TRUE)

  test <- function(env) {
    event_is_impl(after, env) & event_is_impl(before, env)
  }

  new_event(
    description = glue("Between months of the quarter: {x}-{y}"),
    test = test
  )
}
