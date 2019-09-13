#' @export
between_years <- function(x, y) {
  x <- cast_scalar_integer(x)
  y <- cast_scalar_integer(y, "y")

  if (x > y) {
    glubort("`x` ({x}) must come before `y` ({y}).")
  }

  after <- after_year(x, inclusive = TRUE)
  before <- before_year(y, inclusive = TRUE)

  test <- function(env) {
    event_in_impl(after, env) & event_in_impl(before, env)
  }

  new_event(
    description = glue("Between years: {x}-{y}"),
    test = test
  )
}

# ------------------------------------------------------------------------------

#' @export
between_isoyears <- function(x, y) {
  x <- cast_scalar_integer(x)
  y <- cast_scalar_integer(y, "y")

  if (x > y) {
    glubort("`x` ({x}) must come before `y` ({y}).")
  }

  after <- after_isoyear(x, inclusive = TRUE)
  before <- before_isoyear(y, inclusive = TRUE)

  test <- function(env) {
    event_in_impl(after, env) & event_in_impl(before, env)
  }

  new_event(
    description = glue("Between ISO years: {x}-{y}"),
    test = test
  )
}

# ------------------------------------------------------------------------------

#' @export
between_epiyears <- function(x, y) {
  x <- cast_scalar_integer(x)
  y <- cast_scalar_integer(y, "y")

  if (x > y) {
    glubort("`x` ({x}) must come before `y` ({y}).")
  }

  after <- after_epiyear(x, inclusive = TRUE)
  before <- before_epiyear(y, inclusive = TRUE)

  test <- function(env) {
    event_in_impl(after, env) & event_in_impl(before, env)
  }

  new_event(
    description = glue("Between epidemilogical years: {x}-{y}"),
    test = test
  )
}
