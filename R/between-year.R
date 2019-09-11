#' @export
between_years <- function(x, y) {
  x <- vec_cast(x, integer())
  y <- vec_cast(y, integer())

  vec_assert(x, size = 1L)
  vec_assert(y, size = 1L)

  if (x > y) {
    glubort("`x` ({x}) must come before `y` ({y}).")
  }

  test <- function(env) {
    year <- current_year(env)
    x <= year & y >= year
  }

  new_event(
    description = glue("Between years: {x}-{y}"),
    test = test
  )
}

# ------------------------------------------------------------------------------

#' @export
between_isoyears <- function(x, y) {
  x <- vec_cast(x, integer())
  y <- vec_cast(y, integer())

  vec_assert(x, size = 1L)
  vec_assert(y, size = 1L)

  if (x > y) {
    glubort("`x` ({x}) must come before `y` ({y}).")
  }

  test <- function(env) {
    year <- current_isoyear(env)
    x <= year & y >= year
  }

  new_event(
    description = glue("Between ISO years: {x}-{y}"),
    test = test
  )
}

# ------------------------------------------------------------------------------

#' @export
between_epiyears <- function(x, y) {
  x <- vec_cast(x, integer())
  y <- vec_cast(y, integer())

  vec_assert(x, size = 1L)
  vec_assert(y, size = 1L)

  if (x > y) {
    glubort("`x` ({x}) must come before `y` ({y}).")
  }

  test <- function(env) {
    year <- current_epiyear(env)
    x <= year & y >= year
  }

  new_event(
    description = glue("Between epidemilogical years: {x}-{y}"),
    test = test
  )
}
