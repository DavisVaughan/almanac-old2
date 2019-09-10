#' @export
between_years <- function(x, y) {
  x <- vec_cast(x, integer())
  y <- vec_cast(y, integer())

  vec_assert(x, size = 1L)
  vec_assert(y, size = 1L)

  if (x > y) {
    glubort("`x` ({x}) must come before `y` ({y}).")
  }

  test <- function() {
    year <- current_year()
    x <= year & y >= year
  }

  new_event(
    description = glue("Between years: {x}-{y}"),
    test = test
  )
}
