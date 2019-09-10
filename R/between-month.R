#' @export
between_months <- function(x, y) {
  between_ymonths(x, y)
}

#' @export
between_ymonths <- function(x, y) {
  x <- month_normalize(x)
  y <- month_normalize(y)

  x <- vec_cast(x, integer())
  y <- vec_cast(y, integer())

  vec_assert(x, size = 1L)
  vec_assert(y, size = 1L)

  if (!vec_in(x, 1:12)) {
    glubort("`x` ({x}) must be a valid month of the year, in `1:12`.")
  }

  if (!vec_in(y, 1:12)) {
    glubort("`y` ({y}) must be a valid month of the year, in `1:12`.")
  }

  if (x > y) {
    glubort("`x` ({x}) must come before `y` ({y}).")
  }

  test <- function() {
    value <- current_ymonth()
    x <= value & y >= value
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

  x <- vec_cast(x, integer())
  y <- vec_cast(y, integer())

  vec_assert(x, size = 1L)
  vec_assert(y, size = 1L)

  if (!vec_in(x, 1:3)) {
    glubort("`x` ({x}) must be a valid month of the quarter, in `1:3`.")
  }

  if (!vec_in(y, 1:3)) {
    glubort("`y` ({y}) must be a valid month of the quarter, in `1:3`.")
  }

  if (x > y) {
    glubort("`x` ({x}) must come before `y` ({y}).")
  }

  test <- function() {
    value <- current_qmonth()
    x <= value & y >= value
  }

  new_event(
    description = glue("Between months of the quarter: {x}-{y}"),
    test = test
  )
}
