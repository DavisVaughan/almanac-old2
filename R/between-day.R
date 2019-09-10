#' @export
between_days <- function(x, y) {
  between_ydays(x, y)
}

#' @export
between_ydays <- function(x, y) {
  x <- vec_cast(x, integer())
  y <- vec_cast(y, integer())

  vec_assert(x, size = 1L)
  vec_assert(y, size = 1L)

  if (!vec_in(x, 1:366)) {
    glubort("`x` ({x}) must be a valid day of the year, in `1:366`.")
  }

  if (!vec_in(x, 1:366)) {
    glubort("`y` ({y}) must be a valid day of the year, in `1:366`.")
  }

  if (x > y) {
    glubort("`x` ({x}) must come before `y` ({y}).")
  }

  test <- function() {
    value <- current_yday()
    x <= value & y >= value
  }

  new_event(
    description = glue("Between days of the year: {x}-{y}"),
    test = test
  )
}

# ------------------------------------------------------------------------------

#' @export
between_qdays <- function(x, y) {
  x <- vec_cast(x, integer())
  y <- vec_cast(y, integer())

  vec_assert(x, size = 1L)
  vec_assert(y, size = 1L)

  if (x > y) {
    glubort("`x` ({x}) must come before `y` ({y}).")
  }

  test <- function() {
    value <- current_qday()
    x <= value & y >= value
  }

  new_event(
    description = glue("Between days of the quarter: {x}-{y}"),
    test = test
  )
}

# ------------------------------------------------------------------------------

#' @export
between_mdays <- function(x, y) {
  x <- vec_cast(x, integer())
  y <- vec_cast(y, integer())

  vec_assert(x, size = 1L)
  vec_assert(y, size = 1L)

  if (!vec_in(x, 1:31)) {
    glubort("`x` ({x}) must be a valid day of the month, in `1:31`.")
  }

  if (!vec_in(y, 1:31)) {
    glubort("`y` ({y}) must be a valid day of the month, in `1:31`.")
  }

  if (x > y) {
    glubort("`x` ({x}) must come before `y` ({y}).")
  }

  test <- function() {
    value <- current_mday()
    x <= value & y >= value
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

  x <- vec_cast(x, integer())
  y <- vec_cast(y, integer())

  vec_assert(x, size = 1L)
  vec_assert(y, size = 1L)

  if (!vec_in(x, 1:7)) {
    glubort("`x` ({x}) must be a valid day of the week, in `1:7`.")
  }

  if (!vec_in(y, 1:7)) {
    glubort("`y` ({y}) must be a valid day of the week, in `1:7`.")
  }

  if (x > y) {
    glubort("`x` ({x}) must come before `y` ({y}).")
  }

  test <- function() {
    value <- current_wday()
    x <= value & y >= value
  }

  new_event(
    description = glue("Between days of the week: {weekday_print()[x]}-{weekday_print()[y]}"),
    test = test
  )
}
