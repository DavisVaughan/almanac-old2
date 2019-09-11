#' @export
between_weeks <- function(x, y) {
  between_yweeks(x, y)
}

#' @export
between_yweeks <- function(x, y, start = TRUE) {
  x <- vec_cast(x, integer())
  y <- vec_cast(y, integer())

  vec_assert(x, size = 1L)
  vec_assert(y, size = 1L)

  if (!vec_in(x, 1:52)) {
    glubort("`x` ({x}) must be a valid week of the year, in `1:52`.")
  }

  if (!vec_in(y, 1:52)) {
    glubort("`y` ({y}) must be a valid week of the year, in `1:52`.")
  }

  if (x > y) {
    glubort("`x` ({x}) must come before `y` ({y}).")
  }

  test <- function(env) {
    if (start) {
      value <- current_yweek_from_start(env)
    } else {
      value <- current_yweek_from_end(env)
    }

    x <= value & y >= value
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
  x <- vec_cast(x, integer())
  y <- vec_cast(y, integer())

  vec_assert(x, size = 1L)
  vec_assert(y, size = 1L)

  if (x > y) {
    glubort("`x` ({x}) must come before `y` ({y}).")
  }

  test <- function(env) {
    if (start) {
      value <- current_qweek_from_start(env)
    } else {
      value <- current_qweek_from_end(env)
    }

    x <= value & y >= value
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
  x <- vec_cast(x, integer())
  y <- vec_cast(y, integer())

  vec_assert(x, size = 1L)
  vec_assert(y, size = 1L)

  if (!vec_in(x, 1:5)) {
    glubort("`x` ({x}) must be a valid week of the month, in `1:5`.")
  }

  if (!vec_in(x, 1:5)) {
    glubort("`y` ({y}) must be a valid week of the month, in `1:5`.")
  }

  if (x > y) {
    glubort("`x` ({x}) must come before `y` ({y}).")
  }

  test <- function(env) {
    if (start) {
      value <- current_mweek_from_start(env)
    } else {
      value <- current_mweek_from_end(env)
    }

    x <= value & y >= value
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
