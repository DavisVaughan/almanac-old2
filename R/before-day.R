#' @export
before_day <- function(x, inclusive = FALSE, start = TRUE) {
  before_yday(x, inclusive)
}

#' @export
before_yday <- function(x, inclusive = FALSE, start = TRUE) {
  x <- cast_scalar_integer(x)

  vec_assert(inclusive, logical(), 1L)

  if (!vec_in(x, 1:366)) {
    glubort("A day of the year must be in `1:366`, not {x}.")
  }

  test <- function(env) {
    if (start) {
      value <- current_yday(env)
    } else {
      value <- current_yday_from_end(env)
    }

    test_before(x, value, inclusive)
  }

  if (start) {
    desc <- "Before day of the year: {x}"
  } else {
    desc <- "Before day from the end of the year: {x}"
  }

  new_event(
    description = glue(desc),
    test = test
  )
}

# ------------------------------------------------------------------------------

#' @export
before_qday <- function(x, inclusive = FALSE, start = TRUE) {
  x <- cast_scalar_integer(x)

  vec_assert(inclusive, logical(), 1L)

  test <- function(env) {
    if (start) {
      value <- current_qday(env)
    } else {
      value <- current_qday_from_end(env)
    }

    test_before(x, value, inclusive)
  }

  if (start) {
    desc <- "Before day of the quarter: {x}"
  } else {
    desc <- "Before day from the end of the quarter: {x}"
  }

  new_event(
    description = glue(desc),
    test = test
  )
}

# ------------------------------------------------------------------------------

#' @export
before_mday <- function(x, inclusive = FALSE, start = TRUE) {
  x <- cast_scalar_integer(x)

  vec_assert(inclusive, logical(), 1L)

  if (!vec_in(x, 1:31)) {
    glubort("A day of the month must be in `1:31`, not {x}.")
  }

  test <- function(env) {
    if (start) {
      value <- current_mday(env)
    } else {
      value <- current_mday_from_end(env)
    }

    test_before(x, value, inclusive)
  }

  if (start) {
    desc <- "Before day of the month: {x}"
  } else {
    desc <- "Before day from the end of the month: {x}"
  }

  new_event(
    description = glue(desc),
    test = test
  )
}

# ------------------------------------------------------------------------------

#' @export
before_wday <- function(x, inclusive = FALSE, start = TRUE) {
  x <- wday_normalize(x)
  x <- cast_scalar_integer(x)

  vec_assert(inclusive, logical(), 1L)

  if (!vec_in(x, 1:7)) {
    glubort("A day of the week must be in `1:7`, not {x}.")
  }

  test <- function(env) {
    if (start) {
      value <- current_wday(env)
    } else {
      value <- current_wday_from_end(env)
    }

    test_before(x, value, inclusive)
  }

  if (start) {
    desc <- "Before day of the week: {weekday_print()[x]}"
  } else {
    desc <- "Before day from the end of the week: {weekday_print()[x]}"
  }

  new_event(
    description = glue(desc),
    test = test
  )
}
