#' @export
after_day <- function(x, inclusive = FALSE, start = TRUE) {
  after_yday(x, inclusive)
}

#' @export
after_yday <- function(x, inclusive = FALSE, start = TRUE) {
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

    test_after(x, value, inclusive)
  }

  if (start) {
    desc <- "After day of the year: {x}"
  } else {
    desc <- "After day from the end of the year: {x}"
  }

  new_event(
    description = glue(desc),
    test = test
  )
}

# ------------------------------------------------------------------------------

#' @export
after_qday <- function(x, inclusive = FALSE) {
  x <- cast_scalar_integer(x)

  vec_assert(inclusive, logical(), 1L)

  test <- function(env) {
    value <- current_qday(env)
    test_after(x, value, inclusive)
  }

  new_event(
    description = glue("After day of the quarter: {x}"),
    test = test
  )
}

# ------------------------------------------------------------------------------

#' @export
after_mday <- function(x, inclusive = FALSE) {
  x <- cast_scalar_integer(x)

  vec_assert(inclusive, logical(), 1L)

  if (!vec_in(x, 1:31)) {
    glubort("A day of the month must be in `1:31`, not {x}.")
  }

  test <- function(env) {
    value <- current_mday(env)
    test_after(x, value, inclusive)
  }

  new_event(
    description = glue("After day of the month: {x}"),
    test = test
  )
}

# ------------------------------------------------------------------------------

#' @export
after_wday <- function(x, inclusive = FALSE) {
  x <- wday_normalize(x)
  x <- cast_scalar_integer(x)

  vec_assert(inclusive, logical(), 1L)

  if (!vec_in(x, 1:7)) {
    glubort("A day of the week must be in `1:7`, not {x}.")
  }

  test <- function(env) {
    value <- current_wday(env)
    test_after(x, value, inclusive)
  }

  new_event(
    description = glue("After day of the week: {weekday_print()[x]}"),
    test = test
  )
}
