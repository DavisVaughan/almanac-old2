#' @rdname event-yday
#' @export
after_day <- function(x, inclusive = FALSE, start = TRUE) {
  after_yday(x, inclusive)
}

#' @rdname event-yday
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

#' @rdname event-qday
#' @export
after_qday <- function(x, inclusive = FALSE, start = TRUE) {
  x <- cast_scalar_integer(x)

  vec_assert(inclusive, logical(), 1L)

  test <- function(env) {
    if (start) {
      value <- current_qday(env)
    } else {
      value <- current_qday_from_end(env)
    }

    test_after(x, value, inclusive)
  }

  if (start) {
    desc <- "After day of the quarter: {x}"
  } else {
    desc <- "After day from the end of the quarter: {x}"
  }

  new_event(
    description = glue(desc),
    test = test
  )
}

# ------------------------------------------------------------------------------

#' @rdname event-mday
#' @export
after_mday <- function(x, inclusive = FALSE, start = TRUE) {
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

    test_after(x, value, inclusive)
  }

  if (start) {
    desc <- "After day of the month: {x}"
  } else {
    desc <- "After day from the end of the month: {x}"
  }

  new_event(
    description = glue(desc),
    test = test
  )
}

# ------------------------------------------------------------------------------

#' @rdname event-wday
#' @export
after_wday <- function(x, inclusive = FALSE, start = TRUE) {
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

    test_after(x, value, inclusive)
  }

  if (start) {
    desc <- "After day of the week: {weekday_print()[x]}"
  } else {
    desc <- "After day from the end of the week: {weekday_print()[x]}"
  }

  new_event(
    description = glue(desc),
    test = test
  )
}
