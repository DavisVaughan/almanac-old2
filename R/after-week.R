#' @rdname event-yweek
#' @export
after_week <- function(x, inclusive = FALSE, start = TRUE) {
  after_yweek(x, inclusive, start)
}

#' @rdname event-yweek
#' @export
after_yweek <- function(x, inclusive = FALSE, start = TRUE) {
  x <- cast_scalar_integer(x)

  vec_assert(inclusive, logical(), 1L)

  if (!vec_in(x, 1:52)) {
    glubort("A week of the year must be in `1:52`, not {x}.")
  }

  test <- function(env) {
    if (start) {
      value <- current_yweek(env)
    } else {
      value <- current_yweek_from_end(env)
    }

    test_after(x, value, inclusive)
  }

  if (start) {
    desc <- "After week of the year: {x}"
  } else {
    desc <- "After week from the end of the year: {x}"
  }

  new_event(
    description = glue(desc),
    test = test
  )
}

# ------------------------------------------------------------------------------

#' @rdname event-qweek
#' @export
after_qweek <- function(x, inclusive = FALSE, start = TRUE) {
  x <- cast_scalar_integer(x)

  vec_assert(inclusive, logical(), 1L)

  test <- function(env) {
    if (start) {
      value <- current_qweek(env)
    } else {
      value <- current_qweek_from_end(env)
    }

    test_after(x, value, inclusive)
  }

  if (start) {
    desc <- "After week of the quarter: {x}"
  } else {
    desc <- "After week from the end of the quarter: {x}"
  }

  new_event(
    description = glue(desc),
    test = test
  )
}

# ------------------------------------------------------------------------------

#' @rdname event-mweek
#' @export
after_mweek <- function(x, inclusive = FALSE, start = TRUE) {
  x <- cast_scalar_integer(x)

  vec_assert(inclusive, logical(), 1L)

  if (!vec_in(x, 1:5)) {
    glubort("A week of the month must be in `1:5`, not {x}.")
  }

  test <- function(env) {
    if (start) {
      value <- current_mweek(env)
    } else {
      value <- current_mweek_from_end(env)
    }

    test_after(x, value, inclusive)
  }

  if (start) {
    desc <- "After week of the month: {x}"
  } else {
    desc <- "After week from the end of the month: {x}"
  }

  new_event(
    description = glue(desc),
    test = test
  )
}
