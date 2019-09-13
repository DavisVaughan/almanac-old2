#' @rdname event-year
#' @export
before_year <- function(x, inclusive = FALSE) {
  x <- cast_scalar_integer(x)

  vec_assert(inclusive, logical(), 1L)

  test <- function(env) {
    value <- current_year(env)
    test_before(x, value, inclusive)
  }

  new_event(
    description = glue("Before year: {x}"),
    test = test
  )
}

# ------------------------------------------------------------------------------

#' @rdname event-year
#' @export
before_isoyear <- function(x, inclusive = FALSE) {
  x <- cast_scalar_integer(x)

  vec_assert(inclusive, logical(), 1L)

  test <- function(env) {
    value <- current_isoyear(env)

    if (inclusive) {
      x >= value
    } else {
      x > value
    }
  }

  new_event(
    description = glue("Before ISO year: {x}"),
    test = test
  )
}

# ------------------------------------------------------------------------------

#' @rdname event-year
#' @export
before_epiyear <- function(x, inclusive = FALSE) {
  x <- cast_scalar_integer(x)

  vec_assert(inclusive, logical(), 1L)

  test <- function(env) {
    value <- current_epiyear(env)

    if (inclusive) {
      x >= value
    } else {
      x > value
    }
  }

  new_event(
    description = glue("Before epidemilogical year: {x}"),
    test = test
  )
}
