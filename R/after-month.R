#' @export
after_month <- function(x, inclusive = FALSE) {
  after_ymonth(x, inclusive)
}

#' @export
after_ymonth <- function(x, inclusive = FALSE) {
  x <- month_normalize(x)
  x <- cast_scalar_integer(x)

  vec_assert(inclusive, logical(), 1L)

  if (!vec_in(x, 1:12)) {
    glubort("A month of the year must be in `1:12`, not {x}.")
  }

  test <- function(env) {
    value <- current_ymonth(env)
    test_after(x, value, inclusive)
  }

  new_event(
    description = glue("After month of the year: {month_print()[x]}"),
    test = test
  )
}

# ------------------------------------------------------------------------------

#' @export
after_qmonth <- function(x, inclusive = FALSE) {
  x <- cast_scalar_integer(x)

  vec_assert(inclusive, logical(), 1L)

  if (!vec_in(x, 1:3)) {
    glubort("A month of the quarter must be in `1:3`, not {x}.")
  }

  test <- function(env) {
    value <- current_qmonth(env)
    test_after(x, value, inclusive)
  }

  new_event(
    description = glue("After month of the quarter: {x}"),
    test = test
  )
}
