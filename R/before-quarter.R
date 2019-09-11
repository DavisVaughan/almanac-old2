#' @export
before_quarter <- function(x, inclusive = FALSE) {
  x <- cast_scalar_integer(x)

  vec_assert(inclusive, logical(), 1L)

  if (!vec_in(x, 1:4)) {
    glubort("A quarter must be in `1:4`, not {x}.")
  }

  test <- function(env) {
    value <- current_quarter(env)
    test_before(x, value, inclusive)
  }

  new_event(
    description = glue("Before quarter: {x}"),
    test = test
  )
}
