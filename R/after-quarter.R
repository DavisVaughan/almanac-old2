#' @export
after_quarter <- function(x, inclusive = FALSE) {
  x <- cast_scalar_integer(x)

  vec_assert(inclusive, logical(), 1L)

  if (!vec_in(x, 1:4)) {
    glubort("A quarter must be in `1:4`, not {x}.")
  }

  test <- function(env) {
    value <- current_quarter(env)
    test_after(x, value, inclusive)
  }

  new_event(
    description = glue("After quarter: {x}"),
    test = test
  )
}
