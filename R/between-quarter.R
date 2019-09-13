#' @export
between_quarters <- function(x, y) {
  x <- cast_scalar_integer(x)
  y <- cast_scalar_integer(y, "y")

  if (x > y) {
    glubort("`x` ({x}) must come before `y` ({y}).")
  }

  after <- after_quarter(x, inclusive = TRUE)
  before <- before_quarter(y, inclusive = TRUE)

  test <- function(env) {
    event_in_impl(after, env) & event_in_impl(before, env)
  }

  new_event(
    description = glue("Between quarters: {x}-{y}"),
    test = test
  )
}
