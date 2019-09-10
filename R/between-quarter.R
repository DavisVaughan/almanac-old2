#' @export
between_quarters <- function(x, y) {
  x <- vec_cast(x, integer())
  y <- vec_cast(y, integer())

  vec_assert(x, size = 1L)
  vec_assert(y, size = 1L)

  if (!vec_in(x, 1:4)) {
    glubort("`x` ({x}) must be a valid quarter, in `1:4`.")
  }

  if (!vec_in(y, 1:4)) {
    glubort("`y` ({y}) must be a valid quarter, in `1:4`.")
  }

  if (x > y) {
    glubort("`x` ({x}) must come before `y` ({y}).")
  }

  test <- function() {
    value <- current_quarter()
    x <= value & y >= value
  }

  new_event(
    description = glue("Between quarters: {x}-{y}"),
    test = test
  )
}
