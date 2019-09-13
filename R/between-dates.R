#' @rdname event-date
#' @export
between_dates <- function(x, y) {
  x <- vec_cast_date(x)
  vec_assert(x, size = 1L)

  y <- vec_cast_date(y)
  vec_assert(y, size = 1L)

  if (x > y) {
    glubort("`x` ({x}) must come before `y` ({y}).")
  }

  after <- after_date(x, inclusive = TRUE)
  before <- before_date(y, inclusive = TRUE)

  test <- function(env) {
    event_in_impl(after, env) & event_in_impl(before, env)
  }

  new_event(
    description = glue("Between dates: {x} - {y}"),
    test = test
  )
}
