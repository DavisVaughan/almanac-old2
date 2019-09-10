

#' @export
between_mdays <- function(x, y) {
  x <- vec_cast(x, integer())
  y <- vec_cast(y, integer())

  vec_assert(x, size = 1L)
  vec_assert(y, size = 1L)

  if (!vec_in(x, 1:31)) {
    abort("`y` must be a valid month day, `1:31`.")
  }

  if (!vec_in(y, 1:31)) {
    abort("`y` must be a valid month day, `1:31`.")
  }

  test <- function() {
    x <= current_mday() & y >= current_mday()
  }

  new_event(
    description = glue("Between month days: {x} and {y}"),
    test = test
  )
}
