#' @export
on_mday <- function(x) {
  x <- vec_cast(x, integer())

  if (!all(vec_in(x, 1:31))) {
    abort("`x` must be a valid month day, `1:31`.")
  }

  test <- function() {
    vec_in(current_mday(), x)
  }

  new_event(
    description = glue("Day of the month: {collapse_and_trim(x)}"),
    test = test
  )
}

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
