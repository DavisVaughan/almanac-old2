#' @export
on_quarter <- function(x) {
  x <- vec_cast(x, integer())

  if (any(!vec_in(x, 1:4))) {
    glubort("`x` must be a valid quarter, in `1:4`.")
  }

  test <- function(env) {
    vec_in(current_quarter(env), x)
  }

  new_event(
    description = glue("On quarter: {collapse_and_trim(x)}"),
    test = test
  )
}
