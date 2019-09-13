#' Events related to quarters
#'
#' @description
#'
#' - `on_quarter()`: Is the date on quarter `x`?
#'
#' - `before_quarter()`: Is the date before quarter `x`?
#'
#' - `after_quarter()`: Is the date after quarter `x`?
#'
#' - `between_quarters()`: Is the date between quarters `x` and `y`?
#'
#' @param x `[integer(1)]`
#'
#'    A quarter to mark as an event. For `on_quarter()`, this is also
#'    allowed to be a vector.
#'
#' @param y `[integer(1)]`
#'
#'    A quarter to mark as an event.
#'
#' @param inclusive `[logical(1)]`
#'
#'    Should `x` count as an event?
#'
#' @name event-quarter
NULL

#' @rdname event-quarter
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
