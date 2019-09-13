#' Events related to semesters
#'
#' @description
#'
#' - `on_semester()`: Is the date on the `x`-th semester?
#'
#' @param x `[integer]`
#'
#'    A semester to mark as an event.
#'
#' @name event-semester
NULL

#' @rdname event-semester
#' @export
on_semester <- function(x) {
  x <- vec_cast(x, integer())

  if (any(!vec_in(x, 1:2))) {
    abort("`x` must be a valid semester, in `1:2`.")
  }

  test <- function(env) {
    vec_in(current_semester(env), x)
  }

  new_event(
    description = glue("On semester: {collapse_and_trim(x)}"),
    test = test
  )
}
