#' Events related to specific dates
#'
#' @description
#'
#' - `on_date()`: Is the date on `x`?
#'
#' - `before_date()`: Is the date before `x`?
#'
#' - `after_date()`: Is the date after `x`?
#'
#' - `between_dates()`: Is the date between `x` and `y`?
#'
#' @param x `[Date(1)]`
#'
#'    A date to mark as an event. For `on_date()`, this is
#'    also allowed to be a vector.
#'
#' @param y `[Date(1)]`
#'
#'    A date to mark as an event.
#'
#' @param inclusive `[logical(1)]`
#'
#'    Should `x` count as an event?
#'
#' @name event-date
NULL

#' @rdname event-date
#' @export
on_date <- function(x) {
  x <- vec_cast_date(x)

  test <- function(env) {
    vec_in(current_date(env), x)
  }

  new_event(
    description = glue("On date: {collapse_and_trim(x)}"),
    test = test
  )
}
