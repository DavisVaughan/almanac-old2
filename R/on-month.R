#' Events related to months of the year
#'
#' @description
#'
#' - `on_month()` / `on_ymonth()`: Is the date on month `x`?
#'
#' - `before_month()` / `before_ymonth()`: Is the date before month `x`?
#'
#' - `after_month()` / `after_ymonth()`: Is the date after month `x`?
#'
#' - `between_months()` / `between_ymonths()`: Is the date between months `x`
#'    and `y`?
#'
#' @details
#'
#' `on_month()` is an alias of the less common, but more verbose, `on_ymonth()`.
#'
#' @param x `[integer(1) / character(1)]`
#'
#'    A month of the year to mark as an event. For `on_month()`, this is also
#'    allowed to be a vector. This can be a full month string like `"November"`,
#'    or an abbreviation like `"Nov"`.
#'
#' @param y `[integer(1) / character(1)]`
#'
#'    A month of the year to mark as an event.
#'
#' @param inclusive `[logical(1)]`
#'
#'    Should `x` count as an event?
#'
#' @name event-ymonth
NULL

#' @rdname event-ymonth
#' @export
on_month <- function(x) {
  on_ymonth(x)
}

#' @rdname event-ymonth
#' @export
on_ymonth <- function(x) {
  x <- month_normalize(x)
  x <- vec_cast(x, integer())

  if (any(!vec_in(x, 1:12))) {
    abort("`x` must be a valid month of the year, in `1:12`.")
  }

  test <- function(env) {
    vec_in(current_ymonth(env), x)
  }

  new_event(
    description = glue("On month of the year: {collapse_and_trim(month_print()[x])}"),
    test = test
  )
}

ymonth <- function(x) {
  month(x)
}

# ------------------------------------------------------------------------------

#' Events related to months of the quarter
#'
#' @description
#'
#' - `on_qmonth()`: Is the date on the `x`-th month of the quarter?
#'
#' - `before_qmonth()`: Is the date before the `x`-th month of the quarter?
#'
#' - `after_qmonth()`: Is the date after the `x`-th month of the quarter?
#'
#' - `between_qmonths()`: Is the date between the `x`-th and `y`-th months of
#'   the quarter?
#'
#' @param x `[integer(1)]`
#'
#'    A month of the quarter to mark as an event. For `on_qmonth()`, this is
#'    also allowed to be a vector.
#'
#' @param y `[integer(1)]`
#'
#'    A month of the quarter to mark as an event.
#'
#' @param inclusive `[logical(1)]`
#'
#'    Should `x` count as an event?
#'
#' @name event-qmonth
NULL

#' @rdname event-qmonth
#' @export
on_qmonth <- function(x) {
  x <- vec_cast(x, integer())

  if (any(!vec_in(x, 1:3))) {
    abort("`x` must be a valid month of the quarter, in `1:3`.")
  }

  test <- function(env) {
    vec_in(current_qmonth(env), x)
  }

  new_event(
    description = glue("On month of the quarter: {collapse_and_trim(x)}"),
    test = test
  )
}

qmonth <- function(x) {
  (month(x) - 1L) %/% 4L + 1L
}
