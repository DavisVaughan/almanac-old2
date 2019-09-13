#' Events related to weeks of the year
#'
#' @description
#'
#' - `on_week()` / `on_yweek()`: Is the date on the `x`-th week of the year?
#'
#' - `before_week()` / `before_yweek()`: Is the date before the `x`-th week of
#'   the year?
#'
#' - `after_week()` / `after_yweek()`: Is the date after the `x`-th week of
#'   the year?
#'
#' - `between_weeks()` / `between_yweeks()`: Is the date between the `x`-th and
#'   `y`-th weeks of the year?
#'
#' @details
#'
#' `on_week()` is an alias of the less common, but more verbose, `on_yweek()`.
#'
#' @param x `[integer(1)]`
#'
#'    A week of the year to mark as an event. For `on_week()`, this is also
#'    allowed to be a vector.
#'
#' @param y `[integer(1)]`
#'
#'    A week of the year to mark as an event.
#'
#' @param inclusive `[logical(1)]`
#'
#'    Should `x` count as an event?
#'
#' @param start `[logical(1)]`
#'
#'    Should counting start from the beginning of the year? If `FALSE`, the
#'    event is computed as the `x`-th week _from the end_ of the year.
#'
#' @name event-yweek
NULL

#' @rdname event-yweek
#' @export
on_week <- function(x, start = TRUE) {
  on_yweek(x, start = start)
}

#' @rdname event-yweek
#' @export
on_yweek <- function(x, start = TRUE) {
  x <- vec_cast(x, integer())
  start <- vec_assert(start, logical(), 1L)

  test <- function(env) {
    if (start) {
      value <- current_yweek(env)
    } else {
      value <- current_yweek_from_end(env)
    }

    vec_in(value, x)
  }

  if (start) {
    desc <- "On week of the year: {collapse_and_trim(x)}"
  } else {
    desc <- "On week from the end of the year: {collapse_and_trim(x)}"
  }

  new_event(
    description = glue(desc),
    test = test
  )
}

yweek <- function(x) {
  week(x)
}

yweek_from_end <- function(x) {
  yweek_impl(yday_from_end(x))
}

yweek_impl <- function(x) {
  (x - 1L) %/% 7L + 1L
}

# ------------------------------------------------------------------------------

#' Events related to weeks of the quarter
#'
#' @description
#'
#' - `on_qweek()`: Is the date on the `x`-th week of the quarter?
#'
#' - `before_qweek()`: Is the date before the `x`-th week of the quarter?
#'
#' - `after_qweek()`: Is the date after the `x`-th week of the quarter?
#'
#' - `between_qweeks()`: Is the date between the `x`-th and `y`-th weeks of
#'   the quarter?
#'
#' @param x `[integer(1)]`
#'
#'    A week of the quarter to mark as an event. For `on_qweek()`, this is
#'    also allowed to be a vector.
#'
#' @param y `[integer(1)]`
#'
#'    A week of the quarter to mark as an event.
#'
#' @param inclusive `[logical(1)]`
#'
#'    Should `x` count as an event?
#'
#' @param start `[logical(1)]`
#'
#'    Should counting start from the beginning of the quarter? If `FALSE`, the
#'    event is computed as the `x`-th week _from the end_ of the quarter.
#'
#' @name event-qweek
NULL

#' @rdname event-qweek
#' @export
on_qweek <- function(x, start = TRUE) {
  x <- vec_cast(x, integer())
  start <- vec_assert(start, logical(), 1L)

  test <- function(env) {
    if (start) {
      value <- current_qweek(env)
    } else {
      value <- current_qweek_from_end(env)
    }

    vec_in(value, x)
  }

  if (start) {
    desc <- "On week of the quarter: {collapse_and_trim(x)}"
  } else {
    desc <- "On week from the end of the quarter: {collapse_and_trim(x)}"
  }

  new_event(
    description = glue(desc),
    test = test
  )
}

qweek <- function(x) {
  qweek_impl(qday(x))
}

qweek_from_end <- function(x) {
  qweek_impl(qday_from_end(x))
}

qweek_impl <- function(x) {
  (x - 1L) %/% 7L + 1L
}

# ------------------------------------------------------------------------------

#' Events related to weeks of the month
#'
#' @description
#'
#' - `on_mweek()`: Is the date on the `x`-th week of the month?
#'
#' - `before_mweek()`: Is the date before the `x`-th week of the month?
#'
#' - `after_mweek()`: Is the date after the `x`-th week of the month?
#'
#' - `between_mweeks()`: Is the date between the `x`-th and `y`-th weeks of
#'   the month?
#'
#' @param x `[integer(1)]`
#'
#'    A week of the month to mark as an event. For `on_mweek()`, this is
#'    also allowed to be a vector.
#'
#' @param y `[integer(1)]`
#'
#'    A week of the month to mark as an event.
#'
#' @param inclusive `[logical(1)]`
#'
#'    Should `x` count as an event?
#'
#' @param start `[logical(1)]`
#'
#'    Should counting start from the beginning of the month? If `FALSE`, the
#'    event is computed as the `x`-th week _from the end_ of the month.
#'
#' @name event-mweek
NULL

#' @rdname event-mweek
#' @export
on_mweek <- function(x, start = TRUE) {
  x <- vec_cast(x, integer())
  start <- vec_assert(start, logical(), 1L)

  test <- function(env) {
    if (start) {
      value <- current_mweek(env)
    } else {
      value <- current_mweek_from_end(env)
    }

    vec_in(value, x)
  }

  if (start) {
    desc <- "On week of the month: {collapse_and_trim(x)}"
  } else {
    desc <- "On week from the end of the month: {collapse_and_trim(x)}"
  }

  new_event(
    description = glue(desc),
    test = test
  )
}

mweek <- function(x) {
  mweek_impl(mday(x))
}

mweek_from_end <- function(x) {
  mweek_impl(mday_from_end(x))
}

mweek_impl <- function(x) {
  (x - 1L) %/% 7L + 1L
}
