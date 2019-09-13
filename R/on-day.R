#' Events related to days of the year
#'
#' @description
#'
#' - `on_day()` / `on_yday()`: Is the date on the `x`-th day of the year?
#'
#' - `before_day()` / `before_yday()`: Is the date before the `x`-th day of
#'   the year?
#'
#' - `after_day()` / `after_yday()`: Is the date after the `x`-th day of
#'   the year?
#'
#' - `between_days()` / `between_ydays()`: Is the date between the `x`-th and
#'   `y`-th days of the year?
#'
#' @details
#'
#' `on_day()` is an alias of the less common, but more verbose, `on_yday()`.
#'
#' @param x `[integer(1)]`
#'
#'    A day of the year to mark as an event. For `on_day()`, this is also
#'    allowed to be a vector.
#'
#' @param y `[integer(1)]`
#'
#'    A day of the year to mark as an event.
#'
#' @param inclusive `[logical(1)]`
#'
#'    Should `x` count as an event?
#'
#' @param start `[logical(1)]`
#'
#'    Should counting start from the beginning of the year? If `FALSE`, the
#'    event is computed as the `x`-th day _from the end_ of the year.
#'
#' @name event-yday
NULL

#' @rdname event-yday
#' @export
on_day <- function(x, start = TRUE) {
  on_yday(x, start)
}

#' @rdname event-yday
#' @export
on_yday <- function(x, start = TRUE) {
  x <- vec_cast(x, integer())

  if (any(!vec_in(x, 1:366))) {
    abort("`x` must be a valid year day, in `1:366`.")
  }

  test <- function(env) {
    if (start) {
      value <- current_yday(env)
    } else {
      value <- current_yday_from_end(env)
    }

    vec_in(value, x)
  }

  if (start) {
    desc <- "On day of the year: {collapse_and_trim(x)}"
  } else {
    desc <- "On day from the end of the year: {collapse_and_trim(x)}"
  }

  new_event(
    description = glue(desc),
    test = test
  )
}

yday_from_end <- function(x) {
  days_in_year(x) - yday(x) + 1L
}

days_in_year <- function(x) {
  n_days <- rep(365L, times = length(x))
  n_days[leap_year(x)] <- 366L
  n_days
}

# ------------------------------------------------------------------------------

#' Events related to days of the quarter
#'
#' @description
#'
#' - `on_qday()`: Is the date on the `x`-th day of the quarter?
#'
#' - `before_qday()`: Is the date before the `x`-th day of the quarter?
#'
#' - `after_qday()`: Is the date after the `x`-th day of the quarter?
#'
#' - `between_qdays()`: Is the date between the `x`-th and `y`-th days of
#'   the quarter?
#'
#' @param x `[integer(1)]`
#'
#'    A day of the quarter to mark as an event. For `on_qday()`, this is
#'    also allowed to be a vector.
#'
#' @param y `[integer(1)]`
#'
#'    A day of the quarter to mark as an event.
#'
#' @param inclusive `[logical(1)]`
#'
#'    Should `x` count as an event?
#'
#' @param start `[logical(1)]`
#'
#'    Should counting start from the beginning of the quarter? If `FALSE`, the
#'    event is computed as the `x`-th day _from the end_ of the quarter.
#'
#' @name event-qday
NULL

#' @rdname event-qday
#' @export
on_qday <- function(x, start = TRUE) {
  x <- vec_cast(x, integer())

  test <- function(env) {
    if (start) {
      value <- current_qday(env)
    } else {
      value <- current_qday_from_end(env)
    }

    vec_in(value, x)
  }

  if (start) {
    desc <- "On day of the quarter: {collapse_and_trim(x)}"
  } else {
    desc <- "On day from the end of the quarter: {collapse_and_trim(x)}"
  }

  new_event(
    description = glue(desc),
    test = test
  )
}

qday_from_end <- function(x) {
  days_in_quarter(x) - qday(x) + 1L
}

N_DAYS_IN_QUARTER <- c(90L, 91L, 92L, 92L)

days_in_quarter <- function(x) {
  quarter_x <- quarter(x)
  n_days <- N_DAYS_IN_QUARTER[quarter_x]
  n_days[quarter_x == 1L & leap_year(x)] <- 91L
  n_days
}

# ------------------------------------------------------------------------------

#' Events related to days of the month
#'
#' @description
#'
#' - `on_mday()`: Is the date on the `x`-th day of the month?
#'
#' - `before_mday()`: Is the date before the `x`-th day of the month?
#'
#' - `after_mday()`: Is the date after the `x`-th day of the month?
#'
#' - `between_mdays()`: Is the date between the `x`-th and `y`-th days of
#'   the month?
#'
#' @param x `[integer(1)]`
#'
#'    A day of the month to mark as an event. For `on_mday()`, this is
#'    also allowed to be a vector.
#'
#' @param y `[integer(1)]`
#'
#'    A day of the month to mark as an event.
#'
#' @param inclusive `[logical(1)]`
#'
#'    Should `x` count as an event?
#'
#' @param start `[logical(1)]`
#'
#'    Should counting start from the beginning of the month? If `FALSE`, the
#'    event is computed as the `x`-th day _from the end_ of the month.
#'
#' @name event-mday
NULL

#' @rdname event-mday
#' @export
on_mday <- function(x, start = TRUE) {
  x <- vec_cast(x, integer())

  if (!all(vec_in(x, 1:31))) {
    abort("`x` must be a valid month day, in `1:31`.")
  }

  test <- function(env) {
    if (start) {
      value <- current_mday(env)
    } else {
      value <- current_mday_from_end(env)
    }

    vec_in(value, x)
  }

  if (start) {
    desc <- "On day of the month: {collapse_and_trim(x)}"
  } else {
    desc <- "On day from the end of the month: {collapse_and_trim(x)}"
  }

  new_event(
    description = glue(desc),
    test = test
  )
}

mday_from_end <- function(x) {
  days_in_month2(x) - mday(x) + 1L
}

# The names seem to cause problems with vctrs assertions
days_in_month2 <- function(x) {
  unname(days_in_month(x))
}

# ------------------------------------------------------------------------------

#' Events related to days of the week
#'
#' @description
#'
#' - `on_wday()`: Is the date on the `x`-th day of the week?
#'
#' - `on_weekends()`: Is the date on a weekend?
#'
#' - `on_weekdays()`: Is the date on a weekday?
#'
#' - `before_wday()`: Is the date before the `x`-th day of the week?
#'
#' - `after_wday()`: Is the date after the `x`-th day of the week?
#'
#' - `between_wdays()`: Is the date between the `x`-th and `y`-th days of
#'   the week?
#'
#' @param x `[integer(1) / character(1)]`
#'
#'    A day of the week to mark as an event. For `on_wday()`, this is
#'    also allowed to be a vector. This is also allowed to be a full weekday
#'    string like `"Tuesday"`, or an abbreviation like `"Tues"`.
#'
#' @param y `[integer(1) / character(1)]`
#'
#'    A day of the week to mark as an event. This is also allowed to be a full
#'    weekday string like `"Tuesday"`, or an abbreviation like `"Tues"`.
#'
#' @param inclusive `[logical(1)]`
#'
#'    Should `x` count as an event?
#'
#' @param start `[logical(1)]`
#'
#'    Should counting start from the beginning of the week? If `FALSE`, the
#'    event is computed as the `x`-th day _from the end_ of the week.
#'
#' @name event-wday
NULL

#' @rdname event-wday
#' @export
on_wday <- function(x, start = TRUE) {
  x <- wday_normalize(x)
  x <- vec_cast(x, integer())

  test <- function(env) {
    if (start) {
      value <- current_wday(env)
    } else {
      value <- current_wday_from_end(env)
    }

    vec_in(value, x)
  }

  if (start) {
    desc <- "On day of the week: {collapse_and_trim(weekday_print()[x])}"
  } else {
    desc <- "On day from the end of the week: {collapse_and_trim(weekday_print()[x])}"
  }

  new_event(
    description = glue(desc),
    test = test
  )
}

#' @rdname event-wday
#' @export
on_weekends <- function() {
  on_wday(c(1L, 7L))
}

#' @rdname event-wday
#' @export
on_weekdays <- function() {
  on_wday(2:6)
}

wday_from_end <- function(x) {
  7L - wday(x, week_start = 7L) + 1L
}
