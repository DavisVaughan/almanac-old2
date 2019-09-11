#' @export
on_day <- function(x, start = TRUE) {
  on_yday(x, start)
}

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

wday_from_end <- function(x) {
  7L - wday(x, week_start = 7L) + 1L
}
