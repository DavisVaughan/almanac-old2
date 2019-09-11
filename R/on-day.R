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

# ------------------------------------------------------------------------------

#' @export
on_qday <- function(x) {
  x <- vec_cast(x, integer())

  test <- function(env) {
    vec_in(current_qday(env), x)
  }

  new_event(
    description = glue("On day of the quarter: {collapse_and_trim(x)}"),
    test = test
  )
}

current_qday_from_start <- function(env) {
  current_qday(env)
}

current_qday_from_end <- function(env) {
  days_left_in_quarter <- current_days_in_quarter(env) - current_qday(env)
  days_left_in_quarter + 1L
}

# ------------------------------------------------------------------------------

#' @export
on_mday <- function(x) {
  x <- vec_cast(x, integer())

  if (!all(vec_in(x, 1:31))) {
    abort("`x` must be a valid month day, in `1:31`.")
  }

  test <- function(env) {
    vec_in(current_mday(env), x)
  }

  new_event(
    description = glue("On day of the month: {collapse_and_trim(x)}"),
    test = test
  )
}

# ------------------------------------------------------------------------------

#' @export
on_wday <- function(x) {
  x <- wday_normalize(x)
  x <- vec_cast(x, integer())

  test <- function(env) {
    vec_in(current_wday(env), x)
  }

  new_event(
    description = glue("On day of the week: {collapse_and_trim(weekday_print()[x])}"),
    test = test
  )
}
