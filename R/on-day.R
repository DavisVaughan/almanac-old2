#' @export
on_day <- function(x) {
  on_yday(x)
}

#' @export
on_yday <- function(x) {
  x <- vec_cast(x, integer())

  if (any(!vec_in(x, 1:366))) {
    abort("`x` must be a valid year day, in `1:366`.")
  }

  test <- function(env) {
    vec_in(current_yday(env), x)
  }

  new_event(
    description = glue("On day of the year: {collapse_and_trim(x)}"),
    test = test
  )
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
