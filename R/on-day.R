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

  test <- function() {
    vec_in(current_yday(), x)
  }

  new_event(
    description = glue("On day of year: {collapse_and_trim(x)}"),
    test = test
  )
}

# ------------------------------------------------------------------------------

#' @export
on_qday <- function(x) {
  x <- vec_cast(x, integer())

  test <- function() {
    vec_in(current_qday(), x)
  }

  new_event(
    description = glue("On day of quarter: {collapse_and_trim(x)}"),
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

  test <- function() {
    vec_in(current_mday(), x)
  }

  new_event(
    description = glue("On day of month: {collapse_and_trim(x)}"),
    test = test
  )
}

# ------------------------------------------------------------------------------

#' @export
on_wday <- function(x) {
  x <- wday_normalize(x)
  x <- vec_cast(x, integer())

  test <- function() {
    vec_in(current_wday(), x)
  }

  new_event(
    description = glue("On day of week: {collapse_and_trim(weekday_print()[x])}"),
    test = test
  )
}
