#' @export
on_month <- function(x) {
  on_ymonth(x)
}

#' @export
on_ymonth <- function(x) {
  x <- month_normalize(x)
  x <- vec_cast(x, integer())

  if (any(!vec_in(x, 1:12))) {
    abort("`x` must be a valid month of the year, in `1:12`.")
  }

  test <- function() {
    vec_in(current_ymonth(), x)
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

#' @export
on_qmonth <- function(x) {
  x <- vec_cast(x, integer())

  if (any(!vec_in(x, 1:3))) {
    abort("`x` must be a valid month of the quarter, in `1:3`.")
  }

  test <- function() {
    vec_in(current_qmonth(), x)
  }

  new_event(
    description = glue("On month of the quarter: {collapse_and_trim(x)}"),
    test = test
  )
}

qmonth <- function(x) {
  (month(x) - 1L) %/% 4L + 1L
}
