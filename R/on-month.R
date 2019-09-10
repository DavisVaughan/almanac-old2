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
    description = glue("On month of quarter: {collapse_and_trim(x)}"),
    test = test
  )
}

qmonth <- function(x) {
  (month(x) - 1L) %/% 4L + 1L
}
