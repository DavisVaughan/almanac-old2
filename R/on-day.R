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
