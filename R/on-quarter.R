#' @export
on_quarter <- function(x) {
  x <- vec_cast(x, integer())

  test <- function() {
    vec_in(current_quarter(), x)
  }

  new_event(
    description = glue("On quarter: {collapse_and_trim(x)}"),
    test = test
  )
}
