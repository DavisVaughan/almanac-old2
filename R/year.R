#' @export
in_year <- function(x) {
  x <- vec_cast(x, integer())

  test <- function() {
    vec_in(current_year(), x)
  }

  new_event(
    description = glue("In year: {collapse_and_trim(x)}"),
    test = test
  )
}
