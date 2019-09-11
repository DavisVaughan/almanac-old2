#' @export
before_date <- function(x, inclusive = FALSE) {
  x <- vec_cast(x, new_date())
  vec_assert(x, size = 1L)

  test <- function(env) {
    test_before(x, current_date(env), inclusive)
  }

  new_event(
    description = glue("Before date: {collapse_and_trim(x)}"),
    test = test
  )
}
