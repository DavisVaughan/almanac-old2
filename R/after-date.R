#' @rdname event-date
#' @export
after_date <- function(x, inclusive = FALSE) {
  x <- vec_cast_date(x)
  vec_assert(x, size = 1L)

  test <- function(env) {
    test_after(x, current_date(env), inclusive)
  }

  new_event(
    description = glue("After date: {collapse_and_trim(x)}"),
    test = test
  )
}
