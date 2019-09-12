#' @export
on_date <- function(x) {
  x <- vec_cast_date(x)

  test <- function(env) {
    vec_in(current_date(env), x)
  }

  new_event(
    description = glue("On date: {collapse_and_trim(x)}"),
    test = test
  )
}
