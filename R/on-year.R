#' @export
on_year <- function(x) {
  x <- vec_cast(x, integer())

  test <- function(env) {
    vec_in(current_year(env), x)
  }

  new_event(
    description = glue("On year: {collapse_and_trim(x)}"),
    test = test
  )
}

#' @export
on_isoyear <- function(x) {
  x <- vec_cast(x, integer())

  test <- function(env) {
    vec_in(current_isoyear(env), x)
  }

  new_event(
    description = glue("On ISO year: {collapse_and_trim(x)}"),
    test = test
  )
}

#' @export
on_epiyear <- function(x) {
  x <- vec_cast(x, integer())

  test <- function(env) {
    vec_in(current_epiyear(env), x)
  }

  new_event(
    description = glue("On epidemilogical year: {collapse_and_trim(x)}"),
    test = test
  )
}
