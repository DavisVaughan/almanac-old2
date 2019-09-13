#' Events related to years
#'
#' @description
#'
#' - `on_year()`: Is the date on year `x`?
#'
#' - `before_year()`: Is the date before year `x`?
#'
#' - `after_year()`: Is the date after year `x`?
#'
#' - `between_years()`: Is the date between years `x` and `y`?
#'
#' Additionally, there are equivalent functions for ISO years and
#' epidemilogical years.
#'
#' @param x `[integer(1)]`
#'
#'    A year to mark as an event. For `on_year()`, this is also
#'    allowed to be a vector.
#'
#' @param y `[integer(1)]`
#'
#'    A year to mark as an event.
#'
#' @param inclusive `[logical(1)]`
#'
#'    Should `x` count as an event?
#'
#' @name event-year
NULL


#' @rdname event-year
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

#' @rdname event-year
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

#' @rdname event-year
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
