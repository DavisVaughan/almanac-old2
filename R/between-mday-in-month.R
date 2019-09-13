#' @rdname event-mday-in-month
#' @export
between_mdays_in_months <- function(x_month, x_mday, y_month, y_mday) {
  x_month <- month_normalize(x_month)
  x_month <- cast_scalar_integer(x_month)

  y_month <- month_normalize(y_month)
  y_month <- cast_scalar_integer(y_month)

  x_mday <- cast_scalar_integer(x_mday)
  y_mday <- cast_scalar_integer(y_mday)

  after <- after_mday_in_month(x_month, x_mday, inclusive = TRUE)
  before <- before_mday_in_month(y_month, y_mday, inclusive = TRUE)

  test <- function(env) {
    event_in_impl(after, env) & event_in_impl(before, env)
  }

  new_event(
    description = glue("Between days of the month in months: {month_print()[x_month]}-{x_mday} - {month_print()[y_month]}-{y_mday}"),
    test = test
  )
}
