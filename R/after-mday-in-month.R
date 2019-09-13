#' @export
after_mday_in_month <- function(month, mday, inclusive = FALSE) {
  month <- month_normalize(month)
  month <- cast_scalar_integer(month)

  mday <- cast_scalar_integer(mday)

  vec_assert(inclusive, logical(), 1L)

  if (!vec_in(month, 1:12)) {
    glubort("A month of the year must be in `1:12`, not {x}.")
  }

  if (!vec_in(mday, 1:31)) {
    glubort("A day of the month must be in `1:31`, not {x}.")
  }

  strictly_after_ymonth <- after_ymonth(month, inclusive = FALSE)
  in_ymonth_and_after_mday <- on_ymonth(month) & after_mday(mday, inclusive = inclusive)

  event_is_after_mday_in_month <- strictly_after_ymonth | in_ymonth_and_after_mday

  test <- function(env) {
    event_in_impl(event_is_after_mday_in_month, env)
  }

  new_event(
    description = glue("After day of the month in month: {month_print()[month]}-{mday}"),
    test = test
  )
}
