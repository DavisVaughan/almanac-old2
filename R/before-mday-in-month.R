# A shortcut for "before january 24th", built with lower level event functions

# Can technically create any alternative using this method, but it wouldn't
# be worth exporting them all since you can build them from the lower elements.
# maybe an example like:
# `before_mweek_in_month(month, mday, inclusive = FALSE)` created from:
# strictly_before_ymonth <- before_ymonth(month, inclusive = F)
# in_ymonth_and_before_mweek <- on_ymonth(month) & before_mweek(mweek, inclusive = inclusive)
# strictly_before_ymonth | in_ymonth_and_before_mweek

#' @export
before_mday_in_month <- function(month, mday, inclusive = FALSE) {
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

  strictly_before_ymonth <- before_ymonth(month, inclusive = FALSE)
  in_ymonth_and_before_mday <- on_ymonth(month) & before_mday(mday, inclusive = inclusive)

  event_is_before_mday_in_month <- strictly_before_ymonth | in_ymonth_and_before_mday

  test <- function(env) {
    event_is_impl(event_is_before_mday_in_month, env)
  }

  new_event(
    description = glue("Before day of the month in month: {month_print()[month]}-{mday}"),
    test = test
  )
}
