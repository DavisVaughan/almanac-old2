# A shortcut for "before january 24th", built with lower level event functions

# Can technically create any alternative using this method, but it wouldn't
# be worth exporting them all since you can build them from the lower elements.
# maybe an example like:
# `before_mweek_in_month(month, mday, inclusive = FALSE)` created from:
# strictly_before_ymonth <- before_ymonth(month, inclusive = F)
# in_ymonth_and_before_mweek <- on_ymonth(month) & before_mweek(mweek, inclusive = inclusive)
# strictly_before_ymonth | in_ymonth_and_before_mweek

#' Events related to a specific day in a month
#'
#' @description
#'
#' - `before_mday_in_month()`: Is the date before the day of the month `mday`
#'    in month `month`?
#'
#' - `after_mday_in_month()`: Is the date after the day of the month `mday`
#'    in month `month`?
#'
#' - `between_mdays_in_months()`: Is the date between the day of the month
#'   `x_mday` in month `x_month` and `y_mday` in month `y_month`?
#'
#' These functions are convenient helpers for defining events such as "before
#' January 24th". They are constructed from lower level event creators, but
#' are common enough to stand alone.
#'
#' @param month,x_month,y_month `[integer(1) / character(1)]`
#'
#'    A month of the year. Combined with `mday`, this uniquely defines a
#'    specific day of a specific month, such as `"January 24"`.
#'
#' @param mday,x_mday,y_mday `[integer(1)]`
#'
#'    A day of the month.
#'
#' @param inclusive `[logical(1)]`
#'
#'    Should the date defined by `month` and `mday` count as an event?
#'
#' @examples
#' # Any day before Feb 25th
#' before_feb_25 <- before_mday_in_month("Feb", 25)
#'
#' event_in("2019-01-01", before_feb_25)
#' event_in("2019-01-26", before_feb_25)
#'
#' # You might think that you could construct this helper
#' # from these two lower level event helpers
#' bad_before_feb_25 <- before_mday(25) & before_month("Feb")
#'
#' # But the logic isn't right! It works when both conditions are true,
#' # but Jan-26 is before Feb-25, but registers as a non-event with this logic
#' # because 26 > 25.
#' event_in("2019-01-01", bad_before_feb_25)
#' event_in("2019-01-26", bad_before_feb_25)
#'
#' # The actual condition is more like:
#' strictly_before_feb <- before_month("Feb", inclusive = FALSE)
#' in_feb_and_before_25th <- on_month("Feb") & before_mday(25)
#' good_before_feb_25 <- strictly_before_feb | in_feb_and_before_25th
#'
#' event_in("2019-01-01", good_before_feb_25)
#' event_in("2019-01-26", good_before_feb_25)
#'
#' # This pattern can be used to construct other similar helpers that
#' # we have chosen not to export, but still might be useful. For example:
#' # "After the 15th day in the 2nd quarter"
#' strictly_after_q2 <- after_quarter(2, inclusive = FALSE)
#' in_q2_and_after_15th_qday <- on_quarter(2) & after_qday(15)
#' after_15th_day_in_q2 <- strictly_after_q2 | in_q2_and_after_15th_qday
#'
#' event_in(as.Date("2019-04-01") + days(14), after_15th_day_in_q2)
#' event_in(as.Date("2019-04-01") + days(15), after_15th_day_in_q2)
#'
#' @name event-mday-in-month
NULL

#' @rdname event-mday-in-month
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
    event_in_impl(event_is_before_mday_in_month, env)
  }

  new_event(
    description = glue("Before day of the month in month: {month_print()[month]}-{mday}"),
    test = test
  )
}
