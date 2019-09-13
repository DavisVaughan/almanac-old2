#' Shift dates relative to a schedule
#'
#' @description
#'
#' - `sch_jump()` shifts a sequence of dates by "jumping" from `x` to
#'   `x + jump`. After the jump, [sch_adjust()] is called with the `adjustment`
#'   to ensure that if the new dates are events, they are adjusted to the next
#'   available non-event date.
#'
#' - `sch_step()` steps over a sequence of dates 1 day at a time, for `n` days.
#'   After each step, [sch_adjust()] is called with an adjustment of `days(1)`.
#'   This has different results from `sch_jump()` with a jump of `days(n)`, and
#'   is more appropriate for shifting by "n business days".
#'
#' @details
#'
#' For shifting by "n business days", `sch_step()` is often more appropriate.
#' Imagine you are on a Friday and want to shift forward 2 days using a
#' schedule that marks weekends as events. There are two options:
#'
#' - `sch_jump()` - Jump forward 2 days to Sunday, and apply the `adjustment`.
#'   Assuming `adjustment = days(1)` was used, that means the result is Monday.
#'
#' - `sch_step()` - Step forward 1 day to Saturday, apply an adjustment of
#'   `days(1)`, which rolls forward to Monday. Step forward 1 day to Tuesday,
#'   and no further adjustment is required.
#'
#' The second option more naturally lends itself to business logic. Two business
#' days from Friday is Tuesday.
#'
#' @param jump `[Period(1) / character(1)]`
#'
#'   A lubridate period object, such as [lubridate::days()] or
#'   [lubridate::years()]. This can also be a character string parsable
#'   by [lubridate::period()]. Sub-daily periods are not allowed.
#'
#' @param n `[integer(1)]`
#'
#'   The number of days to step. Can be negative to step backwards.
#'
#' @inheritParams sch_adjust
#'
#' @examples
#' # 2019-09-13 is a Friday
#'
#' # Note that here we "jump" to Sunday, then adjust, leaving us on Monday
#' sch_jump("2019-09-13", days(2), on_weekends())
#'
#' # Here we step 1 day to Saturday, adjust to Monday,
#' # then step 1 day to Tuesday
#' sch_step("2019-09-13", 2, on_weekends())
#'
#' @export
sch_jump <- function(x, jump, schedule, adjustment = days(1)) {
  x <- vec_cast_date(x)
  schedule <- as_schedule(schedule)
  jump <- check_jump(jump)

  x <- x + jump
  x <- sch_adjust(x, schedule, adjustment)

  x
}

#' @rdname sch_jump
#' @export
sch_step <- function(x, n, schedule) {
  x <- vec_cast_date(x)
  n <- cast_scalar_integer(n, "n")
  schedule <- as_schedule(schedule)

  # Use integers rather than periods.
  # Avoids SLOW update() function from lubridate
  if (n >= 0) {
    one_day <- 1L # days(1)
  } else {
    one_day <- -1L # days(-1)
  }

  one_day_adjuster <- make_adjuster(one_day)

  n <- abs(n)

  for (i in seq_len(n)) {
    x <- x + one_day
    x <- sch_adjust(x, schedule, one_day_adjuster)
  }

  x
}
