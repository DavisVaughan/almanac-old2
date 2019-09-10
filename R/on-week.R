#' @export
on_qweek <- function(x, from_start = TRUE) {
  x <- vec_cast(x, integer())
  from_start <- vec_assert(from_start, logical(), 1L)

  test <- function() {
    qweek_matches(x, from_start)
  }

  new_event(
    description = glue("On week of quarter: {collapse_and_trim(x)}"),
    test = test
  )
}

qweek_matches <- function(x, from_start) {
  if (from_start) {
    vec_in(current_qweek_from_start(), x)
  } else {
    vec_in(current_qweek_from_end(), x)
  }
}

current_qweek_from_end <- function() {
  days_left_in_quarter <- current_days_in_quarter() - current_qday()
  qweek_impl(days_left_in_quarter + 1L)
}

current_qweek_from_start <- function() {
  current_qweek()
}

# Find the week in the quarter
qweek <- function(x) {
  qweek_impl(qday(x))
}

qweek_impl <- function(x) {
  (x - 1L) %/% 7L + 1L
}

N_DAYS_IN_QUARTER <- c(90L, 91L, 92L, 92L)

days_in_quarter <- function(x) {
  quarter_x <- quarter(x)
  n_days <- N_DAYS_IN_QUARTER[quarter_x]
  n_days[quarter_x == 1L & leap_year(x)] <- 91L
  n_days
}

# ------------------------------------------------------------------------------

#' @export
on_mweek <- function(x, from_start = TRUE) {
  x <- vec_cast(x, integer())
  from_start <- vec_assert(from_start, logical(), 1L)

  test <- function() {
    mweek_matches(x, from_start)
  }

  new_event(
    description = glue("On week of month: {collapse_and_trim(x)}"),
    test = test
  )
}

mweek_matches <- function(x, from_start) {
  if (from_start) {
    vec_in(current_mweek_from_start(), x)
  } else {
    vec_in(current_mweek_from_end(), x)
  }
}

# Find the week in the month
mweek <- function(x) {
  mweek_impl(mday(x))
}

mweek_impl <- function(x) {
  (x - 1L) %/% 7L + 1L
}

current_mweek_from_end <- function() {
  days_left_in_month <- current_days_in_month() - current_mday()
  mweek_impl(days_left_in_month + 1L)
}

current_mweek_from_start <- function() {
  current_mweek()
}
