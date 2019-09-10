#' @export
on_week <- function(x, start = TRUE) {
  on_yweek(x, start = start)
}

#' @export
on_yweek <- function(x, start = TRUE) {
  x <- vec_cast(x, integer())
  start <- vec_assert(start, logical(), 1L)

  test <- function() {
    yweek_matches(x, start)
  }

  if (start) {
    desc <- "On week of the year: {collapse_and_trim(x)}"
  } else {
    desc <- "On week from the end of the year: {collapse_and_trim(x)}"
  }

  new_event(
    description = glue(desc),
    test = test
  )
}

yweek <- function(x) {
  week(x)
}

yweek_impl <- function(x) {
  (x - 1L) %/% 7L + 1L
}

yweek_matches <- function(x, start) {
  if (start) {
    vec_in(current_yweek_from_start(), x)
  } else {
    vec_in(current_yweek_from_end(), x)
  }
}

current_yweek_from_end <- function() {
  days_left_in_year <- current_days_in_year() - current_yday()
  yweek_impl(days_left_in_year + 1L)
}

current_yweek_from_start <- function() {
  current_yweek()
}

days_in_year <- function(x) {
  n_days <- rep(365L, times = length(x))
  n_days[leap_year(x)] <- 366L
  n_days
}

# ------------------------------------------------------------------------------

#' @export
on_qweek <- function(x, start = TRUE) {
  x <- vec_cast(x, integer())
  start <- vec_assert(start, logical(), 1L)

  test <- function() {
    qweek_matches(x, start)
  }

  if (start) {
    desc <- "On week of the quarter: {collapse_and_trim(x)}"
  } else {
    desc <- "On week from the end of the quarter: {collapse_and_trim(x)}"
  }

  new_event(
    description = glue(desc),
    test = test
  )
}

qweek_matches <- function(x, start) {
  if (start) {
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
on_mweek <- function(x, start = TRUE) {
  x <- vec_cast(x, integer())
  start <- vec_assert(start, logical(), 1L)

  test <- function() {
    mweek_matches(x, start)
  }

  if (start) {
    desc <- "On week of the month: {collapse_and_trim(x)}"
  } else {
    desc <- "On week from the end of the month: {collapse_and_trim(x)}"
  }

  new_event(
    description = glue(desc),
    test = test
  )
}

mweek_matches <- function(x, start) {
  if (start) {
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
