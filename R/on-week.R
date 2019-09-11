#' @export
on_week <- function(x, start = TRUE) {
  on_yweek(x, start = start)
}

#' @export
on_yweek <- function(x, start = TRUE) {
  x <- vec_cast(x, integer())
  start <- vec_assert(start, logical(), 1L)

  test <- function(env) {
    if (start) {
      value <- current_yweek(env)
    } else {
      value <- current_yweek_from_end(env)
    }

    vec_in(value, x)
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

yweek_from_end <- function(x) {
  yweek_impl(yday_from_end(x))
}

yweek_impl <- function(x) {
  (x - 1L) %/% 7L + 1L
}

# ------------------------------------------------------------------------------

#' @export
on_qweek <- function(x, start = TRUE) {
  x <- vec_cast(x, integer())
  start <- vec_assert(start, logical(), 1L)

  test <- function(env) {
    if (start) {
      value <- current_qweek(env)
    } else {
      value <- current_qweek_from_end(env)
    }

    vec_in(value, x)
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

qweek <- function(x) {
  qweek_impl(qday(x))
}

qweek_from_end <- function(x) {
  qweek_impl(qday_from_end(x))
}

qweek_impl <- function(x) {
  (x - 1L) %/% 7L + 1L
}

# ------------------------------------------------------------------------------

#' @export
on_mweek <- function(x, start = TRUE) {
  x <- vec_cast(x, integer())
  start <- vec_assert(start, logical(), 1L)

  test <- function(env) {
    if (start) {
      value <- current_mweek(env)
    } else {
      value <- current_mweek_from_end(env)
    }

    vec_in(value, x)
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

mweek <- function(x) {
  mweek_impl(mday(x))
}

mweek_from_end <- function(x) {
  mweek_impl(mday_from_end(x))
}

mweek_impl <- function(x) {
  (x - 1L) %/% 7L + 1L
}
