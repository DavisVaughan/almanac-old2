glubort <- function (..., .sep = "", .envir = parent.frame()) {
  abort(glue(..., .sep = .sep, .envir = .envir))
}

as_posixlt <- function(x) {
  as.POSIXlt(x, tz = tz(x))
}

collapse_and_trim <- function(x) {
  glue::glue_collapse(x, sep = ", ", width = 30L)
}

cast_scalar_integer <- function(x, arg = "x") {
  x <- vec_cast(x, integer(), x_arg = arg)
  vec_assert(x, size = 1L)
  x
}

test_before <- function(x, value, inclusive) {
  if (inclusive) {
    x >= value
  } else {
    x > value
  }
}

test_after <- function(x, value, inclusive) {
  if (inclusive) {
    x <= value
  } else {
    x < value
  }
}

# Safer date cast until:
# https://github.com/r-lib/vctrs/issues/549
vec_cast_date <- function(x, x_arg = "x") {
  if (is.character(x)) {
    vec_cast_date_character(x, x_arg)
  } else {
    vec_cast(x, new_date(), x_arg = x_arg)
  }
}

vec_cast_date_character <- function(x, x_arg) {
  to <- new_date()
  out <- vec_cast(x, to, x_arg = x_arg)
  maybe_lossy_cast(out, x, to, lossy = is.na(out) & !is.na(x))
}

# ------------------------------------------------------------------------------

month_normalize <- function(x) {
  if (!is.character(x)) {
    return(x)
  }

  x <- tolower(x)

  where <- month_match(x)

  misses <- is.na(where)

  if (any(misses)) {
    abort("A character `x` must be a month name or abbreviation.")
  }

  out <- month_int()[where]

  out <- unique(out)

  out
}

month_match <- function(x) {
  vec_match(x, month_name())
}

month_name <- function() {
  c(
    tolower(month.name),
    tolower(month.abb)
  )
}

month_int <- function() {
  c(
    1:12,
    1:12
  )
}

month_print <- function() {
  month.name
}

# ------------------------------------------------------------------------------

wday_normalize <- function(x) {
  if (!is.character(x)) {
    return(x)
  }

  x <- tolower(x)

  where <- wday_match(x)

  misses <- is.na(where)

  if (any(misses)) {
    abort("A character `x` must be a weekday name or abbreviation.")
  }

  out <- weekday_int()[where]

  out <- unique(out)

  out
}

wday_match <- function(x) {
  vec_match(x, weekday_name())
}

weekday_name <- function() {
  c(
    c("sunday", "sun"),
    c("monday", "mon"),
    c("tuesday", "tues", "tu", "tue"),
    c("wednesday", "wed"),
    c("thursday", "thurs", "thur", "thu", "th"),
    c("friday", "fri"),
    c("saturday", "sat")
  )
}

weekday_int <- function() {
  c(
    rep(1L, 2L),
    rep(2L, 2L),
    rep(3L, 4L),
    rep(4L, 2L),
    rep(5L, 5L),
    rep(6L, 2L),
    rep(7L, 2L)
  )
}

weekday_print <- function() {
  c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
}

