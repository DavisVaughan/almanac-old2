context_env <- new.env(parent = emptyenv())

init_context <- function(x) {
  x_lt <- as_posixlt(x)

  .data <- list(
    date = x,
    year = year(x_lt),
    month = month(x_lt),
    day = day(x_lt),
    yday = yday(x_lt),
    mday = mday(x_lt),
    wday = wday(x_lt, week_start = 7L),
    qday = qday(x_lt),
    size = vec_size(x)
  )

  env_bind(context_env, !!!.data)

  invisible(x)
}

reset_context <- function() {
  .data <- list(
    date = NULL,
    year = NULL,
    month = NULL,
    day = NULL,
    yday = NULL,
    mday = NULL,
    wday = NULL,
    qday = NULL,
    size = NULL
  )

  env_bind(context_env, !!!.data)

  invisible()
}

current_date <- function() {
  context_env[["date"]]
}

current_year <- function() {
  context_env[["year"]]
}

current_month <- function() {
  context_env[["month"]]
}

current_day <- function() {
  context_env[["day"]]
}

current_yday <- function() {
  context_env[["yday"]]
}

current_mday <- function() {
  context_env[["mday"]]
}

current_wday <- function() {
  context_env[["wday"]]
}

current_size <- function() {
  context_env[["size"]]
}
