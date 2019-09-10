context_env <- new.env(parent = emptyenv())

init_context <- function(x) {
  x_lt <- as_posixlt(x)

  .data <- list(
    date = expr(x),
    year = expr(year(x_lt)),
    month = expr(month(x_lt)),
    day = expr(day(x_lt)),
    yday = expr(yday(x_lt)),
    mday = expr(mday(x_lt)),
    wday = expr(wday(x_lt, week_start = 7L)),
    qday = expr(qday(x_lt)),
    size = expr(vec_size(x))
  )

  env_bind_lazy(context_env, !!!.data)

  invisible(x)
}

reset_context <- function() {
  # Avoid `env_bind()` triggering the lazy bindings
  context_env$date <- NULL
  context_env$year <- NULL
  context_env$month <- NULL
  context_env$day <- NULL
  context_env$yday <- NULL
  context_env$mday <- NULL
  context_env$wday <- NULL
  context_env$qday <- NULL
  context_env$size <- NULL

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
