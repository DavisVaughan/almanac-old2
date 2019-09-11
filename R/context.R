init_context <- function(x) {
  x_lt <- as_posixlt(x)

  data <- list(
    date = expr(x),

    year = expr(year(x_lt)),
    isoyear = expr(isoyear(x_lt)),
    epiyear = expr(epiyear(x_lt)),

    semester = expr(semester(x_lt)),

    quarter = expr(quarter(x_lt)),

    ymonth = expr(ymonth(x_lt)),
    qmonth = expr(qmonth(x_lt)),

    yweek = expr(yweek(x_lt)),
    qweek = expr(qweek(x_lt)),
    mweek = expr(mweek(x_lt)),

    yweek_from_end = expr(yweek_from_end(x_lt)),
    qweek_from_end = expr(qweek_from_end(x_lt)),

    yday = expr(yday(x_lt)),
    mday = expr(mday(x_lt)),
    wday = expr(wday(x_lt, week_start = 7L)),
    qday = expr(qday(x_lt)),

    yday_from_end = expr(yday_from_end(x_lt)),
    qday_from_end = expr(qday_from_end(x_lt)),

    days_in_year = expr(days_in_year(x_lt)),
    days_in_quarter = expr(days_in_quarter(x_lt)),
    days_in_month = expr(days_in_month(x_lt)),

    size = expr(vec_size(x))
  )

  env <- new.env(parent = emptyenv())

  env_bind_lazy(env, !!!data)

  env
}

current_date <- function(env) {
  env[["date"]]
}


current_year <- function(env) {
  env[["year"]]
}

current_isoyear <- function(env) {
  env[["isoyear"]]
}

current_epiyear <- function(env) {
  env[["epiyear"]]
}


current_semester <- function(env) {
  env[["semester"]]
}


current_quarter <- function(env) {
  env[["quarter"]]
}


current_ymonth <- function(env) {
  env[["ymonth"]]
}

current_qmonth <- function(env) {
  env[["qmonth"]]
}


current_yweek <- function(env) {
  env[["yweek"]]
}

current_qweek <- function(env) {
  env[["qweek"]]
}

current_mweek <- function(env) {
  env[["mweek"]]
}


current_yweek_from_end <- function(env) {
  env[["yweek_from_end"]]
}

current_qweek_from_end <- function(env) {
  env[["qweek_from_end"]]
}


current_yday <- function(env) {
  env[["yday"]]
}

current_mday <- function(env) {
  env[["mday"]]
}

current_wday <- function(env) {
  env[["wday"]]
}

current_qday <- function(env) {
  env[["qday"]]
}


current_yday_from_end <- function(env) {
  env[["yday_from_end"]]
}

current_qday_from_end <- function(env) {
  env[["qday_from_end"]]
}


current_days_in_year <- function(env) {
  env[["days_in_year"]]
}

current_days_in_quarter <- function(env) {
  env[["days_in_quarter"]]
}

current_days_in_month <- function(env) {
  env[["days_in_month"]]
}


current_size <- function(env) {
  env[["size"]]
}
