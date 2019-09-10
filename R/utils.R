glubort <- function (..., .sep = "", .envir = parent.frame()) {
  abort(glue(..., .sep = .sep, .envir = .envir))
}

as_posixlt <- function(x) {
  as.POSIXlt(x, tz = tz(x))
}

collapse_and_trim <- function(x) {
  glue::glue_collapse(x, sep = ", ", width = 30L)
}
