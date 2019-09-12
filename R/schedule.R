#' @export
schedule <- function() {
  new_schedule()
}

#' @export
add_event <- function(x, event, name) {
  assert_schedule(x, "`x`")
  assert_event(event)
  vec_assert(name, character(), 1L)

  if (vec_in(name, names(x$events))) {
    abort("`name` cannot already exist in the names of the events.")
  }

  events <- c(x$events, list2(!!name := event))
  new_schedule(events)
}

#' @export
print.schedule <- function(x, ...) {
  n_events <- length(x$events)

  cat_line(glue("<Schedule [{n_events} event(s)]>"))

  if (n_events == 0L) {
    return(invisible(x))
  }

  names <- names(x$events)
  names <- paste0(paste0(" - ", names), collapse = "\n")

  cat(names)

  invisible(x)
}

# ------------------------------------------------------------------------------

new_schedule <- function(events = list(), ..., class = character()) {
  if (!is.list(events)) {
    abort("`events` must be a list.")
  }

  if (!all(map_lgl(events, is_event_object))) {
    abort("All elements of `events` must be `event` objects.")
  }

  if (length(events) > 0L && !is_named(events)) {
    abort("All elements of `events` must be named.")
  }

  .data <- c(list(events = events), list2(...))

  structure(.data, class = c(class, "schedule"))
}

# ------------------------------------------------------------------------------

# Should we export this? I don't typically want to encourage going directly
# from an event to a schedule, but this is really useful for the `sch_`
# functions that require a `schedule`.

#' @export
as_schedule <- function(x, ...) {
  UseMethod("as_schedule")
}

#' @export
as_schedule.default <- function(x, ...) {
  glubort("Cannot convert object of type {class(x)[1]} to a schedule.")
}

#' @export
as_schedule.schedule <- function(x, ...) {
  x
}

#' @export
as_schedule.event <- function(x, name = NULL, ...) {
  if (is.null(name)) {
    name <- as.character(x$description)
  }

  add_event(schedule(), x, name = name)
}

# ------------------------------------------------------------------------------

is_schedule <- function(x) {
  inherits(x, "schedule")
}

assert_schedule <- function(x, arg = "`schedule`") {
  if (!is_schedule(x)) {
    glubort("{arg} must be a `schedule`.")
  }
  invisible(x)
}

# ------------------------------------------------------------------------------

cat_line <- function(...) {
  cat(paste0(..., "\n", collapse = ""))
}
