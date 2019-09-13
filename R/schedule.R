#' Create a new schedule
#'
#' `schedule()` creates a new empty schedule. Add events to the schedule with
#' [sch_add()].
#'
#' @export
schedule <- function() {
  new_schedule()
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

sch_n_events <- function(x) {
  length(x$events)
}

# ------------------------------------------------------------------------------

# Should we export this? I don't typically want to encourage going directly
# from an event to a schedule, but this is really useful for the `sch_`
# functions that require a `schedule`.

as_schedule <- function(x, ...) {
  UseMethod("as_schedule")
}

as_schedule.default <- function(x, ...) {
  glubort("Cannot convert object of type {class(x)[1]} to a schedule.")
}

as_schedule.schedule <- function(x, ...) {
  x
}

as_schedule.event <- function(x, name = NULL, ...) {
  if (is.null(name)) {
    name <- as.character(x$description)
  }

  sch_add(schedule(), x, name = name)
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
