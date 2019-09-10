#' @export
new_event <- function(description = "An event.",
                      test = NULL,
                      ...,
                      class = character()) {

  if (is.null(test)) {
    test <- function() TRUE
  }

  if (!is.function(test)) {
    abort("`test` must be a function.")
  }

  if (length(fn_fmls(test)) != 0L) {
    abort("`test` must be a function with 0 arguments.")
  }

  if (!is.character(description) || length(description) != 1L) {
    abort("`description` must be a string.")
  }

  .data <- c(list(description = description, test = test), list2(...))

  structure(.Data = .data, class = c(class, "event"))
}

#' @export
print.event <- function(x, ...) {
  cat(format(x, ...))
  invisible(x)
}

#' @export
format.event <- function(x, ...) {
  format_description(x$description)
}

format_description <- function(x) {
  paste0("<", x, ">", "\n", collapse = "")
}

is_event_object <- function(x) {
  inherits(x, "event")
}

assert_event <- function(x, arg = "`event`") {
  if (!is_event_object(x)) {
    glubort("{arg} must be an event")
  }
  invisible(x)
}

# ------------------------------------------------------------------------------

# Uses the "composite" pattern. `composite_event` is an `event` that holds other
# `event`s. The `test` function will use them to compute a composite answer.

#' @export
new_composite_event <- function(description = "A composite event.",
                                test = NULL,
                                events = list(),
                                ...,
                                class = character()) {

  if (!is.list(events)) {
    abort("`events` must be a list.")
  }

  if (!all(map_lgl(events, is_event_object))) {
    abort("All elements of `events` must be `event` objects.")
  }

  new_event(
    description = description,
    test = test,
    events = events,
    ...,
    class = c(class, "composite_event")
  )
}

#' @export
print.composite_event <- function(x, ...) {
  cat(format(x))
  invisible(x)
}

#' @export
format.composite_event <- function(x, ...) {
  out <- format_description(x$description)
  events <- x$events

  for (event in events) {
    out <- paste0(out, " - ", format(event))
  }

  out
}
