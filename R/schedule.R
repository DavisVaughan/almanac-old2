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

#' @export
sched_includes <- function(x, schedule) {
  x <- vec_cast(x, new_date())
  assert_schedule(schedule)

  env <- init_context(x)

  sched_includes_impl(schedule, env)
}

sched_includes_impl <- function(schedule, env) {
  events <- schedule$events
  event <- reduce(events, event_union)
  event_is_impl(event, env)
}

#' @export
sched_generate <- function(from, to, schedule) {
  assert_schedule(schedule)

  from <- vec_cast(from, new_date(), x_arg = "from")
  to <- vec_cast(to, new_date(), x_arg = "to")

  dates <- seq(from, to, by = 1)

  dates[sched_includes(dates, schedule)]
}

#' @export
sched_events <- function(x, schedule) {
  x <- vec_cast(x, new_date())
  assert_schedule(schedule)

  events <- schedule$events

  n_events <- length(events)
  n_x <- vec_size(x)

  env <- init_context(x)

  lst_of_results <- map(events, event_is_impl, env = env)

  out <- vec_init(list(), n_x)
  locs <- vec_init(logical(), n_events)

  # Invert the `lst_of_results` to construct `locs` to slice `events` with
  for (i in seq_len(n_x)) {
    for (j in seq_len(n_events)) {
      locs[[j]] <- lst_of_results[[j]][[i]]
    }
    out[[i]] <- events[locs]
  }

  out
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
