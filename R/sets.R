#' @export
event_intersect <- function(x, y) {
  assert_event(x, arg = "`x`")
  assert_event(y, arg = "`y`")

  events <- combine_events(x, y, class = "intersection_event")

  test <- function() {
    results <- map(events, event_is_impl)
    reduce(results, `&`)
  }

  new_composite_event(
    description = "Intersection",
    test = test,
    events = events,
    class = "intersection_event"
  )
}

#' @export
`%i%` <- function(x, y) {
  event_intersect(x, y)
}

#' @export
event_union <- function(x, y) {
  assert_event(x, arg = "`x`")
  assert_event(y, arg = "`y`")

  events <- combine_events(x, y, class = "union_event")

  test <- function() {
    results <- map(events, event_is_impl)
    reduce(results, `|`)
  }

  new_composite_event(
    description = "Union",
    test = test,
    events = events,
    class = "union_event"
  )
}

#' @export
event_diff <- function(x, y) {
  assert_event(x, arg = "`x`")
  assert_event(y, arg = "`y`")

  test <- function() {
    event_is_impl(x) & !event_is_impl(y)
  }

  new_composite_event(
    description = "Difference",
    test = test,
    events = events,
    class = "difference_event"
  )
}

#' @export
`%d%` <- function(x, y) {
  event_diff(x, y)
}

# Helps to flatten repeated combinations of intersection events
# in_year(2019) %i% in_year(2020) %i% in_year(2021)
combine_events <- function(x, y, class) {
  if (inherits(x, class)) {
    if (inherits(y, class)) {
      events <- c(x$events, y$events)
    } else {
      events <- c(x$events, list(y))
    }
  } else {
    if (inherits(y, class)) {
      events <- c(list(x), y$events)
    } else {
      events <- list(x, y)
    }
  }

  events
}
