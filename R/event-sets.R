#' @export
event_intersect <- function(x, y) {
  assert_event(x, arg = "`x`")
  assert_event(y, arg = "`y`")

  events <- combine_events(x, y, class = "intersection_event")

  test <- function(env) {
    results <- map(events, event_is_impl, env = env)
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
event_union <- function(x, y) {
  assert_event(x, arg = "`x`")
  assert_event(y, arg = "`y`")

  events <- combine_events(x, y, class = "union_event")

  test <- function(env) {
    results <- map(events, event_is_impl, env = env)
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

  test <- function(env) {
    event_is_impl(x, env) & !event_is_impl(y, env)
  }

  new_composite_event(
    description = "Difference",
    test = test,
    events = list(x, y),
    class = "difference_event"
  )
}

#' @export
event_invert <- function(x) {
  assert_event(x, arg = "`x`")

  test <- function(env) {
    !event_is_impl(x, env)
  }

  new_composite_event(
    description = "Inverted",
    test = test,
    events = list(x),
    class = "inverted_event"
  )
}

# Helps to flatten repeated combinations of intersection events
# in_year(2019) & in_year(2020) & in_year(2021)
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

# ------------------------------------------------------------------------------

#' @export
`&.event` <- function(e1, e2) {
  vec_arith("&", e1, e2)
}

#' @export
`|.event` <- function(e1, e2) {
  vec_arith("|", e1, e2)
}

#' @export
`-.event` <- function(e1, e2) {
  if (missing(e2)) {
    vec_arith("-", e1, vctrs::MISSING())
  } else {
    vec_arith("-", e1, e2)
  }
}

#' @export
`!.event` <- function(x) {
  vec_arith("!", x, vctrs::MISSING())
}

# These result in errors

#' @export
`+.event` <- function(e1, e2) {
  if (missing(e2)) {
    vec_arith("+", e1, vctrs::MISSING())
  } else {
    vec_arith("+", e1, e2)
  }
}

#' @export
`/.event` <- function(e1, e2) {
  vec_arith("/", e1, e2)
}

#' @export
`^.event` <- function(e1, e2) {
  vec_arith("^", e1, e2)
}

#' @export
`%%.event` <- function(e1, e2) {
  vec_arith("%%", e1, e2)
}

#' @export
`%/%.event` <- function(e1, e2) {
  vec_arith("%/%", e1, e2)
}

# ------------------------------------------------------------------------------

#' vctrs compatibility functions
#'
#' These functions are the extensions that allow event objects to
#' work with vctrs.
#'
#' @param x,y Objects.
#' @param op An arithmetic operator as a string.
#' @param ... Used to pass along error message information.
#'
#' @return
#'
#' See the corresponding vctrs function for the exact return value.
#'
#' @name vctrs-compat
#'
NULL

#' @rdname vctrs-compat
#' @rdname vec_arith
#' @export vec_arith.event
#' @method vec_arith event
#' @export
vec_arith.event <- function(op, x, y, ...) {
  UseMethod("vec_arith.event", y)
}

#' @method vec_arith.event default
#' @export
vec_arith.event.default <- function(op, x, y, ...) {
  vctrs::stop_incompatible_op(op, x, y)
}

#' @method vec_arith.event event
#' @export
vec_arith.event.event <- function(op, x, y, ...) {
  switch(op,
    `&` = event_intersect(x, y),
    `|` = event_union(x, y),
    `-` = event_diff(x, y),
    vctrs::stop_incompatible_op(op, x, y)
  )
}

#' @method vec_arith.event MISSING
#' @export
vec_arith.event.MISSING <- function(op, x, y, ...) {
  switch(op,
    `!` = event_invert(x),
    vctrs::stop_incompatible_op(op, x, y)
  )
}


