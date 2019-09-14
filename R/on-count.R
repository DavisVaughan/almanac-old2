# Solves - The twelfth occurance of the event in the quarter
# on_count(on_wday(c("Tues", "Thurs"), 12, "quarter"))

# TODO - Maybe allow within to pass through unchanged to floor_date, as it
# can also take "2 months". So the above example could be within "2 quarters"

on_count <- function(event, n, within = "month") {
  assert_event(event)

  n <- cast_scalar_integer(n, "n")

  arg_match(within, c("day", "week", "month", "quarter", "year"))

  test <- function(env) {
    x <- current_date(env)

    split <- vctrs::vec_split_id(floor_date(x, within))
    ids <- split$id

    period_start <- split$key
    period_end <- unique(ceiling_date(x, within)) - days(1L)

    n_unique <- length(ids)

    out <- vctrs::vec_init(logical(), vec_size(x))

    for (i in seq_len(n_unique)) {
      id <- ids[[i]]
      range <- seq(period_start[i], period_end[i], "day")
      result <- event_in(range, event)
      result <- which(result)

      if (length(result) < n) {
        out[id] <- FALSE
        next
      }

      loc <- result[[n]]

      out[id] <- x[id] == range[loc]
    }

    out
  }

  new_event(
    description = "On count",
    test = test
  )
}

# Solves the problem of "5 occurrences since `since`"
# For example: Jan 24th, for 5 occurrences since 2010-01-01.
# Days before 2010-01-01 are automatically considered non-events.

with_limit <- function(event, n, since) {

  test <- function(env) {
    x <- current_date(env)

    until <- max(x)

    if (since > until) {
      return(rep(FALSE, vec_size(x)))
    }

    range <- seq(since, until, by = "1 day")

    range_loc <- event_in(range, event)

    if (length(range_loc) == 0L) {
      return(rep(FALSE, vec_size(x)))
    }

    range_pos <- which(range_loc)
    n_events_since <- length(range_pos)

    n <- min(n_events_since, n)

    range_pos_clamped <- range_pos[seq2(1L, n)]

    event_dates <- range[range_pos_clamped]

    out <- vec_init(logical(), n = vec_size(x))

    before_event <- x < since
    after_event <- !before_event

    out[before_event] <- FALSE
    out[after_event] <- vec_in(x[after_event], event_dates)

    out
  }

  new_composite_event(
    description = glue("With occurrence limit: {n}"),
    test = test,
    events = list(event)
  )
}
