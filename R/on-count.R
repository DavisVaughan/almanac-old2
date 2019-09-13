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
