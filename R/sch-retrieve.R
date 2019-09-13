#' @export
sch_retrieve <- function(x, schedule) {
  x <- vec_cast_date(x)
  assert_schedule(schedule)

  events <- schedule$events

  n_events <- length(events)
  n_x <- vec_size(x)

  env <- init_context(x)

  lst_of_results <- map(events, event_in_impl, env = env)

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
