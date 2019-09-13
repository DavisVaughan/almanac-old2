# TODO - Maybe a second function that only returns the _first_ event found, and
# returns a character vector of event names? If no event occured on that date
# it would return `NA_character_`.

#' What events does `x` fall on?
#'
#' `sch_retrieve()` returns the events that `x` falls on in the `schedule`.
#'
#' @param x `[Date]`
#'
#'    A vector of dates.
#'
#' @param schedule `[schedule]`
#'
#'    A schedule.
#'
#' @examples
#' on_25th <- on_mday(25)
#' on_dec <- on_month("Dec")
#'
#' sch <- schedule()
#' sch <- sch_add(sch, on_25th, "twenty_fifth")
#' sch <- sch_add(sch, on_dec, "december")
#'
#' sch_retrieve(c("2019-01-01", "2019-01-25", "2019-12-25"), sch)
#'
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
