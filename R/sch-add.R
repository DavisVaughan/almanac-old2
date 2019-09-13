#' Add an event to a schedule
#'
#' `sch_add()` allows you to add an `event` to a schedule, optionally providing
#' a `name` for the event.
#'
#' @param x `[schedule]`
#'
#'    A schedule.
#'
#' @param event `[event]`
#'
#'    An event.
#'
#' @param name `[character(1) / NULL]`
#'
#'    A name for the event. If left as `NULL`, a default name will be created.
#'
#' @examples
#' # Labor Day = First Monday in September
#' on_labor_day <- on_month("Sep") & on_wday("Mon") & on_mweek(1)
#'
#' # Christmas = December 25th
#' on_christmas <- on_month("Dec") & on_mday(25)
#'
#' sch <- schedule()
#' sch <- sch_add(sch, on_labor_day, "Labor Day")
#' sch <- sch_add(sch, on_christmas, "Christmas")
#'
#' sch
#'
#' sch_in("2019-01-01", sch)
#'
#' sch_in("2019-12-25", sch)
#'
#' @export
sch_add <- function(x, event, name = NULL) {
  assert_schedule(x, "`x`")
  assert_event(event)

  if (is.null(name)) {
    name <- paste0("Event ", sch_n_events(x) + 1L)
  } else {
    vec_assert(name, character(), 1L)
  }

  if (vec_in(name, names(x$events))) {
    abort("`name` cannot already exist in the names of the events.")
  }

  events <- c(x$events, list2(!!name := event))
  new_schedule(events)
}
