#' @export
on_semester <- function(x) {
  x <- vec_cast(x, integer())

  if (any(!vec_in(x, 1:2))) {
    abort("`x` must be a valid semester, in `1:2`.")
  }

  test <- function() {
    vec_in(current_semester(), x)
  }

  new_event(
    description = glue("On semester: {collapse_and_trim(x)}"),
    test = test
  )
}
