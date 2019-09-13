# ------------------------------------------------------------------------------
# new_event()

test_that("can create a new event", {
  expect_is(new_event(), "event")
})

test_that("bare events always return TRUE", {
  expect_true(event_in("2019-01-01", new_event()))
})

test_that("`new_event()` subclasses can be created", {
  expect_is(new_event(class = "sub_event"), "sub_event")
})

test_that("`new_event()` input is validated", {
  expect_error(new_event(description = 1), "must be a string")
  expect_error(new_event(description = c("x", "y")), "must be a string")

  expect_error(new_event(test = 1), "must be a function")
  expect_error(new_event(test = function(x, y) 1), "must be a function with 1 argument")
})

# ------------------------------------------------------------------------------
# new_event() printing

# TODO

# ------------------------------------------------------------------------------
# new_composite_event()

test_that("can create a new composite event", {
  expect_is(new_composite_event(), "event")
  expect_is(new_composite_event(), "composite_event")
})

test_that("bare composite events always return TRUE", {
  expect_true(event_in("2019-01-01", new_composite_event()))
})

test_that("`new_composite_event()` input is validated", {
  expect_error(new_composite_event(events = 1), "must be a list")
  expect_error(new_composite_event(events = list(1)), "must be `event` objects")
})

# ------------------------------------------------------------------------------
# new_composite_event() printing

# TODO
