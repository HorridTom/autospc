# Helpers that write chart$history. Their use through a real run is covered in
# test-algorithm.R.

new_chart <- function(...) {
  autospc_chart(chart_type = "C", data = test_data, x = "x", y = "y", ...)
}


test_that("a counter move is appended to the path", {

  chart <- record_counter_move(new_chart(), 1L, 22L, "first period established")
  chart <- record_counter_move(chart, 22L, 23L, "candidate rejected")

  expect_identical(nrow(chart$history$counter_path), 2L)
  expect_identical(chart$history$counter_path$from, c(1L, 22L))
  expect_identical(chart$history$counter_path$to, c(22L, 23L))

})


test_that("a move to where the counter already is is not recorded", {

  chart <- record_counter_move(new_chart(), 22L, 22L, "moved to shift rule break")

  expect_null(chart$history$counter_path)

})


test_that("the stop reason replaces rather than accumulates", {

  chart <- record_stop(new_chart(), 44, "reached the end of the series")

  expect_identical(chart$history$stopped$counter, 44L)
  expect_identical(chart$history$stopped$reason, "reached the end of the series")

})


test_that("a break records the limits it was identified against", {

  # aboveOrBelowCl is 1 above the centre line, -1 below
  limits <- data.frame(cl = rep(10, 5),
                       ucl = rep(20, 5),
                       lcl = rep(0, 5),
                       aboveOrBelowCl = c(1L, 1L, -1L, -1L, 1L))

  chart <- record_break(new_chart(), counter = 2L, position = 3L,
                        already_at_break = FALSE, limits_table = limits)

  expect_identical(chart$history$breaks$position, 3L)
  expect_identical(chart$history$breaks$direction, -1L)
  expect_identical(chart$history$breaks$cl, 10)
  expect_identical(chart$history$breaks$already_at_break, FALSE)

})


test_that("a break with no position records nothing to be against", {

  limits <- data.frame(cl = 10, ucl = 20, lcl = 0, aboveOrBelowCl = 1L)

  chart <- record_break(new_chart(), counter = 2L, position = NA,
                        already_at_break = FALSE, limits_table = limits)

  expect_identical(chart$history$breaks$position, NA_integer_)
  expect_identical(chart$history$breaks$direction, NA_integer_)
  expect_identical(chart$history$breaks$cl, NA_real_)

})
