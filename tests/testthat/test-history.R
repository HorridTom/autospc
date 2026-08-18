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
