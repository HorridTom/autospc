# Helpers that write chart$history. Their use through a real run is covered in
# test-establish_limits.R.

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

  # above_or_below_cl is 1 above the centre line, -1 below
  limits <- data.frame(cl = rep(10, 5),
                       ucl = rep(20, 5),
                       lcl = rep(0, 5),
                       above_or_below_cl = c(1L, 1L, -1L, -1L, 1L))

  chart <- record_break(new_chart(), counter = 2L, position = 3L,
                        already_at_break = FALSE, limits_table = limits)

  expect_identical(chart$history$breaks$position, 3L)
  expect_identical(chart$history$breaks$direction, -1L)
  expect_identical(chart$history$breaks$cl, 10)
  expect_identical(chart$history$breaks$already_at_break, FALSE)

})


test_that("a break with no position records nothing to be against", {

  limits <- data.frame(cl = 10, ucl = 20, lcl = 0, above_or_below_cl = 1L)

  chart <- record_break(new_chart(), counter = 2L, position = NA,
                        already_at_break = FALSE, limits_table = limits)

  expect_identical(chart$history$breaks$position, NA_integer_)
  expect_identical(chart$history$breaks$direction, NA_integer_)
  expect_identical(chart$history$breaks$cl, NA_real_)

})


# keep_candidate_tables


# example_series_2a produces two candidates, one rejected and one accepted
candidates_of <- function(...) {

  chart <- autospc_chart(chart_type = "C'",
                         data = example_series_2a,
                         x = "x",
                         y = "y",
                         ...)

  establish_limits(prepare_data(chart))$history$candidates

}


test_that("a chart does not keep the candidate tables unless asked", {

  candidates <- candidates_of()

  expect_length(candidates, 2L)

  expect_true(all(vapply(candidates,
                         function(candidate) is.null(candidate$table),
                         logical(1))))

})


test_that("keep_candidate_tables = TRUE keeps a full table per candidate", {

  candidates <- candidates_of(keep_candidate_tables = TRUE)

  expect_true(all(vapply(candidates,
                         function(candidate) {
                           nrow(candidate$table) == nrow(example_series_2a)
                         },
                         logical(1))))

})


test_that("a candidate has the same element names either way", {

  # the table element is present and NULL rather than being absent, so that the
  # names of a candidate do not depend on the argument
  expect_identical(names(candidates_of()[[1]]),
                   names(candidates_of(keep_candidate_tables = TRUE)[[1]]))

})


test_that("everything else the candidates record is unaffected", {

  without <- candidates_of()
  with    <- candidates_of(keep_candidate_tables = TRUE)

  without_tables <- lapply(with,
                           function(candidate) {
                             candidate$table <- NULL
                             candidate
                           })

  expect_equal(lapply(without,
                      function(candidate) {
                        candidate$table <- NULL
                        candidate
                      }),
               without_tables)

})


test_that("the chart records whether it was asked to keep them", {

  expect_false(new_chart()$keep_candidate_tables)

  expect_true(new_chart(keep_candidate_tables = TRUE)$keep_candidate_tables)

})


test_that("autospc passes the argument down to the chart", {

  plot <- suppressWarnings(
    autospc(example_series_2a, chart_type = "C'", x = x, y = y,
            keep_candidate_tables = TRUE)
  )

  candidates <- autospc_plot_charts(plot)[[1]]$history$candidates

  expect_false(any(vapply(candidates,
                          function(candidate) is.null(candidate$table),
                          logical(1))))

})


test_that("the log text is the same whether the tables are kept or not", {

  log_of <- function(...) {
    result <- suppressWarnings(
      autospc(example_series_2a, chart_type = "C'", x = x, y = y,
              plot_chart = FALSE, ...)
    )
    result$log
  }

  expect_identical(log_of(), log_of(keep_candidate_tables = TRUE))

})
