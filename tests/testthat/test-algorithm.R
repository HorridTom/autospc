# The algorithm's decision history. example_series_2a forms two candidates: the
# first is rejected under no_regrets, the second accepted.

fitted_2a <- function() {
  chart <- autospc_chart(chart_type = "C'",
                         data = example_series_2a,
                         x = "x",
                         y = "y")
  chart <- prepare_data(chart)
  run_limit_algorithm(chart)
}


test_that("every candidate the algorithm forms is recorded", {

  candidates <- fitted_2a()$history$candidates

  expect_length(candidates, 2L)
  expect_identical(vapply(candidates, function(k) k$counter, integer(1)),
                   c(22L, 23L))

})


test_that("the record says which way each decision went, and on what", {

  candidates <- fitted_2a()$history$candidates

  expect_identical(vapply(candidates, function(k) k$accepted, logical(1)),
                   c(FALSE, TRUE))

  # the first is rejected because the final run may revert, not because of an
  # opposing break
  expect_identical(candidates[[1]]$final_run_prevents, TRUE)
  expect_identical(candidates[[1]]$opposite_break, FALSE)

})


test_that("a rejected candidate keeps the limits it would have had", {

  rejected <- fitted_2a()$history$candidates[[1]]

  expect_equal(nrow(rejected$table), nrow(example_series_2a))
  expect_true(all(c("cl", "ucl", "lcl") %in% colnames(rejected$table)))

  # the candidate's own centre line, not the one it was proposing to replace
  expect_false(isTRUE(all.equal(rejected$table$cl[rejected$counter],
                                rejected$prevailing$cl)))

})


test_that("prevailing limits are those in force at the row before", {

  candidates <- fitted_2a()$history$candidates

  for (candidate in candidates) {
    expect_identical(candidate$prevailing$last_row, candidate$counter - 1L)
  }

  expect_identical(candidates[[1]]$period_rows[1], candidates[[1]]$counter)

})


# counter path, baseline, and the result summaries


test_that("the counter's path is recorded, without non-moves", {

  path <- fitted_2a()$history$counter_path

  expect_s3_class(path, "data.frame")
  expect_identical(colnames(path), c("from", "to", "reason"))
  expect_true(all(path$from != path$to))

  expect_identical(path$reason[1], "first period established")
  expect_identical(path$from[1], 1L)

  expect_identical(path$reason[nrow(path)], "limits re-established")

})


test_that("baseline extent is recorded only when baseline_length is set", {

  expect_null(fitted_2a()$history$baseline)

  chart <- autospc_chart(chart_type = "C\'",
                         data = example_series_2a,
                         x = "x",
                         y = "y",
                         baseline_length = 25L)
  fitted <- run_limit_algorithm(prepare_data(chart))

  expect_identical(fitted$history$baseline$length, 25L)
  expect_identical(fitted$history$baseline$rows, 1:25)

})


test_that("the result summarises where limits changed and what was excluded", {

  fitted <- fitted_2a()

  # row 1 establishes limits rather than re-establishing them, so it is not here
  expect_identical(fitted$result$re_establish_rows,
                   vapply(Filter(function(k) k$accepted,
                                 fitted$history$candidates),
                          function(k) k$counter, integer(1)))

})


test_that("the result lists the points excluded as extremes", {

  ed <- data.frame(x = ed_attendances_monthly$month_start,
                   y = ed_attendances_monthly$att_all)
  chart <- autospc_chart(chart_type = "C\'", data = ed, x = "x", y = "y")
  fitted <- run_limit_algorithm(prepare_data(chart))

  expect_identical(fitted$result$exclusions,
                   c(1L, 2L, 3L, 33L, 38L, 58L, 59L, 60L, 81L))
  expect_identical(fitted$result$re_establish_rows, c(23L, 46L, 71L))

})
