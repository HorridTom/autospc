# The algorithm's decision history. example_series_2a forms two candidates: the
# first is rejected under no_regrets, the second accepted.
#
# keep_candidate_tables is TRUE here because the assertions below read
# candidate$table. It is FALSE by default - see test-history.R.

analysed_2a <- function(keep_candidate_tables = TRUE) {
  chart <- autospc_chart(chart_type = "C'",
                         data = example_series_2a,
                         x = "x",
                         y = "y",
                         keep_candidate_tables = keep_candidate_tables)
  chart <- prepare_data(chart)
  run_limit_algorithm(chart)
}


test_that("every candidate the algorithm forms is recorded", {

  candidates <- analysed_2a()$history$candidates

  expect_length(candidates, 2L)
  expect_identical(vapply(candidates, function(k) k$counter, integer(1)),
                   c(22L, 23L))

})


test_that("the record says which way each decision went, and on what", {

  candidates <- analysed_2a()$history$candidates

  expect_identical(vapply(candidates, function(k) k$accepted, logical(1)),
                   c(FALSE, TRUE))

  # the first is rejected because the final run may revert, not because of an
  # opposing break
  expect_identical(candidates[[1]]$final_run_prevents, TRUE)
  expect_identical(candidates[[1]]$opposite_break, FALSE)

})


test_that("a rejected candidate keeps the limits it would have had", {

  rejected <- analysed_2a()$history$candidates[[1]]

  expect_equal(nrow(rejected$table), nrow(example_series_2a))
  expect_true(all(c("cl", "ucl", "lcl") %in% colnames(rejected$table)))

  # the candidate's own centre line, not the one it was proposing to replace
  expect_false(isTRUE(all.equal(rejected$table$cl[rejected$counter],
                                rejected$prevailing$cl)))

})


test_that("prevailing limits are those in force at the row before", {

  candidates <- analysed_2a()$history$candidates

  for (candidate in candidates) {
    expect_identical(candidate$prevailing$last_row, candidate$counter - 1L)
  }

  expect_identical(candidates[[1]]$period_rows[1], candidates[[1]]$counter)

})


# counter path, baseline, and the result summaries


test_that("the counter's path is recorded, without non-moves", {

  path <- analysed_2a()$history$counter_path

  expect_s3_class(path, "data.frame")
  expect_identical(colnames(path), c("from", "to", "reason"))
  expect_true(all(path$from != path$to))

  expect_identical(path$reason[1], "first period established")
  expect_identical(path$from[1], 1L)

  expect_identical(path$reason[nrow(path)], "limits re-established")

})


test_that("baseline extent is recorded only when baseline_length is set", {

  expect_null(analysed_2a()$history$baseline)

  chart <- autospc_chart(chart_type = "C\'",
                         data = example_series_2a,
                         x = "x",
                         y = "y",
                         baseline_length = 25L)
  analysed <- run_limit_algorithm(prepare_data(chart))

  expect_identical(analysed$history$baseline$length, 25L)
  expect_identical(analysed$history$baseline$rows, 1:25)

})


test_that("the result summarises where limits changed and what was excluded", {

  analysed <- analysed_2a()

  # row 1 establishes limits rather than re-establishing them, so it is not here
  expect_identical(analysed$result$re_establish_rows,
                   vapply(Filter(function(k) k$accepted,
                                 analysed$history$candidates),
                          function(k) k$counter, integer(1)))

})


test_that("the result lists the points excluded as extremes", {

  ed <- data.frame(x = ed_attendances_monthly$month_start,
                   y = ed_attendances_monthly$att_all)
  chart <- autospc_chart(chart_type = "C\'", data = ed, x = "x", y = "y")
  analysed <- run_limit_algorithm(prepare_data(chart))

  expect_identical(analysed$result$exclusions,
                   c(1L, 2L, 3L, 33L, 38L, 58L, 59L, 60L, 81L))
  expect_identical(analysed$result$re_establish_rows, c(23L, 46L, 71L))

})


# why the run ended


test_that("the run records why it stopped looking for further periods", {

  analyse <- function(d, ...) {
    chart <- autospc_chart(chart_type = "C\'", data = d, x = "x", y = "y", ...)
    run_limit_algorithm(prepare_data(chart))
  }

  expect_identical(analyse(example_series_2a)$history$stopped$reason,
                   "reached the end of the series")
  expect_identical(analyse(example_series_2b)$history$stopped$reason,
                   "not enough data for a further period")
  expect_identical(analyse(example_series_2c)$history$stopped$reason,
                   "too few points after the shift rule break")
  expect_identical(
    analyse(example_series_2a, baseline_only = TRUE)$history$stopped$reason,
    "baseline only")

  # a step change followed by a stable stretch leaves no further breaks to find
  stable_after_step <- data.frame(
    x = 1:80,
    y = c(rep(c(10L, 12L, 11L, 13L, 9L), 5),
          rep(c(30L, 32L, 31L, 33L, 29L), 11)))

  expect_identical(analyse(stable_after_step)$history$stopped$reason,
                   "no further shift rule breaks")

})


test_that("the stop is recorded at the counter the run reached", {

  chart <- autospc_chart(chart_type = "C\'", data = example_series_2c,
                         x = "x", y = "y")
  analysed <- run_limit_algorithm(prepare_data(chart))

  expect_identical(analysed$history$stopped$counter, 33L)

})


# shift rule breaks the algorithm identified


test_that("every identified break is recorded, with where it was found", {

  chart <- autospc_chart(chart_type = "C\'", data = example_series_2c,
                         x = "x", y = "y")
  breaks <- run_limit_algorithm(prepare_data(chart))$history$breaks

  expect_identical(breaks$counter, c(22L, 23L, 24L, 25L))
  expect_identical(breaks$position, c(22L, 23L, 24L, 33L))

  # the counter was already inside the break for the middle two
  expect_identical(breaks$already_at_break, c(FALSE, TRUE, TRUE, FALSE))

})


test_that("a break is against the prevailing limits, not a candidate's", {

  chart <- autospc_chart(chart_type = "C\'", data = example_series_2c,
                         x = "x", y = "y",
                         keep_candidate_tables = TRUE)
  analysed <- run_limit_algorithm(prepare_data(chart))
  breaks <- analysed$history$breaks
  rejected <- analysed$history$candidates[[1]]

  # the limits recorded with the break are the ones in force in the result
  expect_equal(breaks$cl, analysed$result$table$cl[breaks$position])

  # and are not the rejected candidate's own, which differ
  expect_equal(breaks$cl[1], rejected$prevailing$cl)
  expect_false(isTRUE(all.equal(rejected$table$cl[rejected$counter],
                                breaks$cl[1])))

})
