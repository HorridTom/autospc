# The algorithm's decision history, on three versions of one series:
# - example_series_2a forms one candidate, at row 22. The final run of its
#   calculation period reaches the end of the data, so it is not yet accepted
#   or rejected, and the algorithm stops there.
# - example_series_2a_44 adds a 44th point that ends that run, and the same
#   candidate is accepted.
# - example_series_2c forms three candidates, at rows 22 to 24, each rejected
#   because of an opposing rule break.
#
# keep_candidate_tables is TRUE here because the assertions below read
# candidate$table. It is FALSE by default - see test-history.R.

example_series_2a_44 <- dplyr::bind_rows(
  example_series_2a,
  data.frame(x = 44L, y = 43L)
)

analysed_cp <- function(data = example_series_2a,
                        keep_candidate_tables = TRUE) {
  chart <- autospc_chart(
    chart_type = "C'",
    data = data,
    x = "x",
    y = "y",
    keep_candidate_tables = keep_candidate_tables
  )
  chart <- prepare_data(chart)
  establish_limits(chart)
}


test_that("every candidate the algorithm forms is recorded", {
  candidates <- analysed_cp(example_series_2c)$history$candidates

  expect_length(candidates, 3L)
  expect_identical(
    vapply(candidates, function(k) k$counter, integer(1)),
    c(22L, 23L, 24L)
  )
})


test_that("the record says which way each decision went, and on what", {
  # not accepted because the final run may revert, not because of an opposing
  # break
  undecided <- analysed_cp()$history$candidates

  expect_identical(
    vapply(undecided, function(k) k$accepted, logical(1)),
    FALSE
  )
  expect_identical(undecided[[1]]$final_run_prevents, TRUE)
  expect_identical(undecided[[1]]$opposite_break, FALSE)

  # once the final run has ended, the same candidate is accepted
  accepted <- analysed_cp(example_series_2a_44)$history$candidates

  expect_identical(
    vapply(accepted, function(k) k$accepted, logical(1)),
    TRUE
  )
  expect_identical(accepted[[1]]$counter, 22L)
  expect_identical(accepted[[1]]$final_run_prevents, FALSE)

  # rejected because of an opposing break
  rejected <- analysed_cp(example_series_2c)$history$candidates

  expect_false(rejected[[1]]$accepted)
  expect_identical(rejected[[1]]$opposite_break, TRUE)
})


test_that("a rejected candidate keeps the limits it would have had", {
  rejected <- analysed_cp(example_series_2c)$history$candidates[[1]]

  expect_equal(nrow(rejected$table), nrow(example_series_2c))
  expect_true(all(c("cl", "ucl", "lcl") %in% colnames(rejected$table)))

  # the candidate's own centre line, not the one it was proposing to replace
  expect_false(isTRUE(all.equal(
    rejected$table$cl[rejected$counter],
    rejected$prevailing$cl
  )))
})


test_that("prevailing limits are those in force at the row before", {
  candidates <- analysed_cp(example_series_2c)$history$candidates

  for (candidate in candidates) {
    expect_identical(candidate$prevailing$last_row, candidate$counter - 1L)
  }

  expect_identical(candidates[[1]]$period_rows[1], candidates[[1]]$counter)
})


# counter path, baseline, and the result summaries


test_that("the counter's path is recorded, without non-moves", {
  path <- analysed_cp(example_series_2a_44)$history$counter_path

  expect_s3_class(path, "data.frame")
  expect_identical(colnames(path), c("from", "to", "reason"))
  expect_true(all(path$from != path$to))

  expect_identical(path$reason[1], "first period established")
  expect_identical(path$from[1], 1L)

  expect_identical(path$reason[nrow(path)], "limits re-established")
})


test_that("the first period length is recorded for every chart", {
  expect_identical(analysed_cp()$history$baseline$length, 21L)

  chart <- autospc_chart(
    chart_type = "C\'",
    data = example_series_2a,
    x = "x",
    y = "y",
    baseline_length = 25L
  )
  analysed <- establish_limits(prepare_data(chart))

  expect_identical(analysed$history$baseline$length, 25L)
})


test_that("a baseline longer than the series is the length of the series", {
  thirty <- data.frame(x = 1:30, y = rep(c(10, 14, 11, 16, 12), 6))

  chart <- autospc_chart(
    chart_type = "C\'",
    data = thirty,
    x = "x",
    y = "y",
    baseline_length = 63L
  )
  analysed <- establish_limits(prepare_data(chart))

  expect_identical(analysed$history$baseline$length, 30L)
})


test_that("the result summarises where limits changed and what was excluded", {
  analysed <- analysed_cp(example_series_2a_44)

  expect_identical(analysed$result$re_establish_rows, 22L)

  # row 1 establishes limits rather than re-establishing them, so it is not here
  expect_identical(
    analysed$result$re_establish_rows,
    vapply(
      Filter(
        function(k) k$accepted,
        analysed$history$candidates
      ),
      function(k) k$counter, integer(1)
    )
  )
})


test_that("the result lists the points excluded as extremes", {
  ed <- data.frame(
    x = ed_attendances_monthly$month_start,
    y = ed_attendances_monthly$att_all
  )
  chart <- autospc_chart(chart_type = "C\'", data = ed, x = "x", y = "y")
  analysed <- establish_limits(prepare_data(chart))

  expect_identical(
    analysed$result$exclusions,
    c(1L, 2L, 3L, 33L, 38L, 58L, 59L, 60L, 81L)
  )
  expect_identical(analysed$result$re_establish_rows, c(23L, 46L, 71L))
})


# why the run ended


test_that("the run records why it stopped looking for further periods", {
  analyse <- function(d, ...) {
    chart <- autospc_chart(chart_type = "C\'", data = d, x = "x", y = "y", ...)
    establish_limits(prepare_data(chart))
  }

  expect_identical(
    analyse(example_series_2a)$history$stopped$reason,
    "final run of the candidate period not yet complete"
  )
  expect_identical(
    analyse(example_series_2a_44)$history$stopped$reason,
    "not enough data for a further period"
  )
  expect_identical(
    analyse(example_series_2b)$history$stopped$reason,
    "not enough data for a further period"
  )
  expect_identical(
    analyse(example_series_2c)$history$stopped$reason,
    "too few points after the shift rule break"
  )
  expect_identical(
    analyse(example_series_2a, baseline_only = TRUE)$history$stopped$reason,
    "baseline only"
  )

  # a step change whose new calculation period ends at the last point
  period_ends_at_last_point <- data.frame(
    x = 1:46,
    y = c(
      rep(c(10L, 12L, 11L, 13L, 9L), 5),
      rep(c(30L, 32L, 31L, 33L, 29L), 4),
      33L
    )
  )

  expect_identical(
    analyse(period_ends_at_last_point)$history$stopped$reason,
    "reached the end of the series"
  )

  # a step change followed by a stable stretch leaves no further breaks to find
  stable_after_step <- data.frame(
    x = 1:80,
    y = c(
      rep(c(10L, 12L, 11L, 13L, 9L), 5),
      rep(c(30L, 32L, 31L, 33L, 29L), 11)
    )
  )

  expect_identical(
    analyse(stable_after_step)$history$stopped$reason,
    "no further shift rule breaks"
  )
})


test_that("the stop is recorded at the counter the run reached", {
  chart <- autospc_chart(
    chart_type = "C\'", data = example_series_2c,
    x = "x", y = "y"
  )
  analysed <- establish_limits(prepare_data(chart))

  expect_identical(analysed$history$stopped$counter, 33L)
})


# shift rule breaks the algorithm identified


test_that("every identified break is recorded, with where it was found", {
  chart <- autospc_chart(
    chart_type = "C\'", data = example_series_2c,
    x = "x", y = "y"
  )
  breaks <- establish_limits(prepare_data(chart))$history$breaks

  expect_identical(breaks$counter, c(22L, 23L, 24L, 25L))
  expect_identical(breaks$position, c(22L, 23L, 24L, 33L))

  # the counter was already inside the break for the middle two
  expect_identical(breaks$already_at_break, c(FALSE, TRUE, TRUE, FALSE))
})


test_that("a break is against the prevailing limits, not a candidate's", {
  chart <- autospc_chart(
    chart_type = "C\'", data = example_series_2c,
    x = "x", y = "y",
    keep_candidate_tables = TRUE
  )
  analysed <- establish_limits(prepare_data(chart))
  breaks <- analysed$history$breaks
  rejected <- analysed$history$candidates[[1]]

  # the limits recorded with the break are the ones in force in the result
  expect_equal(breaks$cl, analysed$result$table$cl[breaks$position])

  # and are not the rejected candidate's own, which differ
  expect_equal(breaks$cl[1], rejected$prevailing$cl)
  expect_false(isTRUE(all.equal(
    rejected$table$cl[rejected$counter],
    breaks$cl[1]
  )))
})


test_that("the columns come back in one order whatever route is taken", {
  # A series whose limits are re-established gains run_start at a different
  # point in the analysis from one holding a single calculation period, so the
  # two routes are compared against each other.
  re_established <- suppressWarnings(autospc(ed_attendances_monthly,
    chart_type = "C", x = month_start, y = att_all, plot_chart = FALSE,
    max_exclusions = 3L
  ))

  one_period <- suppressWarnings(autospc(ed_attendances_monthly[1:30, ],
    chart_type = "C", x = month_start, y = att_all, plot_chart = FALSE,
    max_exclusions = 3L
  ))

  expect_identical(names(one_period), names(re_established))
})


# no change of mind


# A prefix of y is its first n points, y[1:n]. Run autospc() on each prefix,
# for n from 2 * period_min to length(y), and return a list with one element
# per prefix: the rows at which limits are re-established when only that
# prefix is analysed.
breaks_by_prefix <- function(y, chart_type, period_min, shift_rule_threshold) {
  lapply(seq(2L * period_min, length(y)), function(n) {
    result <- suppressWarnings(autospc(
      data.frame(x = seq_len(n), y = y[seq_len(n)]),
      x = x,
      y = y,
      chart_type = chart_type,
      period_min = period_min,
      shift_rule_threshold = shift_rule_threshold,
      max_exclusions = 0L,
      plot_chart = FALSE
    ))
    which(result$break_point)
  })
}

# TRUE if every row at which limits are re-established for a prefix is also
# one for the next prefix, which has one more point
no_break_disappears <- function(breaks) {
  all(vapply(
    seq_len(length(breaks) - 1L),
    function(i) all(breaks[[i]] %in% breaks[[i + 1L]]),
    logical(1)
  ))
}


test_that("no break disappears when a point is added to the series", {
  # simulated series in which, before #308 was fixed, a break moved to an
  # earlier row when one more point was added
  x_series <- c(
    0.44, -1.24, -0.22, 0.38, 0.13, 0.8, -0.06, 0.5, 1.09, -0.69, -1.28,
    0.05, -0.24, -0.54, -0.43, -0.65, -0.74, -0.31, -0.47, -1.89, -0.23, -1.74,
    0.29, -0.9, -1.92, -1.56, -1.9, -1.8, -2.3, 0.42, 0.1, -0.96, -0.47
  )
  c_series <- c(
    6L, 6L, 6L, 4L, 7L, 7L, 10L, 7L, 8L, 4L, 4L, 3L, 5L, 2L, 1L, 0L, 2L,
    1L, 0L, 1L, 3L, 1L, 3L, 2L, 3L, 7L, 2L, 1L, 2L, 5L, 5L, 3L, 1L, 2L, 1L,
    5L, 4L, 4L, 3L, 5L, 7L, 4L, 3L, 6L, 4L, 2L, 3L, 4L, 4L, 7L, 4L, 3L, 4L,
    3L, 7L
  )

  expect_true(no_break_disappears(
    breaks_by_prefix(x_series, "X",
      period_min = 8L, shift_rule_threshold = 4L
    )
  ))
  expect_true(no_break_disappears(
    breaks_by_prefix(c_series, "C",
      period_min = 12L, shift_rule_threshold = 6L
    )
  ))
})
