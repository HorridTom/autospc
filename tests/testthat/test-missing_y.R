# The algorithm is applied to only non-missing data (observations): a point with
# no y is not there as far as the analysis is concerned, and is put back for
# plotting with the limits carried across it.

gapped <- function(missing_at, rows = 46L) {
  d <- data.frame(
    x = seq_len(rows),
    y = as.numeric(rep(c(10, 14, 11, 16, 12, 13), length.out = rows))
  )
  d$y[missing_at] <- NA

  return(d)
}

analyse <- function(d, ...) {
  return(autospc(d,
    chart_type = "C\'",
    x = "x",
    y = "y",
    plot_chart = FALSE,
    period_min = 21L,
    ...
  ))
}


# compacting


test_that("a calculation period holds period_min observations, not rows", {
  result <- analyse(gapped(c(5L, 6L, 7L)))

  calculation <- which(result$period_type == "calculation")

  expect_identical(sum(!is.na(result$y[calculation])), 21L)
})


test_that("the analysis is the same as if the missing rows were not supplied", {
  with_gap <- analyse(gapped(c(5L, 6L, 7L)))
  without <- analyse(gapped(integer(0))[-(5:7), ])

  # the same points, analysed the same way; the row numbers differ
  expect_identical(
    with_gap$cl[!is.na(with_gap$y)],
    without$cl
  )
  expect_identical(
    with_gap$rule2[!is.na(with_gap$y)],
    without$rule2
  )
})


# where the limits are drawn


test_that("limits carry across a gap between two observations", {
  result <- analyse(gapped(c(23L, 24L)))

  expect_false(anyNA(result$cl))
  expect_false(anyNA(result$ucl))
  expect_false(anyNA(result$lcl))
})


test_that("limits are not drawn before the first observation", {
  result <- analyse(gapped(1:3))

  expect_true(all(is.na(result$cl[1:3])))
  expect_true(all(is.na(result$ucl[1:3])))
  expect_false(anyNA(result$cl[4:nrow(result)]))
})


test_that("limits are not drawn after the last observation", {
  result <- analyse(gapped(44:46))

  expect_true(all(is.na(result$cl[44:46])))
  expect_true(all(is.na(result$ucl[44:46])))
  expect_false(anyNA(result$cl[1:43]))
})


test_that("the limits carried across a gap are the period's own", {
  result <- analyse(gapped(23L))

  expect_identical(result$cl[23], result$cl[22])
  expect_identical(result$ucl[23], result$ucl[22])
})


# the denominator of a row that holds no observation


# every observation has a denominator of 100, so all the observations share the
# same limits and only the rows with no observation can differ. With no missing
# y before row 22, the calculation period is rows 1 to 21 and the display period
# runs from row 22
proportion_data_with_gaps <- function(gap_n, at = 10) {
  d <- data.frame(
    x = 1:40,
    y = as.numeric(rep(c(18, 22, 20, 21, 19), 8)),
    n = rep(100, 40)
  )
  d$y[at] <- NA
  d$n[at] <- gap_n

  return(d)
}

# the denominators alternate between 50 and 150, and average 100 over the
# display period, so the period's mean denominator is a value that no single
# observation holds
proportion_data_with_varying_n <- function(gap_n, at) {
  n <- rep(c(50, 150), 20)
  d <- data.frame(
    x = 1:40,
    y = round(n * rep(c(0.18, 0.22, 0.20, 0.21, 0.19), 8)),
    n = n
  )
  d$y[at] <- NA
  d$n[at] <- gap_n

  return(d)
}

as_proportion_chart <- function(d, chart_type) {
  return(autospc(d,
    chart_type = chart_type, x = "x", y = "y", n = "n",
    plot_chart = FALSE, period_min = 21L
  ))
}


test_that("the denominator of a row with no observation is reported", {
  result <- as_proportion_chart(proportion_data_with_gaps(400), "P")

  expect_true(is.na(result$y[10]))
  expect_identical(result$n[10], 400)
})


test_that("a P chart takes its gap limits from the row's own denominator", {
  result <- as_proportion_chart(proportion_data_with_gaps(400), "P")

  # at four times the denominator the limits are half as far from the centre
  # line
  half_width <- result$ucl[9] - result$cl[9]

  expect_equal(result$ucl[10], result$cl[9] + half_width / 2)
  expect_equal(result$lcl[10], result$cl[9] - half_width / 2)
})


test_that("a P' chart takes its gap limits from the row's own denominator", {
  result <- as_proportion_chart(proportion_data_with_gaps(400), "P\'")

  half_width <- result$ucl[9] - result$cl[9]

  expect_equal(result$ucl[10], result$cl[9] + half_width / 2)
  expect_equal(result$lcl[10], result$cl[9] - half_width / 2)
})


test_that("gap limits are held within the range a percentage can take", {
  # a denominator of 1 puts the recalculated limits above 100 and below 0
  result <- as_proportion_chart(proportion_data_with_gaps(1), "P")

  expect_equal(result$ucl[10], 100)
  expect_equal(result$lcl[10], 0)
})


test_that("a gap with no denominator keeps the period's limits", {
  result <- as_proportion_chart(proportion_data_with_gaps(NA_real_), "P")

  expect_equal(result$ucl[10], result$ucl[9])
  expect_equal(result$lcl[10], result$lcl[9])
})


test_that("a gap whose denominator is zero keeps the period's limits", {
  result <- as_proportion_chart(proportion_data_with_gaps(0), "P")

  expect_equal(result$ucl[10], result$ucl[9])
  expect_equal(result$lcl[10], result$lcl[9])
})


test_that("a gap in a display period keeps the period's centre line", {
  result <- as_proportion_chart(proportion_data_with_gaps(400, at = 30), "P")

  expect_identical(result$period_type[29], "display")
  expect_equal(result$cl[30], result$cl[29])
})


test_that("a gap in a display period takes its own denominator", {
  result <- as_proportion_chart(proportion_data_with_gaps(400, at = 30), "P")

  half_width <- result$ucl[29] - result$cl[29]

  expect_equal(result$ucl[30], result$cl[29] + half_width / 2)
  expect_equal(result$lcl[30], result$cl[29] - half_width / 2)
})


test_that("a P' gap in a display period takes its own denominator", {
  result <- as_proportion_chart(proportion_data_with_gaps(400, at = 30), "P\'")

  half_width <- result$ucl[29] - result$cl[29]

  expect_equal(result$cl[30], result$cl[29])
  expect_equal(result$ucl[30], result$cl[29] + half_width / 2)
})


test_that("a gap at the first row of a display period is no different", {
  result <- as_proportion_chart(proportion_data_with_gaps(400, at = 22), "P")

  half_width <- result$ucl[21] - result$cl[21]

  expect_equal(result$cl[22], result$cl[21])
  expect_equal(result$ucl[22], result$cl[21] + half_width / 2)
})


test_that("consecutive gaps each take their own denominator", {
  result <- as_proportion_chart(
    proportion_data_with_gaps(c(400, 100, NA_real_), at = 30:32), "P"
  )

  half_width <- result$ucl[29] - result$cl[29]

  expect_equal(result$ucl[30], result$cl[29] + half_width / 2)
  expect_equal(result$ucl[31], result$ucl[29])

  # the third row has no denominator, so it takes the period's mean, which is
  # also 100
  expect_equal(result$ucl[32], result$ucl[29])
})


test_that("a gap with no denominator takes the period's mean denominator", {
  # the mean denominator over the display period is 100, a value that no
  # observation holds
  result <- as_proportion_chart(
    proportion_data_with_varying_n(NA_real_, at = 30), "P"
  )

  constant <- (result$ucl[29] - result$cl[29]) * sqrt(result$n[29])

  expect_identical(result$period_type[29], "display")
  expect_false(isTRUE(all.equal(result$ucl[30], result$ucl[29])))
  expect_equal(result$ucl[30], result$cl[29] + constant / sqrt(100))
})


test_that("a gap in a later period takes that period's limit width", {
  # a shift at row 31 re-establishes the limits, so the first period sits at
  # 20% with a width of 3 * sqrt(0.2 * 0.8) * 100, which is 120, and the second
  # at 50% with a width of 150
  d <- data.frame(x = 1:70, n = rep(100L, 70))
  d$y <- c(rep(20L, 30), rep(50L, 40))
  d$y[40] <- NA
  d$n[40] <- 400L

  result <- as_proportion_chart(d, "P")

  expect_identical(result$plot_period[39], "calculation31")
  expect_equal(result$cl[40], 50)
  expect_equal(result$ucl[40], 50 + 150 / sqrt(400))
  expect_equal(result$lcl[40], 50 - 150 / sqrt(400))
})


test_that("an observation whose limits are held does not distort a gap", {
  # every value is 50%, so the centre line is 50 and the limit width is
  # 3 * sqrt(0.5 * 0.5) * 100, which is 150
  d <- data.frame(x = 1:40, n = rep(100L, 40))

  # row 22, the first observation of the display period, has a denominator of
  # 2, so its own limits are held at 100 and 0
  d$n[22] <- 2L
  d$y <- d$n / 2
  d$y[30] <- NA
  d$n[30] <- 400L

  result <- as_proportion_chart(d, "P")

  expect_equal(result$ucl[22], 100)
  expect_equal(result$lcl[22], 0)

  expect_equal(result$ucl[30], 50 + 150 / sqrt(400))
  expect_equal(result$lcl[30], 50 - 150 / sqrt(400))
})


test_that("a row with no observation says which period it is in", {
  result <- as_proportion_chart(proportion_data_with_gaps(400), "P")

  expect_identical(result$plot_period[10], result$plot_period[9])
  expect_identical(result$period_type[10], result$period_type[9])
  expect_identical(result$period_start[10], result$period_start[9])
  expect_identical(result$limit_width[10], result$limit_width[9])

  expect_false(result$limit_change[10])
  expect_equal(result$cl_change[10], 0)
})


test_that("a chart without a limit width still says which period a gap is in", {
  result <- analyse(gapped(23L))

  expect_false("limit_width" %in% names(result))
  expect_identical(result$plot_period[23], result$plot_period[22])
  expect_identical(result$period_type[23], result$period_type[22])
})


test_that("the limit lines are drawn through a row with no observation", {
  # the row carries its own limits, so it has to sit in the same line as the
  # observations either side of it rather than being stepped over
  d <- proportion_data_with_varying_n(400, at = 10)

  table <- as_proportion_chart(d, "P")
  drawn <- ggplot2::ggplot_build(autospc(d,
    chart_type = "P", x = "x", y = "y", n = "n",
    plot_chart = TRUE, period_min = 21L
  ))$data[[1]]

  expect_setequal(drawn$group[drawn$x == 10], drawn$group[drawn$x == 9])
  expect_true(any(abs(drawn$y[drawn$x == 10] - table$ucl[10]) < 1e-8))
  expect_true(any(abs(drawn$y[drawn$x == 10] - table$lcl[10]) < 1e-8))
})


test_that("a C chart ignores the denominator at a gap", {
  d <- proportion_data_with_gaps(400)

  result <- autospc(d,
    chart_type = "C\'", x = "x", y = "y", n = "n",
    plot_chart = FALSE, period_min = 21L
  )

  expect_equal(result$ucl[10], result$ucl[9])
  expect_equal(result$lcl[10], result$lcl[9])
})


# a series with no observations at all


test_that("compacting a series with no observations gives an empty result", {
  chart <- structure(list(), class = c("autospc_chart_c", "autospc_chart"))
  data <- data.frame(x = 1:3, y = NA_real_)

  compacted <- compact_series(chart, data = data, na_ends_run = TRUE)

  expect_identical(nrow(compacted), 0L)
  expect_identical(compacted$run_break, logical(0))
})


# a period that holds no limits


test_that("row_holding_period_limits returns NULL where no row holds limits", {
  period <- data.frame(cl = c(NA_real_, NA_real_), ucl = c(NA_real_, NA_real_))

  expect_null(row_holding_period_limits(period))
})


test_that("row_holding_period_limits passes over a row holding only one", {
  period <- data.frame(cl = c(10, 10), ucl = c(NA_real_, 20))

  expect_identical(row_holding_period_limits(period)$ucl, 20)
})


test_that("a period that holds no limits gives its missing rows none", {
  chart <- structure(list(), class = c("autospc_chart_c", "autospc_chart"))
  period <- data.frame(cl = NA_real_, ucl = NA_real_, lcl = NA_real_)

  limits <- limits_for_missing_rows(chart,
    period = period,
    rows = data.frame(x = 1:2)
  )

  expect_length(limits$cl, 2L)
  expect_true(all(is.na(unlist(limits))))
})


# an MR chart's first point


test_that("the first row of an MR chart is not treated as missing data", {
  # it holds no moving range because there is no earlier point to measure one
  # against, so it counts towards the period and carries limits
  d <- data.frame(x = 1:40, y = as.numeric(rep(c(10, 14, 11, 16, 12), 8)))

  result <- autospc(d,
    chart_type = "MR", x = "x", y = "y",
    plot_chart = FALSE, period_min = 21L
  )

  expect_true(is.na(result$y[1]))
  expect_false(is.na(result$cl[1]))
  expect_false(is.na(result$ucl[1]))

  calculation <- which(result$period_type == "calculation")
  first <- calculation[calculation <= 30]

  expect_identical(range(first), c(1L, 21L))
  expect_identical(sum(!is.na(result$y[1:21])), 20L)
})


test_that("a missing y in an MR series is still treated as a gap", {
  d <- data.frame(x = 1:40, y = as.numeric(rep(c(10, 14, 11, 16, 12), 8)))
  d$y[30] <- NA

  result <- autospc(d,
    chart_type = "MR", x = "x", y = "y",
    plot_chart = FALSE, period_min = 21L
  )

  # the gap costs the moving range at the missing point and the one after it
  expect_true(all(is.na(result$y[30:31])))

  # and the limits carry across, because the gap is inside the series
  expect_false(anyNA(result$cl[30:31]))
})


# na_ends_run


test_that("na_ends_run TRUE stops a run continuing across a gap", {
  # 25 points on one side of the centre line, then 12 on the other
  d <- data.frame(x = 1:37, y = as.numeric(c(rep(10, 25), rep(20, 12))))
  d$y[30] <- NA

  result <- analyse(d, shift_rule_threshold = 8L, na_ends_run = TRUE)

  expect_length(which(result$rule2), 0L)
})


test_that("na_ends_run FALSE lets a run continue across a gap", {
  d <- data.frame(x = 1:37, y = as.numeric(c(rep(10, 25), rep(20, 12))))
  d$y[30] <- NA

  result <- analyse(d, shift_rule_threshold = 8L, na_ends_run = FALSE)

  # every point of the run except the one that is missing
  expect_identical(which(result$rule2), c(26:29, 31:37))
})


test_that("na_ends_run makes no difference to a series with no gaps", {
  d <- data.frame(x = 1:37, y = as.numeric(c(rep(10, 25), rep(20, 12))))

  expect_identical(
    analyse(d, shift_rule_threshold = 8L, na_ends_run = TRUE),
    analyse(d, shift_rule_threshold = 8L, na_ends_run = FALSE)
  )
})


test_that("na_ends_run defaults to TRUE", {
  expect_true(autospc_default("na_ends_run"))
})


# points on the centre line


test_that("a point on the centre line does not end the run it sits in", {
  # a point within centre_line_tolerance of the centre line is neither above
  # nor below it, so it neither ends the run it sits in nor counts towards its
  # length. test-centre_line_runs.R covers the rules in full
  baseline <- c(rep(c(12, 10), 10), 11)
  d <- data.frame(x = 1:30, y = as.numeric(c(baseline, rep(13, 9))))

  on_the_line <- d
  on_the_line$y[26] <- 11

  flagged <- function(dd) {
    result <- autospc(dd,
      chart_type = "C", x = "x", y = "y", plot_chart = FALSE,
      period_min = 21L, shift_rule_threshold = 8L
    )

    return(which(result$rule2))
  }

  expect_identical(flagged(d), 22:30)
  expect_identical(flagged(on_the_line), c(22:25, 27:30))
})


# what the caller is told


test_that("the too few points warning counts observations, not rows", {
  expect_warning(
    analyse(gapped(10:35)),
    "The input data has 20 points"
  )
})


# what does not reach the caller


test_that("the marker the algorithm uses for gaps is not returned", {
  expect_false("run_break" %in% colnames(analyse(gapped(c(23L, 24L)))))
  expect_false("run_break" %in% colnames(analyse(gapped(integer(0)))))
})


test_that("the recorded row numbers are rows of the series the caller gave", {
  chart <- autospc_chart(
    chart_type = "C\'",
    data = gapped(c(5L, 6L, 7L)),
    x = "x",
    y = "y"
  )
  analysed <- establish_limits(prepare_data(chart))

  # the first period is 21 observations, which ends at row 24 of 46 because
  # three of the rows within it hold none
  expect_identical(analysed$history$counter_path$to[1], 25L)
})
