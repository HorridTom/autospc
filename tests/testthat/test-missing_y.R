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


test_that("a P chart takes its gap limits from the period's mean denominator", {
  set.seed(4)
  d <- data.frame(
    x = 1:40,
    y = rep(c(4, 6, 5, 7, 3), 8),
    n = as.integer(sample(15:40, 40L, replace = TRUE))
  )
  d$y[20] <- NA

  result <- autospc(d,
    chart_type = "P\'", x = "x", y = "y", n = "n",
    plot_chart = FALSE, period_min = 21L
  )

  # the limits of a P chart vary with the denominator, so the value at the gap
  # is neither neighbour's
  expect_false(is.na(result$ucl[20]))
  expect_false(isTRUE(all.equal(result$ucl[20], result$ucl[19])))
  expect_false(isTRUE(all.equal(result$ucl[20], result$ucl[21])))
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


test_that("a point on the centre line ends the run it sits in", {
  # this is what the package has always done, and it is recorded here because
  # add_rule_two() was rewritten: a point within centre_line_tolerance of the
  # centre line is a side of its own, so it ends the run before it and starts
  # one of its own, which is then not itself flagged
  # TO DO: Fix this so points on the centre line are handled correctly.
  base <- rep(c(11, 13), length.out = 21L)
  d <- data.frame(x = 1:30, y = as.numeric(c(base, rep(20, 9))))

  on_the_line <- d
  on_the_line$y[26] <- 12

  flagged <- function(dd) {
    result <- autospc(dd,
      chart_type = "C\'", x = "x", y = "y", plot_chart = FALSE,
      period_min = 21L, shift_rule_threshold = 8L, centre_line_tolerance = 1
    )

    return(which(result$rule2))
  }

  # a run of nine is a shift
  expect_identical(flagged(d), 22:30)

  # the same run, interrupted by one point on the centre line, is two runs of
  # four, and neither reaches the threshold
  expect_length(flagged(on_the_line), 0L)
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
