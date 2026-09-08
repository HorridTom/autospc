# A point on the centre line, within centre_line_tolerance of it, is neither
# above it nor below it. It does not commence a run, does not end one, and does
# not count towards the length of the one it sits in.
#
# The rules, settled 2026-09-06:
#   A a run commences only at a point not on the centre line
#   B a point on the centre line never ends a run
#   C a point on the centre line does not count towards run length
#   D a point on the centre line belongs to the run immediately before it, and
#     to none where no run precedes it
#   E na_ends_run decides only whether a gap stops a run continuing

runs_in <- function(side, threshold = 3L, run_break = NULL) {
  table <- data.frame(above_or_below_cl = side)

  if (!is.null(run_break)) {
    table$run_break <- run_break
  }

  return(add_rule_two(table, shift_rule_threshold = threshold))
}


# nothing to work on


test_that("a table with no points has no runs", {
  result <- runs_in(numeric(0))

  expect_identical(result$run_start, logical(0))
  expect_identical(result$rule2, logical(0))
})


# B and C: inside a run


test_that("a point on the centre line does not end the run it sits in", {
  result <- runs_in(c(1, 1, 0, 1))

  expect_identical(result$run_start, c(TRUE, FALSE, FALSE, FALSE))
})


test_that("a point on the centre line does not count towards run length", {
  # three points above the line, so a threshold of three is reached and one of
  # four is not, whichever side of the run the centre line point falls
  expect_true(all(runs_in(c(1, 1, 0, 1), threshold = 3L)$rule2))
  expect_false(any(runs_in(c(1, 1, 0, 1), threshold = 4L)$rule2))
})


test_that("a run reaches across however many centre line points it meets", {
  # decided rather than overlooked: a run continues across any number of
  # consecutive points on the centre line, so the two points above the line
  # here are one run rather than two
  result <- runs_in(c(1, 0, 0, 0, 0, 1), threshold = 2L)

  expect_identical(result$run_start, c(TRUE, rep(FALSE, 5L)))
  expect_true(all(result$rule2))
})


# A and D: at the edges of a run


test_that("a run does not commence at a point on the centre line", {
  result <- runs_in(c(0, 0, 1, 1, 1))

  expect_identical(result$run_start, c(FALSE, FALSE, TRUE, FALSE, FALSE))
})


test_that("points on the centre line before any run belong to none", {
  # the leading points are in no run, so they are not flagged even though the
  # run that follows them reaches the threshold
  result <- runs_in(c(0, 0, 1, 1, 1), threshold = 3L)

  expect_identical(result$rule2, c(FALSE, FALSE, TRUE, TRUE, TRUE))
})


test_that("points on the centre line after a run belong to it", {
  result <- runs_in(c(1, 1, 1, 0, 0), threshold = 3L)

  expect_identical(result$run_start, c(TRUE, rep(FALSE, 4L)))
  expect_true(all(result$rule2))
})


test_that("a centre line point between sides belongs to the run before it", {
  result <- runs_in(c(1, 1, 0, -1, -1))

  expect_identical(
    result$run_start,
    c(TRUE, FALSE, FALSE, TRUE, FALSE)
  )
})


test_that("points that are all on the centre line form no run", {
  result <- runs_in(c(0, 0, 0, 0), threshold = 1L)

  expect_false(any(result$run_start))
  expect_false(any(result$rule2))
})


# what does end a run


test_that("a run does not continue across a point with no side", {
  result <- runs_in(c(1, 1, NA, 1, 1))

  expect_identical(
    result$run_start,
    c(TRUE, FALSE, FALSE, TRUE, FALSE)
  )
})


test_that("a gap ends a run, and the centre line point after it does not", {
  # rule E: na_ends_run puts the break at the point after the gap, and that
  # point being on the centre line does not change where the next run commences
  before <- runs_in(c(1, 1, 0, 1, 1), run_break = c(F, F, T, F, F))
  after <- runs_in(c(1, 1, 0, 1, 1), run_break = c(F, F, F, T, F))

  expect_identical(before$run_start, c(TRUE, FALSE, FALSE, TRUE, FALSE))
  expect_identical(after$run_start, c(TRUE, FALSE, FALSE, TRUE, FALSE))
})


test_that("without a gap the same points are one run", {
  result <- runs_in(c(1, 1, 0, 1, 1))

  expect_identical(result$run_start, c(TRUE, rep(FALSE, 4L)))
})


# whole charts


# 21 baseline points, 10 twelves and 10 tens alternating and then a single 11,
# so the centre line is exactly 11 and the baseline alternates across it rather
# than forming a run of its own. The single 11 sits exactly on the line, which
# the default centre_line_tolerance of 0 is enough to recognise
baseline <- c(rep(c(12, 10), 10), 11)

series <- function(after) {
  return(data.frame(
    x = seq_len(21L + length(after)),
    y = as.numeric(c(baseline, after))
  ))
}

flagged <- function(d) {
  result <- autospc(d,
    chart_type = "C", x = "x", y = "y", plot_chart = FALSE,
    period_min = 21L, shift_rule_threshold = 8L
  )

  return(which(result$rule2))
}


test_that("nine points above the line are a shift", {
  expect_identical(flagged(series(rep(13, 9))), 22:30)
})


test_that("a point on the centre line no longer hides a shift", {
  one_on_the_line <- series(rep(13, 9))
  one_on_the_line$y[26] <- 11

  # eight points above the line over rows 22 to 30, so still a shift, and the
  # point on the line is not itself flagged
  expect_identical(flagged(one_on_the_line), c(22:25, 27:30))
})


test_that("seven points above the line do not reach a threshold of eight", {
  two_on_the_line <- series(rep(13, 9))
  two_on_the_line$y[25] <- 11
  two_on_the_line$y[28] <- 11

  expect_length(flagged(two_on_the_line), 0L)
})


test_that("a run reaches back across the centre line points before it", {
  # decided rather than overlooked. With centre_line_tolerance = 1 every 11 in
  # this alternating baseline is on the centre line of 11.9524 and every 13 is
  # above it, so the baseline is one run of ten points above the line, and the
  # nine points that follow continue it
  base <- rep(c(11, 13), length.out = 21L)
  d <- data.frame(x = 1:30, y = as.numeric(c(base, rep(20, 9))))

  result <- autospc(d,
    chart_type = "C\'", x = "x", y = "y", plot_chart = FALSE,
    period_min = 21L, shift_rule_threshold = 8L, centre_line_tolerance = 1
  )

  expect_identical(which(result$rule2), c(seq(2L, 20L, by = 2L), 22:30))
})


# the second place the threshold is applied


test_that("counter_at_rule_break counts the points of a run, not its rows", {
  # a run that is already running at the counter, with three points on the
  # centre line among the nine rows that follow it. Nine rows reach a threshold
  # of eight and six points do not, so the two measures disagree here
  table <- data.frame(
    above_or_below_cl = c(1, 1, 1, 0, 1, 1, 0, 1, 0, 1, -1),
    rule2 = c(rep(TRUE, 10L), FALSE),
    run_start = c(TRUE, rep(FALSE, 9L), TRUE)
  )

  expect_false(counter_at_rule_break(
    table = table,
    counter = 2L,
    shift_rule_threshold = 8L
  ))
})


test_that("counter_at_rule_break is reached when the points do follow", {
  # the same shape, with the points on the centre line replaced, so that nine
  # points above the line follow the counter
  table <- data.frame(
    above_or_below_cl = c(rep(1, 10L), -1),
    rule2 = c(rep(TRUE, 10L), FALSE),
    run_start = c(TRUE, rep(FALSE, 9L), TRUE)
  )

  expect_true(counter_at_rule_break(
    table = table,
    counter = 2L,
    shift_rule_threshold = 8L
  ))
})
