# Restarts after long gaps: the algorithm restarts at the first point with a
# value after a gap of at least restart_after_gap points with no value.

# A shift upwards at x = 31, cut short by a gap of 21 points at 41 to 61, after
# which the series is back at its first level
gap_series <- function(gap = 41:61) {
  data <- data.frame(
    x = 1:100,
    y = c(rep(c(9L, 11L), 15L), rep(c(14L, 16L), 15L), rep(c(9L, 11L), 20L))
  )
  data$y[gap] <- NA

  return(data)
}

analyse_gaps <- function(data, chart_type = "C", ...) {
  return(autospc(data,
    chart_type = chart_type,
    plot_chart = FALSE,
    max_exclusions = 0L,
    ...
  ))
}

# The analysed charts, so that both halves of a pair can be read
charts_with_gaps <- function(data, chart_type = "C", ...) {
  plot <- autospc(data,
    chart_type = chart_type,
    max_exclusions = 0L,
    ...
  )

  return(autospc_plot_charts(plot))
}

restart_x <- function(table) {
  return(table$x[table$restart])
}

# The points with a value after the gap that have no centre line: the
# stranded points
stranded_x <- function(table, after = 40) {
  return(table$x[table$x > after & !is.na(table$series) & is.na(table$cl)])
}


test_that("a long gap restarts the series at the first point after it", {
  table <- analyse_gaps(gap_series())

  expect_identical(restart_x(table), 62L)
  expect_identical(table$period_type[table$x == 62], "calculation")
  expect_true(table$break_point[table$x == 62])
})


test_that("a gap one point shorter than restart_after_gap does not", {
  table <- analyse_gaps(gap_series(), restart_after_gap = 22L)

  expect_identical(restart_x(table), integer(0))
  # the limits are carried across the gap
  expect_false(anyNA(table$cl))
})


test_that("restart_after_gap counts the points in the gap", {
  expect_identical(
    restart_x(analyse_gaps(gap_series(41:43), restart_after_gap = 3L)),
    44L
  )
  expect_identical(
    restart_x(analyse_gaps(gap_series(41:42), restart_after_gap = 3L)),
    integer(0)
  )
})


test_that("Inf turns restarts after gaps off", {
  table <- analyse_gaps(gap_series(), restart_after_gap = Inf)

  expect_identical(restart_x(table), integer(0))
})


test_that("a gap after which the series restarts has no limits", {
  table <- analyse_gaps(gap_series())

  expect_true(all(is.na(table$cl[table$x %in% 41:61])))
})


test_that("a candidate before the gap does not use the data after it", {
  restarted <- charts_with_gaps(gap_series())[[1]]$history

  # the shift at 31 has 10 points before the gap, too few for a period
  expect_null(restarted$candidates)
  expect_identical(
    restarted$restarts$previous_segment_ended,
    "not enough data for a further period"
  )

  # without the restart, the candidate is rejected because of the data after
  # the gap
  continued <- charts_with_gaps(gap_series(), restart_after_gap = Inf)[[1]]
  decisions <- vapply(
    continued$history$candidates,
    candidate_decision_code,
    character(1L)
  )

  expect_true(all(decisions == "0710"))
})


test_that("gaps at the start and end of the series do not restart it", {
  table <- analyse_gaps(gap_series(c(1:25, 80:100)))

  expect_identical(restart_x(table), integer(0))
})


test_that("x values with no row in the data are not a gap", {
  data <- gap_series(integer(0))[-(41:61), ]

  expect_identical(restart_x(analyse_gaps(data)), integer(0))
})


test_that("a gap restarts weekly and monthly series", {
  weekly <- gap_series()
  weekly$x <- seq(as.Date("2020-01-06"), by = "week", length.out = 100L)

  monthly <- gap_series()
  monthly$x <- seq(as.Date("2010-01-01"), by = "month", length.out = 100L)

  expect_equal(restart_x(analyse_gaps(weekly)), weekly$x[[62L]])
  expect_equal(restart_x(analyse_gaps(monthly)), monthly$x[[62L]])
})


test_that("a missing denominator is part of a gap", {
  data <- gap_series(integer(0))
  data$n <- 50L
  data$n[41:61] <- NA

  expect_identical(restart_x(analyse_gaps(data, chart_type = "P")), 62L)
})


test_that("both charts of an XMR pair restart where the X chart's gap ends", {
  charts <- charts_with_gaps(gap_series(), chart_type = "XMR")

  expect_identical(restart_x(charts$location$result$table), 62L)
  expect_identical(restart_x(charts$dispersion$result$table), 62L)

  # the gap in the moving ranges is one point longer than the gap in the X
  # values, and does not restart either chart
  charts <- charts_with_gaps(gap_series(),
    chart_type = "XMR",
    restart_after_gap = 22L
  )

  expect_identical(restart_x(charts$location$result$table), integer(0))
  expect_identical(restart_x(charts$dispersion$result$table), integer(0))
})


test_that("an MR chart on its own restarts where its X chart would", {
  charts <- charts_with_gaps(gap_series(), chart_type = "MR")

  expect_identical(restart_x(charts[[1]]$result$table), 62L)
})


# Subgroups of three, with a gap of 23 subgroups at 30 to 52 and subgroups of
# one at 10 to 14
subgroup_series <- function() {
  set.seed(2)
  data <- data.frame(
    x = rep(1:80, each = 3L),
    y = round(stats::rnorm(240L, mean = 10, sd = 1), 1)
  )
  data$y[data$x %in% 30:52] <- NA

  return(data[!(data$x %in% 10:14 & duplicated(data$x)), ])
}


test_that("both charts of an XbarS pair restart where the Xbar chart's gap ends", {
  charts <- charts_with_gaps(subgroup_series(), chart_type = "XbarS")

  expect_identical(restart_x(charts$location$result$table), 53L)
  expect_identical(restart_x(charts$dispersion$result$table), 53L)
})


test_that("subgroups of one are not a gap on an S chart", {
  charts <- charts_with_gaps(subgroup_series(),
    chart_type = "S",
    restart_after_gap = 5L
  )

  expect_identical(restart_x(charts[[1]]$result$table), 53L)
})


test_that("a gap ending in a prevented range restarts after the range", {
  table <- analyse_gaps(gap_series(), prevent_establishment_in = c(55, 70))

  expect_identical(restart_x(table), 71L)
  expect_identical(stranded_x(table), 62:70)
  # the stranded points are counted among the observations before the restart
  expect_identical(table$plot_period[table$x == 71], "calculation50")
})


test_that("a range starting after the gap ends does not move the restart", {
  table <- analyse_gaps(gap_series(), prevent_establishment_in = c(65, 70))

  expect_identical(restart_x(table), 62L)
  expect_identical(stranded_x(table), integer(0))
})


test_that("with no point after the range, every point after the gap is stranded", {
  table <- analyse_gaps(gap_series(), prevent_establishment_in = c(55, NA))

  expect_identical(restart_x(table), integer(0))
  expect_identical(stranded_x(table), 62:100)
})


test_that("a forced restart among the stranded points ends them", {
  table <- analyse_gaps(gap_series(),
    prevent_establishment_in = c(55, 70),
    force_restart_at = 66
  )

  expect_identical(restart_x(table), 66L)
  expect_identical(stranded_x(table), 62:65)
})


test_that("the floating median is not taken over stranded points", {
  table <- analyse_gaps(gap_series(),
    prevent_establishment_in = c(55, 70),
    floating_median = "yes"
  )

  expect_true(all(is.na(table$median[table$x %in% 62:70])))
  before_gap <- table$x[!is.na(table$median) & table$x < 62]
  expect_identical(range(before_gap), c(29L, 40L))
})


test_that("facet_stages restarts each stage whose data reaches past the gap", {
  table <- facet_stages(gap_series(),
    split_at = c(50L, 79L),
    chart_type = "C",
    max_exclusions = 0L,
    plot_chart = FALSE
  )

  expect_identical(table$stage[table$restart], c("2", "3"))
  expect_identical(table$x[table$restart], c(62L, 62L))
})


# The record of a restart after a gap


test_that("the history records a restart after a gap", {
  chart <- charts_with_gaps(gap_series())[[1]]

  expect_identical(
    chart$history$restarts,
    data.frame(
      row = 62L,
      type = "after gap",
      counter = 22L,
      previous_segment_ended = "not enough data for a further period",
      baseline_length = 21L
    )
  )

  path <- chart$history$counter_path
  expect_identical(
    path[path$reason == "restart after gap", c("from", "to")],
    data.frame(from = 22L, to = 62L),
    ignore_attr = TRUE
  )
})


test_that("the log marks a restart after a gap with 0010", {
  table <- analyse_gaps(gap_series())

  expect_identical(table$log[table$x == 62], "0010;0200")
  expect_identical(
    interpret_log_entry("0010", verbosity = 1L),
    "Restart after a gap of at least restart_after_gap points with no value."
  )
})


test_that("a forced restart at the end of a gap is recorded as forced", {
  chart <- charts_with_gaps(gap_series(), force_restart_at = 62)[[1]]

  expect_identical(chart$history$restarts$type, "forced")
  expect_match(chart$result$table$log[62], "^0000;")
})


test_that("print lists forced restarts and restarts after gaps apart", {
  chart <- charts_with_gaps(gap_series(), force_restart_at = 85)[[1]]

  expect_output(print(chart), "Forced restarts at 85")
  expect_output(print(chart), "Restarts after gaps at 62")
})
