# Forced restarts: the series is split at each value of force_restart_at, each
# segment is analysed as if it were a series of its own, and the segments are
# joined.

restart_series <- example_series_1

analyse_c <- function(data, ...) {
  suppressWarnings(
    autospc(data,
      chart_type = "C", x = x, y = y, plot_chart = FALSE,
      max_exclusions = 0L, ...
    )
  )
}

charts_of <- function(data, ..., chart_type = "C") {
  autospc_plot_charts(suppressWarnings(
    autospc(data,
      chart_type = chart_type, x = x, y = y,
      max_exclusions = 0L, ...
    )
  ))
}

# 21 points alternating 9 and 11, then a shift up from row 22 to points
# alternating 19 and 21, then points of 14, below the centre line of the
# candidate period that starts at row 22 (rows 22 to 42), from row 39
final_run_series <- data.frame(
  x = 1:70,
  y = c(
    rep(c(9, 11), length.out = 21),
    rep(c(19, 21), length.out = 17),
    rep(14, 8),
    rep(c(13, 15), length.out = 24)
  )
)

first_candidate <- function(data, ...) {
  chart <- autospc_plot_charts(suppressWarnings(
    autospc(data, chart_type = "C", x = x, y = y, ...)
  ))[[1]]

  return(chart$history$candidates[[1]])
}


test_that("each segment is analysed as if it were a series of its own", {
  for (exclusions in c(0L, 3L)) {
    whole <- suppressWarnings(autospc(ed_attendances_monthly,
      chart_type = "C", x = month_start, y = att_all, plot_chart = FALSE,
      max_exclusions = exclusions,
      force_restart_at = as.Date("2018-01-01")
    ))

    before <- suppressWarnings(autospc(
      dplyr::filter(
        ed_attendances_monthly,
        month_start < as.Date("2018-01-01")
      ),
      chart_type = "C", x = month_start, y = att_all, plot_chart = FALSE,
      max_exclusions = exclusions
    ))

    after <- suppressWarnings(autospc(
      dplyr::filter(
        ed_attendances_monthly,
        month_start >= as.Date("2018-01-01")
      ),
      chart_type = "C", x = month_start, y = att_all, plot_chart = FALSE,
      max_exclusions = exclusions
    ))

    for (column in c(
      "cl", "ucl", "lcl", "period_type", "excluded", "rule1", "rule2",
      "run_start", "above_or_below_cl", "highlight"
    )) {
      expect_identical(
        whole[[column]],
        c(before[[column]], after[[column]]),
        info = paste(column, "max_exclusions", exclusions)
      )
    }
  }
})


test_that("restart is TRUE at the first row of each segment after the first", {
  analysed <- analyse_c(restart_series, force_restart_at = c(40, 60))

  expect_identical(which(analysed$restart), c(40L, 60L))
})


test_that("the first observation after a restart is a break point", {
  analysed <- analyse_c(restart_series, force_restart_at = 60)

  expect_true(analysed$break_point[60])
  expect_true(analysed$limit_change[60])
})


test_that("no two segments share a period name", {
  analysed <- analyse_c(restart_series, force_restart_at = 60)

  first_segment <- unique(analysed$plot_period[1:59])
  second_segment <- unique(analysed$plot_period[60:125])

  expect_length(intersect(first_segment, second_segment), 0L)
})


test_that("cl_change at a restart is the direction from the last centre line", {
  # the centre line goes down from 11.76 to 9.33 at row 60
  analysed <- analyse_c(restart_series, force_restart_at = 60)
  expect_identical(analysed$cl_change[60], -1)

  # where no earlier segment has a centre line, there is no direction
  analysed <- analyse_c(restart_series, force_restart_at = 21)
  expect_identical(analysed$cl_change[21], 0)
})


test_that("fewer than period_min points before a restart have no limits", {
  analysed <- analyse_c(restart_series, force_restart_at = 21)

  expect_true(all(is.na(analysed$cl[1:20])))
  expect_false(anyNA(analysed$cl[21:125]))
})


test_that("fewer than period_min points after the last restart have no limits", {
  analysed <- analyse_c(restart_series,
    force_restart_at = 106,
    extend_limits_to = 130
  )

  expect_true(all(is.na(analysed$cl[106:125])))
  expect_false(anyNA(analysed$cl[1:105]))

  # the limits of an earlier segment are not extended past the end of the data
  expect_false(any(analysed$limit_extension))
})


test_that("two restarts closer together than period_min leave the rows between without limits", {
  analysed <- analyse_c(restart_series, force_restart_at = c(40, 50))

  expect_identical(which(analysed$restart), c(40L, 50L))
  expect_true(all(is.na(analysed$cl[40:49])))
  expect_false(anyNA(analysed$cl[50:70]))
})


test_that("a restart at an x with no value starts the segment's analysis at the next value", {
  gapped <- restart_series
  gapped$y[60] <- NA

  analysed <- analyse_c(gapped, force_restart_at = 60)

  expect_true(analysed$restart[60])
  expect_true(is.na(analysed$cl[60]))
  expect_true(analysed$break_point[61])
  expect_identical(
    analysed$cl[61:125],
    analyse_c(dplyr::slice(gapped, 61:125))$cl
  )
})


test_that("a restart between two values of x starts at the next one", {
  analysed <- analyse_c(restart_series, force_restart_at = 59.5)

  expect_identical(which(analysed$restart), 60L)
})


test_that("limits are not carried across a gap just before a restart", {
  gapped <- restart_series
  gapped$y[59] <- NA

  analysed <- analyse_c(gapped, force_restart_at = 60)

  expect_true(is.na(analysed$cl[59]))
  expect_false(is.na(analysed$cl[58]))
})


test_that("restarts outside the data, or at its first row, change nothing", {
  plain <- analyse_c(restart_series)

  expect_identical(
    analyse_c(restart_series, force_restart_at = c(-5, 1, 200)),
    plain
  )
})


test_that("a candidate before a restart cannot use data after it", {
  # without a restart, the run of points below the candidate centre line from
  # row 39 continues past row 46 into an opposing rule break
  expect_true(first_candidate(final_run_series)$opposite_break)

  # with a restart at row 46 the run ends at row 45, seven points long
  expect_false(
    first_candidate(final_run_series, force_restart_at = 46)$opposite_break
  )
})


test_that("an opposing run completed before a restart rejects the candidate", {
  # restart at row 47: an opposing run of 8, four points inside the candidate
  # period and four after it
  candidate <- first_candidate(final_run_series, force_restart_at = 47)

  expect_identical(candidate$counter, 22L)
  expect_true(candidate$opposite_break)
  expect_false(candidate$accepted)

  chart <- charts_of(final_run_series, force_restart_at = 47)[[1]]
  expect_match(chart$result$table$log[22], "0710$")
})


test_that("a final run that ends at a restart does not stop the algorithm", {
  # restart at row 46: an opposing run of 7, four points inside the candidate
  # period and three after it, which is complete and not a rule break
  candidate <- first_candidate(final_run_series, force_restart_at = 46)

  expect_false(candidate$final_run_prevents)
  expect_true(candidate$accepted)

  # the same points with the series ending there, and no restart, may still
  # grow into an opposing rule break, so the algorithm stops
  candidate <- first_candidate(dplyr::slice_head(final_run_series, n = 45L))

  expect_true(candidate$final_run_prevents)
  expect_false(candidate$accepted)
})


test_that("both charts of an XMR pair restart at the same row", {
  charts <- charts_of(restart_series,
    chart_type = "XMR",
    force_restart_at = 60
  )

  for (chart in charts) {
    expect_identical(which(chart$result$table$restart), 60L)
    expect_true(chart$result$table$break_point[60])
  }
})


test_that("the MR chart has no moving range at a restart", {
  dispersion <- charts_of(restart_series,
    chart_type = "XMR",
    force_restart_at = 60
  )$dispersion$result$table

  expect_true(is.na(dispersion$series[60]))
  expect_false(is.na(dispersion$series[61]))
})


test_that("both charts of an XbarS pair restart at the same row", {
  subgroups <- data.frame(
    x = 1:60,
    y = 10 + rep(c(-0.5, 0.5), 30),
    n = 5L,
    s = rep(c(1, 1.2), 30)
  )

  charts <- autospc_plot_charts(suppressWarnings(
    autospc(subgroups,
      chart_type = "XbarS", x = x, y = y, n = n, s = s,
      period_min = 10L, force_restart_at = 30
    )
  ))

  for (chart in charts) {
    expect_identical(which(chart$result$table$restart), 30L)
    expect_true(chart$result$table$break_point[30])
  }
})


test_that("a facet uses only the restarts in the data it analyses", {
  facets <- autospc_plot_charts(suppressWarnings(
    facet_stages(restart_series,
      split_at = c(50L, 125L), chart_type = "C",
      max_exclusions = 0L, force_restart_at = 80
    )
  ))

  expect_false(any(facets[[1]]$result$table$restart))
  expect_identical(which(facets[[2]]$result$table$restart), 80L)
})


test_that("the history records each restart", {
  chart <- charts_of(restart_series, force_restart_at = 60)[[1]]

  expect_identical(
    chart$history$restarts,
    data.frame(
      row = 60L,
      counter = 47L,
      previous_segment_ended = "not enough data for a further period",
      baseline_length = 21L
    )
  )

  path <- chart$history$counter_path
  expect_identical(
    path[path$reason == "forced restart", c("from", "to")],
    data.frame(from = 47L, to = 60L),
    ignore_attr = TRUE
  )

  # stopped is why the algorithm stopped looking in the last segment, with its
  # row counted in the whole series
  expect_identical(
    chart$history$stopped,
    list(counter = 106L, reason = "not enough data for a further period")
  )
})


test_that("a log entry past the end of a segment is held at its last row", {
  # the first segment is rows 1 to 21, exactly one calculation period, so the
  # counter moves to row 22 when that period is formed - past the segment's end
  analysed <- analyse_c(restart_series, force_restart_at = 22)

  expect_match(analysed$log[21], "co@22\\|0300$")
  expect_match(analysed$log[22], "^0000;0200")
})


test_that("a history with no restart has no restart record", {
  chart <- charts_of(restart_series)[[1]]

  expect_null(chart$history$restarts)
  expect_false("forced restart" %in% chart$history$counter_path$reason)
})


test_that("the log marks each restart with 0000", {
  analysed <- analyse_c(restart_series, force_restart_at = c(40, 50))

  expect_match(analysed$log[1], "^0100;0200")
  expect_identical(analysed$log[40], "0000;0210")
  expect_match(analysed$log[50], "^0000;0200")

  expect_identical(
    interpret_log_entry("0000", verbosity = 1L),
    "Restart forced by force_restart_at."
  )
})


test_that("a chart has limits where any segment has enough points", {
  chart <- charts_of(restart_series, force_restart_at = 21)[[1]]
  expect_true(enough_data_for_limits(chart))

  short <- data.frame(x = 1:30, y = rep(c(10L, 12L), 15L))
  expect_warning(
    autospc(short, chart_type = "C", x = x, y = y, force_restart_at = 15),
    "Each segment of the series between forced restarts has fewer than"
  )
})


test_that("segment_rows gives the rows of each segment", {
  chart <- charts_of(restart_series, force_restart_at = c(60, 40))[[1]]

  expect_identical(segment_rows(chart), list(1:39, 40:59, 60:125))
})


test_that("printing a chart lists its restarts", {
  chart <- charts_of(restart_series, force_restart_at = 60)[[1]]

  expect_output(print(chart), "Forced restarts at 60")
})
