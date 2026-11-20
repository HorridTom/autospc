# The MR chart's first period, and its first period after a forced restart,
# start on the same row as the X chart's when the first value is missing; and
# each chart keeps its first centre line label (#326).

first_row_series <- function(missing_rows) {
  data <- example_series_1[, c("x", "y")]
  data$x <- seq_len(nrow(data))
  data$y[missing_rows] <- NA

  return(data)
}

pair_tables <- function(data, ...) {
  charts <- autospc_plot_charts(autospc(data,
    chart_type = "XMR",
    max_exclusions = 0L,
    ...
  ))

  return(lapply(charts, function(chart) chart$result$table))
}

first_centre_line_row <- function(table) {
  return(match(TRUE, !is.na(table$cl)))
}


test_that("with y missing at row 1, both charts' first periods start at row 2", {
  tables <- pair_tables(first_row_series(1L))

  expect_identical(first_centre_line_row(tables$location), 2L)
  expect_identical(first_centre_line_row(tables$dispersion), 2L)

  calculation <- tables$dispersion$period_type == "calculation" &
    seq_len(nrow(tables$dispersion)) <= 30L
  expect_identical(range(which(calculation)), c(2L, 22L))
  expect_identical(sum(!is.na(tables$dispersion$series[calculation])), 20L)
})


test_that("with y missing at rows 1 and 2, the MR chart has no limits there", {
  tables <- pair_tables(first_row_series(1:2))

  expect_identical(first_centre_line_row(tables$location), 3L)
  expect_identical(first_centre_line_row(tables$dispersion), 3L)
  expect_true(all(is.na(tables$dispersion$ucl[1:2])))
})


test_that("a forced restart where y is missing starts both charts on the same row", {
  for (mode in c("independent", "forced", "shared")) {
    tables <- pair_tables(first_row_series(60L),
      force_restart_at = 60L,
      dispersion_chart_establishment = mode
    )

    # the first break point after the restart; under "independent" the
    # charts' later re-establishments need not coincide
    first_after_restart <- function(table) {
      breaks <- which(table$break_point %in% TRUE)
      return(breaks[breaks >= 60L][[1L]])
    }

    expect_identical(first_after_restart(tables$location), 61L, label = mode)
    expect_identical(first_after_restart(tables$dispersion), 61L, label = mode)
    expect_identical(
      first_centre_line_row(tables$dispersion[60:nrow(tables$dispersion), ]),
      2L,
      label = mode
    )
  }
})


test_that("with y missing at row 1, the MR chart's periods follow the X chart's in every mode", {
  for (mode in c("forced", "shared")) {
    tables <- pair_tables(first_row_series(1L),
      dispersion_chart_establishment = mode
    )

    expect_identical(first_centre_line_row(tables$dispersion), 2L, label = mode)
  }
})


test_that("each chart keeps its first centre line label when the first values are missing", {
  for (missing_rows in list(1L, 1:2)) {
    data <- first_row_series(missing_rows)
    first_value <- max(missing_rows) + 1L

    for (chart_type in c("X", "MR")) {
      plot <- autospc(data, chart_type = chart_type, max_exclusions = 0L)
      labelled <- plot$data$x[
        plot$data$plotted_line == "cl" &
          !is.na(plot$data$cl_label) &
          plot$data$cl_label != ""
      ]

      expected <- if (chart_type == "X") first_value else first_value + 1L
      expect_identical(min(labelled), expected, label = chart_type)
    }
  }
})
