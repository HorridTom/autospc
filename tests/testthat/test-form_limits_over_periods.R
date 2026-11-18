# form_limits_over_periods() given a chart's own calculation periods forms the
# same limits the algorithm formed, and calculation_periods() reads those
# periods from the analysed table.

gapped_series <- example_series_1
gapped_series$y[c(5L, 40L, 41L, 92L)] <- NA

set.seed(115)
subgroup_series <- data.frame(
  x = rep(1:60, each = 4L),
  y = c(stats::rnorm(120L, mean = 10), stats::rnorm(120L, mean = 13))
)

# the analysed charts of a call, each holding its prepared series in
# chart$data and its analysis in chart$result$table
analysed_charts <- function(...) {
  plot <- suppressWarnings(autospc(...))

  return(autospc_plot_charts(plot))
}

# the limits table form_limits_over_periods() gives over the chart's own
# calculation periods, and the analysed table's rows for the same points
reformed <- function(chart) {
  observed <- compact_series(chart,
    data = chart$data,
    na_ends_run = chart$na_ends_run
  )

  table <- chart$result$table %>%
    dplyr::filter(x %in% observed$x)

  return(list(
    reformed = form_limits_over_periods(
      data = observed,
      periods = calculation_periods(chart$result$table),
      chart = chart
    ),
    table = table
  ))
}



test_that("calculation_periods gives the first and last x of each calculation period", {
  chart <- analysed_charts(gapped_series, chart_type = "X", x = x, y = y)[[1]]

  expect_identical(
    calculation_periods(chart$result$table),
    data.frame(first_x = c(1L, 53L, 74L), last_x = c(22L, 73L, 95L))
  )
})


test_that("a missing value inside a calculation period is within its first and last x", {
  chart <- analysed_charts(gapped_series, chart_type = "X", x = x, y = y)[[1]]
  table <- chart$result$table

  expect_identical(table$period_type[table$x == 5L], "calculation")
  expect_true(is.na(table$series[table$x == 5L]))
})


test_that("form_limits_over_periods reproduces the algorithm's limits over its own periods", {
  cases <- list(
    c_chart = list(example_series_1, chart_type = "C"),
    p_prime_chart = list(ed_attendances_monthly,
      chart_type = "P'",
      x = "month_start", y = "within_4h", n = "att_all"
    ),
    xmr_with_missing_values = list(gapped_series, chart_type = "XMR"),
    xmr_with_baseline_length = list(gapped_series,
      chart_type = "XMR",
      baseline_length = 30L
    ),
    xbars = list(subgroup_series, chart_type = "XbarS")
  )

  for (case in names(cases)) {
    for (chart in rlang::exec(analysed_charts, !!!cases[[case]])) {
      result <- reformed(chart)
      info <- paste(case, class(chart)[[1]])

      # every column of the limits table that the analysed table also has
      columns <- intersect(names(result$reformed), names(result$table))
      expect_true(
        all(c("ucl", "lcl", "cl", "period_type", "excluded", "rule2") %in%
          columns),
        info = info
      )

      for (column in columns) {
        expect_identical(result$reformed[[column]],
          result$table[[column]],
          info = paste(info, column)
        )
      }
    }
  }
})


test_that("the reproduced cases have more than one period, excluded points and missing values", {
  # the test above shows nothing about these unless the cases contain them
  periods <- function(...) {
    return(vapply(analysed_charts(...), function(chart) {
      nrow(calculation_periods(chart$result$table))
    }, integer(1L)))
  }

  expect_identical(
    unname(periods(example_series_1, chart_type = "C")),
    4L
  )
  expect_identical(
    unname(periods(gapped_series, chart_type = "XMR", baseline_length = 30L)),
    c(3L, 2L)
  )

  p_prime <- analysed_charts(ed_attendances_monthly,
    chart_type = "P'",
    x = month_start, y = within_4h, n = att_all
  )[[1]]
  expect_gt(sum(p_prime$result$table$excluded %in% TRUE), 0L)

  mr <- analysed_charts(gapped_series, chart_type = "XMR")$dispersion
  expect_gt(sum(is.na(mr$result$table$series)), 1L)
})


test_that("the first period starts at the first row of data whatever its first_x", {
  chart <- analysed_charts(example_series_1, chart_type = "C")[[1]]
  observed <- compact_series(chart,
    data = chart$data,
    na_ends_run = chart$na_ends_run
  )
  periods <- calculation_periods(chart$result$table)
  later_start <- periods
  later_start$first_x[[1L]] <- 10L

  expect_identical(
    form_limits_over_periods(observed, periods = later_start, chart = chart),
    form_limits_over_periods(observed, periods = periods, chart = chart)
  )
})


test_that("a later period holds the rows from its first to its last x, however many", {
  # with no exclusions, a C chart's centre line is the mean of the period
  chart <- analysed_charts(example_series_1,
    chart_type = "C",
    max_exclusions = 0L
  )[[1]]
  observed <- compact_series(chart,
    data = chart$data,
    na_ends_run = chart$na_ends_run
  )

  table <- form_limits_over_periods(observed,
    periods = data.frame(first_x = c(1L, 40L), last_x = c(21L, 69L)),
    chart = chart
  )

  expect_identical(
    table$x[table$period_type == "calculation"],
    c(1:21, 40:69)
  )
  expect_identical(which(table$break_point), 40L)
  expect_equal(table$cl[40:125], rep(mean(observed$series[40:69]), 86L))
})


test_that("calculation_periods puts the periods in order of x", {
  # by name, calculation10 sorts before calculation5
  table <- data.frame(
    x = 1:12,
    period_type = rep(
      c("calculation", "display", "calculation", "display", "calculation",
        "display"),
      c(2L, 2L, 2L, 3L, 2L, 1L)
    ),
    plot_period = rep(
      c("calculation1", "display3", "calculation5", "display7",
        "calculation10", "display12"),
      c(2L, 2L, 2L, 3L, 2L, 1L)
    )
  )

  expect_identical(
    calculation_periods(table),
    data.frame(first_x = c(1L, 5L, 10L), last_x = c(2L, 6L, 11L))
  )
})
