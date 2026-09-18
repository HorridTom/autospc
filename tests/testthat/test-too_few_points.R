# load in test data
test_data <- data.frame(
  x = 1:10,
  y = c(49, 50, 50, 50, 48, 49, 50, 49, 50, 47),
  n = c(100, 104, 108, 110, 120, 102, 111, 108, 109, 130)
)

test_data2 <- data.frame(
  x = 1:50,
  y = c(
    50, 48, 49, 50, 52, 52, 48, 53, 51, 51, 47, 52, 51, 47, 48,
    49, 51, 51, 45, 49, 49, 50, 48, 53, 49, 48, 51, 46, 48, 49, 49,
    51, 47, 53, 49, 52, 50, 58, 57, 51, 48, 52, 52, 54, 56, 50, 51,
    51, 52, 52
  ),
  n = c(
    98, 104, 94, 104, 102, 103, 102, 100, 96, 98, 106, 104, 102,
    101, 102, 101, 100, 96, 98, 100, 101, 101, 99, 99, 99, 98, 103,
    100, 97, 101, 102, 99, 97, 99, 100, 96, 101, 101, 108, 100, 96,
    102, 100, 99, 104, 96, 95, 108, 95, 97
  )
)


test_that("Charts with fewer points than min period error handle", {
  # hide warnings for part of this test
  result_C <- suppressWarnings(
    autospc(test_data, plot_chart = FALSE, chart_type = "C", period_min = 21)
  )
  result_P <- suppressWarnings(
    autospc(test_data, plot_chart = FALSE, chart_type = "P", period_min = 21)
  )

  # the same columns a series long enough for limits returns, the analysed
  # ones holding no value. aggregate_data() summarises to the columns the class
  # analyses, so a C chart drops n and a P chart keeps it
  testthat::expect_equal(ncol(result_C), 22)
  testthat::expect_equal(ncol(result_P), 23)
  testthat::expect_warning(
    autospc(test_data, plot_chart = TRUE, chart_type = "C")
  )
  testthat::expect_warning(
    autospc(test_data, plot_chart = TRUE, chart_type = "P")
  )
})

test_that("Charts with show_limits = FALSE behave as expected", {
  result_C <- autospc(test_data2,
    plot_chart = FALSE, chart_type = "C", period_min = 21, show_limits = FALSE
  )
  result_P <- autospc(test_data2,
    plot_chart = FALSE, chart_type = "P", period_min = 21, show_limits = FALSE
  )

  # expect full limits table to be returned regardless of show_limits status,
  # including the four columns describing the periods
  testthat::expect_equal(ncol(result_C), 22)
  testthat::expect_equal(ncol(result_P), 23)

  testthat::expect_true(all(c(
    "limit_change", "period_start", "plot_period",
    "cl_change"
  ) %in% colnames(result_C)))

  # expect no warning even for data passed in with too few points
  testthat::expect_warning(
    autospc(test_data,
      plot_chart = TRUE, chart_type = "C", show_limits = FALSE
    ),
    regexp = NA
  )
  testthat::expect_warning(
    autospc(test_data,
      plot_chart = TRUE, chart_type = "P", show_limits = FALSE
    ),
    regexp = NA
  )
  testthat::expect_warning(
    autospc(test_data,
      plot_chart = TRUE, chart_type = "C", show_limits = FALSE
    ),
    regexp = NA
  )
  testthat::expect_warning(
    autospc(test_data,
      plot_chart = TRUE, chart_type = "P", show_limits = FALSE
    ),
    regexp = NA
  )
})


test_that("the warning is about the input data, not about named charts", {
  # a faceted chart names the stages that are short; a single chart or a pair
  # has only the one series to talk about
  expect_warning(
    autospc(test_data,
      plot_chart = FALSE, chart_type = "XMR",
      period_min = 21L
    ),
    "^The input data has \\d+ points, fewer than the minimum number of points"
  )
})


chart_of_length <- function(rows, chart_type = "C", period_min = 21L) {
  # prepared, because enough_data_for_limits() reads the prepared series
  return(prepare_data(autospc_chart(
    chart_type = chart_type,
    data = data.frame(
      x = seq_len(rows),
      y = rep(c(10L, 12L, 11L), length.out = rows)
    ),
    x = "x",
    y = "y",
    period_min = period_min
  )))
}


test_that("enough_data_for_limits compares the series with period_min", {
  expect_true(enough_data_for_limits(chart_of_length(21L)))

  expect_false(enough_data_for_limits(chart_of_length(20L)))
})


test_that("an MR chart has as much data for limits as its own series", {
  # the moving ranges are one shorter than the series they come from, and
  # n_effective_points() adds that point back
  long_enough <- prepare_data(chart_of_length(21L, chart_type = "MR"))
  one_short <- prepare_data(chart_of_length(20L, chart_type = "MR"))

  expect_true(enough_data_for_limits(long_enough))

  expect_false(enough_data_for_limits(one_short))
})


test_that("a chart with no limits draws the series, not the column supplied", {
  # an MR chart analyses the moving ranges of what the caller passed, so those
  # are what it plots whether or not it has enough points for limits
  values <- data.frame(x = 1:10, y = c(10, 14, 11, 16, 12, 15, 10, 18, 13, 11))

  plot <- suppressWarnings(autospc(values,
    chart_type = "MR", x = "x", y = "y", period_min = 21L
  ))

  drawn <- ggplot2::layer_data(plot, 1)

  expect_identical(drawn$y, moving_ranges(values$y))

  # and the axis is scaled to the moving ranges rather than to the values
  expect_lt(
    autospc_plot_axis_extents(plot, value = "ylimhigh"),
    min(values$y)
  )
})


test_that("a chart with no limits formats its x axis", {
  plot <- suppressWarnings(autospc(test_data,
    chart_type = "C", period_min = 21L, x_break = 2L
  ))

  expect_identical(
    ggplot2::layer_scales(plot)$x$get_breaks(),
    seq(1L, 9L, 2L)
  )
})


test_that("a chart with no limits formats a date x axis", {
  dated <- data.frame(
    x = seq(as.Date("2020-01-01"), by = "month", length.out = 10L),
    y = test_data$y
  )

  plot <- suppressWarnings(autospc(dated,
    chart_type = "C", period_min = 21L, x_break = 90,
    x_date_format = "%b %Y"
  ))

  expect_identical(
    ggplot2::layer_scales(plot)$x$get_labels(),
    c("Jan 2020", "Mar 2020", "Jun 2020", "Sep 2020")
  )
})


test_that("a series with limits takes the limits path", {
  result <- autospc(test_data2,
    plot_chart = FALSE, chart_type = "C",
    period_min = 21
  )

  # the columns that describe the periods are there
  expect_true(all(c("limit_change", "cl_change", "plot_period") %in%
    colnames(result)))
})


test_that("a series without limits has limits columns holding no value", {
  # the columns are there, because they do not depend on the data, and they
  # hold nothing, because no limits were established
  result <- suppressWarnings(
    autospc(test_data, plot_chart = FALSE, chart_type = "C", period_min = 21)
  )

  expect_true("cl" %in% colnames(result))

  expect_true(all(is.na(result$cl)))

  expect_true("limit_change" %in% colnames(result))

  expect_true(all(is.na(result$limit_change)))
})


# The columns a chart returns do not depend on whether the series held enough
# points to form a period


test_that("a short series returns the columns a full one does", {
  set.seed(5)

  sized <- function(rows) {
    return(data.frame(
      x = seq_len(rows),
      y = as.integer(stats::rpois(rows, 50)),
      n = rep(100L, rows)
    ))
  }

  short <- sized(10L)
  full <- sized(40L)

  analyse <- function(data, chart_type) {
    return(suppressWarnings(
      autospc(data,
        chart_type = chart_type,
        x = "x", y = "y", n = "n",
        period_min = 21L,
        plot_chart = FALSE
      )
    ))
  }

  for (chart_type in c("C", "C'", "P", "P'", "X", "MR", "XMR")) {
    from_short <- analyse(short, chart_type)
    from_full <- analyse(full, chart_type)

    expect_identical(names(from_short), names(from_full), info = chart_type)

    # a column of missing values is a different column if it is of a different
    # type, so the types are asserted as well as the names
    expect_identical(
      vapply(from_short, function(column) class(column)[1L], character(1L)),
      vapply(from_full, function(column) class(column)[1L], character(1L)),
      info = chart_type
    )
  }
})


test_that("a short series reports no re-established rows and no exclusions", {
  short <- data.frame(x = 1:10, y = as.integer(c(
    49, 50, 50, 50, 48, 49, 50,
    49, 50, 47
  )))

  chart <- autospc_plot_charts(
    suppressWarnings(autospc(short, chart_type = "C", period_min = 21L))
  )[[1L]]

  expect_identical(chart$result$re_establish_rows, integer(0))

  expect_identical(chart$result$exclusions, integer(0))
})


test_that("the analysed columns a short series gets hold no value", {
  short <- data.frame(x = 1:10, y = as.integer(c(
    49, 50, 50, 50, 48, 49, 50,
    49, 50, 47
  )))

  result <- suppressWarnings(
    autospc(short, chart_type = "C", period_min = 21L, plot_chart = FALSE)
  )

  # limit_extension is not one of them: it says whether the extension put the
  # row there, and none did
  filled <- setdiff(
    names(analysis_column_types()),
    c("sd_estimate", "limit_extension")
  )

  for (column in filled) {
    expect_true(all(is.na(result[[column]])), info = column)
  }

  expect_false(any(result$limit_extension))

  # and the data is there as it was
  expect_identical(result$x, short$x)

  expect_identical(result$y, short$y)
})


test_that("a series with no rows to analyse returns the same columns", {
  # every x is missing, so every row is dropped before the analysis and the
  # table it returns has no rows at all
  previous <- options(autospc.warn_missing_x = FALSE)
  on.exit(options(previous), add = TRUE)

  no_rows <- data.frame(
    x = rep(NA_integer_, 30L),
    y = as.integer(rep(c(10L, 12L), 15L))
  )

  full <- data.frame(
    x = 1:40,
    y = as.integer(rep(c(10L, 12L), 20L))
  )

  from_none <- suppressWarnings(
    autospc(no_rows, chart_type = "C", period_min = 5L, plot_chart = FALSE)
  )

  from_full <- suppressWarnings(
    autospc(full, chart_type = "C", period_min = 5L, plot_chart = FALSE)
  )

  expect_identical(nrow(from_none), 0L)

  expect_identical(names(from_none), names(from_full))
})
