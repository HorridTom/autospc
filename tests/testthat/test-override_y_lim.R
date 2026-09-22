# The ends of the vertical axis a caller can set, what the axis does with a
# value that would hide a data point, and the zooming that replaced clipping.

axis_series <- function() {
  set.seed(3)

  data <- data.frame(x = 1:40, n = rep(60L, 40L))
  data$y <- as.integer(stats::rbinom(40L, 60L, 0.5))

  return(data)
}


charted <- function(...) {
  return(suppressWarnings(autospc(axis_series(),
    chart_type = "P", x = x, y = y, n = n, period_min = 21L, ...
  )))
}


panel_range <- function(plot) {
  built <- suppressWarnings(ggplot2::ggplot_build(plot))

  return(built$layout$panel_params[[1L]]$y.range)
}


rows_drawn <- function(plot) {
  built <- suppressWarnings(ggplot2::ggplot_build(plot))

  return(vapply(built$data, function(layer) {
    if (!"y" %in% names(layer)) {
      return(0L)
    }

    return(sum(!is.na(layer$y)))
  }, integer(1)))
}


test_that("a single number is the upper end, as it was before", {
  expect_equal(panel_range(charted(override_y_lim = 65))[2L], 65 * 1.05)
})


test_that("two numbers are the lower and upper ends", {
  expect_equal(panel_range(charted(override_y_lim = c(20, 80))), c(17, 83))
})


test_that("NA leaves that end as the chart would have set it", {
  ends <- function(...) {
    return(unlist(autospc_plot_axis_extents(charted(...))[
      c("ylimlow", "ylimhigh")
    ]))
  }

  own <- ends()

  # the panel is the range plus ggplot's expansion of it, so a range with one
  # end moved expands differently at the other; the ends themselves are what
  # this is about
  expect_identical(ends(override_y_lim = c(NA, 65)), ends(override_y_lim = 65))
  expect_identical(
    ends(override_y_lim = c(20, NA)),
    c(ylimlow = 20, ylimhigh = own[["ylimhigh"]])
  )
  expect_identical(
    ends(override_y_lim = c(NA, 65)),
    c(ylimlow = own[["ylimlow"]], ylimhigh = 65)
  )
})


test_that("a range that would hide a data point is an error", {
  expect_error(
    charted(override_y_lim = 50),
    "leaves 18 of 40 data points outside the vertical axis"
  )
  expect_error(
    charted(override_y_lim = c(40, 80)),
    "leaves 2 of 40 data points outside the vertical axis"
  )
})


test_that("the error names the argument and the range the series needs", {
  expect_error(
    charted(override_y_lim = 50),
    "`override_y_lim` of 0 to 50"
  )
  expect_error(
    charted(override_y_lim = 50),
    "The series runs from 33.33 to 60"
  )
})


test_that("a limit outside the axis is kept rather than dropped", {
  table <- charted(override_y_lim = 65, plot_chart = FALSE)

  # the upper limit is above the axis, so it is outside the panel; the rows are
  # still drawn, which is what zooming gives and clipping did not
  expect_gt(max(table$ucl, na.rm = TRUE), 65)
  expect_identical(
    rows_drawn(charted(override_y_lim = 65)),
    rows_drawn(charted())
  )
})


test_that("the axis a chart sets for itself is unchanged", {
  expect_equal(panel_range(charted()), c(-5.5, 115.5))
})


test_that("override_y_lim rejects what is not one or two finite numbers", {
  # NA and c(NA, NA) are logical, so they are refused for not being numeric.
  # NA_real_ is numeric, and is refused for leaving both ends unset.
  refused <- list(
    "banana", c(1, 2, 3), c(NA, NA), NA, c(80, 20), numeric(0),
    NA_real_, c(NA_real_, NA_real_)
  )

  for (value in refused) {
    expect_error(charted(override_y_lim = value), "`override_y_lim` must be")
  }
})
