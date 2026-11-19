# Charts with no variation, short series, and charts whose limits are all
# negative are drawn on a y axis that contains the data, with centre line
# labels beyond the limits (#314).

zeros <- data.frame(x = 1:40, y = 0L)
zero_rates <- data.frame(x = 1:40, y = 0L, n = 10L)

y_axis_of <- function(plot) {
  return(c(
    autospc_plot_axis_extents(plot, value = "ylimlow"),
    autospc_plot_axis_extents(plot, value = "ylimhigh")
  ))
}


test_that("a C' chart of zeros is drawn on an axis from 0 to 10, with no warning", {
  expect_no_warning(plot <- autospc(zeros, chart_type = "C'"))
  expect_no_warning(drawn(plot))

  expect_identical(y_axis_of(plot), c(0, 10))
})


test_that("a chart of zeros whose range has no width is drawn from 0 to 1", {
  expect_identical(y_axis_of(autospc(zero_rates, chart_type = "U")), c(0, 1))
  expect_identical(y_axis_of(autospc(zero_rates, chart_type = "U'")), c(0, 1))
  expect_identical(y_axis_of(autospc(zeros, chart_type = "XMR")), c(0, 1))
  expect_identical(
    y_axis_of(autospc(data.frame(x = rep(1:40, each = 3L), y = 0),
      chart_type = "XbarS"
    )),
    c(0, 1)
  )
})


test_that("a short series is drawn on the chart type's own axis", {
  short <- data.frame(x = 1:10, y = rep(c(10L, 12L), 5L))
  short_proportions <- data.frame(x = 1:10, y = rep(c(8L, 9L), 5L), n = 10L)

  expect_warning(plot_c <- autospc(short, chart_type = "C"), "fewer than")
  expect_warning(plot_x <- autospc(short, chart_type = "X"), "fewer than")
  expect_warning(
    plot_p <- autospc(short_proportions, chart_type = "P"),
    "fewer than"
  )

  expect_identical(y_axis_of(plot_c), c(0, 12 + 10))
  expect_identical(y_axis_of(plot_x), c(10 * 0.9, 12 * 1.1))
  expect_identical(y_axis_of(plot_p), c(0, 110))
})


test_that("an X chart with negative limits is drawn, its axis above the data", {
  set.seed(4)
  negative <- data.frame(x = 1:40, y = stats::rnorm(40, mean = -20))

  expect_no_error(plot <- autospc(negative, chart_type = "X"))
  expect_no_warning(drawn(plot))

  table <- autospc(negative, chart_type = "X", plot_chart = FALSE)
  expect_gt(y_axis_of(plot)[[2]], max(table$ucl, table$y, na.rm = TRUE))
  expect_lt(y_axis_of(plot)[[1]], min(table$lcl, table$y, na.rm = TRUE))
})


test_that("an Xbar chart with negative limits is drawn, its axis above the data", {
  set.seed(5)
  negative <- data.frame(
    x = rep(1:40, each = 3L),
    y = stats::rnorm(120, mean = -20)
  )

  expect_no_error(plot <- autospc(negative, chart_type = "XbarS", max_exclusions = 0L))
  expect_no_warning(drawn(plot))

  table <- autospc(negative,
    chart_type = "Xbar", max_exclusions = 0L,
    plot_chart = FALSE
  )
  expect_gt(y_axis_of(plot)[[2]], max(table$ucl, na.rm = TRUE))
})


test_that("a centre line label is outside a limit of either sign", {
  expect_equal(
    label_position(c(10, -10, 0, NA), scale_factor = 1.1),
    c(11, -9, 0, NA)
  )
  expect_equal(
    label_position(c(10, -10), scale_factor = 0.9),
    c(9, -11)
  )
})


test_that("the label of an X chart with negative limits is above the upper limit", {
  set.seed(4)
  negative <- data.frame(x = 1:40, y = stats::rnorm(40, mean = -20))
  plot <- autospc(negative, chart_type = "X")
  table <- autospc(negative, chart_type = "X", plot_chart = FALSE)

  levels <- plot$data$annotation_level[
    plot$data$plotted_line == "cl" &
      !is.na(plot$data$cl_label) &
      plot$data$cl_label != ""
  ]

  expect_length(levels, 1L)
  expect_gt(levels, max(table$ucl, na.rm = TRUE))
  # at the top of the axis, as on an X chart with positive limits
  expect_equal(levels, autospc_plot_axis_extents(plot, value = "ylimhigh"))
})
