returns_data <- data.frame(
  x = 1:30,
  y = c(10, 12, 11, 13, 9, 14, 10, 11, 12, 13,
        10, 12, 11, 13, 9, 14, 10, 11, 12, 13,
        10, 12, 11, 13, 9, 14, 10, 11, 12, 13),
  n = rep(100L, 30)
)

returns_short <- returns_data[1:10, ]

run_returns <- function(...) {

  suppressWarnings(
    autospc(returns_data, chart_type = "C", period_min = 21L, ...)
  )

}


test_that("autospc returns an autospc_plot", {

  expect_s3_class(run_returns(), "autospc_plot")

})


test_that("what autospc returns is still a ggplot", {

  expect_identical(class(run_returns())[1], "autospc_plot")

  expect_s3_class(run_returns(), "gg")

  expect_s3_class(run_returns(), "ggplot")

})


test_that("the returned plot carries the analysed chart", {

  charts <- autospc_plot_charts(run_returns())

  expect_length(charts, 1L)

  expect_s3_class(charts[[1]], "autospc_chart_c")

})


test_that("the chart it carries has been analysed, not just built", {

  chart <- autospc_plot_charts(run_returns())[[1]]

  expect_true("cl" %in% colnames(chart$result$table))

  expect_gt(length(chart$history$candidates) +
              NROW(chart$history$breaks) +
              length(chart$history$stopped),
            0L)

})


test_that("the returned plot carries the presentation parameters", {

  plot <- run_returns(point_size = 4, r1_col = "red")

  expect_identical(autospc_plot_visualisation_params(plot, "point_size"), 4)

  expect_identical(autospc_plot_visualisation_params(plot, "r1_col"), "red")

})


test_that("the presentation records resolved values, not the arguments", {

  # override_y_title is NULL as passed; axis_specifications() fills it from the
  # class
  plot <- run_returns()

  expect_false(is.null(autospc_plot_visualisation_params(plot,
                                                         "override_y_title")))

})


test_that("a chart has no x axis title unless one is given", {

  expect_null(run_returns()$labels$x)

  expect_identical(run_returns(override_x_title = "Point")$labels$x, "Point")

})


test_that("a series drawn without limits has no x axis title either", {

  plot <- suppressWarnings(
    autospc(returns_short, chart_type = "C", period_min = 21L)
  )

  expect_null(plot$labels$x)

})


test_that("a chart with too few points for limits is still an autospc_plot", {

  plot <- suppressWarnings(
    autospc(returns_short, chart_type = "C", period_min = 21L)
  )

  expect_s3_class(plot, "autospc_plot")

  expect_length(autospc_plot_charts(plot), 1L)

})


test_that("show_limits = FALSE is still an autospc_plot", {

  plot <- run_returns(show_limits = FALSE)

  expect_s3_class(plot, "autospc_plot")

  expect_false(autospc_plot_visualisation_params(plot, "show_limits"))

})


test_that("an XMR run returns an autospc_plot", {

  plot <- suppressWarnings(
    autospc(returns_data, chart_type = "XMR", period_min = 21L)
  )

  expect_s3_class(plot, "autospc_plot")

})


test_that("an XMR run carries both charts of the pair, X first", {

  charts <- autospc_plot_charts(suppressWarnings(
    autospc(returns_data, chart_type = "XMR", period_min = 21L)
  ))

  expect_length(charts, 2L)

  expect_s3_class(charts[[1]], "autospc_chart_x")

  expect_s3_class(charts[[2]], "autospc_chart_mr")

})


test_that("both charts of the pair have been analysed", {

  charts <- autospc_plot_charts(suppressWarnings(
    autospc(returns_data, chart_type = "XMR", period_min = 21L)
  ))

  expect_true(all(vapply(charts,
                         function(chart) "cl" %in% colnames(chart$result$table),
                         logical(1))))

})


test_that("plot_chart = FALSE still returns a data frame", {

  result <- run_returns(plot_chart = FALSE)

  expect_s3_class(result, "data.frame")

  expect_false(inherits(result, "autospc_plot"))

})


test_that("the returned plot carries the values it was drawn with", {

  plot <- run_returns()

  expect_setequal(names(autospc_plot_axis_extents(plot)),
                  c("start_x", "x_max", "end_x", "ylimlow", "ylimhigh"))

  expect_identical(autospc_plot_axis_extents(plot, "start_x"), 1L)

  expect_identical(autospc_plot_axis_extents(plot, "x_max"), 30L)

})


test_that("the y limits recorded are the ones the class asks for", {

  # y_axis_range.autospc_chart_c() starts the axis at zero
  expect_identical(autospc_plot_axis_extents(run_returns(), "ylimlow"), 0)

})


test_that("extend_limits_to reaches the recorded end of the x axis", {

  plot <- run_returns(extend_limits_to = 40)

  expect_identical(autospc_plot_axis_extents(plot, "x_max"), 30L)

  expect_identical(autospc_plot_axis_extents(plot, "end_x"), 40)

})


test_that("x_pad_end wins over extend_limits_to for the end of the x axis", {

  plot <- run_returns(extend_limits_to = 40, x_pad_end = 50)

  expect_identical(autospc_plot_axis_extents(plot, "end_x"), 50)

})


test_that("override_y_lim reaches the recorded y limit", {

  plot <- run_returns(override_y_lim = 40)

  expect_identical(autospc_plot_axis_extents(plot, "ylimhigh"), 40)

})


# as.data.frame()

test_that("as.data.frame returns the analysis", {

  plot <- run_returns()

  result <- as.data.frame(plot)

  expect_s3_class(result, "data.frame")

  expect_identical(result,
                   as.data.frame(autospc_plot_charts(plot)[[1]]$result$table))

})


test_that("as.data.frame carries the columns describing the periods", {

  result <- as.data.frame(run_returns())

  expect_true(all(c("limit_change", "period_start", "plot_period", "cl_change")
                  %in% colnames(result)))

})


test_that("as.data.frame does not carry the drawing columns", {

  result <- as.data.frame(run_returns())

  expect_false(any(c("annotation_level", "annotation_curvature", "cl_label")
                   %in% colnames(result)))

})


test_that("as.data.frame joins an XmR pair wide", {

  plot <- suppressWarnings(
    autospc(returns_data, chart_type = "XMR", period_min = 21L)
  )

  result <- as.data.frame(plot)

  expect_identical(nrow(result), 30L)

  expect_true(all(c("mr", "amr", "url", "lrl") %in% colnames(result)))

  expect_false("stage" %in% colnames(result))

})


test_that("the wide join carries the moving ranges, not a second y column", {

  plot <- suppressWarnings(
    autospc(returns_data, chart_type = "XMR", period_min = 21L)
  )

  result <- as.data.frame(plot)

  mr_chart <- autospc_plot_charts(plot)[[2]]

  expect_identical(result$mr, mr_chart$result$table$y)

  expect_identical(result$y, autospc_plot_charts(plot)[[1]]$result$table$y)

})


test_that("as.data.frame identifies the stage when there is more than one", {

  chart <- autospc_plot_charts(run_returns())[[1]]

  two <- validate_autospc_plot(
    new_autospc_plot(
      plot = ggplot2::ggplot(returns_data, ggplot2::aes(x = x, y = y)),
      charts = list(chart, chart),
      presentation = list(visualisation_params = list(), axis_extents = list())
    )
  )

  result <- as.data.frame(two)

  expect_true("stage" %in% colnames(result))

  expect_setequal(unique(result$stage), c("1", "2"))

})
