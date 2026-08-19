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

  expect_identical(class(run_returns()),
                   c("autospc_plot", "gg", "ggplot"))

})


test_that("the returned plot carries the fitted chart", {

  charts <- autospc_plot_charts(run_returns())

  expect_length(charts, 1L)

  expect_s3_class(charts[[1]], "autospc_chart_c")

})


test_that("the chart it carries has been fitted, not just built", {

  chart <- autospc_plot_charts(run_returns())[[1]]

  expect_true("cl" %in% colnames(chart$result$table))

  expect_gt(length(chart$history$candidates) +
              NROW(chart$history$breaks) +
              length(chart$history$stopped),
            0L)

})


test_that("the returned plot carries the presentation parameters", {

  plot <- run_returns(point_size = 4, r1_col = "red")

  expect_identical(autospc_plot_presentation(plot, "point_size"), 4)

  expect_identical(autospc_plot_presentation(plot, "r1_col"), "red")

})


test_that("the presentation records resolved values, not the arguments", {

  # override_y_title is NULL as passed; postprocess() fills it from the class
  plot <- run_returns()

  expect_false(is.null(autospc_plot_presentation(plot, "override_y_title")))

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

  expect_false(autospc_plot_presentation(plot, "show_limits"))

})


test_that("an XMR run returns an autospc_plot", {

  # the pair is drawn, but only the X chart is carried: the MR chart is fitted
  # inside the chart_type = "MR" re-invocation, which returns a plot rather
  # than a chart
  plot <- suppressWarnings(
    autospc(returns_data, chart_type = "XMR", period_min = 21L)
  )

  expect_s3_class(plot, "autospc_plot")

  expect_length(autospc_plot_charts(plot), 1L)

})


test_that("plot_chart = FALSE still returns a data frame", {

  result <- run_returns(plot_chart = FALSE)

  expect_s3_class(result, "data.frame")

  expect_false(inherits(result, "autospc_plot"))

})
