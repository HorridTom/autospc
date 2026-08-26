# print() on a chart summarises the analysis. The drawing is what an
# autospc_plot does.

print_data <- data.frame(
  x = 1:60,
  y = c(rep(c(10, 12, 11, 13, 9, 14, 10, 11, 12, 13), 3),
        rep(c(20, 22, 21, 23, 19, 24, 20, 21, 22, 23), 3))
)

analysed_print_chart <- function(...) {

  plot <- suppressWarnings(
    autospc(print_data, chart_type = "C", period_min = 21L, ...)
  )

  autospc_plot_charts(plot)[[1]]

}


test_that("print names the class, the chart type and the size", {

  expect_output(print(analysed_print_chart()),
                "<autospc_chart_c> C chart, 60 points, period_min = 21",
                fixed = TRUE)

})


test_that("print lists the calculation periods with their centre lines", {

  chart <- analysed_print_chart()

  first_cl <- format(signif(chart$result$table$cl[1], 6))

  expect_output(print(chart), "Calculation periods")

  expect_output(print(chart), paste0("rows +1- +21 +cl = ", first_cl))

})


test_that("print reports where limits were re-established", {

  chart <- analysed_print_chart()

  expect_output(print(chart),
                paste("Limits re-established at",
                      paste(chart$result$re_establish_rows, collapse = ", ")))

})


test_that("print reports the candidates, singular when there is one", {

  expect_output(print(analysed_print_chart()),
                "1 candidate period considered, 1 accepted",
                fixed = TRUE)

})


test_that("print omits the exclusions line when nothing was excluded", {

  chart <- analysed_print_chart()

  expect_length(chart$result$exclusions, 0L)

  expect_no_match(paste(capture.output(print(chart)), collapse = " "),
                  "excluded")

})


test_that("print reports exclusions and candidates in the plural", {

  plot <- suppressWarnings(
    autospc(ed_attendances_monthly, chart_type = "C'", x = month_start,
            y = att_all, period_min = 21L)
  )

  chart <- autospc_plot_charts(plot)[[1]]

  expect_output(print(chart),
                "9 points excluded from the limit calculations",
                fixed = TRUE)

  expect_output(print(chart),
                "3 candidate periods considered, 3 accepted",
                fixed = TRUE)

})


test_that("print reports why the run stopped", {

  chart <- analysed_print_chart()

  expect_output(print(chart),
                paste0("Stopped at row ", chart$history$stopped$counter, ": ",
                       chart$history$stopped$reason))

})


test_that("print says so when the chart has not been analysed", {

  unanalysed <- autospc_chart(chart_type = "C",
                              data = print_data,
                              x = "x",
                              y = "y")

  expect_output(print(unanalysed), "Not analysed", fixed = TRUE)

})


test_that("print says so when there were too few points for limits", {

  short <- suppressWarnings(
    autospc(print_data[1:10, ], chart_type = "C", period_min = 21L)
  )

  expect_output(print(autospc_plot_charts(short)[[1]]),
                "No limits: too few points",
                fixed = TRUE)

})


test_that("print returns the chart invisibly", {

  chart <- analysed_print_chart()

  expect_output(returned <- print(chart))

  expect_identical(returned, chart)

  expect_false(withVisible(print(chart))$visible)

})


# format_calculation_periods()

test_that("a long analysis is truncated rather than filling the console", {

  many <- data.frame(period_type = rep("calculation", 30),
                     plot_period = paste0("calculation", 1:30),
                     cl = seq_len(30))

  lines <- format_calculation_periods(many, max_shown = 4L)

  expect_length(lines, 5L)

  expect_match(lines[5], "and 26 more")

})


test_that("display periods are not listed as calculation periods", {

  table <- data.frame(period_type = c("calculation", "calculation", "display"),
                      plot_period = c("calculation1", "calculation1",
                                      "display3"),
                      cl = c(10, 10, 10))

  expect_length(format_calculation_periods(table), 1L)

})
