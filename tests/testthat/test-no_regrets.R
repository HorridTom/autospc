# data ends at end of second calculation period
test_data <- structure(
  list(
    x = 1:51,
    y = c(
      15L,
      23L, 22L, 18L, 18L,
      13L, 22L, 20L, 25L,
      20L, 14L, 19L, 19L,
      19L, 21L, 24L, 18L,
      25L, 15L, 17L, 23L,
      17L, 16L, 19L, 20L,
      22L, 17L, 24L, 17L,
      21L, 29L, 27L, 26L,
      23L, 23L, 22L, 28L,
      27L, 25L, 31L, 24L,
      25L, 31L, 21L, 23L,
      26L,
      20L, 19L, 20L, 18L, 21L
    )
  ),
  row.names = c(NA, 51L), class = "data.frame"
)

# point added below line in display period
test_data2 <- dplyr::add_row(test_data, x = 52, y = 18)
test_data3 <- dplyr::add_row(test_data2, x = 53, y = 16)

# eighth rule break point
test_data4 <- dplyr::add_row(test_data3, x = 54, y = 17)

# above the line scenario - stops rule break
test_data5 <- dplyr::add_row(test_data3, x = 54, y = 27)

test_that("No regrets = TRUE", {
  output_no_regrets <- autospc::autospc(test_data, no_regrets = T, chart_type = "C'", plot_chart = F)
  output_no_regrets2 <- autospc::autospc(test_data2, no_regrets = T, chart_type = "C'", plot_chart = F)
  output_no_regrets3 <- autospc::autospc(test_data3, no_regrets = T, chart_type = "C'", plot_chart = F)
  output_no_regrets4 <- autospc::autospc(test_data4, no_regrets = T, chart_type = "C'", plot_chart = F)
  output_no_regrets5 <- autospc::autospc(test_data5, no_regrets = T, chart_type = "C'", plot_chart = F)

  # expect no breakpoint (no re-establishment)
  testthat::expect_equal(sum(output_no_regrets$break_point, na.rm = T), 0)
  testthat::expect_equal(sum(output_no_regrets2$break_point, na.rm = T), 0)
  testthat::expect_equal(sum(output_no_regrets3$break_point, na.rm = T), 0)
  testthat::expect_equal(sum(output_no_regrets4$break_point, na.rm = T), 0)
  testthat::expect_equal(sum(output_no_regrets5$break_point, na.rm = T), 1)
})


test_that("No regrets = FALSE", {
  output_regrets <- autospc::autospc(test_data, no_regrets = F, chart_type = "C'", plot_chart = F)
  output_regrets2 <- autospc::autospc(test_data2, no_regrets = F, chart_type = "C'", plot_chart = F)
  output_regrets3 <- autospc::autospc(test_data3, no_regrets = F, chart_type = "C'", plot_chart = F)
  output_regrets4 <- autospc::autospc(test_data4, no_regrets = F, chart_type = "C'", plot_chart = F)
  output_regrets5 <- autospc::autospc(test_data5, no_regrets = F, chart_type = "C'", plot_chart = F)

  # expect no breakpoint (no re-establishment)
  testthat::expect_equal(sum(output_regrets$break_point, na.rm = T), 1)
  testthat::expect_equal(sum(output_regrets2$break_point, na.rm = T), 1)
  testthat::expect_equal(sum(output_regrets3$break_point, na.rm = T), 1)
  testthat::expect_equal(sum(output_regrets4$break_point, na.rm = T), 0)
  testthat::expect_equal(sum(output_regrets5$break_point, na.rm = T), 1)
})


# no_regrets and overhanging_reversions are resolved once per call


inconsistent_pair <- data.frame(
  x = 1:40,
  y = rep(c(10L, 12L, 11L, 13L, 9L, 14L, 10L, 12L), 5L)
)

count_pair_warnings <- function(result) {
  warnings_given <- character()

  withCallingHandlers(
    force(result),
    warning = function(w) {
      warnings_given <<- c(warnings_given, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  sum(grepl("does not make sense", warnings_given, fixed = TRUE))
}


test_that("an inconsistent pair is warned about once for a single chart", {
  count <- count_pair_warnings(
    autospc(inconsistent_pair,
      chart_type = "C", period_min = 21L,
      overhanging_reversions = FALSE, plot_chart = FALSE
    )
  )

  expect_identical(count, 1L)
})


test_that("an XmR pair is warned about once, not once per chart", {
  count <- count_pair_warnings(
    autospc(inconsistent_pair,
      chart_type = "XMR", period_min = 21L,
      overhanging_reversions = FALSE, plot_chart = FALSE
    )
  )

  expect_identical(count, 1L)
})


test_that("a faceted chart is warned about once, not once per facet", {
  count <- count_pair_warnings(
    facet_stages(inconsistent_pair,
      split_rows = c(20L, 40L), chart_type = "C",
      period_min = 21L, overhanging_reversions = FALSE,
      plot_chart = FALSE
    )
  )

  expect_identical(count, 1L)
})


test_that("the resolved value is what the chart carries", {
  plot <- suppressWarnings(
    autospc(inconsistent_pair,
      chart_type = "C", period_min = 21L,
      overhanging_reversions = FALSE
    )
  )

  expect_true(autospc_plot_charts(plot)[[1]]$overhanging_reversions)
})


test_that("a consistent pair is left alone and doesn't warn of inconsistency", {
  count <- count_pair_warnings(
    plot <- autospc(inconsistent_pair,
      chart_type = "C", period_min = 21L,
      no_regrets = FALSE, overhanging_reversions = FALSE
    )
  )

  expect_identical(count, 0L)

  expect_false(autospc_plot_charts(plot)[[1]]$overhanging_reversions)
})
