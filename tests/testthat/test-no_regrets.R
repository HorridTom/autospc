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
  output_no_regrets <- autospc::autospc(test_data,
    no_regrets = T, chart_type = "C'", plot_chart = F
  )
  output_no_regrets2 <- autospc::autospc(test_data2,
    no_regrets = T, chart_type = "C'", plot_chart = F
  )
  output_no_regrets3 <- autospc::autospc(test_data3,
    no_regrets = T, chart_type = "C'", plot_chart = F
  )
  output_no_regrets4 <- autospc::autospc(test_data4,
    no_regrets = T, chart_type = "C'", plot_chart = F
  )
  output_no_regrets5 <- autospc::autospc(test_data5,
    no_regrets = T, chart_type = "C'", plot_chart = F
  )

  # expect no breakpoint (no re-establishment)
  testthat::expect_equal(sum(output_no_regrets$break_point, na.rm = T), 0)
  testthat::expect_equal(sum(output_no_regrets2$break_point, na.rm = T), 0)
  testthat::expect_equal(sum(output_no_regrets3$break_point, na.rm = T), 0)
  testthat::expect_equal(sum(output_no_regrets4$break_point, na.rm = T), 0)
  testthat::expect_equal(sum(output_no_regrets5$break_point, na.rm = T), 1)
})


test_that("No regrets = FALSE", {
  output_regrets <- autospc::autospc(test_data,
    no_regrets = F, chart_type = "C'", plot_chart = F
  )
  output_regrets2 <- autospc::autospc(test_data2,
    no_regrets = F, chart_type = "C'", plot_chart = F
  )
  output_regrets3 <- autospc::autospc(test_data3,
    no_regrets = F, chart_type = "C'", plot_chart = F
  )
  output_regrets4 <- autospc::autospc(test_data4,
    no_regrets = F, chart_type = "C'", plot_chart = F
  )
  output_regrets5 <- autospc::autospc(test_data5,
    no_regrets = F, chart_type = "C'", plot_chart = F
  )

  # expect no breakpoint (no re-establishment)
  testthat::expect_equal(sum(output_regrets$break_point, na.rm = T), 1)
  testthat::expect_equal(sum(output_regrets2$break_point, na.rm = T), 1)
  testthat::expect_equal(sum(output_regrets3$break_point, na.rm = T), 1)
  testthat::expect_equal(sum(output_regrets4$break_point, na.rm = T), 0)
  testthat::expect_equal(sum(output_regrets5$break_point, na.rm = T), 1)
})


test_that("a break does not move when a point is added (#308)", {
  # with 43 points, the final run of the candidate calculation period starting
  # at row 22 is unfinished, so limits are not yet re-established
  output_43 <- autospc(example_series_2a,
    x = x, y = y, chart_type = "C'", plot_chart = FALSE
  )

  # a 44th point ends that run, and limits are re-established at row 22
  output_44 <- autospc(
    dplyr::bind_rows(example_series_2a, data.frame(x = 44L, y = 43L)),
    x = x, y = y, chart_type = "C'", plot_chart = FALSE
  )

  expect_identical(which(output_43$break_point), integer(0))
  expect_identical(which(output_44$break_point), 22L)
})


# no_regrets = TRUE with overhanging_reversions = FALSE is an error (#243)


inconsistent_pair <- data.frame(
  x = 1:40,
  y = rep(c(10L, 12L, 11L, 13L, 9L, 14L, 10L, 12L), 5L)
)


test_that("an inconsistent pair is an error for a single chart", {
  expect_error(
    autospc(inconsistent_pair,
      chart_type = "C", period_min = 21L,
      overhanging_reversions = FALSE, plot_chart = FALSE
    ),
    class = "autospc_inconsistent_ssa_parameters_error"
  )
})


test_that("an inconsistent pair is an error for an XmR pair", {
  expect_error(
    autospc(inconsistent_pair,
      chart_type = "XMR", period_min = 21L,
      overhanging_reversions = FALSE, plot_chart = FALSE
    ),
    class = "autospc_inconsistent_ssa_parameters_error"
  )
})


test_that("an inconsistent pair is an error for facet_stages", {
  expect_error(
    facet_stages(inconsistent_pair,
      split_at = c(20L, 40L), chart_type = "C",
      period_min = 21L, overhanging_reversions = FALSE,
      plot_chart = FALSE
    ),
    class = "autospc_inconsistent_ssa_parameters_error"
  )
})


test_that("the error names the caller's call and says what to set", {
  given <- tryCatch(
    autospc(inconsistent_pair,
      chart_type = "C", period_min = 21L,
      overhanging_reversions = FALSE, plot_chart = FALSE
    ),
    autospc_inconsistent_ssa_parameters_error = function(e) e
  )

  expect_identical(rlang::call_name(given$call), "autospc")
  expect_match(
    conditionMessage(given),
    "`no_regrets = TRUE` requires `overhanging_reversions = TRUE`",
    fixed = TRUE
  )
})


test_that("a consistent pair runs and keeps overhanging_reversions = FALSE", {
  plot <- autospc(inconsistent_pair,
    chart_type = "C", period_min = 21L, max_exclusions = 0L,
    no_regrets = FALSE, overhanging_reversions = FALSE
  )

  expect_false(autospc_plot_charts(plot)[[1]]$overhanging_reversions)
})
