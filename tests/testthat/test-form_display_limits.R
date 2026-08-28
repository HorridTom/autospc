# load in test data
test_data <- readRDS("testdata/test_data_display_limits.rds")

# form_display_limits() dispatches on the chart object, so one has to be
# supplied. A C chart carries its limits forward unchanged. The y column of the
# fixture is not whole numbers, so constructing the chart rounds it and warns;
# the warning is suppressed here because chart$data is not used by the tests,
# which pass test_data to form_display_limits() directly.
test_chart <- suppressWarnings(
  autospc_chart_c(data = test_data, x = "x", y = "y")
)

test_that("Display period is formed correctly for C chart", {
  result <- form_display_limits(test_data, counter = 22, chart = test_chart)
  result_ucl <- result$ucl[22:nrow(test_data)]
  result_cl <- result$cl[22:nrow(test_data)]
  result_lcl <- result$lcl[22:nrow(test_data)]
  results <- list(result_ucl, result_cl, result_lcl)

  correct_answer_ucl <- rep(test_data$ucl[21], nrow(test_data) - 21)
  correct_answer_cl <- rep(test_data$cl[21], nrow(test_data) - 21)
  correct_answer_lcl <- rep(test_data$lcl[21], nrow(test_data) - 21)
  correct_answers <- list(correct_answer_ucl, correct_answer_cl, correct_answer_lcl)

  testthat::expect_equal(results, correct_answers)
})
