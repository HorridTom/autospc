# load in test data
test_data <- readRDS("testdata/test_data_display_limits.rds")

# form_display_limits() forms the limits for the chart's class, so a chart has
# to be supplied. A C chart's display limits are those of the last calculated
# row. The y column of the
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
  correct_answers <- list(
    correct_answer_ucl, correct_answer_cl, correct_answer_lcl
  )

  testthat::expect_equal(results, correct_answers)
})


# a limits table as the algorithm builds it: three calculated rows, then three
# rows waiting for display limits
display_table <- data.frame(
  x = 1:6,
  y = c(10, 12, 11, 14, 9, 13),
  n = rep(20, 6),
  ucl = c(rep(18, 3), rep(NA_real_, 3)),
  lcl = c(rep(4, 3), rep(NA_real_, 3)),
  cl = c(rep(11, 3), rep(NA_real_, 3)),
  sd_estimate = c(rep(7 / 3, 3), rep(NA_real_, 3)),
  period_type = c(
    rep("calculation", 3),
    rep(NA_character_, 3)
  )
)


test_that("display rows take the limits of the last calculated row", {
  chart <- structure(list(), class = c("autospc_chart_c", "autospc_chart"))

  extended <- form_display_limits(display_table, counter = 4, chart = chart)

  expect_equal(extended$ucl, rep(18, 6))
  expect_equal(extended$lcl, rep(4, 6))
  expect_identical(extended$cl, rep(11, 6))
  expect_identical(extended$sd_estimate, rep(7 / 3, 6))
  expect_identical(
    extended$period_type,
    c(rep("calculation", 3), rep("display", 3))
  )
})


test_that("the calculated rows are left alone", {
  chart <- structure(list(), class = c("autospc_chart_c", "autospc_chart"))

  extended <- form_display_limits(display_table, counter = 4, chart = chart)

  expect_identical(extended[1:3, ], display_table[1:3, ])
})


test_that("every class without a denominator gives display rows one limit", {
  for (class in c("c", "cp", "x", "mr")) {
    chart <- structure(list(),
      class = c(paste0("autospc_chart_", class), "autospc_chart")
    )

    extended <- form_display_limits(display_table, counter = 4, chart = chart)

    expect_equal(extended$ucl, rep(18, 6), info = class)
  }
})
