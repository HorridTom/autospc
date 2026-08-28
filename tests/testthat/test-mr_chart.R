library(testthat)

# load test data
mr_data <- readRDS("testdata/test_mr_data.rds")
extreme_mr_data <- readRDS("testdata/test_mr_data.rds")

# Correct answer created using:
# test_mr_limit_answer <- qicharts2::qic(y, data = mr_data, chart = 'mr',
#                                         return.data = TRUE)
# qicharts2 v.0.7.2
test_mr_limit_answer <- readRDS(file.path(
  "testdata",
  "test_mr_limit_answer.rds"
))

# test that mr control limits match those from qicharts2 v.0.7.2
# when mr_screen_max_loops = 0
test_that("mR chart limits the same as qicharts2 v.0.7.2", {
  mrs <- moving_ranges(y = mr_data$y)
  results <- get_mr_limits(
    mr = mrs,
    mr_screen_max_loops = 0
  )

  expect_equal(results$cl, test_mr_limit_answer$cl)
  expect_equal(results$ucl, test_mr_limit_answer$ucl)
  expect_equal(results$lcl, rlang::rep_along(test_mr_limit_answer$ucl, 0))
  expect_equal(results$mr, test_mr_limit_answer$y)
})

test_that("mR chart created without error", {
  expect_no_error(
    chart <- autospc(mr_data,
      chart_type = "MR",
      plot_chart = TRUE
    )
  )
})

test_that("mr_screen_max_loops makes no difference to mr chart limits", {
  results_table_0 <- autospc(extreme_mr_data,
    chart_type = "MR",
    plot_chart = FALSE,
    mr_screen_max_loops = 0
  )

  results_table_1 <- autospc(extreme_mr_data,
    chart_type = "MR",
    plot_chart = FALSE,
    mr_screen_max_loops = 1
  )

  results_table_inf <- autospc(extreme_mr_data,
    chart_type = "MR",
    plot_chart = FALSE,
    mr_screen_max_loops = Inf
  )

  expect_equal(results_table_1, results_table_0)
  expect_equal(results_table_inf, results_table_0)
})


test_that("an MR chart gets limits from the same number of points as an X chart", {
  # 21 values give 20 moving ranges, so without the +1 in
  # n_effective_points.autospc_chart_mr() an MR chart would be refused limits
  # at exactly period_min points while the X chart it came from is not
  just_enough <- data.frame(x = 1:21, y = rep(c(4, 7, 5), 7))
  one_short <- data.frame(x = 1:20, y = rep(c(4, 7, 5, 6), 5))

  expect_warning(
    autospc(just_enough,
      chart_type = "MR", plot_chart = FALSE,
      period_min = 21
    ),
    regexp = NA
  )

  expect_warning(
    autospc(one_short,
      chart_type = "MR", plot_chart = FALSE,
      period_min = 21
    ),
    "fewer than the minimum"
  )
})


test_that("an MR chart analyses the moving ranges, not the values passed in", {
  # prepare_data.autospc_chart_mr() replaces y with the moving ranges. Without
  # it the algorithm would run on the raw series and the returned y would be
  # what was passed in.
  values <- data.frame(x = 1:25, y = rep(c(10, 14, 11, 16, 12), 5))

  result <- autospc(values,
    chart_type = "MR", plot_chart = FALSE,
    period_min = 21
  )

  expect_identical(result$y, moving_ranges(values$y))

  # and the centre line is the mean moving range, not the mean value
  expect_lt(result$cl[21], mean(values$y))
})
