library(testthat)

# load in test data
test_data <- readRDS("testdata/test_individual_data.rds")
# Correct answer created using:
# test_individual_answer <- qicharts2::qic(x, y, data = test_data, chart = 'i',
#                                           return.data = TRUE)
# qicharts2 v.0.7.2
test_individual_answer <- readRDS(file.path(
  "testdata",
  "test_individual_answer.rds"
))

# test that the X chart limits is the same as qicharts2 results
test_that("X chart limits the same as live qicharts2 v.0.7.2", {
  # qicharts2 uses the published rounded constants, so the agreement holds
  # under the option that selects them
  previous <- options(autospc.rounded_constants = TRUE)
  on.exit(options(previous))

  chart <- autospc_chart_x(data = test_data, x = "x", y = "y")

  statistics <- get_x_limits(
    y = test_data$y,
    mr_screen_max_loops = 1
  )
  limits <- limits_from_statistics(chart, statistics, test_data)

  expect_equal(statistics$cl, test_individual_answer$cl)
  expect_equal(limits$lcl, test_individual_answer$lcl)
  expect_equal(limits$ucl, test_individual_answer$ucl)
})

# test that moving range limits that are above ucl_mr are removed when
# mr_screen_max_loops = 1
extreme_mr_data <- readRDS("testdata/test_mr_data.rds")
# Correct answer created using:
# test_mr_answer <- qicharts2::qic(x, y, data = extreme_mr_data, chart = 'i',
#                                   return.data = TRUE)
# qicharts2 v.0.7.2
test_mr_answer <- readRDS(file.path(
  "testdata",
  "test_mr_answer.rds"
))

test_that("X chart limits with mr screening remove extreme moving ranges", {
  # qicharts2 uses the published rounded constants, so the agreement holds
  # under the option that selects them
  previous <- options(autospc.rounded_constants = TRUE)
  on.exit(options(previous))

  chart <- autospc_chart_x(data = extreme_mr_data, x = "x", y = "y")

  statistics <- get_x_limits(
    y = extreme_mr_data$y,
    mr_screen_max_loops = 1
  )
  limits <- limits_from_statistics(chart, statistics, extreme_mr_data)

  expect_equal(statistics$cl, test_mr_answer$cl)
  expect_equal(limits$lcl, test_mr_answer$lcl)
  expect_equal(limits$ucl, test_mr_answer$ucl)
})

# ...and that they are not when mr_screen_max_loops = 0
test_that("X chart limits correct without mr screening", {
  chart <- autospc_chart_x(data = extreme_mr_data, x = "x", y = "y")

  statistics <- get_x_limits(
    y = extreme_mr_data$y,
    mr_screen_max_loops = 0
  )
  results <- c(
    statistics,
    limits_from_statistics(chart, statistics, extreme_mr_data)
  )

  expect_equal(results$cl,
    rep(
      85.55363,
      length(results$cl)
    ),
    tolerance = 1e-3
  )
  expect_equal(results$lcl,
    rep(
      45.0676,
      length(results$lcl)
    ),
    tolerance = 1e-3
  )
  expect_equal(results$ucl,
    rep(
      126.0396,
      length(results$ucl)
    ),
    tolerance = 1e-3
  )
})
