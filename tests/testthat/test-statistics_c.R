test_data <- structure(
  list(
    x = 1:21,
    y = c(
      55L, 42L, 46L, 50L, 49L, 53L, 44L, 46L, 46L,
      54L, 48L, 56L, 47L, 43L, 57L, 42L, 55L, 51L,
      52L, 55L, 48L
    ),
    n = c(
      197L, 196L, 198L, 209L, 201L, 199L, 202L,
      195L, 205L, 191L, 197L, 207L, 196L, 195L,
      205L, 197L, 206L, 197L, 183L, 195L, 192L
    )
  ),
  row.names = c(NA, -21L), class = "data.frame"
)

# Correct answer created using:
# test_c_limit_answer <- qicharts2::qic(x, y, data = test_data, chart = 'c',
#                                         return.data = TRUE)
# qicharts2 v.0.7.2
test_c_limit_answer <- readRDS(file.path(
  "testdata",
  "test_c_limit_answer.rds"
))

test_that("C chart limits the same as qicharts2 v.0.7.2", {
  chart <- autospc_chart_c(data = test_data, x = "x", y = "y")

  statistics <- get_c_statistics(y = test_data$y)
  limits <- limits_from_statistics(chart, statistics, test_data)

  expect_equal(statistics$cl, test_c_limit_answer$cl)
  expect_equal(limits$lcl, test_c_limit_answer$lcl)
  expect_equal(limits$ucl, test_c_limit_answer$ucl)
})


# Correct answer created using:
# test_cp_limit_answer <- qicharts2::qic(x, y, n = rep(1, nrow(test_data)),
#                                         data = test_data, chart = 'up',
#                                         return.data = TRUE)
# qicharts2 v.0.7.2
test_cp_limit_answer <- readRDS(file.path(
  "testdata",
  "test_cp_limit_answer.rds"
))

test_that("C prime chart limits the same as qicharts2 v.0.7.2", {
  # qicharts2 uses the published rounded constants, so the agreement holds
  # under the option that selects them
  previous <- options(autospc.rounded_constants = TRUE)
  on.exit(options(previous))

  chart <- autospc_chart_cp(data = test_data, x = "x", y = "y")

  statistics <- get_cp_statistics(y = test_data$y)
  limits <- limits_from_statistics(chart, statistics, test_data)

  expect_equal(statistics$cl, test_cp_limit_answer$cl)
  expect_equal(limits$lcl, test_cp_limit_answer$lcl)
  expect_equal(limits$ucl, test_cp_limit_answer$ucl)
})
